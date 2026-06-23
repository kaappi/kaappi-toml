;;; (kaappi toml) — TOML parser and serializer
;;;
;;; Mapping:
;;;   TOML table       → alist  (("key" . value) ...)
;;;   TOML array       → list   (v1 v2 ...)
;;;   TOML string      → string
;;;   TOML integer     → exact integer
;;;   TOML float       → inexact number (or +inf.0 / -inf.0 / +nan.0)
;;;   TOML boolean     → #t / #f
;;;   TOML datetime    → string (preserved as-is)

(define-library (kaappi toml)
  (import (scheme base) (scheme char) (scheme write))
  (export toml-read toml-read-string
          toml-write toml-write-string
          toml-ref toml-ref*)
  (begin

    ;; ---------------------------------------------------------------
    ;; Public API
    ;; ---------------------------------------------------------------

    (define (toml-read-string str)
      (let ((port (open-input-string str)))
        (let ((val (toml-read port)))
          (close-input-port port)
          val)))

    (define (toml-read . args)
      (let ((port (if (pair? args) (car args) (current-input-port))))
        (parse-toml port)))

    (define (toml-write-string table)
      (let ((port (open-output-string)))
        (toml-write table port)
        (get-output-string port)))

    (define (toml-write table . args)
      (let ((port (if (pair? args) (car args) (current-output-port))))
        (write-table table '() port)))

    (define (toml-ref table key)
      (let ((pair (assoc key table)))
        (if pair (cdr pair) #f)))

    (define (toml-ref* table . keys)
      (let loop ((t table) (ks keys))
        (if (null? ks)
            t
            (let ((pair (assoc (car ks) t)))
              (if pair
                  (loop (cdr pair) (cdr ks))
                  #f)))))

    ;; ---------------------------------------------------------------
    ;; Parser internals
    ;; ---------------------------------------------------------------

    (define (parse-toml port)
      (let ((root '()))
        (let loop ((result root)
                   (current-path '()))
          (skip-ws-and-newlines port)
          (let ((ch (peek-char port)))
            (cond
              ((eof-object? ch)
               (reverse result))
              ((char=? ch #\#)
               (skip-comment port)
               (loop result current-path))
              ((char=? ch #\[)
               (read-char port)
               (let ((is-array (and (not (eof-object? (peek-char port)))
                                    (char=? (peek-char port) #\[))))
                 (when is-array (read-char port))
                 (let ((path (read-key-path port)))
                   (if is-array
                       (begin
                         (expect-char port #\])
                         (expect-char port #\])
                         (skip-to-newline port)
                         (let ((entries (read-inline-entries port)))
                           (loop (deep-merge-array result path entries)
                                 path)))
                       (begin
                         (expect-char port #\])
                         (skip-to-newline port)
                         (let ((entries (read-inline-entries port)))
                           (loop (deep-merge result path entries)
                                 path)))))))
              (else
               (let* ((key (read-key port))
                      (dummy (begin (skip-ws port) (expect-char port #\=) (skip-ws port)))
                      (val (read-value port)))
                 (begin
                   dummy
                   (skip-to-newline port)
                   (if (null? current-path)
                       (loop (cons (cons key val) result) current-path)
                       (loop (deep-merge result current-path
                                         (list (cons key val)))
                             current-path))))))))))

    (define (read-inline-entries port)
      (let loop ((entries '()))
        (skip-ws-and-newlines port)
        (let ((ch (peek-char port)))
          (cond
            ((eof-object? ch) (reverse entries))
            ((char=? ch #\#) (skip-comment port) (loop entries))
            ((char=? ch #\[) (reverse entries))
            (else
             (let* ((key (read-key port))
                    (dummy (begin (skip-ws port) (expect-char port #\=) (skip-ws port)))
                    (val (read-value port)))
               (begin dummy (skip-to-newline port)
                      (loop (cons (cons key val) entries)))))))))

    ;; ---- Key parsing ----

    (define (read-key port)
      (skip-ws port)
      (let ((ch (peek-char port)))
        (cond
          ((char=? ch #\") (read-basic-string port))
          ((char=? ch #\') (read-literal-string port))
          (else (read-bare-key port)))))

    (define (read-key-path port)
      (let loop ((keys (list (read-key port))))
        (skip-ws port)
        (let ((ch (peek-char port)))
          (if (and (not (eof-object? ch)) (char=? ch #\.))
              (begin (read-char port) (skip-ws port)
                     (loop (cons (read-key port) keys)))
              (reverse keys)))))

    (define (read-bare-key port)
      (let loop ((acc '()))
        (let ((ch (peek-char port)))
          (if (and (not (eof-object? ch))
                   (or (char-alphabetic? ch) (char-numeric? ch)
                       (char=? ch #\-) (char=? ch #\_)))
              (begin (read-char port) (loop (cons ch acc)))
              (if (null? acc)
                  (error "toml: expected key" ch)
                  (list->string (reverse acc)))))))

    ;; ---- Value parsing ----

    (define (read-value port)
      (skip-ws port)
      (let ((ch (peek-char port)))
        (cond
          ((eof-object? ch) (error "toml: unexpected end of input"))
          ((char=? ch #\") (read-string-value port))
          ((char=? ch #\') (read-literal-string-value port))
          ((char=? ch #\t) (read-true port))
          ((char=? ch #\f) (read-false port))
          ((char=? ch #\[) (read-array port))
          ((char=? ch #\{) (read-inline-table port))
          (else (read-number-or-date port)))))

    ;; ---- Strings ----

    (define (read-string-value port)
      (read-char port)
      (if (and (not (eof-object? (peek-char port)))
               (char=? (peek-char port) #\"))
          (begin
            (read-char port)
            (if (and (not (eof-object? (peek-char port)))
                     (char=? (peek-char port) #\"))
                (begin (read-char port) (read-multiline-basic-string port))
                ""))
          (read-basic-string-body port)))

    (define (read-basic-string port)
      (read-char port)
      (read-basic-string-body port))

    (define (read-basic-string-body port)
      (let loop ((acc '()))
        (let ((ch (read-char port)))
          (cond
            ((eof-object? ch) (error "toml: unterminated string"))
            ((char=? ch #\") (list->string (reverse acc)))
            ((char=? ch #\\) (loop (cons (read-escape port) acc)))
            (else (loop (cons ch acc)))))))

    (define (read-escape port)
      (let ((ch (read-char port)))
        (cond
          ((char=? ch #\n) #\newline)
          ((char=? ch #\t) #\tab)
          ((char=? ch #\r) #\return)
          ((char=? ch #\\) #\\)
          ((char=? ch #\") #\")
          ((char=? ch #\b) (integer->char 8))
          ((char=? ch #\f) (integer->char 12))
          ((char=? ch #\u) (read-unicode-escape port 4))
          ((char=? ch #\U) (read-unicode-escape port 8))
          (else (error "toml: unknown escape" ch)))))

    (define (read-unicode-escape port n)
      (let loop ((i 0) (acc 0))
        (if (= i n)
            (integer->char acc)
            (let ((ch (read-char port)))
              (loop (+ i 1)
                    (+ (* acc 16) (hex-digit ch)))))))

    (define (hex-digit ch)
      (cond
        ((and (char>=? ch #\0) (char<=? ch #\9))
         (- (char->integer ch) (char->integer #\0)))
        ((and (char>=? ch #\a) (char<=? ch #\f))
         (+ 10 (- (char->integer ch) (char->integer #\a))))
        ((and (char>=? ch #\A) (char<=? ch #\F))
         (+ 10 (- (char->integer ch) (char->integer #\A))))
        (else (error "toml: invalid hex digit" ch))))

    (define (read-multiline-basic-string port)
      (when (and (not (eof-object? (peek-char port)))
                 (char=? (peek-char port) #\newline))
        (read-char port))
      (when (and (not (eof-object? (peek-char port)))
                 (char=? (peek-char port) #\return))
        (read-char port)
        (when (and (not (eof-object? (peek-char port)))
                   (char=? (peek-char port) #\newline))
          (read-char port)))
      (let loop ((acc '()))
        (let ((ch (read-char port)))
          (cond
            ((eof-object? ch) (error "toml: unterminated multiline string"))
            ((char=? ch #\\)
             (let ((next (peek-char port)))
               (if (and (not (eof-object? next))
                        (or (char=? next #\newline) (char=? next #\return)))
                   (begin (skip-ws-and-newlines port) (loop acc))
                   (loop (cons (read-escape port) acc)))))
            ((char=? ch #\")
             (if (and (not (eof-object? (peek-char port)))
                      (char=? (peek-char port) #\"))
                 (begin
                   (read-char port)
                   (if (and (not (eof-object? (peek-char port)))
                            (char=? (peek-char port) #\"))
                       (begin (read-char port)
                              (list->string (reverse acc)))
                       (loop (cons #\" (cons #\" acc)))))
                 (loop (cons #\" acc))))
            (else (loop (cons ch acc)))))))

    (define (read-literal-string-value port)
      (read-char port)
      (if (and (not (eof-object? (peek-char port)))
               (char=? (peek-char port) #\'))
          (begin
            (read-char port)
            (if (and (not (eof-object? (peek-char port)))
                     (char=? (peek-char port) #\'))
                (begin (read-char port) (read-multiline-literal-string port))
                ""))
          (read-literal-string-body port)))

    (define (read-literal-string port)
      (read-char port)
      (read-literal-string-body port))

    (define (read-literal-string-body port)
      (let loop ((acc '()))
        (let ((ch (read-char port)))
          (cond
            ((eof-object? ch) (error "toml: unterminated literal string"))
            ((char=? ch #\') (list->string (reverse acc)))
            (else (loop (cons ch acc)))))))

    (define (read-multiline-literal-string port)
      (when (and (not (eof-object? (peek-char port)))
                 (char=? (peek-char port) #\newline))
        (read-char port))
      (when (and (not (eof-object? (peek-char port)))
                 (char=? (peek-char port) #\return))
        (read-char port)
        (when (and (not (eof-object? (peek-char port)))
                   (char=? (peek-char port) #\newline))
          (read-char port)))
      (let loop ((acc '()))
        (let ((ch (read-char port)))
          (cond
            ((eof-object? ch) (error "toml: unterminated multiline literal"))
            ((char=? ch #\')
             (if (and (not (eof-object? (peek-char port)))
                      (char=? (peek-char port) #\'))
                 (begin
                   (read-char port)
                   (if (and (not (eof-object? (peek-char port)))
                            (char=? (peek-char port) #\'))
                       (begin (read-char port)
                              (list->string (reverse acc)))
                       (loop (cons #\' (cons #\' acc)))))
                 (loop (cons #\' acc))))
            (else (loop (cons ch acc)))))))

    ;; ---- Booleans ----

    (define (read-true port)
      (expect-char port #\t) (expect-char port #\r)
      (expect-char port #\u) (expect-char port #\e)
      #t)

    (define (read-false port)
      (expect-char port #\f) (expect-char port #\a)
      (expect-char port #\l) (expect-char port #\s)
      (expect-char port #\e)
      #f)

    ;; ---- Arrays ----

    (define (read-array port)
      (read-char port)
      (let loop ((acc '()))
        (skip-ws-and-newlines port)
        (let ((ch (peek-char port)))
          (cond
            ((eof-object? ch) (error "toml: unterminated array"))
            ((char=? ch #\]) (read-char port) (reverse acc))
            ((char=? ch #\#) (skip-comment port) (loop acc))
            ((char=? ch #\,) (read-char port) (loop acc))
            (else
             (let ((val (read-value port)))
               (loop (cons val acc))))))))

    ;; ---- Inline tables ----

    (define (read-inline-table port)
      (read-char port)
      (let loop ((acc '()))
        (skip-ws port)
        (let ((ch (peek-char port)))
          (cond
            ((eof-object? ch) (error "toml: unterminated inline table"))
            ((char=? ch #\}) (read-char port) (reverse acc))
            ((char=? ch #\,) (read-char port) (loop acc))
            (else
             (let* ((key (read-key port))
                    (dummy (begin (skip-ws port) (expect-char port #\=)
                                 (skip-ws port)))
                    (val (read-value port)))
               (begin dummy (loop (cons (cons key val) acc)))))))))

    ;; ---- Numbers and dates ----

    (define (read-number-or-date port)
      (let loop ((acc '()))
        (let ((ch (peek-char port)))
          (if (and (not (eof-object? ch))
                   (not (char=? ch #\space)) (not (char=? ch #\tab))
                   (not (char=? ch #\newline)) (not (char=? ch #\return))
                   (not (char=? ch #\,)) (not (char=? ch #\]))
                   (not (char=? ch #\})) (not (char=? ch #\#)))
              (begin (read-char port) (loop (cons ch acc)))
              (let ((s (list->string (reverse acc))))
                (parse-number-or-date s))))))

    (define (parse-number-or-date s)
      (let ((clean (remove-underscores s)))
        (cond
          ((string=? clean "inf") +inf.0)
          ((string=? clean "+inf") +inf.0)
          ((string=? clean "-inf") -inf.0)
          ((string=? clean "nan") +nan.0)
          ((string=? clean "+nan") +nan.0)
          ((string=? clean "-nan") +nan.0)
          ((looks-like-date? clean) clean)
          ((string-prefix? "0x" clean)
           (or (string->number (substring clean 2 (string-length clean)) 16) clean))
          ((string-prefix? "0o" clean)
           (or (string->number (substring clean 2 (string-length clean)) 8) clean))
          ((string-prefix? "0b" clean)
           (or (string->number (substring clean 2 (string-length clean)) 2) clean))
          (else (or (string->number clean) clean)))))

    (define (remove-underscores s)
      (let loop ((i 0) (acc '()))
        (if (= i (string-length s))
            (list->string (reverse acc))
            (let ((ch (string-ref s i)))
              (if (char=? ch #\_)
                  (loop (+ i 1) acc)
                  (loop (+ i 1) (cons ch acc)))))))

    (define (looks-like-date? s)
      (and (>= (string-length s) 10)
           (char-numeric? (string-ref s 0))
           (char-numeric? (string-ref s 1))
           (char-numeric? (string-ref s 2))
           (char-numeric? (string-ref s 3))
           (char=? (string-ref s 4) #\-)))

    (define (string-prefix? prefix s)
      (and (>= (string-length s) (string-length prefix))
           (string=? (substring s 0 (string-length prefix)) prefix)))

    ;; ---- Deep merge for [table] and [[array]] headers ----

    (define (deep-merge root path entries)
      (if (null? path)
          (append root entries)
          (let ((key (car path))
                (rest (cdr path)))
            (let ((existing (assoc key root)))
              (if existing
                  (map (lambda (pair)
                         (if (string=? (car pair) key)
                             (cons key (deep-merge (cdr pair) rest entries))
                             pair))
                       root)
                  (append root
                          (list (cons key
                                     (deep-merge '() rest entries)))))))))

    (define (deep-merge-array root path entries)
      (if (= (length path) 1)
          (let* ((key (car path))
                 (existing (assoc key root)))
            (if existing
                (map (lambda (pair)
                       (if (string=? (car pair) key)
                           (cons key (append (cdr pair) (list entries)))
                           pair))
                     root)
                (append root (list (cons key (list entries))))))
          (let ((key (car path))
                (rest (cdr path)))
            (let ((existing (assoc key root)))
              (if existing
                  (map (lambda (pair)
                         (if (string=? (car pair) key)
                             (cons key (deep-merge-array (cdr pair) rest entries))
                             pair))
                       root)
                  (append root
                          (list (cons key
                                     (deep-merge-array '() rest entries)))))))))

    ;; ---- Helpers ----

    (define (skip-ws port)
      (let loop ()
        (let ((ch (peek-char port)))
          (when (and (not (eof-object? ch))
                     (or (char=? ch #\space) (char=? ch #\tab)))
            (read-char port) (loop)))))

    (define (skip-ws-and-newlines port)
      (let loop ()
        (let ((ch (peek-char port)))
          (when (and (not (eof-object? ch))
                     (or (char=? ch #\space) (char=? ch #\tab)
                         (char=? ch #\newline) (char=? ch #\return)))
            (read-char port) (loop)))))

    (define (skip-comment port)
      (let loop ()
        (let ((ch (read-char port)))
          (unless (or (eof-object? ch) (char=? ch #\newline))
            (loop)))))

    (define (skip-to-newline port)
      (skip-ws port)
      (let ((ch (peek-char port)))
        (cond
          ((eof-object? ch) #t)
          ((char=? ch #\#) (skip-comment port))
          ((char=? ch #\newline) (read-char port))
          ((char=? ch #\return) (read-char port)
           (when (and (not (eof-object? (peek-char port)))
                      (char=? (peek-char port) #\newline))
             (read-char port))))))

    (define (expect-char port expected)
      (let ((ch (read-char port)))
        (unless (and (not (eof-object? ch)) (char=? ch expected))
          (error "toml: expected" expected "got" ch))))

    ;; ---------------------------------------------------------------
    ;; Writer
    ;; ---------------------------------------------------------------

    (define (write-table table path port)
      (let ((simple '()) (subtables '()) (arrays '()))
        (for-each
          (lambda (pair)
            (let ((key (car pair)) (val (cdr pair)))
              (cond
                ((and (list? val) (pair? val) (pair? (car val))
                      (not (null? (car val))) (string? (caar val)))
                 (set! subtables (cons pair subtables)))
                ((and (list? val) (pair? val) (list? (car val))
                      (pair? (car val)) (pair? (caar val))
                      (string? (caaar val)))
                 (set! arrays (cons pair arrays)))
                (else (set! simple (cons pair simple))))))
          table)
        (set! simple (reverse simple))
        (set! subtables (reverse subtables))
        (set! arrays (reverse arrays))
        (when (and (pair? path) (or (pair? simple) (null? table)))
          (write-string "[" port)
          (write-key-path path port)
          (write-string "]\n" port))
        (for-each
          (lambda (pair)
            (write-key-str (car pair) port)
            (write-string " = " port)
            (write-toml-value (cdr pair) port)
            (write-string "\n" port))
          simple)
        (when (and (pair? simple) (or (pair? subtables) (pair? arrays)))
          (write-string "\n" port))
        (for-each
          (lambda (pair)
            (write-table (cdr pair) (append path (list (car pair))) port)
            (write-string "\n" port))
          subtables)
        (for-each
          (lambda (pair)
            (for-each
              (lambda (entry)
                (write-string "[[" port)
                (write-key-path (append path (list (car pair))) port)
                (write-string "]]\n" port)
                (for-each
                  (lambda (kv)
                    (write-key-str (car kv) port)
                    (write-string " = " port)
                    (write-toml-value (cdr kv) port)
                    (write-string "\n" port))
                  entry)
                (write-string "\n" port))
              (cdr pair)))
          arrays)))

    (define (write-key-path path port)
      (let loop ((p path) (first? #t))
        (when (pair? p)
          (unless first? (write-string "." port))
          (write-key-str (car p) port)
          (loop (cdr p) #f))))

    (define (write-key-str key port)
      (if (bare-key? key)
          (write-string key port)
          (begin (write-string "\"" port)
                 (write-escaped-string key port)
                 (write-string "\"" port))))

    (define (bare-key? key)
      (and (> (string-length key) 0)
           (let loop ((i 0))
             (if (= i (string-length key))
                 #t
                 (let ((ch (string-ref key i)))
                   (and (or (char-alphabetic? ch) (char-numeric? ch)
                            (char=? ch #\-) (char=? ch #\_))
                        (loop (+ i 1))))))))

    (define (write-toml-value val port)
      (cond
        ((boolean? val)
         (write-string (if val "true" "false") port))
        ((exact? val)
         (write-string (number->string val) port))
        ((number? val)
         (cond
           ((infinite? val) (write-string (if (positive? val) "inf" "-inf") port))
           ((nan? val) (write-string "nan" port))
           (else (write-string (number->string val) port))))
        ((string? val)
         (write-string "\"" port)
         (write-escaped-string val port)
         (write-string "\"" port))
        ((and (list? val) (null? val))
         (write-string "[]" port))
        ((and (list? val) (pair? val) (pair? (car val)) (string? (caar val)))
         (write-string "{" port)
         (let loop ((pairs val) (first? #t))
           (when (pair? pairs)
             (unless first? (write-string ", " port))
             (write-key-str (caar pairs) port)
             (write-string " = " port)
             (write-toml-value (cdar pairs) port)
             (loop (cdr pairs) #f)))
         (write-string "}" port))
        ((list? val)
         (write-string "[" port)
         (let loop ((items val) (first? #t))
           (when (pair? items)
             (unless first? (write-string ", " port))
             (write-toml-value (car items) port)
             (loop (cdr items) #f)))
         (write-string "]" port))
        (else
         (write-string "\"" port)
         (write-escaped-string (if (string? val) val
                                   (let ((p (open-output-string)))
                                     (write val p)
                                     (get-output-string p)))
                               port)
         (write-string "\"" port))))

    (define (write-escaped-string s port)
      (let loop ((i 0))
        (when (< i (string-length s))
          (let ((ch (string-ref s i)))
            (cond
              ((char=? ch #\\) (write-string "\\\\" port))
              ((char=? ch #\") (write-string "\\\"" port))
              ((char=? ch #\newline) (write-string "\\n" port))
              ((char=? ch #\tab) (write-string "\\t" port))
              ((char=? ch #\return) (write-string "\\r" port))
              (else (write-char ch port)))
            (loop (+ i 1))))))))
