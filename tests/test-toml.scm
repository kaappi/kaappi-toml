(import (scheme base) (scheme write)
        (kaappi toml))

(define pass 0)
(define fail 0)

(define-syntax check
  (syntax-rules (=>)
    ((_ expr => expected)
     (let ((result expr) (exp expected))
       (if (equal? result exp)
           (set! pass (+ pass 1))
           (begin
             (set! fail (+ fail 1))
             (display "FAIL: ") (write 'expr)
             (display " => ") (write result)
             (display ", expected ") (write exp)
             (newline)))))))

;; --- Basic key-value pairs ---

(display "Basic key-value pairs\n")

(check (toml-read-string "key = \"value\"")
  => '(("key" . "value")))

(check (toml-read-string "num = 42")
  => '(("num" . 42)))

(check (toml-read-string "pi = 3.14")
  => '(("pi" . 3.14)))

(check (toml-read-string "flag = true")
  => '(("flag" . #t)))

(check (toml-read-string "flag = false")
  => '(("flag" . #f)))

;; --- Multiple keys ---

(display "Multiple keys\n")

(let ((result (toml-read-string "a = 1\nb = 2\nc = 3")))
  (check (toml-ref result "a") => 1)
  (check (toml-ref result "b") => 2)
  (check (toml-ref result "c") => 3))

;; --- Comments ---

(display "Comments\n")

(check (toml-read-string "# comment\nkey = 42\n# another")
  => '(("key" . 42)))

(check (toml-read-string "key = 42 # inline comment")
  => '(("key" . 42)))

;; --- String types ---

(display "Strings\n")

(check (toml-read-string "s = \"hello\\nworld\"")
  => '(("s" . "hello\nworld")))

(check (toml-read-string "s = 'no \\escape'")
  => '(("s" . "no \\escape")))

(check (toml-read-string "s = \"\"\"multi\nline\"\"\"")
  => '(("s" . "multi\nline")))

(check (toml-read-string "s = '''raw\nmulti'''")
  => '(("s" . "raw\nmulti")))

;; --- Numbers ---

(display "Numbers\n")

(check (toml-read-string "n = 1_000") => '(("n" . 1000)))
(check (toml-read-string "n = -17") => '(("n" . -17)))
(check (toml-read-string "n = +99") => '(("n" . 99)))
(check (toml-read-string "n = 0xff") => '(("n" . 255)))
(check (toml-read-string "n = 0o77") => '(("n" . 63)))
(check (toml-read-string "n = 0b1010") => '(("n" . 10)))

;; --- Special floats ---

(display "Special floats\n")

(let ((r (toml-read-string "x = inf")))
  (check (infinite? (toml-ref r "x")) => #t))

(let ((r (toml-read-string "x = nan")))
  (check (nan? (toml-ref r "x")) => #t))

;; --- Arrays ---

(display "Arrays\n")

(check (toml-read-string "a = [1, 2, 3]")
  => '(("a" . (1 2 3))))

(check (toml-read-string "a = [\"x\", \"y\"]")
  => '(("a" . ("x" "y"))))

(check (toml-read-string "a = []")
  => '(("a" . ())))

(check (toml-read-string "a = [\n  1,\n  2,\n  # comment\n  3,\n]")
  => '(("a" . (1 2 3))))

;; --- Inline tables ---

(display "Inline tables\n")

(check (toml-read-string "t = {a = 1, b = \"two\"}")
  => '(("t" . (("a" . 1) ("b" . "two")))))

;; --- Tables ---

(display "Tables\n")

(let ((r (toml-read-string "[server]\nhost = \"localhost\"\nport = 8080")))
  (check (toml-ref* r "server" "host") => "localhost")
  (check (toml-ref* r "server" "port") => 8080))

(let ((r (toml-read-string "[a.b]\nx = 1")))
  (check (toml-ref* r "a" "b" "x") => 1))

;; --- Array of tables ---

(display "Array of tables\n")

(let ((r (toml-read-string "[[fruits]]\nname = \"apple\"\n\n[[fruits]]\nname = \"banana\"")))
  (let ((fruits (toml-ref r "fruits")))
    (check (length fruits) => 2)
    (check (toml-ref (car fruits) "name") => "apple")
    (check (toml-ref (cadr fruits) "name") => "banana")))

;; --- toml-ref / toml-ref* ---

(display "toml-ref*\n")

(let ((r (toml-read-string "[database]\nhost = \"db.local\"\nport = 5432\n[database.pool]\nmax = 10")))
  (check (toml-ref* r "database" "host") => "db.local")
  (check (toml-ref* r "database" "pool" "max") => 10)
  (check (toml-ref* r "nonexistent") => #f))

;; --- Dates (preserved as strings) ---

(display "Dates\n")

(let ((r (toml-read-string "d = 2024-01-15")))
  (check (toml-ref r "d") => "2024-01-15"))

(let ((r (toml-read-string "d = 2024-01-15T10:30:00Z")))
  (check (toml-ref r "d") => "2024-01-15T10:30:00Z"))

;; --- Round-trip (write then read) ---

(display "Round-trip\n")

(let* ((original '(("name" . "kaappi")
                    ("version" . "0.2.0")
                    ("debug" . #f)
                    ("ports" . (8080 8443))
                    ("database" . (("host" . "localhost")
                                   ("port" . 5432)))))
       (serialized (toml-write-string original))
       (parsed (toml-read-string serialized)))
  (check (toml-ref parsed "name") => "kaappi")
  (check (toml-ref parsed "version") => "0.2.0")
  (check (toml-ref parsed "debug") => #f)
  (check (toml-ref parsed "ports") => '(8080 8443))
  (check (toml-ref* parsed "database" "host") => "localhost")
  (check (toml-ref* parsed "database" "port") => 5432))

;; --- Summary ---

(newline)
(display pass) (display " passed, ")
(display fail) (display " failed\n")
(when (> fail 0) (exit 1))
