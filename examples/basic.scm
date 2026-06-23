(import (kaappi toml))

(define config (toml-read-string "
[package]
name = \"my-app\"
version = \"1.0.0\"

[server]
host = \"0.0.0.0\"
port = 8080
debug = false

[database]
url = \"sqlite:///app.db\"
pool_size = 5

[[routes]]
path = \"/\"
handler = \"home\"

[[routes]]
path = \"/api\"
handler = \"api\"
"))

(display "App: ") (display (toml-ref* config "package" "name")) (newline)
(display "Port: ") (display (toml-ref* config "server" "port")) (newline)
(display "DB: ") (display (toml-ref* config "database" "url")) (newline)
(display "Routes: ") (display (length (toml-ref config "routes"))) (newline)
(for-each
  (lambda (route)
    (display "  ") (display (toml-ref route "path"))
    (display " -> ") (display (toml-ref route "handler"))
    (newline))
  (toml-ref config "routes"))
