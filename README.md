# kaappi-toml

TOML parser and serializer for [Kaappi Scheme](https://github.com/kaappi/kaappi).

Pure Scheme — no C dependencies, no build step.

## Install

```bash
thottam install kaappi-toml
```

## Quick start

```scheme
(import (kaappi toml))

(define config (toml-read-string "
[server]
host = \"localhost\"
port = 8080

[database]
url = \"sqlite:///app.db\"
"))

(toml-ref* config "server" "port")    ;=> 8080
(toml-ref* config "database" "url")   ;=> "sqlite:///app.db"
```

## API

### Reading

```scheme
(toml-read [port])           ; parse TOML from port (default: current-input-port)
(toml-read-string string)    ; parse TOML from string
```

### Writing

```scheme
(toml-write table [port])    ; serialize to port (default: current-output-port)
(toml-write-string table)    ; serialize to string
```

### Access helpers

```scheme
(toml-ref table key)          ; lookup key in table, returns #f if missing
(toml-ref* table key ...)     ; nested lookup: (toml-ref* t "a" "b" "c")
```

## Type mapping

| TOML type | Scheme type |
|-----------|-------------|
| String | string |
| Integer | exact integer |
| Float | inexact number |
| Boolean | `#t` / `#f` |
| Array | list |
| Table | alist `(("key" . value) ...)` |
| Datetime | string (preserved as-is) |
| inf / nan | `+inf.0` / `+nan.0` |

## Supported TOML features

- Basic and literal strings (single and multiline)
- Integers (decimal, hex `0x`, octal `0o`, binary `0b`, underscores)
- Floats (including `inf`, `nan`)
- Booleans
- Arrays (including multiline with comments)
- Inline tables
- Tables (`[table]`, `[dotted.key]`)
- Array of tables (`[[array]]`)
- Comments (`#`)
- Datetimes (preserved as strings)
- Unicode escapes (`\uXXXX`, `\UXXXXXXXX`)

## License

MIT
