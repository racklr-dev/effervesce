#lang racket

;; Probe a battery of Python constructs through racklr's Python3 front end and
;; report, per construct, one of:
;;   OK            parse + lower, no fallback symbols
;;   FALLBACK      lower succeeded but produced `?`-prefixed unhandled symbols
;;   PARSE-FAIL    tokenize ok, parse-file_input returned #f (grammar gap)
;;   RAISE <msg>   tokenize or lower raised (lexer/lowering bug)
;;
;; This is the construct-level catalogue companion to tools/scan-python.rkt.
;; Usage: racket tools/probe-python.rkt

(require racklr/uir
         racklr/lower-python
         "../check/host-python.rkt")

;; Collect the `?`-prefixed unhandled-node symbols in a UIR value.
(define (fallback-symbols n acc)
  (cond
    [(uir-symbol? n)
     (define nm (uir-symbol-name n))
     (if (and (> (string-length nm) 0) (char=? (string-ref nm 0) #\?))
         (cons nm acc)
         acc)]
    [(struct? n)
     (for/fold ([a acc]) ([f (in-list (cdr (vector->list (struct->vector n))))])
       (fallback-symbols f a))]
    [(pair? n)
     (fallback-symbols (cdr n) (fallback-symbols (car n) acc))]
    [else acc]))

(define (probe name src)
  (define status 'OK)
  (define detail #f)
  (with-handlers ([exn:fail? (λ (e) (set! status 'RAISE) (set! detail (exn-message e)))])
    (define tks (py-tokenize src))
    (define res (py-parse-file_input tks 0))
    (if (and (list? res) (= (length res) 2))
        (let* ([uir (lower-python (second res) py-tok-type py-tok-value)]
               [fb (remove-duplicates (fallback-symbols uir '()))])
          (if (pair? fb)
              (begin (set! status 'FALLBACK) (set! detail (sort fb string<?)))
              (void)))
        (set! status 'PARSE-FAIL)))
  (list name status detail))

(define probes
  (list
   ;; ── imports ────────────────────────────────────────────────
   (cons "import-simple"        "import os\n")
   (cons "import-dotted"        "from django.db import models\n")
   (cons "import-as"            "from a import b as c\n")
   (cons "import-star"          "from a.b import *\n")
   (cons "import-paren-single"  "from a.b import (CASCADE, SET)\n")
   (cons "import-paren-multi"   "from a.b import (\n    CASCADE,\n    SET,\n)\n")
   (cons "import-relative"      "from . import views\n")
   (cons "import-relative-dots" "from ...a.b import c\n")

   ;; ── NEWLINE inside brackets (implicit continuation) ────────
   (cons "call-multiline"       "def f():\n    return obj.method(\n        a,\n        b,\n    )\n")
   (cons "dict-multiline"       "d = {\n    \"a\": 1,\n    \"b\": 2,\n}\n")
   (cons "list-multiline"       "x = [\n    1,\n    2,\n]\n")
   (cons "tuple-multiline"      "x = (\n    1,\n    2,\n)\n")
   (cons "set-multiline"        "x = {\n    1,\n    2,\n}\n")
   (cons "cond-multiline"       "if (a and\n        b):\n    pass\n")
   (cons "subscript-multiline"  "x = d[\n    \"k\",\n]\n")
   (cons "call-trailing-comma"  "def f():\n    return g(\n        a,\n    )\n")

   ;; ── line continuation ──────────────────────────────────────
   (cons "backslash-cont"       "x = 1 + \\\n    2\n")

   ;; ── blank / comment / whitespace-only lines (indent machinery) ──
   (cons "blank-line-in-nested"   "def f():\n    if x:\n        a()\n\n        b()\n    else:\n        c()\n")
   (cons "blank-line-top-level"   "a = 1\n\nb = 2\n")
   (cons "comment-line-in-nested" "def f():\n    if x:\n        a()\n        # note\n        b()\n    else:\n        c()\n")
   (cons "ws-only-line-in-nested" "def f():\n    if x:\n        a()\n    \n        b()\n    else:\n        c()\n")

   ;; ── function signatures ────────────────────────────────────
   (cons "def-annotated"        "def f(a: int) -> str:\n    return \"x\"\n")
   (cons "def-kwonly-star"      "def f(a, *, b=None):\n    return b\n")
   (cons "def-positional-only"  "def f(a, /, b):\n    return b\n")
   (cons "def-star-args"        "def f(*args, **kwargs):\n    return g(*args, **kwargs)\n")
   (cons "def-default"          "def f(a=1, b=[2]):\n    return a\n")

   ;; ── modern constructs ──────────────────────────────────────
   (cons "walrus"               "def f():\n    if (n := len(x)) > 0:\n        return n\n")
   (cons "fstring"              "def f():\n    return f\"hi {x}\"\n")
   (cons "fstring-nested"       "def f():\n    return f\"{a!r:>{w}}\"\n")
   (cons "async-def"            "async def f():\n    await g()\n")
   (cons "async-for"            "async def f():\n    async for x in y:\n        pass\n")
   (cons "async-with"           "async def f():\n    async with a as b:\n        pass\n")
   (cons "match-case"           "def f(x):\n    match x:\n        case 1:\n            return 1\n")
   (cons "yield-from"           "def f():\n    yield from g()\n")
   (cons "nonlocal"             "def f():\n    x = 0\n    def g():\n        nonlocal x\n        x = 1\n    return g\n")

   ;; ── expressions ────────────────────────────────────────────
   (cons "lambda"               "f = lambda a, b=1: a + b\n")
   (cons "comprehension"        "x = [i for i in range(10) if i % 2]\n")
   (cons "dict-comp"            "d = {k: v for k, v in items}\n")
   (cons "set-comp"             "s = {i for i in x}\n")
   (cons "genexpr"              "g = (i for i in x)\n")
   (cons "chained-compare"      "def f(a, b, c):\n    return a < b < c\n")
   (cons "slice"                "def f(x):\n    return x[1:2:3]\n")
   (cons "starred-unpack"       "a, *b = items\n")
   (cons "ternary"              "x = a if cond else b\n")
   (cons "attribute-chain"      "def f():\n    return a.b.c.d()\n")
   (cons "power-op"             "x = a ** b\n")
   (cons "matrix-op"            "x = a @ b\n")
   (cons "string-concat"        "x = \"a\" \"b\"\n")

   ;; ── statements ─────────────────────────────────────────────
   (cons "raise-from"           "def f():\n    raise X from e\n")
   (cons "with-multi"           "def f():\n    with a() as x, b() as y:\n        pass\n")
   (cons "try-except-finally"   "def f():\n    try:\n        g()\n    except E as e:\n        pass\n    finally:\n        pass\n")
   (cons "del"                  "def f():\n    del x\n")
   (cons "assert"               "def f():\n    assert x, \"msg\"\n")
   (cons "global"               "x = 0\ndef f():\n    global x\n    x = 1\n")
   (cons "class-bases"          "class A(B, metaclass=M):\n    pass\n")
   (cons "kwarg-soft-keywords"  "def f():\n    return g(case=1, match=2, async=3, await=4)\n")
   (cons "class-body"           "class A(B):\n    x = 1\n    def m(self):\n        return 2\n")

   ;; ── literals / decorators ──────────────────────────────────
   (cons "decorator-args"       "@app.get(\"/x\")\ndef f():\n    return 1\n")
   (cons "decorator-stacked"    "@a\n@b()\ndef f():\n    pass\n")
   (cons "triple-string"        "x = \"\"\"multi\nline\"\"\"\n")
   (cons "bytes-literal"        "x = b\"bytes\"\n")
   (cons "raw-literal"          "x = r\"raw\\n\"\n")
   (cons "numeric-underscore"   "x = 1_000_000\n")
   (cons "complex"              "x = 1 + 2j\n")
   (cons "ellipsis"             "x = ...\n")
   (cons "unicode-ident"        "caf\u00e9 = 1\n")
   (cons "annotation"           "x: int = 1\n")
   (cons "type-alias"           "type T = int | str\n")
   (cons "union-annotation"     "def f(x: int | None) -> str | None:\n    return None\n")

   ;; ── classes (lowering vs parse) ─────────────────────────────
   (cons "class-no-base-pass"   "class A:\n    pass\n")
   (cons "class-single-pass"    "class A(B):\n    pass\n")
   (cons "class-multi-pass"     "class A(B, C):\n    pass\n")
   (cons "class-no-base-body"   "class A:\n    x = 1\n")
   (cons "class-nested-meta"    "class A(B):\n    class Meta:\n        pass\n")
   (cons "class-metaclass-kw"   "class A(B, metaclass=M):\n    pass\n")
   (cons "class-kw-only-base"   "class A(metaclass=M):\n    pass\n")
   (cons "class-star-base"      "class A(*bases):\n    pass\n")
   (cons "class-dstar-base"     "class A(**kw):\n    pass\n")

   ;; ── docstrings / string literals ────────────────────────────
   (cons "def-docstring"        "def f():\n    \"\"\"a\"\"\"\n    return 1\n")
   (cons "def-docstring-multi"  "def f():\n    \"\"\"a\n    b\"\"\"\n    return 1\n")
   (cons "expr-triple-single"   "x = \"\"\"abc\"\"\"\n")
   (cons "expr-triple-multi"    "x = \"\"\"a\nb\"\"\"\n")
   (cons "def-str-literal"      "def f():\n    \"plain\"\n    return 1\n")

   ;; ── numeric literals ────────────────────────────────────────
   (cons "num-decimal"          "x = 12345\n")
   (cons "num-negative"         "x = -5\n")
   (cons "num-hex"              "x = 0xFF\n")
   (cons "num-octal"            "x = 0o17\n")
   (cons "num-binary"           "x = 0b1010\n")
   (cons "num-float"            "x = 1.5\n")
   (cons "num-float-exp"        "x = 1.5e10\n")
   (cons "num-complex"          "x = 1j\n")
   (cons "num-underscore"       "x = 1_000_000\n")
   (cons "num-leading-dot"      "x = .5\n")))

(define (main)
  (printf "construct\tstatus\tdetail\n")
  (for ([p (in-list probes)])
    (match-define (list name status detail) (probe (car p) (cdr p)))
    (printf "~a\t~a\t~a\n" name status
            (if detail (format "~a" detail) ""))))

(main)
