#lang info
(define collection "llm-graph-executor")
(define deps '("base" "typed-racket-lib" "typed-racket-more" "https://github.com/tojoqk/graph-executor.git"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/llm-graph-executor.scrbl" ())))
(define pkg-desc "An LLM plugin for graph-executor")
(define version "0.0")
(define pkg-authors '(tojoqk))
(define license '(Apache-2.0))
