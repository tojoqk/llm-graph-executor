#lang typed/racket

(require graph-executor/prompt)
(require graph-executor/executor/console)
(require "../llm/api.rkt")
(require "../llm.rkt")
(require typed/json)

(provide console-llm-prompt)

(: console-llm-prompt (-> (Listof LLM-Message) Prompt-Implementation))
(define ((console-llm-prompt msgs) title op)
  (case (car op)
    [(choose) (llm-choose msgs title op)]
    [(string) (llm-string msgs title op)]
    [(integer natural positive-integer) (llm-input-number msgs title op)]
    [(range) (llm-range msgs title op)]
    [(random) (llm-random title op)]))

(: llm-choose (-> (Listof LLM-Message)
                  String (U (List 'choose Procedure (Listof String))
                            (List 'choose (Listof String)))
                  Prompt-Info-Choose))
(define (llm-choose msgs title op)
  (let* ([choices (if (procedure? (second op))
                      (third op)
                      (second op))]
         [out : Output-Port (open-output-string)])
    (fprintf out "* ~a\n" title)
    (for ([choice choices])
      (fprintf out "- ~a\n" choice))
    (let ([text (get-output-string out)])
      (: schema JSExpr)
      (define schema
        (hash 'type "object"
              'properties (hash '1_reasoning (hash 'type "string")
                                '2_choice (hash 'type "string"
                                                'enum choices))
              'required (list "1_reasoning" "2_choice")
              'additionalProperties #f))
      (display text)
      (with-retry (current-console-llm-prompt-retry-count)
        (let* ([response (assert (request-llm schema (cons (list 'system text) msgs))
                                 hash?)]
               [choice (assert (hash-ref response '2_choice) string?)]
               [reasoning (assert (hash-ref response '1_reasoning) string?)])
          (cond [(member choice choices)
                 (printf "> ~a\n(reasoning: ~a)\n\n" choice reasoning)
                 (prompt-info-choose title
                                     `((llm-reasoning . ,reasoning))
                                     choice
                                     choices)]
                [else (error 'llm-choose "~a is not found" choice)]))))))

(: llm-string (-> (Listof LLM-Message)
                  String (List 'string)
                  Prompt-Info-String))
(define (llm-string msgs title op)
  (: schema JSExpr)
  (define schema
    (hash 'type "object"
          'properties (hash '1_reasoning (hash 'type "string")
                            '2_content (hash 'type "string"))
          'required (list "1_reasoning" "2_content")
          'additionalProperties #f))
  (printf "* ~a\n" title)
  (with-retry (current-console-llm-prompt-retry-count)
    (let* ([response (assert (request-llm schema (cons (list 'system title) msgs))
                             hash?)]
           [content (assert (hash-ref response '2_content) string?)]
           [reasoning (assert (hash-ref response '1_reasoning) string?)])
      (printf "> ~a\n(reasoning: ~a)\n\n" content reasoning)
      (prompt-info-string title `((llm-reasoning . ,reasoning)) content))))

(: llm-input-number (case-> (-> (Listof LLM-Message) String (List 'integer)
                                Prompt-Info-Integer)
                            (-> (Listof LLM-Message) String (List 'natural)
                                Prompt-Info-Natural)
                            (-> (Listof LLM-Message) String (List 'positive-integer)
                                Prompt-Info-Positive-Integer)))
(define (llm-input-number msgs title op)
  (: schema JSExpr)
  (define schema
    (hash 'type "object"
          'properties (hash '1_reasoning (hash 'type "string")
                            '2_content (apply hash `(type
                                                     "number"
                                                     ,@(case (car op)
                                                         [(integer) '()]
                                                         [(natural) '(minimum 0)]
                                                         [(positive-integer) '(minimum 1)]))))
          'required (list  "1_reasoning" "2_content")
          'additionalProperties #f))
  (printf "* ~a\n" title)
  (with-retry (current-console-llm-prompt-retry-count)
    (let* ([response (assert (request-llm schema (cons (list 'system title) msgs)) hash?)]
           [content (assert (hash-ref response '2_content) exact?)]
           [reasoning (assert (hash-ref response '1_reasoning) string?)])
      (begin0
          (case (car op)
            [(integer)
             (assert content integer?)
             (prompt-info-integer title `((llm-reasoning . ,reasoning)) content)]
            [(natural)
             (assert content natural?)
             (prompt-info-natural title `((llm-reasoning . ,reasoning)) content)]
            [(positive-integer)
             (assert content positive-integer?)
             (prompt-info-positive-integer title `((llm-reasoning . ,reasoning)) content)])
        (printf "> ~a\n(reasoning: ~a)\n\n" content reasoning)))))

(: llm-range (-> (Listof LLM-Message) String (List 'range Integer Integer) Prompt-Info-Range))
(define (llm-range msgs title op)
  (: schema JSExpr)
  (define schema
    (hash 'type "object"
          'properties (hash '1_reasoning (hash 'type "string")
                            '2_content (hash 'type "number"
                                             'minimum (second op)
                                             'maximum (third op)))
          'required (list  "1_reasoning" "2_content")
          'additionalProperties #f))
  (printf "* ~a\n" title)
  (with-retry (current-console-llm-prompt-retry-count)
    (let* ([response (assert (request-llm schema (cons
                                                  (list 'system (format "* ~a\n(~a..~a)?"
                                                                        title
                                                                        (second op)
                                                                        (third op)))
                                                  msgs)) hash?)]
           [content (assert (assert (hash-ref response '2_content) exact?) integer?)]
           [reasoning (assert (hash-ref response '1_reasoning) string?)])
      (printf "> ~a\n(reasoning: ~a)\n\n" content reasoning)
      (if (and (<= (second op) content)
               (<= content (third op)))
          (prompt-info-range title `((llm-reasoning . ,reasoning)) content (second op) (third op))
          (error 'llm-range "range error")))))

(: llm-random (-> String (List 'random Positive-Integer) Prompt-Info-Random))
(define (llm-random title op)
  (let ([r (random (second op))])
    (case (current-console-random-prompt-display)
      [(show)
       (printf "* ~a\n" title)
       (printf "(random) > ~a\n" r)
       (prompt-info-random title '() r (second op))]
      [(hide)
       (prompt-info-random title '() r (second op))])))


(: call-with-retry (All (A) (-> Natural (-> A) A)))
(define (call-with-retry n proc)
  (let retry ([c : Natural n])
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (printf "response-llm error: ~a\n" e)
                       (if (positive? c)
                           (retry (sub1 c))
                           (error 'console-llm-prompt "exeeds retry count")))])
      (proc))))

(define-syntax with-retry
  (syntax-rules ()
    [(_ n expr expr* ...)
     (call-with-retry n (lambda () expr expr* ...))]))

(: current-console-llm-prompt-retry-count (Parameterof Natural))
(define current-console-llm-prompt-retry-count (make-parameter 10))
