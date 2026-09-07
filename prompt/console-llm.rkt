#lang typed/racket

(require graph-executor/plugin/prompt)
(require "../llm/api.rkt")
(require "../llm.rkt")
(require typed/json)

(provide console-llm-prompt)

(: console-llm-prompt (-> (Listof LLM-Message) Prompt-Implementation))
(define ((console-llm-prompt msgs) info op)
  (case (car op)
    [(choose) (llm-choose msgs info op)]
    [(string) (llm-string msgs info op)]
    [(integer natural positive-integer) (llm-input-number msgs info op)]
    [(between) (llm-between msgs info op)]
    [(random) (llm-random info op)]))

(: llm-choose (-> (Listof LLM-Message)
                  Prompt-Info (U (List 'choose Procedure (Listof Symbol) (-> Symbol String)))
                  (Values Symbol Any)))
(define (llm-choose msgs info op)
  (let* ([choices (third op)]
         [show (fourth op)]
         [out : Output-Port (open-output-string)])
    (fprintf out "* ~a\n" (prompt-info-title info))
    (for ([choice choices])
      (fprintf out "- ~a\n" (show choice)))
    (let ([text (get-output-string out)])
      (: schema JSExpr)
      (define schema
        (hash 'type "object"
              'properties (hash '1_reasoning (hash 'type "string")
                                '2_choice (hash 'type "string"
                                                'enum (map show choices)))
              'required (list "1_reasoning" "2_choice")
              'additionalProperties #f))
      (display text)
      (with-retry (current-console-llm-prompt-retry-count)
        (let* ([response (assert (request-llm schema (cons (list 'system text) msgs))
                                 hash?)]
               [content (assert (hash-ref response '2_choice) string?)]
               [reasoning (assert (hash-ref response '1_reasoning) string?)])
          (cond [(findf (lambda ([choice : Symbol])
                          (string=? (show choice) content))
                        choices)
                 => (lambda ([choice : Symbol])
                      (printf "> ~a\n(reasoning: ~a)\n\n" choice reasoning)
                      (values choice `((llm-reasoning . ,reasoning))))]
                [else (error 'llm-choose "~a is not found" content)]))))))

(: llm-string (-> (Listof LLM-Message)
                  Prompt-Info (List 'string)
                  (Values String Any)))
(define (llm-string msgs info op)
  (: schema JSExpr)
  (define schema
    (hash 'type "object"
          'properties (hash '1_reasoning (hash 'type "string")
                            '2_content (hash 'type "string"))
          'required (list "1_reasoning" "2_content")
          'additionalProperties #f))
  (printf "* ~a\n" (prompt-info-title info))
  (with-retry (current-console-llm-prompt-retry-count)
    (let* ([response (assert (request-llm schema (cons (list 'system (prompt-info-title info)) msgs))
                             hash?)]
           [content (assert (hash-ref response '2_content) string?)]
           [reasoning (assert (hash-ref response '1_reasoning) string?)])
      (printf "> ~a\n(reasoning: ~a)\n\n" content reasoning)
      (values content `((llm-reasoning . ,reasoning))))))

(: llm-input-number (case-> (-> (Listof LLM-Message) Prompt-Info (List 'integer)
                                (Values Integer Any))
                            (-> (Listof LLM-Message) Prompt-Info (List 'natural)
                                (Values Natural Any))
                            (-> (Listof LLM-Message) Prompt-Info (List 'positive-integer)
                                (Values Positive-Integer Any))))
(define (llm-input-number msgs meta op)
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
  (printf "* ~a\n" (prompt-info-title meta))
  (with-retry (current-console-llm-prompt-retry-count)
    (let* ([response (assert (request-llm schema (cons (list 'system (prompt-info-title meta)) msgs)) hash?)]
           [content (assert (hash-ref response '2_content) exact?)]
           [reasoning (assert (hash-ref response '1_reasoning) string?)])
      (begin0
          (case (car op)
            [(integer)
             (assert content integer?)
             (values content `((llm-reasoning . ,reasoning)))]
            [(natural)
             (assert content natural?)
             (values content `((llm-reasoning . ,reasoning)))]
            [(positive-integer)
             (assert content positive-integer?)
             (values content `((llm-reasoning . ,reasoning)))])
        (printf "> ~a\n(reasoning: ~a)\n\n" content reasoning)))))

(: llm-between (case-> (-> (Listof LLM-Message) Prompt-Info (List 'between Positive-Integer Positive-Integer) (Values Positive-Integer Any))
                     (-> (Listof LLM-Message) Prompt-Info (List 'between Natural Natural) (Values Natural Any))
                     (-> (Listof LLM-Message) Prompt-Info (List 'between Integer Integer) (Values Integer Any))))
(define (llm-between msgs meta op)
  (: schema JSExpr)
  (define schema
    (hash 'type "object"
          'properties (hash '1_reasoning (hash 'type "string")
                            '2_content (hash 'type "number"
                                             'minimum (second op)
                                             'maximum (third op)))
          'required (list  "1_reasoning" "2_content")
          'additionalProperties #f))
  (printf "* ~a\n" (prompt-info-title meta))
  (with-retry (current-console-llm-prompt-retry-count)
    (let* ([response (assert (request-llm schema (cons
                                                  (list 'system (format "* ~a\n(~a..~a)?"
                                                                        (prompt-info-title meta)
                                                                        (second op)
                                                                        (third op)))
                                                  msgs)) hash?)]
           [content (assert (assert (hash-ref response '2_content) exact?) integer?)]
           [reasoning (assert (hash-ref response '1_reasoning) string?)])
      (printf "> ~a\n(reasoning: ~a)\n\n" content reasoning)
      (if (and (<= (second op) content)
               (<= content (third op)))
          (values content `((llm-reasoning . ,reasoning)))
          (error 'llm-between "between error")))))

(: llm-random (-> Prompt-Info (List 'random Positive-Integer) (Values Natural Any)))
(define (llm-random meta op)
  (let ([r (random (second op))])
    (values r #f)))

(: call-with-retry (All (A B) (-> Natural (-> (Values A B)) (Values A B))))
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
