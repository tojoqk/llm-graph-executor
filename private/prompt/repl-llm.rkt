#lang typed/racket

(require graph-executor/prompt)
(require graph-executor/executor/repl)
(require "../../llm/api.rkt")
(require "../../llm.rkt")
(require typed/json)

(provide llm-prompt)

(: llm-prompt (All (A) (-> (-> String (Option String) Void) (Listof LLM-Message) (Prompt A))))
(define ((llm-prompt set-prompt msgs) title op [_ (hash)])
  (define-values (value prompt-text reasoning)
    (case (car op)
      [(choose) ((inst llm-choose A) msgs title op)]
      [(string) (llm-string msgs title op)]
      [(integer natural positive) (llm-input-number msgs title op)]
      [(range) (llm-range msgs title op)]
      [(random) (llm-random title op)]))
  (set-prompt prompt-text reasoning)
  value)

(: llm-choose (All (A)
                   (-> (Listof LLM-Message)
                       String (List 'choose
                                    (-> Any Boolean : #:+ A)
                                    (Listof (U (∩ String A)
                                               (List (∩ String A) String))))
                       (Values (∩ String A) String String))))
(define (llm-choose msgs title op)
  (: choice->item (-> (U (∩ String A) (List (∩ String A) String))
                      (∩ String A)))
  (define (choice->item c) (if (pair? c) (car c) c))
  (let* ([choices (third op)]
         [items : (Listof String) (map choice->item choices)]
         [out : Output-Port (open-output-string)])
    (fprintf out "* ~a\n" title)
    (for ([choice choices])
      (if (pair? choice)
          (cond [(car choice)
                 => (lambda ([target : String])
                      (fprintf out "- ~a: ~a\n" (car choice) (cadr choice)))])
          (fprintf out "  - ~a\n" (choice->item choice))))
    (let ([text (get-output-string out)])
      (: schema JSExpr)
      (define schema
        (hash 'type "object"
              'properties (hash '1_reasoning (hash 'type "string")
                                '2_choice (hash 'type "string"
                                                'enum items))
              'required (list "1_reasoning" "2_choice")
              'additionalProperties #f))
      (display text)
      (with-retry (current-llm-prompt-retry-count)
        (let* ([response (assert (request-llm schema (cons (list 'system text) msgs))
                                 hash?)]
               [choice (assert (hash-ref response '2_choice) string?)]
               [reasoning (assert (hash-ref response '1_reasoning) string?)])
          (assert choice (second op))
          (printf "> ~a\n(reasoning: ~a)\n\n" choice reasoning)
          (values choice text reasoning))))))

(: llm-string (-> (Listof LLM-Message)
                  String (List 'string)
                  (Values String String (Option String))))
(define (llm-string msgs title op)
  (: schema JSExpr)
  (define schema
    (hash 'type "object"
          'properties (hash '1_reasoning (hash 'type "string")
                            '2_content (hash 'type "string"))
          'required (list "1_reasoning" "2_content")
          'additionalProperties #f))
  (printf "* ~a\n" title)
  (with-retry (current-llm-prompt-retry-count)
    (let* ([response (assert (request-llm schema (cons (list 'system title) msgs))
                             hash?)]
           [content (assert (hash-ref response '2_content) string?)]
           [reasoning (assert (hash-ref response '1_reasoning) string?)])
      (printf "> ~a\n(reasoning: ~a)\n\n" content reasoning)
      (values content title reasoning))))

(: llm-input-number (case-> (-> (Listof LLM-Message) String (List 'integer)
                                (Values Integer String (Option String)))
                            (-> (Listof LLM-Message) String (List 'natural)
                                (Values Natural String (Option String)))
                            (-> (Listof LLM-Message) String (List 'positive)
                                (Values Positive-Integer String (Option String)))))
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
                                                         [(positive) '(minimum 1)]))))
          'required (list  "1_reasoning" "2_content")
          'additionalProperties #f))
  (printf "* ~a\n" title)
  (with-retry (current-llm-prompt-retry-count)
    (let* ([response (assert (request-llm schema (cons (list 'system title) msgs))
                             hash?)]
           [content (assert (hash-ref response '2_content) exact?)]
           [reasoning (assert (hash-ref response '1_reasoning) string?)])
      (case (car op)
        [(integer) (assert content integer?)]
        [(natural) (assert content natural?)]
        [(positive) (assert content positive-integer?)])
      (printf "> ~a\n(reasoning: ~a)\n\n" content reasoning)
      (values content title reasoning))))

(: llm-range (case-> (-> (Listof LLM-Message) String (List 'range Positive-Integer Positive-Integer)
                         (Values Positive-Integer String (Option String)))
                     (-> (Listof LLM-Message) String (List 'range Natural Natural)
                         (Values Natural String (Option String)))
                     (-> (Listof LLM-Message) String (List 'range Integer Integer)
                         (Values Integer String (Option String)))))
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
  (with-retry (current-llm-prompt-retry-count)
   (let* ([response (assert (request-llm schema (cons (list 'system title) msgs))
                            hash?)]
          [content (assert (assert (hash-ref response '2_content) exact?) integer?)]
          [reasoning (assert (hash-ref response '1_reasoning) string?)])
     (printf "> ~a\n(reasoning: ~a)\n\n" content reasoning)
     (if (and (<= (second op) content)
              (<= content (third op)))
         (values content title reasoning)
         (error 'llm-range "range error")))))

(: llm-random (-> String (List 'random Positive-Integer) (Values Natural String (Option String))))
(define (llm-random title op)
  (let ([r (random (second op))])
    (case (current-repl-random-prompt-mode)
      [(verbose)
       (printf "* ~a\n" title)
       (printf "(random) > ~a\n" r)])
    (values r title #f)))


(: call-with-retry (All (A B C) (-> Natural (-> (Values A B C))
                                (Values A B C))))
(define (call-with-retry n proc)
  (let retry ([c : Natural n])
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (printf "response-llm error: ~a\n" e)
                       (if (positive? c)
                           (retry (sub1 c))
                           (error 'llm-prompt "exeeds retry count")))])
      (proc))))

(define-syntax with-retry
  (syntax-rules ()
    [(_ n expr expr* ...)
     (call-with-retry n (lambda () expr expr* ...))]))

(: current-llm-prompt-retry-count (Parameterof Natural))
(define current-llm-prompt-retry-count (make-parameter 10))
