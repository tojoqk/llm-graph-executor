#lang typed/racket

(require graph-executor/prompt)
(require "../../llm/api.rkt")
(require "../../llm/llm.rkt")
(require typed/json)

(provide llm-prompt)

(: llm-prompt (All (A) (-> (-> String (Option String) Void) (Listof Message) (Prompt A))))
(define ((llm-prompt set-prompt msgs) title op [_ (hash)])
  (define-values (value prompt-text reasoning)
    (case (car op)
      [(const) ((inst llm-const A) title op)]
      [(choose) ((inst llm-choose A) msgs title op)]
      [(string) (llm-string msgs title op)]
      [(integer natural positive) (llm-input-number msgs title op)]
      [(range) (llm-range msgs title op)]
      [(random) (llm-random title op)]))
  (set-prompt prompt-text reasoning)
  value)

(: llm-choose (All (A)
                   (-> (Listof Message)
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
              'properties (hash 'reasoning (hash 'type "string")
                                'choice (hash 'type "string"
                                              'enum items))
              'required (list "reasoning" "choice")
              'additionalProperties #f))
      (display text)
      (let* ([response (assert (request-llm schema (cons (list 'system text) msgs))
                               hash?)]
             [choice (assert (hash-ref response 'choice) string?)]
             [reasoning (assert (hash-ref response 'reasoning) string?)])
        (printf "> ~a\n\n(reasoning: ~a)\n" choice reasoning)
        (values (assert choice (second op)) text reasoning)))))

(: llm-string (-> (Listof Message)
                  String (List 'string)
                  (Values String String (Option String))))
(define (llm-string msgs title op)
  (: schema JSExpr)
  (define schema
    (hash 'type "object"
          'properties (hash 'content (hash 'type "string")
                            'reasoning (hash 'type "string"))
          'required (list "content" "reasoning")
          'additionalProperties #f))
  (printf "* ~a\n" title)
  (let* ([response (assert (request-llm schema (cons (list 'system title) msgs))
                           hash?)]
         [content (assert (hash-ref response 'content) string?)]
         [reasoning (assert (hash-ref response 'reasoning) string?)])
    (printf "> ~a\n\n(reasoning: ~a)\n" content reasoning)
    (values content title reasoning)))

(: llm-input-number (case-> (-> (Listof Message) String (List 'integer)
                                (Values Integer String (Option String)))
                            (-> (Listof Message) String (List 'natural)
                                (Values Natural String (Option String)))
                            (-> (Listof Message) String (List 'positive)
                                (Values Positive-Integer String (Option String)))))
(define (llm-input-number msgs title op)
  (: schema JSExpr)
  (define schema
    (hash 'type "object"
          'properties (hash 'content (apply hash `(type
                                                   "number"
                                                   ,@(case (car op)
                                                       [(integer) '()]
                                                       [(natural) '(minimum 0)]
                                                       [(positive) '(minimum 1)])))
                            'reasoning (hash 'type "string"))
          'required (list "content" "reasoning")
          'additionalProperties #f))
  (printf "* ~a\n" title)
  (let* ([response (assert (request-llm schema (cons (list 'system title) msgs))
                           hash?)]
         [content (assert (hash-ref response 'content) string?)]
         [reasoning (assert (hash-ref response 'reasoning) string?)])
    (printf "> ~a\n\n(reasoning: ~a)\n" content reasoning)
    (assert content exact?)
    (values (case (car op)
              [(integer) (assert content integer?)]
              [(natural) (assert content natural?)]
              [(positive) (assert content positive-integer?)])
            title
            reasoning)))

(: llm-range (case-> (-> (Listof Message) String (List 'range 'from Natural 'to Natural)
                          (Values Natural String (Option String)))
                      (-> (Listof Message) String (List 'range 'from Integer 'to Integer)
                          (Values Integer String (Option String)))))
(define (llm-range msgs title op)
  (: schema JSExpr)
  (define schema
    (hash 'type "object"
          'properties (hash 'content (hash 'type "number"
                                           'minimum (third op)
                                           'maximum (fifth op))
                            'reasoning (hash 'type "string"))
          'required (list "content" "reasoning")
          'additionalProperties #f))
  (printf "* ~a\n" title)
  (let* ([response (assert (request-llm schema (cons (list 'system title) msgs))
                           hash?)]
         [content (assert (hash-ref response 'content) string?)]
         [reasoning (assert (hash-ref response 'reasoning) string?)])
    (printf "> ~a\n\n(reasoning: ~a)\n" content reasoning)
    (assert content exact?)
    (assert content integer?)
    (if (and (<= (third op) content)
             (<= content (fifth op)))
        (values content title reasoning)
        (error 'llm-range "range error"))))

(: llm-const (All (A)
                  (-> String
                      (List 'const (-> Any Boolean : #:+ A) (∩ A Prompt-Value))
                      (Values (∩ A Prompt-Value) String (Option String)))))
(define (llm-const title op)
  (printf "* ~a\n> ~a\n" title (third op))
  (values (assert (third op) (second op)) title #f))

(: llm-random (-> String (List 'random Positive-Integer) (Values Natural String (Option String))))
(define (llm-random title op)
  (printf "* ~a\n" title)
  (let ([r (random (second op))])
    (printf "(random) > ~a\n" r)
    (values r title #f)))
