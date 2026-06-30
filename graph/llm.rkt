#lang typed/racket

(require graph-executor/graph
         "../llm/llm.rkt")

(provide llm-node-maker node-role)

(: llm-node-maker (All (T S)
                       (-> String
                           (-> String
                               #:type T
                               [#:desc (Option String)]
                               [#:trans (Option (-> S S))]
                               [#:prompt (Option String)]
                               [#:role (Option Role)]
                               (Node T S)))))
(define ((llm-node-maker graph-name) name
                                     #:type type #:desc [desc #f] #:trans [tr #f]
                                     #:prompt [pmt #f]
                                     #:role [role #f])
  (((inst node-maker T S) graph-name) name
                                      #:type type #:desc desc #:trans tr
                                      #:prompt pmt
                                      #:attributes ((inst hash Symbol Any)
                                                    'role
                                                    (or role (current-default-role)))))

(: node-role (All (T S) (-> (Node T S) Role)))
(define (node-role n)
  (cond [(hash-ref (node-attributes n) 'role #f)
         => (lambda ([r : Any])
              (cond [(or (eq? r 'user) (eq? r 'assistant)) r]
                    [else (current-default-role)]))]
        [else (current-default-role)]))
