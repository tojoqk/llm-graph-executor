#lang typed/racket

(require graph-executor/graph
         "../llm/llm.rkt")

(provide llm-node-maker make-llm-edge make-llm-bridge
         node-role edge-role)

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

(: make-llm-bridge (All (T1 S1 T2 S2)
                        (-> String
                            [#:mode (Option EdgeMode)]
                            #:dom (Node T1 S1)
                            #:cod (Node T2 S2)
                            [#:desc (Option String)]
                            [#:when (Option (-> S1 Any))]
                            #:trans (-> S1 S2)
                            [#:priority (Option Integer)]
                            [#:weight (Option Exact-Positive-Integer)]
                            [#:role (Option Role)]
                            (Bridge T1 S1 T2 S2))))
(define (make-llm-bridge name
                         #:mode [mode #f]
                         #:dom dom
                         #:cod cod
                         #:desc [desc #f]
                         #:when [when #f]
                         #:trans tr
                         #:priority [priority #f]
                         #:weight [weight #f]
                         #:role [role #f])
  ((inst make-bridge T1 S1 T2 S2) name
                                  #:mode mode
                                  #:dom dom
                                  #:cod cod
                                  #:desc desc
                                  #:when when
                                  #:trans tr
                                  #:priority priority
                                  #:weight weight
                                  #:attributes ((inst hash Symbol Any)
                                                'role (or role (current-default-role)))))

(: make-llm-edge (All (T S)
                      (-> String
                          [#:mode (Option EdgeMode)]
                          #:dom (Node T S)
                          #:cod (Node T S)
                          [#:desc (Option String)]
                          [#:when (Option (-> S Any))]
                          [#:trans (Option (-> S S))]
                          [#:priority (Option Integer)]
                          [#:weight (Option Exact-Positive-Integer)]
                          [#:role (Option Role)]
                      (Edge T S))))
(define (make-llm-edge name
                       #:mode [mode #f]
                       #:dom dom
                       #:cod cod
                       #:desc [desc #f]
                       #:when [when #f]
                       #:trans [tr #f]
                       #:priority [priority #f]
                       #:weight [weight #f]
                       #:role [role #f])
  ((inst make-llm-bridge T S T S) name
                                  #:mode mode
                                  #:dom dom
                                  #:cod cod
                                  #:desc desc
                                  #:when when
                                  #:trans (or tr (inst identity S))
                                  #:priority priority
                                  #:weight weight
                                  #:role role))

(: bridge-role (All (T1 S1 T2 S2) (-> (Bridge T1 S1 T2 S2) Role)))
(define (bridge-role n)
  (cond [(hash-ref (edge-attributes n) 'role #f)
         => (lambda ([r : Any])
              (cond [(or (eq? r 'user) (eq? r 'assistant)) r]
                    [else (current-default-role)]))]
        [else (current-default-role)]))

(: edge-role (All (T S) (-> (Edge T S) Role)))
(define (edge-role n)
  ((inst bridge-role T S T S) n))
