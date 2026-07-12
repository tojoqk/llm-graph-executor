#lang typed/racket

(require graph-executor/graph)
(require graph-executor/prompt)
(require graph-executor/prompt/console)
(require graph-executor/executor)
(require graph-executor/executor/console)
(require graph-executor/journal)
(require graph-executor/history)
(require graph-executor/message)
(require graph-executor/event-logger)
(require graph-executor/journal)
(require "../prompt/console-llm.rkt")
(require "../llm.rkt")

(provide console-llm-run)

(: console-llm-run (All (T S) (-> (Listof (Graph T S)) (Node T S) S
                                  [#:llm-role (-> T LLM-Role)]
                                  [#:llm-messages (-> LLM-Role
                                                      (History-Record T S)
                                                      (Listof LLM-Message))]
                                  [#:journal Journal]
                                  (Values (Node T S) S Journal))))
(define (console-llm-run gs entry initial-state
                         #:llm-role [type->llm-role (const 'assistant)]
                         #:llm-messages [role-record->llm-messages
                                         (inst default-llm-messages T S)]
                         #:journal [j '()])
  (: record->llm-role (-> (History-Record T S) LLM-Role))
  (define (record->llm-role rec)
    (case (car rec)
      [(node) (type->llm-role (node-type (history-node-node (cdr rec))))]
      [(auto choose) (type->llm-role (node-type (edge-dom (history-edge-edge (cdr rec)))))]))
  (: record->llm-messages (-> (History-Record T S) (Listof LLM-Message)))
  (define (record->llm-messages rec)
    (role-record->llm-messages (record->llm-role rec) rec))
  (: history->llm-messages (-> (History T S) (Listof LLM-Message)))
  (define (history->llm-messages h)
    (append-map record->llm-messages h))
  (define-values (n st h) (replay gs entry initial-state j))
  (let loop ([n n]
             [st st]
             [h h])
    (define (terminate)
      (when (current-console-trace-display?)
        (displayln ">> Terminated"))
      (values n st (history->journal h)))
    (let ([ne (next-edges gs st n)])
      (case (car ne)
        [(terminated) (terminate)]
        [(auto)
         (let* ([chosen-edge (auto-choose ne)]
                [logger (make-event-logger chosen-edge (edge-cod chosen-edge))])
           (when (current-console-trace-display?)
             (displayln (format ">> [Auto] ~a" (edge-name chosen-edge))))
           (let ([next-st (console-llm-step st chosen-edge logger
                                            h type->llm-role history->llm-messages)])
             (loop (edge-cod chosen-edge)
                   next-st
                   (list* (event-logger->history-node logger)
                          (event-logger->history-edge logger)
                          h))))]
        [(choose)
         (let-values ([(chosen-edge attrs) (console-choose ne)])
           (let* ([logger (make-event-logger chosen-edge
                                             (second ne)
                                             attrs
                                             (edge-cod chosen-edge))]
                  [next-st (console-llm-step st chosen-edge logger
                                             h type->llm-role history->llm-messages)])
             (loop (edge-cod chosen-edge)
                   next-st
                   (list* (event-logger->history-node logger)
                          (event-logger->history-edge logger)
                          h))))]))))

(: console-llm-step (All (T S) (-> S (Edge T S) (Event-Logger T S) (History T S) (-> T LLM-Role) (-> (History T S) (Listof LLM-Message)) S)))
(define (console-llm-step st e logger h type->role history->messages)
  (: message-with-log (-> (U 'node 'edge) (-> Any Void)))
  (define ((message-with-log type) val)
    (event-logger-message-log! logger type (message-info val))
    (newline)
    (displayln val))
  (let ([dom (edge-dom e)]
        [cod (edge-cod e)]
        [msgs (history->messages h)])
    (define st-1
      (parameterize ([current-prompt
                      (case (type->role (node-type dom))
                        [(assistant) (console-llm-prompt/log logger 'edge msgs history->messages)]
                        [(user system) (console-prompt/log logger 'edge)])]
                     [current-message (message-with-log 'edge)])
        ((edge-trans e) st)))
    (when (current-console-trace-display?)
      (printf "--- Current Node: ~a (Graph: ~a) ---\n"
              (node-name cod)
              (node-graph-name cod)))
    (cond [(node-desc cod) => displayln])
    (define st-2
      (parameterize ([current-prompt
                      (case (type->role (node-type cod))
                        [(assistant) (console-llm-prompt/log logger 'node msgs history->messages)]
                        [(user system) (console-prompt/log logger 'node)])]
                     [current-message (message-with-log 'node)])
        ((node-trans cod) st-1)))
    st-2))

(: llm-choose (All (T S)
                   (-> (List 'choose (Pairof (Edge T S) (Listof (Edge T S))))
                       (Event-Logger T S)
                       (Listof LLM-Message)
                       (Values (Edge T S) Prompt-Attributes))))
(define (llm-choose ne logger msgs)
  (let* ([edges (second ne)]
         [edge-names ((inst map String (Edge T S)) edge-name edges)]
         [dom (edge-dom (car edges))]
         [title (node-prompt dom)])
    (let* ([info ((console-llm-prompt msgs) title `(choose ,string? ,edge-names))]
           [name (prompt-info-choose-value info)]
           [attrs (prompt-info-attributes info)])
      (cond [(findf (lambda ([edge : (Edge T S)]) (string=? name (edge-name edge))) edges)
             => (lambda ([e : (Edge T S)]) (values e attrs))]
            [else (error 'llm-choose "unexpected error")]))))

(: console-llm-prompt/log (All (T S)
                               (-> (Event-Logger T S)
                                   (U 'edge 'node)
                                   (Listof LLM-Message)
                                   (-> (History T S) (Listof LLM-Message))
                                   Prompt-Implementation)))
(define ((console-llm-prompt/log logger type msgs history->messages) title op)
  (let ([msgs (case type
                [(edge)
                 (append (history->messages (list (event-logger->history-edge logger)))
                         msgs)]
                [(node)
                 (append (history->messages (list (event-logger->history-node logger)))
                         (history->messages (list (event-logger->history-edge logger)))
                         msgs)])])
    (let ([info ((console-llm-prompt msgs) title op)])
      (event-logger-prompt-log! logger type info)
      info)))

(: console-prompt/log (All (T S) (-> (Event-Logger T S) (U 'edge 'node) Prompt-Implementation)))
(define ((console-prompt/log logger type) title op)
  (let ([info (console-prompt title op)])
    (event-logger-prompt-log! logger type info)
    info))
