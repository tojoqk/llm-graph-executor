#lang typed/racket

(require graph-executor/graph)
(require graph-executor/model)
(require graph-executor/prompt)
(require graph-executor/prompt/console)
(require graph-executor/executor)
(require graph-executor/executor/console)
(require graph-executor/journal)
(require graph-executor/history)
(require graph-executor/message)
(require graph-executor/journal)
(require graph-executor/effect/emitter)
(require graph-executor/effect/state)
(require "../prompt/console-llm.rkt")
(require "../llm.rkt")

(provide console-llm-run)

(define-type Event (U Prompt-Info Message-Info))
(define-type Pmt (Pairof Prompt-Value Prompt-Attributes))

(: make-event-emitter (All (S B) (-> (-> (Listof Event) (Record S))
                                     (Values (-> (-> B) (Pairof (Record S) B))
                                             (-> Event Void)
                                             (-> (Record S))))))
(define (make-event-emitter convert)
  (define-values (call-with-state state-get state-set)
    ((inst make-state (Listof Event) B)))

  (: call-with-event-emitter (-> (-> B)
                                 (Pairof (Record S) B)))
  (define (call-with-event-emitter proc)
    (match-define (cons evs b)
      (call-with-state '() proc))
    (cons (convert (reverse evs)) b))

  (: emit (-> Event Void))
  (define (emit ev)
    (state-set (cons ev (state-get))))

  (: peek (-> (Record S)))
  (define (peek)
    (convert (reverse (state-get))))

  (values call-with-event-emitter emit peek))

(: console-llm-run (All (S) (-> (Model S)
                                [#:llm-role (-> Symbol LLM-Role)]
                                [#:llm-messages (-> LLM-Role
                                                    (Record S)
                                                    (Listof LLM-Message))]
                                [#:journal Journal]
                                (Values (Node S) S Journal))))
(define (console-llm-run m
                         #:llm-role [type->llm-role (const 'assistant)]
                         #:llm-messages [role-record->llm-messages
                                         (inst default-llm-messages S)]
                         #:journal [j '()])
  (: record->llm-role (-> (Record S) LLM-Role))
  (define (record->llm-role rec)
    (case (car rec)
      [(node) (type->llm-role (node-type (record-node rec)))]
      [(auto choose) (type->llm-role (node-type (edge-from (record-edge rec))))]))
  (: record->llm-messages (-> (Record S) (Listof LLM-Message)))
  (define (record->llm-messages rec)
    (role-record->llm-messages (record->llm-role rec) rec))
  (: history->llm-messages (-> (History S) (Listof LLM-Message)))
  (define (history->llm-messages h)
    (append-map record->llm-messages h))
  (define-values (n st h) (replay m j))
  (define gs (model-graphs m))
  (let loop ([n n]
             [st st]
             [h h])
    (define command-dispatch
      (console-command-dispatch m
                                (lambda (_n _st [l-j : Journal])
                                  (define-values (n* st* h*) (replay m l-j))
                                  (loop n* st* h*))))
    (define (terminate)
      (when (current-console-trace-display?)
        (displayln ">> Terminated"))
      (values n st j))
    (let ([ne (next-edges gs st n)])
      (case (car ne)
        [(terminated) (terminate)]
        [(auto)
         (let* ([chosen-edge (auto-choose ne)])
           (when (current-console-trace-display?)
             (displayln (format ">> [Auto] ~a" (edge-name chosen-edge))))
           (define-values (r/edge r/node next-st)
             (console-llm-step st chosen-edge h
                               (lambda ([evs : (Listof Event)]) : (Record S)
                                 (auto-edge-record evs chosen-edge))
                               (lambda ([evs : (Listof Event)]) : (Record S)
                                 (node-record evs (edge-to chosen-edge)))
                               type->llm-role history->llm-messages))
           (loop (edge-to chosen-edge)
                 next-st
                 (list* r/node r/edge h)))]
        [(choose)
         (define choose-pmt ((node-prompt n) st))
         (let-values ([(cmd attrs)
                       (case (type->llm-role (node-type n))
                         [(assistant) (llm-choose choose-pmt ne (history->llm-messages h))]
                         [(user system) (values
                                         (console-choose choose-pmt
                                                         (map (inst edge-name S) (second ne)))
                                         '())])])
           (cond
             [(string? cmd)
              (define chosen-edge (find-edge (second ne) cmd))
              (define-values (r/edge r/node next-st)
                (console-llm-step st chosen-edge h
                                  (lambda ([evs : (Listof Event)]) : (Record S)
                                    (choose-edge-record evs chosen-edge choose-pmt (second ne) attrs))
                                  (lambda ([evs : (Listof Event)]) : (Record S)
                                    (node-record evs (edge-to chosen-edge)))
                                  type->llm-role history->llm-messages))
              (loop (edge-to chosen-edge) next-st (list* r/node r/edge h))]
             [else (command-dispatch n st j cmd)]))]))))

(: console-llm-step (All (S) (-> S (Edge S) (History S)
                                 (-> (Listof Event) (Record S))
                                 (-> (Listof Event) (Record S))
                                 (-> Symbol LLM-Role)
                                 (-> (History S) (Listof LLM-Message))
                                 (Values (Record S) (Record S) S))))
(define (console-llm-step st e h convert/edge convert/node type->role history->messages)
  (: message-with-log (-> (-> Event Void) (-> Any Void)))
  (define ((message-with-log emit) val)
    (emit (message-info val))
    (newline)
    (displayln val))
  (define-values (call-with-event-emitter/edge emit/edge peek/edge)
    ((inst make-event-emitter S S) convert/edge))
  (define-values (call-with-event-emitter/node emit/node peek/node)
    ((inst make-event-emitter S S) convert/node))
  (let ([from (edge-from e)]
        [to (edge-to e)])
    (match-define (list* r/edge st-1)
      (call-with-event-emitter/edge
       (thunk
        (let ([msgs (history->messages h)])
          (parameterize ([current-prompt
                          (case (type->role (node-type from))
                            [(assistant) (console-llm-prompt/log emit/edge peek/edge msgs history->messages)]
                            [(user system) (console-prompt/log emit/edge)])]
                         [current-message (message-with-log emit/edge)])
            ((edge-trans e) st))))))
    (when (current-console-trace-display?)
      (printf "--- Current Node: ~a (Graph: ~a) ---\n"
              (node-name to)
              (node-graph-name to)))
    (match-define (list* r/node st-2)
      (call-with-event-emitter/node
       (thunk
        (let ([msgs (history->messages (cons r/edge h))])
          (parameterize ([current-prompt
                          (case (type->role (node-type to))
                            [(assistant) (console-llm-prompt/log emit/node peek/node msgs history->messages)]
                            [(user system) (console-prompt/log emit/node)])]
                         [current-message (message-with-log emit/node)])
            ((node-trans to) st-1))))))
    (values r/edge r/node st-2)))

(: llm-choose (All (S)
                   (-> Prompt-Meta
                       (List 'choose (Pairof (Edge S) (Listof (Edge S))))
                       (Listof LLM-Message)
                       (Values String Prompt-Attributes))))
(define (llm-choose meta ne msgs)
  (let* ([edges (second ne)]
         [edge-names ((inst map String (Edge S)) edge-name edges)])
    (define-values (name attrs) ((console-llm-prompt msgs) meta `(choose ,string? ,edge-names)))
    (cond [(findf (lambda ([edge : (Edge S)]) (string=? name (edge-name edge))) edges)
           => (lambda ([e : (Edge S)]) (values (edge-name e) attrs))]
          [else (error 'llm-choose "unexpected error")])))

(: console-llm-prompt/log (All (S)
                               (-> (-> Event Void)
                                   (-> (Record S))
                                   (Listof LLM-Message)
                                   (-> (History S) (Listof LLM-Message))
                                   Prompt-Implementation)))
(define ((console-llm-prompt/log emit peek msgs history->messages) meta op)
  (let ([msgs (append (history->messages (list (peek))) msgs)])
    (define-values (val attrs) ((console-llm-prompt msgs) meta op))
    (emit (prompt-info op meta val attrs))
    (values val attrs)))

(: console-prompt/log (All (S) (-> (-> Event Void) Prompt-Implementation)))
(define ((console-prompt/log emit) meta op)
  (define-values (val attrs) (console-prompt meta op))
  (emit (prompt-info op meta val attrs))
  (values val attrs))
