#lang typed/racket

(require graph-executor/plugin/graph)
(require graph-executor/plugin/model)
(require graph-executor/plugin/prompt)
(require graph-executor/plugin/prompt/console)
(require graph-executor/plugin/executor)
(require graph-executor/plugin/executor/console)
(require graph-executor/plugin/journal)
(require graph-executor/plugin/trace)
(require graph-executor/plugin/message)
(require graph-executor/plugin/journal)
(require graph-executor/plugin/effect/state)
(require "../prompt/console-llm.rkt")
(require "../llm.rkt")

(provide console-llm-run)

(define-type Event (U Prompt-Result Message-Result))
(define-type Pmt (Pairof Prompt-Value Any))

(: make-event-emitter (All (B) (-> (-> (Listof Event) Record)
                                   (Values (-> (-> B) (Pairof Record B))
                                           (-> Event Void)
                                           (-> Record)))))
(define (make-event-emitter convert)
  (define-values (call-with-state state-get state-set)
    ((inst make-state (Listof Event) B)))

  (: call-with-event-emitter (-> (-> B)
                                 (Pairof Record B)))
  (define (call-with-event-emitter proc)
    (match-define (cons evs b)
      (call-with-state '() proc))
    (cons (convert (reverse evs)) b))

  (: emit (-> Event Void))
  (define (emit ev)
    (state-set (cons ev (state-get))))

  (: peek (-> Record))
  (define (peek)
    (convert (reverse (state-get))))

  (values call-with-event-emitter emit peek))

(: console-llm-run (All (S) (-> (Model S)
                                [#:llm-role (-> Symbol LLM-Role)]
                                [#:llm-messages (-> LLM-Role
                                                    Record
                                                    (Listof LLM-Message))]
                                [#:journal (Listof Journal-Entry)]
                                (Listof Journal-Entry))))
(define (console-llm-run m
                         #:llm-role [type->llm-role (const 'assistant)]
                         #:llm-messages [role-record->llm-messages default-llm-messages]
                         #:journal [j '()])
  (: record->llm-role (-> Record LLM-Role))
  (define (record->llm-role rec)
    (cond [(node-record? rec) (type->llm-role (node-info-type (node-record-node-info rec)))]
          [(edge-record? rec) (type->llm-role (node-info-type (edge-info-from (edge-record-edge-info rec))))]))
  (: record->llm-messages (-> Record (Listof LLM-Message)))
  (define (record->llm-messages rec)
    (role-record->llm-messages (record->llm-role rec) rec))
  (: trace->llm-messages (-> Trace (Listof LLM-Message)))
  (define (trace->llm-messages h)
    (append-map record->llm-messages h))
  (define-values (n st h) (trace m j))
  (define gs (model-graphs m))
  (define-values (_n _st result-j)
    (let loop : (Values (Node S) S (Listof Journal-Entry)) ([n n] [st st] [h h])
      (define command-dispatch
        (console-command-dispatch m
                                  (lambda (_n _st [l-j : (Listof Journal-Entry)])
                                    (define-values (n* st* h*) (trace m l-j))
                                    (loop n* st* h*))))
      (define (terminate)
        (displayln ">> Terminated")
        (values n st (trace->journal h)))
      (let ([ne (next-edges gs st n)])
        (case (car ne)
          [(terminated auto-conflicted)
           (case (car ne)
             [(auto-conflicted)
              (newline)
              (printf ">> Auto conflicted: ~s" (cdr ne))])
           (terminate)]
          [(auto)
           (let* ([chosen-edge (auto-choose ne)])
             (displayln (format ">> [Auto] ~a" (edge-name chosen-edge)))
             (define-values (r/edge r/node next-st)
               (console-llm-step st chosen-edge h
                                 (lambda ([evs : (Listof Event)]) : Record
                                   (auto-edge-record (edge-id chosen-edge)
                                                     (edge-edge-info chosen-edge)
                                                     evs))
                                 (lambda ([evs : (Listof Event)]) : Record
                                   (node-record (node-id (edge-to chosen-edge))
                                                (node-node-info (edge-to chosen-edge))
                                                evs))
                                 type->llm-role trace->llm-messages))
             (loop (edge-to chosen-edge)
                   next-st
                   (list* r/node r/edge h)))]
          [(choose)
           (define choose-pmt ((node-prompt n) st))
           (let-values ([(cmd extra)
                         (case (type->llm-role (node-type n))
                           [(assistant) (llm-choose choose-pmt ne (trace->llm-messages h))]
                           [(user system)
                            (values (console-choose 'choose (console-config) choose-pmt (second ne))
                                    #f)])])
             (cond
               [(edge? cmd)
                (define chosen-edge cmd)
                (define-values (r/edge r/node next-st)
                  (console-llm-step st chosen-edge h
                                    (lambda ([evs : (Listof Event)]) : Record
                                      (choose-edge-record (edge-id chosen-edge) (edge-edge-info chosen-edge) evs
                                                          choose-pmt
                                                          (let ([choices (second ne)])
                                                            (cons (edge-edge-info (car choices))
                                                                  (map (inst edge-edge-info S) (cdr choices))))
                                                          extra))
                                    (lambda ([evs : (Listof Event)]) : Record
                                      (node-record (node-id (edge-to chosen-edge))
                                                   (node-node-info (edge-to chosen-edge))
                                                   evs))
                                    type->llm-role trace->llm-messages))
                (loop (edge-to chosen-edge) next-st (list* r/node r/edge h))]
               [else
                (command-dispatch n st (trace->journal h) cmd)]))]))))
  result-j)

(: console-llm-step (All (S) (-> S (Edge S) Trace
                                 (-> (Listof Event) Record)
                                 (-> (Listof Event) Record)
                                 (-> Symbol LLM-Role)
                                 (-> Trace (Listof LLM-Message))
                                 (Values Record Record S))))
(define (console-llm-step st e h convert/edge convert/node type->role trace->messages)
  (: message-with-log (-> (-> Event Void) (-> Any Void)))
  (define ((message-with-log emit) val)
    (emit (message-result val))
    (newline)
    (displayln val))
  (define-values (call-with-event-emitter/edge emit/edge peek/edge)
    ((inst make-event-emitter S) convert/edge))
  (define-values (call-with-event-emitter/node emit/node peek/node)
    ((inst make-event-emitter S) convert/node))
  (let ([from (edge-from e)]
        [to (edge-to e)])
    (match-define (list* r/edge st-1)
      (call-with-event-emitter/edge
       (thunk
        (let ([msgs (trace->messages h)])
          (parameterize ([current-prompt
                          (case (type->role (node-type from))
                            [(assistant) (console-llm-prompt/log emit/edge peek/edge msgs trace->messages)]
                            [(user system) (console-prompt/log emit/edge)])]
                         [current-message (message-with-log emit/edge)])
            ((edge-trans e) st))))))
    (printf "--- Current Node: ~a (Graph: ~a) ---\n"
            (node-name to)
            (node-graph-name to))
    (match-define (list* r/node st-2)
      (call-with-event-emitter/node
       (thunk
        (let ([msgs (trace->messages (cons r/edge h))])
          (parameterize ([current-prompt
                          (case (type->role (node-type to))
                            [(assistant) (console-llm-prompt/log emit/node peek/node msgs trace->messages)]
                            [(user system) (console-prompt/log emit/node)])]
                         [current-message (message-with-log emit/node)])
            ((node-trans to) st-1))))))
    (values r/edge r/node st-2)))

(: llm-choose (All (S)
                   (-> String
                       (List 'choose (Pairof (Edge S) (Listof (Edge S))))
                       (Listof LLM-Message)
                       (Values (Edge S) Any))))
(define (llm-choose title ne msgs)
  (let* ([edges (second ne)]
         [edge-ids ((inst map Symbol (Edge S)) edge-id edges)])
    (: edge-id->edge (-> Symbol (Edge S)))
    (define (edge-id->edge id)
      (cond [(findf (lambda ([edge : (Edge S)]) (eq? id (edge-id edge))) edges) => identity]
            [else (error 'llm-choose "unexpected error")]))
    (: edge-id->edge-name (-> Symbol String))
    (define (edge-id->edge-name id)
      (edge-name (edge-id->edge id)))
    (define-values (id extra)
      ((console-llm-prompt msgs) (prompt-info title) (op-choose symbol? edge-ids #:show edge-id->edge-name)))
    (values (edge-id->edge id) extra)))

(: console-llm-prompt/log (All (S)
                               (-> (-> Event Void)
                                   (-> Record)
                                   (Listof LLM-Message)
                                   (-> Trace (Listof LLM-Message))
                                   Prompt-Implementation)))
(define ((console-llm-prompt/log emit peek msgs trace->messages) info op)
  (let ([msgs (append (trace->messages (list (peek))) msgs)])
    (define-values (val extra) ((console-llm-prompt msgs) info op))
    (emit (prompt-result op info (prompt-record val #:extra extra)))
    (values val extra)))

(: console-prompt/log (All (S) (-> (-> Event Void) Prompt-Implementation)))
(define ((console-prompt/log emit) info op)
  (define-values (val extra) (console-prompt info op))
  (emit (prompt-result op info (prompt-record val #:extra extra)))
  (values val extra))
