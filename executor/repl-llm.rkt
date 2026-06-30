#lang typed/racket

(require graph-executor/graph)
(require graph-executor/prompt)
(require graph-executor/executor)
(require graph-executor/executor/repl)
(require graph-executor/history)
(require "../private/prompt/llm.rkt")
(require "../graph/llm.rkt")
(require "../llm/llm.rkt")
(require "../history/llm.rkt")

(provide repl-llm-run)

(: repl-llm-run (All (T S) (-> (Listof (Graph T S)) (Node T S) S
                               (Values (Node T S) S History))))
(define (repl-llm-run gs entry initial-state)
  (let loop ([n entry]
             [st initial-state]
             [h : History '()])
    (define (terminate)
      (displayln ">> Terminated")
      (values n st h))
    (cond [(find-graph gs (node-graph-id n))
           => (lambda ([g : (Graph T S)])
                (let ([ne (next-edges gs st n)])
                  (case (car ne)
                    [(terminated) (terminate)]
                    [(auto)
                     (let ([chosen-edge (auto-choose ne)])
                       (displayln (format ">> [Auto] ~a" (edge-name chosen-edge)))
                       (define-values (next-st next-node next-h)
                         (llm-repl-step st chosen-edge
                                        (cons
                                         (make-history-edge
                                          'auto
                                          (edge-name chosen-edge)
                                          (string-join `(,(node-name n)
                                                         ,@(cond [(node-desc n) => list]
                                                                 [else '()])
                                                         ,@(cond [(edge-desc chosen-edge) => list]
                                                                 [else '()]))
                                                       "\n"))
                                         h)))
                       (loop next-node next-st next-h))]
                    [(choose)
                     (define-values (chosen-edge next-h-1)
                       (case (node-llm-role n)
                         [(user system) (repl-choose ne h)]
                         [(assistant) (llm-choose ne h)]))
                     (define-values (next-st next-node next-h-2)
                       (llm-repl-step st chosen-edge next-h-1))
                     (loop next-node next-st next-h-2)])))]
          [else (terminate)])))

(: llm-repl-step (All (T S) (-> S (Edge T S) History (values S (Node T S) History))))
(define (llm-repl-step st e h)
  (let ([n (edge-cod e)]
        [bh : (Boxof History) (box h)])
    (: log-prompt (-> String Prompt-Value Void))
    (define (log-prompt title val)
      (set-box! bh (cons (make-history-prompt val title) (unbox bh))))
    (: llm-log-prompt (-> String Prompt-Value (Option String) Void))
    (define (llm-log-prompt title val reasoning)
      (set-box! bh (cons (make-llm-history-prompt val title 'assistant reasoning) (unbox bh))))

    (define st-1
      (parameterize ([current-prompt
                      ((inst llm-repl-prompt Any) log-prompt llm-log-prompt
                                                  (history->messages (unbox bh)))])
        ((edge-trans e) st)))
    (printf "--- Current Node: ~a (Graph: ~a) ---\n"
            (node-name n)
            (node-graph-name n))
    (cond [(node-desc n) => displayln])
    (set-box! bh (cons (make-history-node (node-name n) (node-desc n)) (unbox bh)))
    (define st-2
      (parameterize ([current-prompt
                      ((inst llm-repl-prompt Any) log-prompt llm-log-prompt
                                                  (history->messages (unbox bh)))])
        ((node-trans n) st-1)))
    (values st-2 n (unbox bh))))

(: llm-choose (All (T S)
                   (-> (List 'choose (Pairof (Edge T S) (Listof (Edge T S))))
                       History
                       (Values (Edge T S) History))))
(define (llm-choose ne h)
  (let* ([edges : (Pairof (Edge T S) (Listof (Edge T S))) (second ne)]
         [edge-names ((inst map String (Edge T S)) edge-name edges)]
         [dom : (Node T S) (edge-dom (car edges))]
         [title : String (node-prompt dom)]
         [msgs (history->messages h)]
         [prompt-text-box : (Boxof String) (box "")]
         [reasoning-box : (Boxof (Option String)) (box #f)])
    (: log-reasoning (-> String Prompt-Value (Option String) Void))
    (define (log-reasoning prompt-text _val reasoning)
      (set-box! prompt-text-box prompt-text)
      (set-box! reasoning-box reasoning))
    (let ([name : String ((llm-prompt/log log-reasoning msgs) title `(choose ,string? ,edge-names))])
      (cond [(findf (lambda ([edge : (Edge T S)]) (string=? name (edge-name edge))) edges)
             => (lambda ([e : (Edge T S)])
                  (values e (cons (make-llm-history-edge 'choose
                                                         (edge-name e)
                                                         (unbox prompt-text-box)
                                                         'assistant
                                                         (unbox reasoning-box))
                                  h)))]
            [else
             (error 'llm-choose "unexpected error")]))))

(: llm-prompt/log (All (A) (-> (-> String Prompt-Value (Option String) Void)
                               (Listof Message)
                               (Prompt A))))
(define ((llm-prompt/log k msgs) title op [_ (hash)])
  (let ([reasoning-box : (Boxof (Option String)) (box #f)]
        [prompt-text-box : (Boxof String) (box "")])
    (: log-prompt (-> String (Option String) Void))
    (define (log-prompt text reasoning)
      (set-box! prompt-text-box text)
      (set-box! reasoning-box reasoning))
    (let ([value (((inst llm-prompt A) log-prompt msgs) title op)])
      (k (unbox prompt-text-box) value (unbox reasoning-box))
      value)))

(: llm-repl-prompt (All (A) (-> (-> String Prompt-Value Void)
                                (-> String Prompt-Value (Option String) Void)
                                (Listof Message)
                                (Prompt A))))
(define ((llm-repl-prompt repl-logger llm-logger msgs) title op [attrs ((inst hash Symbol Any))])
  (let ([role (hash-ref attrs 'llm-role #f)])
    (case role
      [(assistant)
       (((inst llm-prompt/log A) llm-logger msgs) title op attrs)]
      [else
       (case role
         [(system)
          (parameterize ([current-default-llm-role 'system])
            (((inst repl-prompt/log A) repl-logger) title op attrs))]
         [else (((inst repl-prompt/log A) repl-logger) title op attrs)])])))
