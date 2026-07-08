#lang typed/racket

(require graph-executor/graph)
(require graph-executor/prompt)
(require graph-executor/executor)
(require graph-executor/executor/console)
(require graph-executor/history)
(require graph-executor/message)
(require "../private/prompt/console-llm.rkt")
(require "../graph/llm.rkt")
(require "../llm.rkt")
(require "../history/llm.rkt")

(provide console-llm-run)

(: console-llm-run (All (T S) (-> (Listof (Graph T S)) (Node T S) S
                               (Values (Node T S) S History))))
(define (console-llm-run gs entry initial-state)
  (let loop ([n entry]
             [st initial-state]
             [h : History '()])
    (define (terminate)
      (when (current-console-trace-display?)
        (displayln ">> Terminated"))
      (values n st h))
    (cond [(find-graph gs (node-graph-id n))
           => (lambda ([g : (Graph T S)])
                (let ([ne (next-edges gs st n)])
                  (case (car ne)
                    [(terminated) (terminate)]
                    [(auto)
                     (let ([chosen-edge (auto-choose ne)])
                       (when (current-console-trace-display?)
                         (displayln (format ">> [Auto] ~a" (edge-name chosen-edge))))
                       (define-values (next-st next-node next-h)
                         (console-llm-step st chosen-edge
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
                         [(user system) (console-choose ne h)]
                         [(assistant) (llm-choose ne h)]))
                     (define-values (next-st next-node next-h-2)
                       (console-llm-step st chosen-edge next-h-1))
                     (loop next-node next-st next-h-2)])))]
          [else (terminate)])))

(: console-llm-step (All (T S) (-> S (Edge T S) History (values S (Node T S) History))))
(define (console-llm-step st e h)
  (let ([n (edge-cod e)]
        [bh : (Boxof History) (box h)])
    (: log-prompt (-> Prompt-Type Prompt-Value String Void))
    (define (log-prompt type val title)
      (set-box! bh (cons (make-history-prompt type val title) (unbox bh))))
    (: llm-log-prompt (-> Prompt-Type Prompt-Value String (Option String) Void))
    (define (llm-log-prompt type val title reasoning)
      (set-box! bh (cons (make-llm-history-prompt type val title 'assistant reasoning) (unbox bh))))
    (: message-with-log (-> Any Void))
    (define (message-with-log val)
      (let ([str (~a val)])
        (set-box! bh (cons (make-history-message str) (unbox bh)))
        (displayln val)))

    (define st-1
      (parameterize ([current-prompt
                      ((inst console-llm-prompt Any) log-prompt llm-log-prompt
                                                  (history->messages (unbox bh)))]
                     [current-message message-with-log])
        ((edge-trans e) st)))
    (when (current-console-trace-display?)
      (printf "--- Current Node: ~a (Graph: ~a) ---\n"
              (node-name n)
              (node-graph-name n)))
    (cond [(node-desc n) => displayln])
    (set-box! bh (cons (make-history-node (node-name n) (node-desc n)) (unbox bh)))
    (define st-2
      (parameterize ([current-prompt
                      ((inst console-llm-prompt Any) log-prompt llm-log-prompt
                                                  (history->messages (unbox bh)))]
                     [current-message message-with-log])
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
    (: log-reasoning (-> Prompt-Type Prompt-Value String (Option String) Void))
    (define (log-reasoning _type _val prompt-text reasoning)
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

(: llm-prompt/log (All (A) (-> (-> Prompt-Type Prompt-Value String (Option String) Void)
                               (Listof LLM-Message)
                               (Prompt A))))
(define ((llm-prompt/log k msgs) title op [_ (hash)])
  (let ([reasoning-box : (Boxof (Option String)) (box #f)]
        [prompt-text-box : (Boxof String) (box "")])
    (: log-prompt (-> String (Option String) Void))
    (define (log-prompt text reasoning)
      (set-box! prompt-text-box text)
      (set-box! reasoning-box reasoning))
    (let ([value (((inst llm-prompt A) log-prompt msgs) title op)])
      (k (first op) value (unbox prompt-text-box) (unbox reasoning-box))
      value)))

(: console-llm-prompt (All (A) (-> (-> Prompt-Type Prompt-Value String Void)
                                (-> Prompt-Type Prompt-Value String (Option String) Void)
                                (Listof LLM-Message)
                                (Prompt A))))
(define ((console-llm-prompt console-logger llm-logger msgs) title op)
  (case (current-llm-role)
    [(assistant)
     (((inst llm-prompt/log A) llm-logger msgs) title op)]
    [(user system)
     (((inst console-prompt/log A) console-logger) title op)]))
