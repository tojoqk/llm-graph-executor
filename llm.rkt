#lang typed/racket

(require graph-executor/graph)
(require graph-executor/history)
(require graph-executor/prompt)
(require graph-executor/message)

(provide LLM-Role llm-role? LLM-Message default-llm-messages)

(define-type LLM-Role (U 'user 'assistant 'system))
(define-predicate llm-role? LLM-Role)
(define-type LLM-Message (List LLM-Role String))

(: default-llm-messages (All (T S) (-> LLM-Role (History-Record T S) (Listof LLM-Message))))
(define (default-llm-messages role rec)
  (: prompt-messages (-> Prompt-Info (Listof LLM-Message)))
  (define (prompt-messages x)
    (let ([prompt-text (if (prompt-info-choose? x)
                           (let ([out (open-output-string)]
                                 [items (prompt-info-choose-items x)])
                             (fprintf out "~a\n" (prompt-info-title x))
                             (for ([item items])
                               (if (pair? item)
                                   (fprintf out "  - ~a: ~a\n" (car item) (cadr item))
                                   (fprintf out "  - ~a\n" item)))
                             (get-output-string out))
                           (prompt-info-title x))])
      (list (list role
                  (cond [(assoc 'llm-reasoning (prompt-info-attributes x))
                         => (lambda (p)
                              (format "{\"1_reasoning\": ~s, \"2_content\": ~s}"
                                      (prompt-info-value x) (cdr p)))]
                        [else (format "~a" (prompt-info-value x))]))
            (list 'system
                  (format "~a" prompt-text)))))
  (: auto-messages (-> (History-Auto T S) (Listof LLM-Message)))
  (define (auto-messages x)
    (let ([e (history-edge-edge x)])
      (list (list 'system
                  (if (edge-desc e)
                      (format "(auto) ~a\n~a" (edge-name e) (edge-desc e))
                      (format "(auto) ~a" (edge-name e)))))))
  (: choose-messages (-> (History-Choose T S) (Listof LLM-Message)))
  (define (choose-messages x)
    (let* ([e (history-edge-edge x)]
           [dom (edge-dom e)])
      (let ([prompt-text (let ([out (open-output-string)]
                               [items (history-choose-items x)])
                           (fprintf out "~a\n" (or (node-prompt dom) (current-node-prompt)))
                           (for ([item (history-choose-items x)])
                             (if (edge-desc item)
                                 (fprintf out "  - ~a: ~a\n" (edge-name item) (edge-desc item))
                                 (fprintf out "  - ~a\n" (edge-name item))))
                           (get-output-string out))])
        (list (list role
                    (cond [(assoc 'llm-reasoning (history-choose-attributes x))
                           => (lambda (p)
                                (format "{\"1_reasoning\": ~s, \"2_choice\": ~s}"
                                        (cdr p) (edge-name e)))]
                          [else (format "~a" (edge-name e))]))
              (list 'system (format "~a" prompt-text))))))
  (: node-messages (-> (History-Node T S) (Listof LLM-Message)))
  (define (node-messages x)
    (let ([n (history-node-node x)])
      (list (list 'system
                  (if (node-desc n)
                      (format "~a\n~a" (node-name n) (node-desc n))
                      (format "~a" (node-name n)))))))
  (: message-messages (-> Message-Info (Listof LLM-Message)))
  (define (message-messages m)
    (list (list 'system (format "~a" (message-info-message m)))))
  (: event-messages (-> (U Prompt-Info Message-Info) (Listof LLM-Message)))
  (define (event-messages e)
    (if (message-info? e)
        (message-messages e)
        (prompt-messages e)))
  (case (car rec)
    [(node) (node-messages (cdr rec))]
    [(auto) (append (append-map event-messages (history-item-events (cdr rec)))
                    (auto-messages (cdr rec)))]
    [(choose) (append (append-map event-messages (history-item-events (cdr rec)))
                      (choose-messages (cdr rec)))]))
