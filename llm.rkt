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
    (let* ([op (second x)]
           [prompt-text (case (car op)
                          [(choose)
                           (let ([out (open-output-string)]
                                 [items (if (procedure? (second op))
                                            (third op)
                                            (second op))])
                             (fprintf out "~a\n" (prompt-info-title x))
                             (for ([item items])
                               (if (pair? item)
                                   (fprintf out "  - ~a: ~a\n" (car item) (cadr item))
                                   (fprintf out "  - ~a\n" item)))
                             (get-output-string out))]
                          [else (prompt-info-title x)])])
      (list (list role
                  (cond [(assoc 'llm-reasoning (prompt-info-attributes x))
                         => (lambda (p)
                              (format "{\"1_reasoning\": ~s, \"2_content\": ~s}"
                                      (prompt-info-value x) (cdr p)))]
                        [else (format "~a" (prompt-info-value x))]))
            (list 'system
                  (format "~a" prompt-text)))))
  (: auto-messages (-> (History-Record-Auto T S) (Listof LLM-Message)))
  (define (auto-messages x)
    (let* ([e (history-record-edge x)]
           [prompt-text (node-prompt (edge-dom e))])
      (list* (list 'system
                   (if (edge-desc e)
                       (format "(auto) ~a\n~a" (edge-name e) (edge-desc e))
                       (format "(auto) ~a" (edge-name e))))
             (if prompt-text
                 (list (list 'system (format "~a" prompt-text)))
                 '()))))
  (: choose-messages (-> (History-Record-Choose T S) (Listof LLM-Message)))
  (define (choose-messages x)
    (let* ([e (history-record-edge x)]
           [dom (edge-dom e)])
      (let ([prompt-text (let ([out (open-output-string)])
                           (fprintf out "~a\n" (or (node-prompt dom) (current-node-prompt)))
                           (for ([item (history-record-choices x)])
                             (if (edge-desc item)
                                 (fprintf out "  - ~a: ~a\n" (edge-name item) (edge-desc item))
                                 (fprintf out "  - ~a\n" (edge-name item))))
                           (get-output-string out))])
        (list (list role
                    (cond [(assoc 'llm-reasoning (history-record-attributes x))
                           => (lambda (p)
                                (format "{\"1_reasoning\": ~s, \"2_choice\": ~s}"
                                        (cdr p) (edge-name e)))]
                          [else (format "~a" (edge-name e))]))
              (list 'system (format "~a" prompt-text))))))
  (: node-messages (-> (History-Record-Node T S) (Listof LLM-Message)))
  (define (node-messages x)
    (let ([n (history-record-node x)])
      (list (list 'system
                  (if (node-desc n)
                      (format "~a\n~a" (node-name n) (node-desc n))
                      (format "~a" (node-name n)))))))
  (: message-messages (-> Message-Info (Listof LLM-Message)))
  (define (message-messages m)
    (list (list 'system (format "~a" (message-info-message m)))))
  (: event-messages (-> (U Prompt-Info Message-Info) (Listof LLM-Message)))
  (define (event-messages e)
    (case (car e)
      [(message) (message-messages e)]
      [(prompt) (prompt-messages e)]))
  (case (car rec)
    [(node) (node-messages rec)]
    [(auto) (append (append-map event-messages (history-record-events rec))
                    (auto-messages rec))]
    [(choose) (append (append-map event-messages (history-record-events rec))
                      (choose-messages rec))]))
