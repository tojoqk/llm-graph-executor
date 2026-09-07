#lang typed/racket

(require graph-executor/plugin/graph)
(require graph-executor/plugin/trace)
(require graph-executor/plugin/prompt)
(require graph-executor/plugin/message)

(provide LLM-Role llm-role? LLM-Message default-llm-messages)

(define-type LLM-Role (U 'user 'assistant 'system))
(define-predicate llm-role? LLM-Role)
(define-type LLM-Message (List LLM-Role String))

(: default-llm-messages (-> LLM-Role Record (Listof LLM-Message)))
(define (default-llm-messages role rec)
  (: prompt-messages (-> Prompt-Result (Listof LLM-Message)))
  (define (prompt-messages x)
    (let* ([op (second x)]
           [prompt-text (case (car op)
                          [(choice)
                           (let ([out (open-output-string)]
                                 [items (if (procedure? (second op))
                                            (third op)
                                            (second op))])
                             (fprintf out "~a\n"
                                      (prompt-info-title (prompt-result-info x)))
                             (for ([item items])
                               (if (pair? item)
                                   (fprintf out "  - ~a: ~a\n" (car item) (cadr item))
                                   (fprintf out "  - ~a\n" item)))
                             (get-output-string out))]
                          [else (prompt-info-title (prompt-result-info x))])]
           [extra (prompt-result-extra x)])
      (list (list role
                  (cond [(and (list? extra)
                              (findf (lambda (pair)
                                       (and (pair? pair)
                                            (eq? (car pair) 'llm-reasoning)))
                                     extra))
                         => (lambda (pair)
                              (assert pair pair?)
                              (format "{\"1_reasoning\": ~s, \"2_choice\": ~s}"
                                      (cdr pair)
                                      (prompt-result-value x)))]
                        [else (format "~a" (prompt-result-value x))]))
            (list 'system
                  (format "~a" prompt-text)))))
  (: auto-messages (-> Auto-Edge-Record (Listof LLM-Message)))
  (define (auto-messages x)
    (let* ([e (edge-record-edge-info x)])
      (list (list 'system
                  (if (edge-info-desc e)
                      (format "(auto) ~a\n~a" (edge-info-name e) (edge-info-desc e))
                      (format "(auto) ~a" (edge-info-name e)))))))
  (: choice-messages (-> Choice-Edge-Record (Listof LLM-Message)))
  (define (choice-messages x)
    (let* ([e (edge-record-edge-info x)]
           [from (edge-info-from e)])
      (let ([prompt-text (let ([out (open-output-string)])
                           (fprintf out "~a\n" (choice-edge-record-prompt x))
                           (for ([item (choice-edge-record-choices x)])
                             (if (edge-info-desc item)
                                 (fprintf out "  - ~a: ~a\n" (edge-info-name item) (edge-info-desc item))
                                 (fprintf out "  - ~a\n" (edge-info-name item))))
                           (get-output-string out))]
            [extra (choice-edge-record-extra x)])
        (list (list role
                    (cond [(and (list? extra)
                                (findf (lambda (pair)
                                         (and (pair? pair)
                                              (eq? (car pair) 'llm-reasoning)))
                                       extra))
                           => (lambda (pair)
                                (assert pair pair?)
                                (format "{\"1_reasoning\": ~s, \"2_choice\": ~s}"
                                        (cdr pair) (edge-info-name e)))]
                          [else (format "~a" (edge-info-name e))]))
              (list 'system (format "~a" prompt-text))))))
  (: node-messages (-> Node-Record (Listof LLM-Message)))
  (define (node-messages x)
    (let ([n (node-record-node-info x)])
      (list (list 'system
                  (if (node-info-desc n)
                      (format "~a\n~a" (node-info-name n) (node-info-desc n))
                      (format "~a" (node-info-name n)))))))
  (: message-messages (-> Message-Result (Listof LLM-Message)))
  (define (message-messages m)
    (list (list 'system (format "~a" (message-result-message m)))))
  (: event-messages (-> (U Prompt-Result Message-Result) (Listof LLM-Message)))
  (define (event-messages e)
    (case (car e)
      [(message) (message-messages e)]
      [(prompt) (prompt-messages e)]))
  (cond [(node-record? rec) (append (append-map event-messages (node-record-events rec))
                                    (node-messages rec))]
        [(auto-edge-record? rec) (append (append-map event-messages (edge-record-events rec))
                                         (auto-messages rec))]
        [(choice-edge-record? rec) (append (append-map event-messages (edge-record-events rec))
                                           (choice-messages rec))]))
