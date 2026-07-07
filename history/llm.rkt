#lang typed/racket

(require typed/json)
(require graph-executor/prompt)
(require graph-executor/history)
(require "../llm.rkt")

(provide make-llm-history-edge history-edge-role history-edge-reasoning
         make-llm-history-prompt history-prompt-role history-prompt-reasoning
         history->messages)

(require racket/hash)

(define-type Attribute-Value (U Symbol String Integer Boolean))

(: make-llm-history-edge (->* ((U 'choose 'auto) String String Role (Option String))
                              ((Immutable-HashTable Symbol Attribute-Value))
                              History-Edge))
(define (make-llm-history-edge mode name prompt role reasoning [attrs ((inst hash Symbol Attribute-Value))])
  (make-history-edge mode name prompt
                     (hash-union attrs
                                 (hash 'llm-role role
                                       'reasoning reasoning))))

(: make-llm-history-prompt (->* (Prompt-Value String Role (Option String))
                                ((Immutable-HashTable Symbol Attribute-Value))
                                History-Prompt))
(define (make-llm-history-prompt value text role reasoning [attrs ((inst hash Symbol Attribute-Value))])
  (make-history-prompt value text (hash-union attrs
                                              (hash 'llm-role role
                                                    'reasoning reasoning))))


(: value->role (-> Any Role))
(define (value->role v)
  (case v
    [(user) 'user]
    [(assistant) 'assistant]
    [(system) 'system]
    [else 'user]))

(: history-prompt-role (-> History-Prompt Role))
(define (history-prompt-role x)
  (cond [(hash-ref (history-prompt-attributes x) 'llm-role #f) => value->role]
        [else 'user]))

(: history-edge-role (-> History-Edge Role))
(define (history-edge-role x)
  (cond [(hash-ref (history-edge-attributes x) 'llm-role #f) => value->role]
        [else 'user]))

(: value->reasoning (-> Any (Option String)))
(define (value->reasoning v)
  (if (string? v)
      v
      #f))

(: history-prompt-reasoning (-> History-Prompt (Option String)))
(define (history-prompt-reasoning x)
  (cond [(hash-ref (history-prompt-attributes x) 'reasoning #f) => value->reasoning]
        [else #f]))

(: history-edge-reasoning (-> History-Edge (Option String)))
(define (history-edge-reasoning x)
  (cond [(hash-ref (history-edge-attributes x) 'reasoning #f) => value->reasoning]
        [else #f]))

(: history->messages (-> History (Listof Message)))
(define (history->messages h)
  (: format-json (-> JSExpr JSExpr String))
  (define (format-json value reasoning)
    (if reasoning
        (jsexpr->string (hash 'value value
                              'reasoning reasoning))
        (jsexpr->string (hash 'value value))))
  (append-map (lambda ([x : (U History-Record)])
                (cond [(history-prompt? x)
                       (list (list (history-prompt-role x)
                                   (format-json (history-prompt-value x)
                                                (history-prompt-reasoning x)))
                             (list 'system (history-prompt-text x)))]
                      [(history-edge? x)
                       (list (list (history-edge-role x)
                                   (format-json (history-edge-prompt x)
                                                (history-edge-reasoning x)))
                             (list 'system
                                   (history-edge-prompt x)))]
                      [(history-node? x)
                       (list (list 'system
                                   (string-join `(,(history-node-name x)
                                                  ,@(cond [(history-node-desc x) => list]
                                                          [else '()]))
                                                "\n")))]))
              h))
