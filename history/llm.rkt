#lang typed/racket

(require typed/json)
(require graph-executor/prompt)
(require graph-executor/history)
(require "../llm/llm.rkt")

(provide (struct-out llm-history-edge) LLM-History-Edge
         (struct-out llm-history-prompt) LLM-History-Prompt
         history-edge-role history-prompt-reasoning
         history->messages)

(struct llm-history-edge history-edge ([role : Role]
                                       [reasoning : (Option String)])
  #:transparent
  #:type-name LLM-History-Edge)

(struct llm-history-prompt history-prompt ([role : Role]
                                           [reasoning : (Option String)])
  #:transparent
  #:type-name LLM-History-Prompt)

(: history-prompt-role (-> History-Prompt Role))
(define (history-prompt-role x)
  (if (llm-history-prompt? x)
      (llm-history-prompt-role x)
      (current-default-role)))

(: history-edge-role (-> History-Edge Role))
(define (history-edge-role x)
  (if (llm-history-edge? x)
      (llm-history-edge-role x)
      (current-default-role)))

(: history-prompt-reasoning (-> History-Prompt (Option String)))
(define (history-prompt-reasoning x)
  (if (llm-history-prompt? x)
      (llm-history-prompt-reasoning x)
      #f))

(: history-edge-reasoning (-> History-Edge (Option String)))
(define (history-edge-reasoning x)
  (if (llm-history-edge? x)
      (llm-history-edge-reasoning x)
      #f))

(: prompt-value->string (-> Prompt-Value String))
(define (prompt-value->string x)
  (if (number? x)
      (number->string x)
      x))

(: history->messages (-> History (Listof Message)))
(define (history->messages h)
  (: format-json (-> String (Option String) String))
  (define (format-json value reasoning)
    (if reasoning
        (jsexpr->string (hash 'value value
                              'reasoning reasoning))
        (jsexpr->string (hash 'value value))))
  (append-map (lambda ([x : (U History-Record)])
                (cond [(history-prompt? x)
                       (list (list (history-prompt-role x)
                                   (format-json (prompt-value->string (history-prompt-value x))
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
