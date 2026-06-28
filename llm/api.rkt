#lang typed/racket
(require typed/json
         typed/net/http-client)
(require "llm.rkt")

(provide request-llm current-llm-model current-llm-host current-llm-port)

(: message->jsexpr (-> Message JSExpr))
(define (message->jsexpr m)
  (hash 'role (symbol->string (first m))
        'content (second m)))

(: current-llm-model (Parameterof String))
(define current-llm-model (make-parameter "gemma4:12b"))
(: current-llm-host (Parameterof String))
(define current-llm-host (make-parameter "127.0.0.1"))
(: current-llm-port (Parameterof Positive-Integer))
(define current-llm-port (make-parameter 8000))

(: make-payload (-> JSExpr (Listof Message) JSExpr))
(define (make-payload schema messages)
  (apply (inst hash Symbol JSExpr)
         `(
           model ,(current-llm-model)
           messages ,(map message->jsexpr (reverse messages))
           stream #f
           ,@(if schema
                 (list 'response_format
                       (hasheq 'type "json_schema"
                               'json_schema
                               (hasheq 'name "schema"
                                       'strict #t
                                       'schema schema)))
                 '()))))

(: default-schema JSExpr)
(define default-schema
  (hash 'type "object"
        'properties (hash 'text (hash 'type "string"))
        'required (list "text")))

(: request-llm (-> JSExpr (Listof Message) JSExpr))
(define (request-llm schema messages)
  (define host (current-llm-host))
  (define port (current-llm-port))
  (define payload (make-payload schema messages))

  (let retry ([c : Natural 5])
    (when (zero? c)
      (error 'response-llm "exeeds-retry-count"))
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (printf "response-llm error: ~a\n" e)
                       (retry (sub1 c)))])
      (define-values (status-line _headers response-port)
        (http-sendrecv host
                       "/v1/chat/completions"
                       #:port port
                       #:method 'POST
                       #:headers (list "Content-Type: application/json")
                       #:data (jsexpr->string payload)))

      (define response-json (read-json response-port))
      (close-input-port response-port)
      (let ([status (assert (string->number (second (string-split (bytes->string/utf-8 status-line) " ")))
                            number?)])
        (if (= status 200)
            (begin
              (call/cc
               (lambda ([return : (-> JSExpr Nothing)])
                 (cond [(and (not (eof-object? response-json))
                             (hash? response-json)
                             (hash-ref response-json 'choices #f))
                        => (lambda ([choices : JSExpr])
                             (cond [(and (pair? choices)
                                         (let ([choice (car choices)])
                                           (and (hash? choice)
                                                (hash-ref choice 'message #f))))
                                    => (lambda ([message : JSExpr])
                                         (cond [(and (hash? message)
                                                     (hash-ref message 'content #f))
                                                => (lambda ([content : JSExpr])
                                                     (when (string? content)
                                                       (return (string->jsexpr content))))]))]))])
                 (error 'response-llm "message-parse-error"))))
            (error 'response-llm "unexpected status code" response-json))))))
