#lang typed/racket

(provide LLM-Role llm-role? LLM-Message current-llm-role)

(define-type LLM-Role (U 'user 'assistant 'system))
(define-predicate llm-role? LLM-Role)
(define-type LLM-Message (List LLM-Role String))

(: current-llm-role (Parameterof LLM-Role))
(define current-llm-role (make-parameter 'user))

