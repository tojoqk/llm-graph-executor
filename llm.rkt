#lang typed/racket

(provide Role role? Message current-llm-role)

(define-type Role (U 'user 'assistant 'system))
(define-predicate role? Role)
(define-type Message (List Role String))

(: current-llm-role (Parameterof Role))
(define current-llm-role (make-parameter 'user))

