#lang typed/racket

(provide Role Message  current-default-llm-role)

(define-type Role (U 'user 'assistant 'system))
(define-type Message (List Role String))

(: current-default-llm-role (Parameterof Role))
(define current-default-llm-role (make-parameter 'user))

