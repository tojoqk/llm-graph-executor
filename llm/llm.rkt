#lang typed/racket

(provide Role Message  current-llm-role)

(define-type Role (U 'user 'assistant 'system))
(define-type Message (List Role String))

(: current-llm-role (Parameterof Role))
(define current-llm-role (make-parameter 'user))

