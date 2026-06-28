#lang typed/racket

(provide Role Message  current-default-role)

(define-type Role (U 'user 'assistant 'system))
(define-type Message (List Role String))

(: current-default-role (Parameterof Role))
(define current-default-role (make-parameter 'user))

