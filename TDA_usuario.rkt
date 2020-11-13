#lang racket
(provide (all-defined-out))

;TDA Usuario
;(USERNAME x PASS x REPUTACION x REPUTACIONRELATIVA)
;Constructor (usuarioNuevo "username" pass)
(define usuarioNuevo (lambda(username pass)
                       (list username pass 30 30)))

;Pertenencia

;Selectores (getUsername usuario)(getPass usuario)(getReputacion usuario)
(define getUsername car)

(define getPass cadr)

(define getReputacion caddr)

(define getReputacionRelativa cadddr)
