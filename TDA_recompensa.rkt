#lang racket

(provide(all-defined-out))

;TDA recompensas (IdRespuesta x UsuarioRecompensa x recompensa x UsuarioResponde)
;Constructor (recompensa idRespuesta usuarioRecompensa recompensa)
(define consRecompensa (lambda(idRespuesta usuarioRecompensa recompensa)
                     (list idRespuesta usuarioRecompensa recompensa null)))

;Selectores (idRecompensa recompensa)(usuarioRecompensa recompensa)(getRecompensa recompensa)(usuarioResponde recompensa)
;(primeraRecompensa recompensas)(sigRecompensa recompensas)
(define primeraRecompensa car)
(define sigRecompensa cdr)
(define idRecompensa car)
(define usuarioRecompensa cadr)
(define getRecompensa caddr)
(define usuarioResponde cadddr)
;Modificadores (agregarUsuarioResponde recompensa username)
;agrega a usuario que respondio la recompensa
(define addUsuarioResponde (lambda(recompensa username)
                                 (list (idRecompensa recompensa)(usuarioRecompensa recompensa)(getRecompensa recompensa)username)))