#lang racket
(provide (all-defined-out))

;TDA Usuario
;(USERNAME x PASS x REPUTACION x REPUTACIONRELATIVA)
;realiza la construccion de un nuevo usuario
;Constructor (usuarioNuevo "username" pass)
(define usuarioNuevo (lambda(username pass)
                       (list username pass 30 30)))

;Pertenencia

;Selectores (getUsername usuario)(getPass usuario)(getReputacion usuario)
;realiza la seleccion del username del usuario
(define getUsername car)

;realiza la seleccion del pass del usuario
(define getPass cadr)

;realiza la seleccion de la reputacion real del usuario
(define getReputacion caddr)

;realiza la seleccion de la reputacion relativa del usuario
(define getReputacionRelativa cadddr)

;moficadores

#|Dom: usuario x  entero(reputacion) 
  Rec: usuario
  Descr: suma la reputacion a un determinado usuario
  |#
;realiza la accion de sumar reputacion a un usuario;
(define sumarReputacion(lambda(usuario reputacion)
                         (list(getUsername usuario)(getPass usuario)(+(getReputacion usuario)reputacion)(+(getReputacionRelativa usuario)reputacion))))

#|Dom: usuario x  entero(reputacion) 
  Rec: usuario
  Descr: resta la reputacion a un determinado usuario
  |#
;realiza la accion de restar reputacion a un usuario;
(define restarReputacion(lambda(usuario reputacion)
                         (list(getUsername usuario)(getPass usuario)(-(getReputacion usuario)reputacion)(getReputacionRelativa usuario))))
