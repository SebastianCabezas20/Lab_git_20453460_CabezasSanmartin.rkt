#lang racket

;----------------------------------------USUARIO--------------------------------------------
(define reputacionVacia 0)
(define idVacio null)

;Constructor
(define usuario (lambda(username pass)
                    (cons (cons(cons(cons username pass)reputacionVacia)"activo")idVacio)))
;Pertenencia
(define usuario? (lambda(usuario username)
                   (if(equal? (getUsername usuario) username)
                      #t
                      #f)))
(define reputacionMayor? (lambda(usuario valor)
                   (if( >= (getReputacion usuario) valor)
                      #t
                      #f)))
(define activo? (lambda(usuario)
                   (if(equal? (getActividad usuario) "activo")
                      #t
                      #f)))
(define id? (lambda(ids id);busca en la lista de ids
                   (if(equal? (getPrimerId ids) idVacio );si llega al ultimo no esta
                      idVacio
                      (if(equal?(getPrimerId ids) id);caso que no de null
                         #t;existe esa id en el usuario
                      (id? (getSigId ids) id)))));busca en los siguientes ids

;Selectores
(define getUsername (lambda(usuario)
                           (car(car(car (car usuario))))))

(define getPass (lambda(usuario)
                           (cdr(car(car(car usuario))))))

(define getReputacion (lambda(usuario)
                           (cdr(car(car usuario)))))

(define getActividad (lambda(usuario)
                           (cdr(car usuario))))

(define getIds (lambda(usuario)
                 (cdr usuario)))

(define getPrimerId (lambda(ids)
                           (car ids)))

(define getSigId (lambda(ids)
                    (cdr ids)))
;Modificadores
(define setActividad (lambda(usuario)
                       (if(activo? usuario)
                          (cons(cons(car(car usuario))"inactivo")(getIds usuario))
                          (cons(cons(car(car usuario))"activo") (getIds usuario)))))
;(define addId )
;(define removeId)

;------------------------------------- STACK USUARIOS---------------------------------------------

(define stackUsuariosVacia null)
;Constructores
;Selectores

(define getPrimerUsuario (lambda(stackUsuarios)
                           (car stackUsuarios)))

(define getSigUsuario (lambda(stackUsuarios)
                            (cdr stackUsuarios)))

(define getUsuario (lambda(stackUsuarios username)
                     (if(equal?(getPrimerUsuario stackUsuarios)stackUsuariosVacia);si llega a null
                        stackUsuariosVacia
                        (if(usuario?(getPrimerUsuario stackUsuarios) username)
                           (getPrimerUsuario stackUsuarios);usuario que estamos buscando lo retornamos
                           (getUsuario (getSigUsuario stackUsuarios) username)))));seguimos buscando 



(define usuariox (usuario "hola" 1234))



