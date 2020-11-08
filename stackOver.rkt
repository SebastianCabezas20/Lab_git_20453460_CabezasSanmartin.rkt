#lang racket
;TDA Usuario
;((USERNAME x PASS x REPUTACION x ACTIVIDAD)ID)
;Constructor (usuario "username" pass)
;Pertenencia (

;TDA Usuarios
;(USUARIO x USUARIO x USUARIO)

;----------------------------------------USUARIO--------------------------------------------
(define reputacionVacia 0)
(define idVacio null)

;Constructor
(define usuario (lambda(username pass)
                    (cons(list username pass reputacionVacia "activo") idVacio)))
;Pertenencia


;Selectores
(define getUsername (lambda(usuario)
                           (car(car usuario))))

(define getPass (lambda(usuario)
                           (cadr(car usuario))))

(define getReputacion (lambda(usuario)
                           (caddr(car usuario))))

(define getActividad (lambda(usuario)
                           (cadddr(car usuario))))

(define getIds cdr)

(define getPrimerId car)

(define getSigId cdr)

(define getId (lambda(ids id);busca en la lista de ids
                   (if(equal? (getPrimerId ids) idVacio );si llega al ultimo no esta
                      idVacio
                      (if(equal?(getPrimerId ids) id);caso que no de null
                         #t;existe esa id en el usuario
                      (getId (getSigId ids) id)))));busca en los siguientes ids
;Modificadores
(define setActividad (lambda(usuario)
                       (if(equal? (getActividad usuario)"activo")
                          (cons(cons(car(car usuario))"inactivo")(getIds usuario))
                          (cons(cons(car(car usuario))"activo") (getIds usuario)))))
;(define addId )
;(define removeId)

;------------------------------------- STACK USUARIOS---------------------------------------------

(define stackUsuariosVacia null)
;Constructores
;Selectores

(define getPrimerUsuario car)

(define getSigUsuario cdr)

(define getUsuario (lambda(stackUsuarios username)
                     (if(equal?(getPrimerUsuario stackUsuarios)stackUsuariosVacia);si llega a null
                        stackUsuariosVacia
                        (if(equal? (getPrimerUsuario stackUsuarios) username)
                           (getPrimerUsuario stackUsuarios);usuario que estamos buscando lo retornamos
                           (getUsuario (getSigUsuario stackUsuarios) username)))));seguimos buscando 



(define usuariox (usuario "hola" 1234))
(define listaUsuarios (list (usuario "primero" 1234)(usuario "segundo" 5678)(usuario "tercero" 0000)))
(getUsername usuariox)
(getPass usuariox)
(getActividad usuariox)
(getReputacion usuariox)
(getIds usuariox)

(setActividad usuariox)


