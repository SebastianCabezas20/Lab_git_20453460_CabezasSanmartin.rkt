#lang racket

(provide(all-defined-out))

;TDA recompensas (IdPregunta x UsuarioRecompensa x recompensa x UsuarioResponde)
;Constructor (recompensa idRespuesta usuarioRecompensa recompensa)
;realiza la construccion de la recompensa
(define consRecompensa (lambda(idPregunta usuarioRecompensa recompensa)
                     (list idPregunta usuarioRecompensa recompensa null)))

;Selectores (idRecompensa recompensa)(usuarioRecompensa recompensa)(getRecompensa recompensa)(usuarioResponde recompensa)
;(primeraRecompensa recompensas)(sigRecompensa recompensas)

;realiza la seleccion de la primera recompensa
(define primeraRecompensa car)

;realiza la seleccion de la siguiente recompensa
(define sigRecompensa cdr)

;realiza la seleccion del id de la recompensa
(define idRecompensa car)

;realiza la seleccion del usuario que ofrece la recompensa
(define usuarioRecompensa cadr)

;realiza la seleccion de la recompensa
(define getRecompensa caddr)

;realiza la seleccion del usuario que responde a la recompensa
(define usuarioResponde cadddr)

;Modificadores (agregarUsuarioResponde recompensa username)

;agrega a usuario que respondio la recompensa
(define addUsuarioResponde (lambda(recompensa username)
                                 (list (idRecompensa recompensa)(usuarioRecompensa recompensa)(getRecompensa recompensa)username)))

;borra recompensa de la lista
;Dom: lista recompensas x id
;Rec:lista recompensas
;recursion: natural,se necesita la lista completa
(define removeRecompensa(lambda(recompensas id)
                          (if(null? recompensas)
                             null
                             (if(equal?(idRecompensa(primeraRecompensa recompensas))id)
                                (sigRecompensa recompensas)
                                (cons(primeraRecompensa recompensas)(removeRecompensa(sigRecompensa recompensas)id))))))


#|Dom: lista recompensas x entero(id pregunta) x string(username)
  Rec: lista recompensas
  Descr: añade username de usuario que responde a informacion de recompensas, solo si la pregunta tiene recompensa
  Recursividad: natural, se necesita la lista completa|#
(define addUsuarioRecompensa (lambda(recompensas idP usernameActivo)
                               (if(null? recompensas)
                                  null
                                  (if(equal?(idRecompensa(primeraRecompensa recompensas))idP)
                               (cons(addUsuarioResponde (primeraRecompensa recompensas)usernameActivo)(sigRecompensa recompensas))
                               (cons(primeraRecompensa recompensas)(addUsuarioRecompensa (sigRecompensa recompensas)idP usernameActivo))))))



;otras funciones
#|Dom: lista recompensas x entero(id pregunta) 
  Rec: informacion de recompensa
  Descr: busca la informacion de la recompensa mediante id de la pregunta
  Recursividad: cola, se necesita la primera recompensa de la lista|#
(define buscarRecompensa(lambda(recompensas id)
                          (if(null? recompensas)
                             null
                             (if(equal?(idRecompensa(primeraRecompensa recompensas))id)
                                (primeraRecompensa recompensas)
                                (buscarRecompensa(sigRecompensa recompensas)id)))))