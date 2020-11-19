#lang racket

;FUNCIONES PRINCIPALES


(require "complementarias.rkt")
(require "TDA_usuario.rkt")
(require "TDA_recompensa.rkt")
(require "TDA_stack.rkt")
(require "TDA_fecha.rkt")
(require "TDA_respuesta.rkt")
(require "TDA_pregunta.rkt")

;----------------------------------------USUARIO--------------------------------------------

(define idVacio null)
(define usuarioInactivo null)



;------------REGISTER
#|Dom: stack de usuarios x string(username) x entero o string(pass)
  Rec: stack de usuarios 
  Descr:añade nuevo usuario una lista de usuarios
  Recursividad: natural|#
(define registerFuncion (lambda(stack username pass)
                          (if(null? stack)
                             (cons(usuarioNuevo username pass)null)
                             (if(equal?(getUsername(getPrimerUsuario stack))username)
                                (cons(getPrimerUsuario stack)(getSigUsuario stack))
                                (cons(getPrimerUsuario stack)(registerFuncion (getSigUsuario stack) username pass))))))

#|Dom: stack general x string(username) x entero o string(pass)
  Rec: stack general
  Descr: llama a la funcion interna register la cual solo interactuara con los usuarios|#
(define register (lambda(stack username pass)
                   (list (registerFuncion(getUsuarios stack)username pass)(getPreguntas stack)(getRespuestas stack)(getUsuarioActivo stack))))

;------------------------LOGIN
#|Dom: stack general x string(username) x entero o string(pass)x funcion(operacion)
  Rec: la operacion currificada
  Descr:llama a una funcion interna login, esta funcion le pasara dos veces el stack a la funcion interna
|#
(define login (lambda(stack username pass operation)
                (funcionLogin stack username pass operation stack)))

#|Dom: stack general a modificar x string(username) x entero o string(pass)x stack general que se le pasara a la operacion.
  Rec: la operacion currificada
  Descr: autenticara la cuenta del usuario,añadira el usuario activo al stack que se le pasara a la operacion.
  Recursividad: cola, porque se necesita saber el usuario que esta en la cabeza de la lista|#
(define funcionLogin (lambda(stack username pass operation stackFinal)
                       (if(null?(getUsuarios stack))
                          (operation stackFinal)
                          (if(and(equal?(getUsername(getPrimerUsuario(getUsuarios stack)))username)(equal?(getPass(getPrimerUsuario(getUsuarios stack)))pass))
                             (operation (addUsuarioActivo stackFinal (getPrimerUsuario(getUsuarios stack))))
                             (funcionLogin (sigUsuariosStack stack)username pass operation stackFinal)))))

;------------------ASK
#|Dom: stack general x enteros(dia x mes x año) x string(pregunta del usuario) x strings(et1 x et2 x et3)
  Rec: stack general 
  Descr:añade una nueva pregunta realizada por el usuario a la lista de preguntas
  |#
(define ask (lambda(stack)(lambda(dia mes año)(lambda(preguntaUsuario et1 et2 et3)
                                                (if(pair?(getUsuarioActivo stack))
   (addPregunta stack (pregunta (+ (contador(getPreguntas stack))1) (getUsername(getUsuarioActivo stack)) (fecha dia mes año) preguntaUsuario et1 et2 et3))
                                                   stack)))))

;---------------------REWARD
#|Dom: stack general x entero(id pregunta) x entero(recompensa)
  Rec: stack general 
  Descr:añade recompensa a pregunta y lista de recompensas
 |#
(define reward (lambda(stack)
                 (lambda(idP)
                   (lambda(recompensa)
                     (if(pair?(getUsuarioActivo stack))
                        (if( <= recompensa (getReputacionRelativa(getUsuarioActivo stack)))
                           (addRecompensa stack (getUsername(getUsuarioActivo stack)) idP recompensa)
                           (removeUsuarioActivo stack))
                        stack)))))

;----------------------------------------ANSWERS
#|Dom: stack general x enteros(dia x mes x año) x entero (id pregunta) x string(respuesta del usuario) x string(et1 x et2 x et3)
  Rec: stack general 
  Descr: añade respuesta a lista de respuestas.
|#
;Funcion principal de answer
(define answer (lambda(stack)(lambda(dia mes año)(lambda(idP)(lambda(respuestaUsuario et1 et2 et3)
              (if(pair?(getUsuarioActivo stack))
 (addRespuesta stack (respuesta(+(contador(getRespuestas stack))1)(getUsername(getUsuarioActivo stack)) idP (fecha dia mes año)respuestaUsuario et1 et2 et3) idP) 
 stack))))))
;----------------------------------------ACCEPT

#|Dom: stack general x entero(id pregunta) x entero(id respuesta)
  Rec: stack general
  Descr: acepta respuesta a pregunta, si la pregunta tiene recompensa esta se cobrara
 |#
;Funcion principal de accept
(define accept (lambda(stack)(lambda(idP)(lambda(idR)
                     (if(pair?(getUsuarioActivo stack))
                        (if(esDelUsuario? (getPreguntas stack)(getUsername(getUsuarioActivo stack)) idP)
                           (if(tieneRecompensa? (getRecompensas stack) idP)
                              (cobrarRecompensa stack idP idR)
                              (asignarRespuesta stack idP idR))
                           (removeUsuarioActivo stack))
                        stack)))))



;-------------------------------STACK->STRING
#|Dom: stack general
  Rec: string
  Descr: organiza usuarios,preguntas,respuestas y recompensas para ser impresos por la funcion display
 |#
(define stack->string(lambda(stack)
                      (if(pair?(getUsuarioActivo stack))
                         (imprimirStackActivo stack)
                         (imprimirStack stack ))))


;------------VOTE
#|Dom: stack general x funcion(operacion)
  Rec: stack general
  Descr: permite realizar la accion de votar negativa o positivamente
 |#
;funcion principal de vote
(define vote(lambda(stack)(lambda(operacion)
                               (operacion stack))))


;Se definiran listas para ejemplos
(define stackRecompensas null)
(define stackPreguntas null)
(define stackRespuestas null)
(define sinUsuarioActivo null)
(define stackUsuarios (list (usuarioNuevo "primero" 1234)(usuarioNuevo "segundo" 5678)(usuarioNuevo "tercero" 45)));stack de usuarios
(define stackOver (list stackUsuarios stackPreguntas stackRespuestas sinUsuarioActivo stackRecompensas))

(register stackOver "user100" "pass45")

#|
(register stackOver "user1" "pass1")
(register stackOver "user2" "pass2")
(register stackOver "user3" 1234 )
|#

#|
(login stackOver "primero" 1234 ask)
(login stackOver "segundo" 567 reward)
(login stackOver "tercero" 45 answer)
|#
(define SO2 (((login stackOver "primero" 1234 ask)27 11 2020)"pregunta1" "et1" "et2" "et3"))
#|
(((login stackOver "primero" 1234 ask)27 11 2020)"pregunta1" "et1" "et2" "et3")
(((login stackOver "segundo" 567 ask)22 12 2020)"pregunta2" "et1" "et2" "et3")
(((login stackOver "tercero" 45 ask)15 09 2020)"pregunta3" "et1" "et2" "et3")
|#
(define SO3 (((login SO2 "segundo" 5678 reward)1)20))
#|
(((login SO2 "segundo" 5678 reward)1)20)
(((login SO2 "primero" 1234 reward)2)10)
(((login SO2 "tercero" 45 reward)3)10)
|#

(define SO4 ((((login SO3 "tercero" 45 answer)31 12 2020)1)"Respuesta1" "et1" "et2" "et3"))
#|
((((login SO3 "tercero" 45 answer)31 12 2020)1)"Respuesta1" "et1" "et2" "et3")
((((login SO3 "primero" 1234 answer)31 12 2020)2)"Respuesta2" "et1" "et2" "et3")
((((login SO3 "segundo" 567 answer)31 12 2020)3)"Respuesta2" "et1" "et2" "et3")
|#
(define SO5 (((login SO4 "primero" 1234 accept)1)1))
#|
(((login SO4 "segundo" 5678 accept)2)2)
(((login SO4 "primero" 1234 accept)1)1)
(((login SO4 "tercero" 45 accept)3)3)
|#
(define SO12 ((((login SO5 "primero" 1234 vote)getQuestion)1)"true"))
(define SO13 (((((login SO5 "primero" 1234 vote)getAnswers)1)1)"true"))
#|
((((login SO11 "primero" 1234 vote)getQuestion)1)"true")
(((((login SO12 "primero" 1234 vote)getAnswers)1)1)"true")
((((login SO11 "primero" 1234 vote)getQuestion)1)"false")
|#


#|
(display (stack->string SO2))
(display (stack->string SO3))
(display (stack->string SO4))
|#

(display (stack->string SO13))
