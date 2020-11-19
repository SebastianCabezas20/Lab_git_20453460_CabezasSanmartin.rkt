#lang racket
(require "TDA_usuario.rkt")
(require "TDA_recompensa.rkt")
(require "TDA_stack.rkt")
(require "TDA_fecha.rkt")
(require "TDA_respuesta.rkt")
(require "TDA_pregunta.rkt")
(provide (all-defined-out))

;---------------------REWARD
#|Dom: lista de usuarios x string(username) x entero(recompensa)
  Rec: lista de usuarios
  Descr:Realiza la resta relativa a la recompensa que ofrecio
  Recursividad: natural, porque se necesita la lista completa|#
(define restaRelativa (lambda(usuarios usernameActivo recompensa)
                        (if(equal?(getUsername(getPrimerUsuario usuarios))usernameActivo)
(cons(list(getUsername(getPrimerUsuario usuarios))(getPass(getPrimerUsuario usuarios))
     (getReputacion(getPrimerUsuario usuarios))(- (getReputacionRelativa(getPrimerUsuario usuarios))recompensa))(getSigUsuario usuarios))
(cons(getPrimerUsuario usuarios)(restaRelativa (getSigUsuario usuarios) usernameActivo recompensa)))))

#|Dom: lista de preguntas x entero(id pregunta) x entero(recompensa)
  Rec: lista de preguntas
  Descr: pone la recompensa en la pregunta 
  Recursividad: natural, porque se necesita la lista completa|#
(define activarRecompensa (lambda(preguntas id recompensa)
                            (if(equal?(idPregunta(primeraPregunta preguntas))id)
  (cons(list(primerosDatosPregunta (primeraPregunta preguntas))(tagsPregunta(primeraPregunta preguntas))(idRespuestas(primeraPregunta preguntas))recompensa(votosPosPregunta(primeraPregunta preguntas))
   (votosNegPregunta(primeraPregunta preguntas))(estadoPregunta(primeraPregunta preguntas))) (sigPregunta preguntas))
  (cons(primeraPregunta preguntas)(activarRecompensa(sigPregunta preguntas)id)))))

#|Dom: stack general x string(username) x entero(id pregunta) x entero(recompensa)
  Rec: stack general
  Descr: modifica el stack general mediante funciones y añade informacion de recompensas al lista de recompensas
 |#
(define addRecompensa (lambda(stack usernameActivo id recompensaUsuario)
(list (restaRelativa (getUsuarios stack)usernameActivo recompensaUsuario)(activarRecompensa (getPreguntas stack) id recompensaUsuario)
  (getRespuestas stack) usuarioInactivo (cons(consRecompensa id usernameActivo recompensaUsuario)(getRecompensas stack)))))

;----------------------------------------ANSWERS

#|Dom: stack general x respuesta x entero(id pregunta)
  Rec: stack general
  Descr: modifica el stack general mediante funciones y añade respuesta a lista de respuestas
 |#
(define addRespuesta (lambda(stack respuesta idP)
                       (list(getUsuarios stack)(getPreguntas stack)(cons respuesta (getRespuestas stack))usuarioInactivo
                            (addUsuarioRecompensa(getRecompensas stack)idP (getUsername(getUsuarioActivo stack))))))


;----------------------------------------ACCEPT
#|Dom: lista preguntas x string(username) x entero(id pregunta) 
  Rec: booleano
  Descr: verifica si el id de la pregunta pertenece al usuario
  Recursividad: cola, se necesita saber la primera pregunta de la lista|#
(define esDelUsuario? (lambda(preguntas usernameActivo idP)
       (if(null? preguntas)
          #f
        (if(and(equal?(idPregunta(primeraPregunta preguntas))idP)(equal?(autorPregunta(primeraPregunta preguntas))usernameActivo))
           #t
           (esDelUsuario?(sigPregunta preguntas)usernameActivo idP)))))

#|Dom: lista recompensas x entero(id pregunta) 
  Rec: booleano
  Descr: verifica si la pregunta tiene recompensa
  Recursividad: cola, se necesita la primera recompensa de la lista|#
(define tieneRecompensa? (lambda(recompensas idP)
                           (if(null? recompensas)
                              #f
                              (if(equal?(idRecompensa(primeraRecompensa recompensas))idP)
                                 #t
                                 (tieneRecompensa?(sigRecompensa recompensas) idP)))))

#|Dom: stack general x entero(id pregunta) x entero(id respuesta) 
  Rec: stack general
  Descr: realiza cambios en el stack mediante funciones y saca a usuario activo
 |#
(define cobrarRecompensa (lambda(stack idP idR)
 (list(sumarDefinitiva(restarDefinitiva (getUsuarios stack)(buscarRecompensa(getRecompensas stack) idP))(buscarRecompensa (getRecompensas stack)idP))
      (addIdP(getPreguntas stack) idP idR)(cambiarEstado(getRespuestas stack) idR)usuarioInactivo(removeRecompensa(getRecompensas stack)idP))))



#|Dom: lista usuarios x  recompensa(informacion) 
  Rec: lista de usuarios
  Descr: selecciona el usuario al cual se le sumara la recompensa
  Recursividad: natural, porque se necesita la lista completa|#
(define sumarDefinitiva (lambda(usuarios recompensa)
                         (if(null? usuarios)
                            null
                            (if(equal?(getUsername(getPrimerUsuario usuarios))(usuarioResponde recompensa))
                               (cons(sumarReputacion (getPrimerUsuario usuarios)(getRecompensa recompensa))(getSigUsuario usuarios))
                               (cons(getPrimerUsuario usuarios)(sumarDefinitiva(getSigUsuario usuarios)recompensa))))))

#|Dom: lista usuarios x  recompensa(informacion) 
  Rec: lista de usuarios
  Descr: selecciona el usuario al cual se le restara la recompensa
  Recursividad: natural,porque se necesita la lista completa|#
(define restarDefinitiva (lambda(usuarios recompensa)
                         (if(null? usuarios)
                            null
                            (if(equal?(getUsername(getPrimerUsuario usuarios))(usuarioRecompensa recompensa))
                               (cons(restarReputacion(getPrimerUsuario usuarios)(getRecompensa recompensa))(getSigUsuario usuarios))
                               (cons(getPrimerUsuario usuarios)(restarDefinitiva(getSigUsuario usuarios)recompensa))))))


#|Dom: stack general x entero(id pregunta) x entero(id respuesta) 
  Rec: stack general
  Descr: modifica el stack general mediante funciones
|#
(define asignarRespuesta(lambda(stack idP idR)
    (list(getUsuarios stack)(addIdP(getPreguntas stack) idP idR)(cambiarEstado(getRespuestas stack) idR)usuarioInactivo(getRecompensas stack))))




;------------------------------------------stack->string

#|Dom: stack general 
  Rec: string
  Descr: mediante funciones ordena el string
|#
(define imprimirStackActivo (lambda(stack)
                              (list "USUARIO LOGIN:""\n"(ordenarUsuario(getUsuarioActivo stack))"\n"
                              "Las preguntas"(seleccionarPreguntas (getPreguntas stack)(getRespuestas stack)))))

#|Dom:lista preguntas x lista respuestas x string(username) 
  Rec: string
  Descr: selecciona las preguntas 
  recursividad: si no encuentra ninguna pregunta sera de cola y encuentra sera natural, se necesita lista de string
|#
(define seleccionarPreguntas(lambda(preguntas respuestas)
                          (if(null? preguntas)
                             null
                             (cons(vincularRespuestas (primeraPregunta preguntas)respuestas)(seleccionarPreguntas(sigPregunta preguntas)respuestas)))))


#|Dom: pregunta x lista de respuestas 
  Rec: string
  Descr: prepara el string mediante funciones
|#
;busca las respuestas vinculadas a la pregunta
(define vincularRespuestas(lambda(pregunta respuestas)
  (list"\n"" "(ordenarPreguntas pregunta)" Sus respuestas son:\n" (buscador (idRespuestas pregunta) respuestas)"\n\n")))

#|Dom: lista de id de respuestas x lista de respuestas 
  Rec: string
  Descr: funcion auxiliar que seleccionara el id de una determinada respuesta a buscar
  recursion: natural, porque se necesita la lista completa|#
;ayuda a la busqueda de las respuestas
(define buscador(lambda(idResp respuestas)
                  (if(null? idResp)
                     null
                     (cons(buscadorRespuestas(primerId idResp)respuestas)(buscador(sigId idResp)respuestas)))))

#|Dom: entero(id respuesta) x lista de respuestas
  Rec: string
  Descr: busca la respuesta mediante un id para ordenar como string
  recursion: cola, porque se necesita el primer id de la lista|#
(define buscadorRespuestas(lambda(idResp respuestas)
                            (if(null? respuestas)
                               null
                               (if(equal?(idRespuesta(primeraRespuesta respuestas))idResp)
                                  (ordenarRespuestas(primeraRespuesta respuestas))
                                  (buscadorRespuestas idResp(sigRespuesta respuestas))))))

#|Dom: stack general 
  Rec: string
  Descr: prepara el string del stack mediante funciones
|#
(define imprimirStack (lambda(stack)
(list "USUARIOS DEL STACK""\n"(imprimirUsuarios(getUsuarios stack))"\n" "PREGUNTAS:""\n""\n"(seleccionarPreguntas (getPreguntas stack)(getRespuestas stack))
      "\n""RECOMPENSAS:""\n"(imprimirRecompensas(getRecompensas stack))"\n")))

#|Dom: lista usuarios 
  Rec: string
  Descr: selecciona el usuario a ordenar como string
  recursion: natural, porque se necesita la lista completa|#
(define imprimirUsuarios (lambda(usuarios)
                        (if(null? usuarios)
                           null
                           (cons(ordenarUsuario(getPrimerUsuario usuarios))(imprimirUsuarios(getSigUsuario usuarios))))))

#|Dom: usuario 
  Rec: string
  Descr: ordena al usuario como string
  |#
(define ordenarUsuario (lambda(usuario)
                  (list"      Nombre del usuario:"(getUsername usuario)"\n" "         Pass del usuario"(getPass usuario)" Reputacion del usuario"(getReputacion usuario)"\n\n")))


#|Dom: pregunta 
  Rec: string
  Descr: ordena la pregunta como string
  |#
(define ordenarPreguntas(lambda(pregunta)
 (list"   El usuario"(autorPregunta pregunta)"pregunta:""\n""      "(getPregunta pregunta)" "(getDia(fechaPregunta pregunta))"/"
      (getMes(fechaPregunta pregunta))"/"(getAño(fechaPregunta pregunta))" ""likes:"(votosPosPregunta pregunta)
   "dislike:"(votosNegPregunta pregunta) "\n""      'Tags'"(primerTag(tagsPregunta pregunta))(segundoTag(tagsPregunta pregunta))(tercerTag(tagsPregunta pregunta))" ID:"(idPregunta pregunta) "\n")))


#|Dom: respuesta 
  Rec: string
  Descr: ordena la respuesta como string
  |#
(define ordenarRespuestas(lambda(respuesta)
  (list "   El usuario"(autorRespuesta respuesta)"a respondido a la pregunta""\n""     "(getRespuesta respuesta)" "
  (getDia(fechaRespuesta respuesta))"/"(getMes(fechaRespuesta respuesta))"/"(getAño(fechaRespuesta respuesta))" ""likes:"(votoPosRespuesta respuesta)
                 "dislike:"(votoNegRespuesta respuesta) "\n"
  "      'Tags'"(primerTag(tagsRespuesta respuesta))(segundoTag(tagsRespuesta respuesta))(tercerTag(tagsRespuesta respuesta))" ID:"(idRespuesta respuesta)" \n")))


#|Dom: lista de recompensas
  Rec: string
  Descr: selecciona la recompensa que sera ordenada como string
  recursion: natural, porque se necesita la lista completa|#
(define imprimirRecompensas (lambda(recompensas)
                        (if(null? recompensas)
                           null
                           (cons(ordenarRecompensa(primeraRecompensa recompensas))(imprimirUsuarios(sigRecompensa recompensas))))))
#|Dom: recompensa
  Rec: string
  Descr: ordena la recompensa como string
  |#
(define ordenarRecompensa (lambda(recompensa)
 (list "La pregunta con el ID"(idRecompensa recompensa)"dada por"(usuarioRecompensa recompensa)"la cantidad es"(getRecompensa recompensa)"\n"
       "y el usuario que respodio a esta pregunta fue"(usuarioResponde recompensa)"\n")))

;-------------------------------------------------VOTE

;----------RESPUESTA
#|Dom: stack general x entero(id pregunta) x entero(idRespuesta) x string(booleano)
  Rec: stack general
  Descr: funcion que determinara si votar negativamente o positivamente una respuesta
 |#
(define getAnswers (lambda(stack)(lambda(idP)(lambda(idR)(lambda(boolean)
                      (if(pair?(getUsuarioActivo stack))
                          (if(equal? boolean "true")
     (list(getUsuarios stack)(getPreguntas stack)(votarPositivoRespuesta(getRespuestas stack)(getPreguntas stack) idP idR) usuarioInactivo (getRecompensas stack))
     (list(getUsuarios stack)(getPreguntas stack)(votarNegativoRespuesta(getRespuestas stack)(getPreguntas stack) idP idR) usuarioInactivo (getRecompensas stack)))
                          stack))))))


#|Dom: lista de respuestas x lista de preguntas x entero(id pregunta) x entero(id respuesta)
  Rec: lista de respuestas 
  Descr: elige la respuesta a votar positivamente si esta se encuentra en la lista de id de la pregunta
  recursividad:cola , porque se necesita saber la primera pregunta de la lista|#
(define votarPositivoRespuesta(lambda(respuestas preguntas idP idR)
                                (if(null? preguntas)
                                   respuestas
                                   (if(existeRespuesta?(idRespuestas(primeraPregunta preguntas)) idR)
                                      (votarPositivoR respuestas idR)
                                      (votarPositivoRespuesta respuestas (sigPregunta preguntas) idP idR)))))

#|Dom: lista de respuestas x lista de preguntas x entero(id pregunta) x entero(id respuesta)
  Rec: lista de respuestas 
  Descr: elige la respuesta a votar negativamente si esta se encuentra en la lista de id de la pregunta
  recursividad:cola, se necesita la primera pregunta de la lista |#
(define votarNegativoRespuesta(lambda(respuestas preguntas idP idR)
                                (if(null? preguntas)
                                   respuestas
                                   (if(existeRespuesta?(idRespuestas(primeraPregunta preguntas)) idR)
                                      (votarNegativoR respuestas idR)
                                      (votarNegativoRespuesta respuestas (sigPregunta preguntas) idP idR)))))


;--------------------------------------PREGUNTA

#|Dom: stack general x entero(id pregunta) x string(booleano)
  Rec: stack general
  Descr: funcion que determinara si votar negativamente o positivamente una pregunta
 |#
(define getQuestion (lambda(stack)(lambda(idP)(lambda(boolean)
         (if(pair?(getUsuarioActivo stack))
          (if(equal? boolean "true")
        (list(getUsuarios stack)(votarPositivoPregunta (getPreguntas stack) idP)(getRespuestas stack)usuarioInactivo (getRecompensas stack))
        (list(getUsuarios stack)(votarNegativoPregunta (getPreguntas stack) idP)(getRespuestas stack)usuarioInactivo (getRecompensas stack)))
          stack)))))







