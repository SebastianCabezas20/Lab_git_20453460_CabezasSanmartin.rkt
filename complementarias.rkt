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
  Recursividad: natural|#
(define restaRelativa (lambda(usuarios usernameActivo recompensa)
                        (if(equal?(getUsername(getPrimerUsuario usuarios))usernameActivo)
(cons(list(getUsername(getPrimerUsuario usuarios))(getPass(getPrimerUsuario usuarios))
     (getReputacion(getPrimerUsuario usuarios))(- (getReputacionRelativa(getPrimerUsuario usuarios))recompensa))(getSigUsuario usuarios))
(cons(getPrimerUsuario usuarios)(restaRelativa (getSigUsuario usuarios) usernameActivo recompensa)))))

#|Dom: lista de preguntas x entero(id pregunta) x entero(recompensa)
  Rec: lista de preguntas
  Descr: pone la recompensa en la pregunta 
  Recursividad: natural|#
(define activarRecompensa (lambda(preguntas id recompensa)
                            (if(equal?(idPregunta(primeraPregunta preguntas))id)
  (cons(list(primerosDatosPregunta (primeraPregunta preguntas))(tagsPregunta(primeraPregunta preguntas))(idRespuestas(primeraPregunta preguntas))recompensa(votosPosPregunta(primeraPregunta preguntas))
   (votosNegPregunta(primeraPregunta preguntas))(numeroVisual(primeraPregunta preguntas))(estadoPregunta(primeraPregunta preguntas))) (sigPregunta preguntas))
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

#|Dom: lista recompensas x entero(id pregunta) x string(username)
  Rec: lista recompensas
  Descr: añade username de usuario que responde a informacion de recompensas, solo si la pregunta tiene recompensa
  Recursividad: natural|#
(define addUsuarioRecompensa (lambda(recompensas idP usernameActivo)
                               (if(null? recompensas)
                                  null
                                  (if(equal?(idRecompensa(primeraRecompensa recompensas))idP)
                               (cons(addUsuarioResponde (primeraRecompensa recompensas)usernameActivo)(sigRecompensa recompensas))
                               (cons(primeraRecompensa recompensas)(addUsuarioRecompensa (sigRecompensa recompensas)idP usernameActivo))))))

;----------------------------------------ACCEPT
#|Dom: lista preguntas x string(username) x entero(id pregunta) 
  Rec: booleano
  Descr: verifica si el id de la pregunta pertenece al usuario
  Recursividad: cola|#
(define esDelUsuario? (lambda(preguntas usernameActivo idP)
       (if(null? preguntas)
          #f
        (if(and(equal?(idPregunta(primeraPregunta preguntas))idP)(equal?(autorPregunta(primeraPregunta preguntas))usernameActivo))
           #t
           (esDelUsuario?(sigPregunta preguntas)usernameActivo idP)))))

#|Dom: lista recompensas x entero(id pregunta) 
  Rec: booleano
  Descr: verifica si la pregunta tiene recompensa
  Recursividad: cola|#
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

#|Dom: lista recompensas x entero(id pregunta) 
  Rec: informacion de recompensa
  Descr: busca la informacion de la recompensa mediante id de la pregunta
  Recursividad: cola|#
(define buscarRecompensa(lambda(recompensas id)
                          (if(null? recompensas)
                             null
                             (if(equal?(idRecompensa(primeraRecompensa recompensas))id)
                                (primeraRecompensa recompensas)
                                (buscarRecompensa(sigRecompensa recompensas)id)))))

#|Dom: lista usuarios x  recompensa(informacion) 
  Rec: lista de usuarios
  Descr: selecciona el usuario al cual se le sumara la recompensa
  Recursividad: natural|#
(define sumarDefinitiva (lambda(usuarios recompensa)
                         (if(null? usuarios)
                            null
                            (if(equal?(getUsername(getPrimerUsuario usuarios))(usuarioResponde recompensa))
                               (cons(sumarReputacion (getPrimerUsuario usuarios)(getRecompensa recompensa))(getSigUsuario usuarios))
                               (cons(getPrimerUsuario usuarios)(sumarDefinitiva(getSigUsuario usuarios)recompensa))))))

#|Dom: lista usuarios x  recompensa(informacion) 
  Rec: lista de usuarios
  Descr: selecciona el usuario al cual se le restara la recompensa
  Recursividad: natural|#
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
                              "Sus preguntas"(filtrarPreguntas (getPreguntas stack)(getRespuestas stack)(getUsername(getUsuarioActivo stack))))))

#|Dom:lista preguntas x lista respuestas x string(username) 
  Rec: string
  Descr: selecciona las preguntas que son del usuario activo
  recursividad: si no encuentra ninguna pregunta sera de cola y encuentra sera natural
|#
;Filtra las preguntas que son del usuario
(define filtrarPreguntas(lambda(preguntas respuestas usernameActivo)
                          (if(null? preguntas)
                             null
                             (if(equal?(autorPregunta(primeraPregunta preguntas))usernameActivo)
                                (cons(vincularRespuestas (primeraPregunta preguntas)respuestas)(filtrarPreguntas(sigPregunta preguntas)respuestas usernameActivo))
                                (filtrarPreguntas(sigPregunta preguntas)respuestas usernameActivo)))))


#|Dom: pregunta x lista de respuestas 
  Rec: string
  Descr: prepara el string mediante funciones
|#
;busca las respuestas vinculadas a la pregunta
(define vincularRespuestas(lambda(pregunta respuestas)
  (list"su pregunta es: ""\n"(ordenarPreguntas pregunta) (buscador (idRespuestas pregunta) respuestas))))

#|Dom: lista de id de respuestas x lista de respuestas 
  Rec: string
  Descr: funcion auxiliar que seleccionara el id de una determinada respuesta a buscar
  recursion: natural|#
;ayuda a la busqueda de las respuestas
(define buscador(lambda(idResp respuestas)
                  (if(null? idResp)
                     null
                     (cons(buscadorRespuestas(primerId idResp)respuestas)(buscador(sigId idResp)respuestas)))))

#|Dom: entero(id respuesta) x lista de respuestas
  Rec: string
  Descr: busca la respuesta mediante un id para ordenar como string
  recursion: cola|#
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
(list "USUARIOS DEL STACK""\n"(imprimirUsuarios(getUsuarios stack))"\n" "PREGUNTAS:""\n"(imprimirPreguntas(getPreguntas stack))"\n"
      "RESPUESTAS:""\n"(imprimirRespuestas(getRespuestas stack))"\n""RECOMPENSAS:""\n"(imprimirRecompensas(getRecompensas stack))"\n")))

#|Dom: lista usuarios 
  Rec: string
  Descr: selecciona el usuario a ordenar como string
  recursion: natural|#
(define imprimirUsuarios (lambda(usuarios)
                        (if(null? usuarios)
                           null
                           (cons(ordenarUsuario(getPrimerUsuario usuarios))(imprimirUsuarios(getSigUsuario usuarios))))))

#|Dom: usuario 
  Rec: string
  Descr: ordena al usuario como string
  |#
(define ordenarUsuario (lambda(usuario)
                  (list" Nombre del usuario:"(getUsername usuario)"\n" "Pass del usuario"(getPass usuario)"Reputacion del usuario"(getReputacion usuario)"\n")))

#|Dom: lista de preguntas 
  Rec: string
  Descr: selecciona la pregunta que sera ordenada como string
  recursion: natural|#
(define imprimirPreguntas (lambda(preguntas)
                        (if(null? preguntas)
                           null
                           (cons(ordenarPreguntas(primeraPregunta preguntas))(imprimirPreguntas(sigPregunta preguntas))))))

#|Dom: pregunta 
  Rec: string
  Descr: ordena la pregunta como string
  |#
(define ordenarPreguntas(lambda(pregunta)
 (list" El usuario"(autorPregunta pregunta)"pregunta:""\n"(getPregunta pregunta)" "(getDia(fechaPregunta pregunta))"/"
      (getMes(fechaPregunta pregunta))"/"(getAño(fechaPregunta pregunta))" ""likes:"(votosPosPregunta pregunta)
   "dislike:"(votosNegPregunta pregunta) "\n""'Tags'"" ID:"(idPregunta pregunta) "\n")))

#|Dom: lista de respuestas
  Rec: string
  Descr: selecciona la respuesta que sera ordenada como string
  recursion: natural|#
(define imprimirRespuestas (lambda(respuestas)
                        (if(null? respuestas)
                           null
                           (cons(ordenarRespuestas(primeraRespuesta respuestas))(imprimirRespuestas(sigRespuesta respuestas))))))

#|Dom: respuesta 
  Rec: string
  Descr: ordena la respuesta como string
  |#
(define ordenarRespuestas(lambda(respuesta)
  (list "El usuario"(autorRespuesta respuesta)"a respondido a la pregunta"(idPRespuesta respuesta)"\n"(getRespuesta respuesta)" "
  (getDia(fechaRespuesta respuesta))"/"(getMes(fechaRespuesta respuesta))"/"(getAño(fechaRespuesta respuesta))" ""likes:"(votoPosRespuesta respuesta)
                 "dislike:"(votoNegRespuesta respuesta) "\n"
  "'Tags'"(primerTag(tagsRespuesta respuesta))(segundoTag(tagsRespuesta respuesta))(tercerTag(tagsRespuesta respuesta))" ID:"(idRespuesta respuesta)" "(verEstado respuesta)"\n")))

#|Dom: respuestas 
  Rec: string
  Descr: verifica si la respuesta a sido aceptada
  |#
(define verEstado (lambda(respuesta)
                    (if(equal?(estadoRespuesta respuesta)0)
                       "NO A SIDO ACEPTADA"
                       " ")))
#|Dom: lista de recompensas
  Rec: string
  Descr: selecciona la recompensa que sera ordenada como string
  recursion: natural|#
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
                          (if(equal? boolean "true")
     (list(getUsuarios stack)(getPreguntas stack)(votarPositivoRespuesta(getRespuestas stack)(getPreguntas stack) idP idR) usuarioInactivo (getRecompensas stack))
     (list(getUsuarios stack)(getPreguntas stack)(votarNegativoRespuesta(getRespuestas stack)(getPreguntas stack) idP idR) usuarioInactivo (getRecompensas stack))))))))

#|Dom: lista de respuestas x lista de preguntas x entero(id pregunta) x entero(id respuesta)
  Rec: lista de respuestas 
  Descr: elige la respuesta a votar positivamente si esta se encuentra en la lista de id de la pregunta
  recursividad:cola |#
(define votarPositivoRespuesta(lambda(respuestas preguntas idP idR)
                                (if(null? preguntas)
                                   respuestas
                                   (if(existeRespuesta?(idRespuestas(primeraPregunta preguntas)) idR)
                                      (votarPositivoR respuestas idR)
                                      (votarPositivoRespuesta respuestas (sigPregunta preguntas) idP idR)))))

#|Dom: lista de respuestas x entero(id respuesta) 
  Rec: lista de respuestas 
  Descr: realiza la accion de votar positivamente
  recursividad: natural |#
(define votarPositivoR (lambda(respuestas idR)
                         (if(null? respuestas)
                            null
                            (if(equal?(idRespuesta(primeraRespuesta respuestas)) idR)
   (cons(list(primerosDatosRespuesta(primeraRespuesta respuestas))(tagsRespuesta(primeraRespuesta respuestas))(estadoRespuesta(primeraRespuesta respuestas))(+(votoPosRespuesta(primeraRespuesta respuestas))1)
                 (votoNegRespuesta(primeraRespuesta respuestas)))(sigRespuesta respuestas))
                        (cons(primeraRespuesta respuestas)(votarPositivoR (sigRespuesta respuestas) idR))))))

#|Dom: lista de respuestas x lista de preguntas x entero(id pregunta) x entero(id respuesta)
  Rec: lista de respuestas 
  Descr: elige la respuesta a votar negativamente si esta se encuentra en la lista de id de la pregunta
  recursividad:cola |#
(define votarNegativoRespuesta(lambda(respuestas preguntas idP idR)
                                (if(null? preguntas)
                                   respuestas
                                   (if(existeRespuesta?(idRespuestas(primeraPregunta preguntas)) idR)
                                      (votarNegativoR respuestas idR)
                                      (votarNegativoRespuesta respuestas (sigPregunta preguntas) idP idR)))))

#|Dom: lista de respuestas x entero(id respuesta) 
  Rec: lista de respuestas 
  Descr: realiza la accion de votar negativamente
  recursividad: natural |#
(define votarNegativoR (lambda(respuestas idR)
                         (if(null? respuestas)
                            null
                            (if(equal?(idRespuesta(primeraRespuesta respuestas)) idR)
   (cons(list(primerosDatosRespuesta(primeraRespuesta respuestas))(tagsRespuesta(primeraRespuesta respuestas))(estadoRespuesta(primeraRespuesta respuestas))(+(votoPosRespuesta(primeraRespuesta respuestas))1)
                 (votoNegRespuesta(primeraRespuesta respuestas)))(sigRespuesta respuestas))
                        (cons(primeraRespuesta respuestas)(votarPositivoR (sigRespuesta respuestas) idR))))))

#|Dom: lista de ids respuestas x entero(id respuesta) 
  Rec: booleano 
  Descr: verifica si existe ese id en la lista
  recursividad: cola |#
;verifica si existe respuesta en los ids de la pregunta
(define existeRespuesta? (lambda(idResp idR)
                           (if(null? idResp)
                              #f
                            (if(equal?(primerId idResp)idR)
                               #t
                               (existeRespuesta? (sigId idResp) idR)))))



;--------------------------------------PREGUNTA

#|Dom: stack general x entero(id pregunta) x string(booleano)
  Rec: stack general
  Descr: funcion que determinara si votar negativamente o positivamente una pregunta
 |#
(define getQuestion (lambda(stack)(lambda(idP)(lambda(boolean)
                   (if(equal? boolean "true")
        (list(getUsuarios stack)(votarPositivoPregunta (getPreguntas stack) idP)(getRespuestas stack)usuarioInactivo (getRecompensas stack))
        (list(getUsuarios stack)(votarNegativoPregunta (getPreguntas stack) idP)(getRespuestas stack)usuarioInactivo (getRecompensas stack)))))))







