#lang racket
(require "complementarias.rkt")
(require "TDA_usuario.rkt")
(require "TDA_recompensa.rkt")
(require "TDA_stack.rkt")
(require "TDA_fecha.rkt")
(require "TDA_respuesta.rkt")
(require "TDA_pregunta.rkt")

;----------------------------------------USUARIO--------------------------------------------
(define reputacionVacia 0)
(define idVacio null)
(define usuarioInactivo null)

;------------------------------------- STACK USUARIOS---------------------------------------------

(define stackUsuariosVacia null)

;------------REGISTER


(define register (lambda(stack username pass)
                   (list (registerFuncion(getUsuarios stack)username pass)(getPreguntas stack)(getRespuestas stack)(getUsuarioActivo stack))))

;------------------------LOGIN

(define login (lambda(stack username pass operation)
                (funcionLogin stack username pass operation stack)))

(define funcionLogin (lambda(stack username pass operation stackFinal)
                       (if(null?(getUsuarios stack))
                          stack
                          (if(and(equal?(getUsername(getPrimerUsuario(getUsuarios stack)))username)(equal?(getPass(getPrimerUsuario(getUsuarios stack)))pass))
                             (operation (addUsuarioActivo stackFinal (getPrimerUsuario(getUsuarios stack))))
                             (funcionLogin (sigUsuariosStack stack)username pass operation stackFinal)))))

;------------------ASK
(define ask (lambda(stack)(lambda(dia mes año)(lambda(preguntaUsuario et1 et2 et3)
                                                (if(pair?(getUsuarioActivo stack))
   (addPregunta stack (pregunta (+ (contador(getPreguntas stack))1) (getUsername(getUsuarioActivo stack)) (fecha dia mes año) preguntaUsuario et1 et2 et3))
                                                   stack)))))

;---------------------REWARD
;Realiza la resta relativa a la recompensa que ofrecio
(define restaRelativa (lambda(usuarios usernameActivo recompensa)
                        (if(equal?(getUsername(getPrimerUsuario usuarios))usernameActivo)
(cons(list(getUsername(getPrimerUsuario usuarios))(getPass(getPrimerUsuario usuarios))
     (getReputacion(getPrimerUsuario usuarios))(- (getReputacionRelativa(getPrimerUsuario usuarios))recompensa))(getSigUsuario usuarios))
(cons(getPrimerUsuario usuarios)(restaRelativa (getSigUsuario usuarios) usernameActivo recompensa)))))

;Activa la recompensa en la pregunta 
(define activarRecompensa (lambda(preguntas id recompensa)
                            (if(equal?(idPregunta(primeraPregunta preguntas))id)
  (cons(list(primerosDatosPregunta (primeraPregunta preguntas))(tagsPregunta(primeraPregunta preguntas))(idRespuestas(primeraPregunta preguntas))recompensa(votosPosPregunta(primeraPregunta preguntas))
   (votosNegPregunta(primeraPregunta preguntas))(numeroVisual(primeraPregunta preguntas))(estadoPregunta(primeraPregunta preguntas))) (sigPregunta preguntas))
  (cons(primeraPregunta preguntas)(activarRecompensa(sigPregunta preguntas)id)))))

;añade recompensa en el stack
(define addRecompensa (lambda(stack usernameActivo id recompensaUsuario)
(list (restaRelativa (getUsuarios stack)usernameActivo recompensaUsuario)(activarRecompensa (getPreguntas stack) id recompensaUsuario)
  (getRespuestas stack) usuarioInactivo (cons(consRecompensa id usernameActivo recompensaUsuario)(getRecompensas stack)))))

;Funcion principal de reward
(define reward (lambda(stack)
                 (lambda(idP)
                   (lambda(recompensa)
                     (if(pair?(getUsuarioActivo stack))
                        (if( <= recompensa (getReputacionRelativa(getUsuarioActivo stack)))
                           (addRecompensa stack (getUsername(getUsuarioActivo stack)) idP recompensa)
                           (removeUsuarioActivo stack))
                        stack)))))

;----------------------------------------ANSWERS
;añade respuesta a stack de respuestas Y REGISTRA SI TIENE RECOMPENSA
(define addRespuesta (lambda(stack respuesta idP)
                       (list(getUsuarios stack)(getPreguntas stack)(cons respuesta (getRespuestas stack))usuarioInactivo
                            (addUsuarioRecompensa(getRecompensas stack)idP (getUsername(getUsuarioActivo stack))))))

;agrega al usuario que responde si tiene recompensa
(define addUsuarioRecompensa (lambda(recompensas idP usernameActivo)
                               (if(null? recompensas)
                                  null
                                  (if(equal?(idRecompensa(primeraRecompensa recompensas))idP)
                               (cons(addUsuarioResponde (primeraRecompensa recompensas)usernameActivo)(sigRecompensa recompensas))
                               (cons(primeraRecompensa recompensas)(addUsuarioRecompensa (sigRecompensa recompensas)idP usernameActivo))))))



;Funcion principal de answer
(define answer (lambda(stack)(lambda(dia mes año)(lambda(idP)(lambda(respuestaUsuario et1 et2 et3)
              (if(pair?(getUsuarioActivo stack))
 (addRespuesta stack (respuesta(+(contador(getRespuestas stack))1)(getUsername(getUsuarioActivo stack)) idP (fecha dia mes año)respuestaUsuario et1 et2 et3) idP) 
 stack))))))
;----------------------------------------ACCEPT
;verifica que la pregunta sea del usuario
(define esDelUsuario? (lambda(preguntas usernameActivo idP)
       (if(null? preguntas)
          #f
        (if(and(equal?(idPregunta(primeraPregunta preguntas))idP)(equal?(autorPregunta(primeraPregunta preguntas))usernameActivo))
           #t
           (esDelUsuario?(sigPregunta preguntas)usernameActivo idP)))))

;verifica si esa pregunta tiene recompesa
(define tieneRecompensa? (lambda(recompensas idP)
                           (if(null? recompensas)
                              #f
                              (if(equal?(idRecompensa(primeraRecompensa recompensas))idP)
                                 #t
                                 (tieneRecompensa?(sigRecompensa recompensas) idP)))))

;realiza las acciones en caso de que haya recompensa en la pregunta
(define cobrarRecompensa (lambda(stack idP idR)
 (list(sumarDefinitiva(restarDefinitiva (getUsuarios stack)(buscarRecompensa(getRecompensas stack) idP))(buscarRecompensa (getRecompensas stack)idP))
      (addIdP(getPreguntas stack) idP idR)(cambiarEstado(getRespuestas stack) idR)usuarioInactivo(removeRecompensa(getRecompensas stack)idP))))

;Buscar recompensa por el id de pregunta
(define buscarRecompensa(lambda(recompensas id)
                          (if(null? recompensas)
                             null
                             (if(equal?(idRecompensa(primeraRecompensa recompensas))id)
                                (primeraRecompensa recompensas)
                                (buscarRecompensa(sigRecompensa recompensas)id)))))

;suma al usuario que gana recompensa
(define sumarDefinitiva (lambda(usuarios recompensa)
                         (if(null? usuarios)
                            null
                            (if(equal?(getUsername(getPrimerUsuario usuarios))(usuarioResponde recompensa))
                               (cons(sumarReputacion (getPrimerUsuario usuarios)(getRecompensa recompensa))(getSigUsuario usuarios))
                               (cons(getPrimerUsuario usuarios)(sumarDefinitiva(getSigUsuario usuarios)recompensa))))))

;resta al usuario que da recompensa
(define restarDefinitiva (lambda(usuarios recompensa)
                         (if(null? usuarios)
                            null
                            (if(equal?(getUsername(getPrimerUsuario usuarios))(usuarioRecompensa recompensa))
                               (cons(restarReputacion(getPrimerUsuario usuarios)(getRecompensa recompensa))(getSigUsuario usuarios))
                               (cons(getPrimerUsuario usuarios)(restarDefinitiva(getSigUsuario usuarios)recompensa))))))

;realiza la accion de sumar reputacion a un usuario;
(define sumarReputacion(lambda(usuario reputacion)
                         (list(getUsername usuario)(getPass usuario)(+(getReputacion usuario)reputacion)(+(getReputacionRelativa usuario)reputacion))))

;realiza la accion de restar reputacion a un usuario;
(define restarReputacion(lambda(usuario reputacion)
                         (list(getUsername usuario)(getPass usuario)(-(getReputacion usuario)reputacion)(getReputacionRelativa usuario))))


;Funcion principal de accept
(define accept (lambda(stack)(lambda(idP)(lambda(idR)
                     (if(pair?(getUsuarioActivo stack))
                        (if(esDelUsuario? (getPreguntas stack)(getUsername(getUsuarioActivo stack)) idP)
                           (if(tieneRecompensa? (getRecompensas stack) idP)
                              (cobrarRecompensa stack idP idR)
                              (asignarRespuesta stack idP idR))
                           (removeUsuarioActivo stack))
                        stack)))))

;añade id de respuesta a pregunta
(define addIdP (lambda(preguntas idPreg idResp)
                  (if(null? preguntas)
                     null
                     (if(equal?(idPregunta(primeraPregunta preguntas))idPreg)
                        (cons(addIdPregunta (primeraPregunta preguntas)idResp)(sigPregunta preguntas))
                        (cons(primeraPregunta preguntas)(addIdP(sigPregunta preguntas)idPreg idResp))))))

;borra recompensa de la lista
(define removeRecompensa(lambda(recompensas id)
                          (if(null? recompensas)
                             null
                             (if(equal?(idRecompensa(primeraRecompensa recompensas))id)
                                (sigRecompensa recompensas)
                                (cons(primeraRecompensa recompensas)(removeRecompensa(sigRecompensa recompensas)id))))))

;Asigna la respuesta a la pregunta
(define asignarRespuesta(lambda(stack idP idR)
    (list(getUsuarios stack)(addIdP(getPreguntas stack) idP idR)(cambiarEstado(getRespuestas stack) idR)usuarioInactivo(getRecompensas stack))))

;cambia el estado a acceptada
(define cambiarEstado(lambda(respuestas idR)
                       (if(equal?(idRespuesta(primeraRespuesta respuestas)) idR)
       (cons(list(primerosDatosRespuesta(primeraRespuesta respuestas))(tagsRespuesta(primeraRespuesta respuestas))1(votoPosRespuesta(primeraRespuesta respuestas))
                 (votoNegRespuesta(primeraRespuesta respuestas)))(sigRespuesta respuestas))
       (cons(primeraRespuesta respuestas)(cambiarEstado(sigRespuesta respuestas)idR)))))

;-------------------------------STACK->STRING
;Funcion principal de stack->string
(define stack->string(lambda(stack)
                      (if(pair?(getUsuarioActivo stack))
                         (imprimirStackActivo stack)
                         (imprimirStack stack ))))

;Imprimir stack con login en una funcin pasar la pregunta que sea del usuario y nnexar las respeustas conn un busqueda
(define imprimirStackActivo (lambda(stack)
                              (list "USUARIO LOGIN:""\n"(ordenarUsuario(getUsuarioActivo stack))"\n"
                              "Sus preguntas"(filtrarPreguntas (getPreguntas stack)(getRespuestas stack)(getUsername(getUsuarioActivo stack))))))

;Filtra las preguntas que son del usuario
(define filtrarPreguntas(lambda(preguntas respuestas usernameActivo)
                          (if(null? preguntas)
                             null
                             (if(equal?(autorPregunta(primeraPregunta preguntas))usernameActivo)
                                (cons(vincularRespuestas (primeraPregunta preguntas)respuestas)(filtrarPreguntas(sigPregunta preguntas)respuestas usernameActivo))
                                (filtrarPreguntas(sigPregunta preguntas)respuestas usernameActivo)))))

;busca las respuestas vinculadas a la pregunta
(define vincularRespuestas(lambda(pregunta respuestas)
  (list"su pregunta es: ""\n"(ordenarPreguntas pregunta) (buscador (idRespuestas pregunta) respuestas))))

(define primerId car)
(define sigId cdr)
;ayuda a la busqueda de las respuestas
(define buscador(lambda(idResp respuestas)
                  (if(null? idResp)
                     null
                     (cons(buscadorRespuestas(primerId idResp)respuestas)(buscador(sigId idResp)respuestas)))))

;busca la pregunta segun el id de esta
(define buscadorRespuestas(lambda(idResp respuestas)
                            (if(null? respuestas)
                               null
                               (if(equal?(idRespuesta(primeraRespuesta respuestas))idResp)
                                  (ordenarRespuestas(primeraRespuesta respuestas))
                                  (buscadorRespuestas idResp(sigRespuesta respuestas))))))

;organiza el stack para ser imprimido
(define imprimirStack (lambda(stack)
(list "USUARIOS DEL STACK""\n"(imprimirUsuarios(getUsuarios stack))"\n" "PREGUNTAS:""\n"(imprimirPreguntas(getPreguntas stack))"\n"
      "RESPUESTAS:""\n"(imprimirRespuestas(getRespuestas stack))"\n""RECOMPENSAS:""\n"(imprimirRecompensas(getRecompensas stack))"\n")))

;string del stack usuarios
(define imprimirUsuarios (lambda(usuarios)
                        (if(null? usuarios)
                           null
                           (cons(ordenarUsuario(getPrimerUsuario usuarios))(imprimirUsuarios(getSigUsuario usuarios))))))

;prepara el usuario para ser impreso
(define ordenarUsuario (lambda(usuario)
                  (list" Nombre del usuario:"(getUsername usuario)"\n" "Pass del usuario"(getPass usuario)"Reputacion del usuario"(getReputacion usuario)"\n")))

;string de las preguntas
(define imprimirPreguntas (lambda(preguntas)
                        (if(null? preguntas)
                           null
                           (cons(ordenarPreguntas(primeraPregunta preguntas))(imprimirPreguntas(sigPregunta preguntas))))))
;ordena la pregunta para ser impresa
(define ordenarPreguntas(lambda(pregunta)
 (list" El usuario"(autorPregunta pregunta)"pregunta:""\n"(getPregunta pregunta)" "(getDia(fechaPregunta pregunta))"/"
      (getMes(fechaPregunta pregunta))"/"(getAño(fechaPregunta pregunta))" ""likes:"(votosPosPregunta pregunta)
   "dislike:"(votosNegPregunta pregunta) "\n""'Tags'"" ID:"(idPregunta pregunta) "\n")))

;string de las respuestas
(define imprimirRespuestas (lambda(respuestas)
                        (if(null? respuestas)
                           null
                           (cons(ordenarRespuestas(primeraRespuesta respuestas))(imprimirRespuestas(sigRespuesta respuestas))))))
;ordena respuesta para ser impresa
(define ordenarRespuestas(lambda(respuesta)
  (list "El usuario"(autorRespuesta respuesta)"a respondido a la pregunta"(idPRespuesta respuesta)"\n"(getRespuesta respuesta)" "
  (getDia(fechaRespuesta respuesta))"/"(getMes(fechaRespuesta respuesta))"/"(getAño(fechaRespuesta respuesta))" ""likes:"(votoPosRespuesta respuesta)
                 "dislike:"(votoNegRespuesta respuesta) "\n"
  "'Tags'"(primerTag(tagsRespuesta respuesta))(segundoTag(tagsRespuesta respuesta))(tercerTag(tagsRespuesta respuesta))" ID:"(idRespuesta respuesta)" "(verEstado respuesta)"\n")))

;ve el estado de la respuesta
(define verEstado (lambda(respuesta)
                    (if(equal?(estadoRespuesta respuesta)0)
                       "NO A SIDO ACEPTADA"
                       " ")))
;string del stack recompensas
(define imprimirRecompensas (lambda(recompensas)
                        (if(null? recompensas)
                           null
                           (cons(ordenarRecompensa(primeraRecompensa recompensas))(imprimirUsuarios(sigRecompensa recompensas))))))
;ordena la recompensa
(define ordenarRecompensa (lambda(recompensa)
 (list "La pregunta con el ID"(idRecompensa recompensa)"dada por"(usuarioRecompensa recompensa)"la cantidad es"(getRecompensa recompensa)"\n"
       "y el usuario que respodio a esta pregunta fue"(usuarioResponde recompensa)"\n")))

#|;------------VOTE
;funcion principal de vote
(define vote(lambda(stack)(lambda(operacion)
                            (operacion stack))))

(define getQuestion (lambda(stack)(lambda(idP)(lambda(boolean)
                   (if(equal? boolean "true")
        (list(getUsuarios stack)(votarPositivoPregunta (getPreguntas stack) idP)(getRespuestas stack)usuarioInactivo (getRecompensas stack))
        (list(getUsuarios stack)(votarNegativoPregunta (getPreguntas stack) idP)(getRespuestas stack)usuarioInactivo (getRecompensas stack)))))))

;Buscar la pregunta para votar positivamente una pregunta
(define votarPositivoPregunta(lambda(preguntas idP)
                       (if(null? preguntas)
                          null
                          (if(equal?(idPregunta(primeraPregunta preguntas)) idP)
                             (cons(accionVotarPositivo(primeraPregunta Preguntas))(sigPregunta preguntas))
                             (cons(primeraPregunta preguntas)(votarPositivoPregunta(sigPregunta preguntas) idP))))))

;Buscar la pregunta para vota negativamente una pregunta
(define votarNegativoPregunta(lambda(preguntas idp)
                       (if(null? preguntas)
                          null
                          (if(equal?(idPregunta(primeraPregunta preguntas)) idP)
                             (cons(votarNegativoP(primeraPregunta Preguntas))(sigPregunta preguntas))
                             (cons(primeraPregunta preguntas)(votarNegativoPregunta(sigPregunta preguntas) idP))))))

|#

(define stackRecompensas null)
(define stackPreguntas null)
(define stackRespuestas null)
(define sinUsuarioActivo null)
(define stackUsuarios (list (usuarioNuevo "primero" 1234)(usuarioNuevo "segundo" 5678)(usuarioNuevo "tercero" 45)));stack de usuarios
(define stackOver (list stackUsuarios stackPreguntas stackRespuestas sinUsuarioActivo stackRecompensas))


"---"
(define SO2 (((login stackOver "segundo" 5678 ask)12 10 2020)"pregunta1" "et1" "et2" "et3"))
"------------"
(define SO3 (((login SO2 "segundo" 5678 reward)1)20))

(define SO4 ((((login SO3 "tercero" 45 answer)31 12 2020)1)"Respuesta1" "et1" "et2" "et3"))
(define SO5 (((login SO4 "segundo" 5678 accept)1)1))
(define SO6 (((login SO5 "segundo" 5678 ask)12 10 2020)"Pregunta2" "et1" "et2" "et3"))
(define SO7 ((((login SO6 "tercero" 45 answer)31 12 2020)2)"Respuesta2" "et1" "me" "h"))
(define SO8 (((login SO7 "segundo" 5678 accept)2)2))
(define SO9 (((login SO8 "primero" 1234 ask)12 10 2020)"pregunta3" "estado" "eos" "ers"))
(define SO10 ((((login SO9 "segundo" 5678 answer)31 12 2020)3)"Respuesta3" "medida" "me" "h"))
(define SO11 (((login SO10 "primero" 1234 accept)3)3))
(display (stack->string SO11))
