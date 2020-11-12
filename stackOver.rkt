#lang racket
;TDA stack
;(USUARIOS x PREGUNTAS x RESPUESTAS x USUARIOACTIVO X RECOMPENSAS)
;Selectores
;(getUsuarios stack)(getPreguntas stack)(getRespuestas stack)(getUsuarioActivo stack)(sigUsuariosStack stack)(getRecompensas stack)
(define getUsuarios car) (define getPreguntas cadr)(define getRespuestas caddr)(define getUsuarioActivo cadddr)

(define getRecompensas (lambda(stack)
                         (car(cdr(cdr(cdr(cdr stack)))))))

(define sigUsuariosStack (lambda(stack)
                           (list (getSigUsuario(getUsuarios stack))(getPreguntas stack)(getRespuestas stack)(getUsuarioActivo stack)(getRecompensas stack))))
;Modificador
;(addUsuarioActivo stack usuario)
(define addUsuarioActivo (lambda(stack usuario)
                           (list (getUsuarios stack)(getPreguntas stack)(getRespuestas stack) usuario(getRecompensas stack))))
(define removeUsuarioActivo (lambda(stack)
                           (list (getUsuarios stack)(getPreguntas stack)(getRespuestas stack) usuarioInactivo (getRecompensas stack))))

;TDA fecha(DIA x MES x AÑO)
;Constructor (fecha dia mes año)
(define fecha (lambda(dia mes año)
                (list dia mes año)))

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


;TDA Pregunta ((ID x AUTOR x FECHA x PREGUNTA) x (TAGS) x (ID RESPUESTAS) x REWARD) REWARD 0 = NO 1 = SI
;Constructor (pregunta id autor fecha pregunta tags)
(define pregunta (lambda(id autor fecha pregunta tag1 tag2 tag3)
                   (list (list id autor fecha pregunta)(list tag1 tag2 tag3) null 0)))
;Selectores
;(idPregunta pregunta)(autorPregunta pregunta)(fechaPregunta pregunta)(Pregunta pregunta)(tagsPregunta pregunta)
;(idRespuestas pregunta)(reward pregunta)
(define primeraPregunta car)
(define sigPregunta cdr)
(define tagsPregunta cadr)
(define idPregunta (lambda(pregunta)
                     (car(car pregunta))))
(define autorPregunta (lambda(pregunta)
                        (cadr(car pregunta))))
(define fechaPregunta (lambda(pregunta)
                        (caddr(car pregunta))))
(define getPregunta (lambda(pregunta)
                      (cadddr(car pregunta))))
(define idRespuestas caddr)
(define getReward cadddr)
(define primerosDatosPregunta car)

;Modificadores
;(addPregunta stack pregunta);saca el estado activo del usuario
(define addPregunta (lambda(stack pregunta)
                      (list(getUsuarios stack)(cons pregunta (getPreguntas stack))(getRespuestas stack) usuarioInactivo (getRecompensa stack))))
;((ID x AUTOR x FECHA x PREGUNTA) x (TAGS) x (ID RESPUESTAS) x REWARD)
(define addIdPregunta (lambda(pregunta idResp)
                        (list(primerosDatosPregunta pregunta)(tagsPregunta pregunta)(cons idResp(idRespuestas pregunta))(getReward pregunta))))

;otras funciones
;(contador preguntas)
(define contador (lambda(preguntas)
                   (if(null? preguntas)
                      0
                      (+ (contador(sigPregunta preguntas))1))))

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



;TDA Usuarios
;(USUARIO x USUARIO x USUARIO)

;Selectores
;(getPrimerUsuario usuarios)(getSigUsuario usuarios)
(define getPrimerUsuario car)

(define getSigUsuario cdr)

;modificadores
(define agregarUsuario cons)

;TDA respuestas (ID x AUTOR x IDPREGUNTA x FECHA x RESPUESTA x (TAGS)ESTADO)ESTADO 0=NO ACEPTADA 1 =ACEPTADA
;Constructor (respuesta id autor idPregunta fecha respuesta tags)
(define respuesta (lambda(id autor idPregunta fecha respuesta et1 et2 et3)
                    (cons(cons(list id autor idPregunta fecha respuesta)(list et1 et2 et3))0)))
;Selectores(idRespuesta respuesta)(autorRespuesta respuesta)(idPRespuesta respuesta)(fechaRespuesta respuesta)(getRespuesta respuesta)(tagsRespuesta respuesta)
(define idRespuesta (lambda(respuesta)
                      (car(car respuesta))))
(define autorRespuesta (lambda(respuesta)
                      (cadr(car respuesta))))
(define idPRespuesta (lambda(respuesta)
                      (caddr(car respuesta))))
(define fechaRespuesta (lambda(respuesta)
                      (cadddr(car respuesta))))
(define getRespuesta (lambda(respuesta)
                      (cdr(cdddr respuesta))))
(define tagsRespuesta cadr)
(define estadoRespuesta cddr)

;Modificadores (addRespuesta stack respuesta)
;añade respuesta a stack de respuestas Y REGISTRA SI TIENE RECOMPENSA
(define addRespuesta (lambda(stack respuesta idP)
                       (list(getUsuarios stack)(getPreguntas stack)(cons respuesta (getRespuestas stack))usuarioInactivo
                            (addUsuarioRecompensa(getRecompensas stack)idP (getUsername(getUsuarioActivo stack))))))

;----------------------------------------USUARIO--------------------------------------------
(define reputacionVacia 0)
(define idVacio null)
(define usuarioInactivo null)

;------------------------------------- STACK USUARIOS---------------------------------------------

(define stackUsuariosVacia null)

;------------REGISTER
(define registerFuncion (lambda(stack username pass)
                          (if(null? stack)
                             (usuarioNuevo username pass)
                             (if(equal?(getUsername(getPrimerUsuario stack))username)
                                (cons(getPrimerUsuario stack)(getSigUsuario stack))
                                (cons(getPrimerUsuario stack)(registerFuncion (getSigUsuario stack) username pass))))))

(define register (lambda(stack username pass)
                   (list (registerFuncion(getUsuarios stack)username pass)(getPreguntas stack)(getRespuestas stack)(getUsuarioActivo stack))))

;------------------------LOGIN

(define login (lambda(stack username pass operation)
                (funcionLogin stack username pass operation stack)))

(define funcionLogin (lambda(stack username pass operation stackFinal)
                       (if(null?(getUsuarios stack))
                          stack
                          (if(and(equal?(getUsername(getPrimerUsuario(getUsuarios stack)))username)(equal?(getPass(getPrimerUsuario(getUsuarios stack)))pass))
                             (if(equal? operation "ask")
                                (ask (addUsuarioActivo stackFinal (getPrimerUsuario(getUsuarios stack))))
                                (if(equal? operation "reward")
                                   (reward (addUsuarioActivo stackFinal (getPrimerUsuario(getUsuarios stack))))
                                   (if(equal? operation "answer")
                                      (answer (addUsuarioActivo stackFinal (getPrimerUsuario(getUsuarios stack))))
                                      (if(equal? operation "accept")
                                         (accept (addUsuarioActivo stackFinal (getPrimerUsuario(getUsuarios stack))))
                                         "no"))))
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
(define activarRecompensa (lambda(preguntas id)
                            (if(equal?(idPregunta(primeraPregunta preguntas))id)
  (cons(list(list(idPregunta(primeraPregunta preguntas))(autorPregunta(primeraPregunta preguntas))(fechaPregunta(primeraPregunta preguntas))(getPregunta(primeraPregunta preguntas)))
       (tagsPregunta(primeraPregunta preguntas))(idRespuestas(primeraPregunta preguntas))1) (sigPregunta preguntas))
  (cons(primeraPregunta preguntas)(activarRecompensa(sigPregunta preguntas)id)))))

;añade recompensa en el stack
(define addRecompensa (lambda(stack usernameActivo id recompensaUsuario)
(list (restaRelativa (getUsuarios stack)usernameActivo recompensaUsuario)(activarRecompensa (getPreguntas stack) id)
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
      (addIdP(getPreguntas stack) idP idR)(getRespuestas stack)usuarioInactivo(removeRecompensa(getRecompensas stack)idP))))

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
    (list(getUsuarios stack)(addIdP(getPreguntas stack) idP idR)(getRespuestas stack)usuarioInactivo(getRecompensas stack))))


(define stackRecompensas null)
(define stackPreguntas null)
(define stackRespuestas null)
(define sinUsuarioActivo null)
(define stackUsuarios (list (usuarioNuevo "primero" 1234)(usuarioNuevo "segundo" 5678)(usuarioNuevo "tercero" 45)));stack de usuarios
(define stackOver (list stackUsuarios stackPreguntas stackRespuestas sinUsuarioActivo stackRecompensas))


"---"
(define SO2 (((login stackOver "segundo" 5678 "ask")12 10 2020)"cuanto es el estado" "estado" "eos" "ers"))
"------------"
(define SO3 (((login SO2 "segundo" 5678 "reward")1)20))

(define SO4 ((((login SO3 "tercero" 45 "answer")31 12 2020)1)"la medida es 1" "medida" "me" "h"))
(define SO5 (((login SO4 "segundo" 5678 "accept")1)1))

