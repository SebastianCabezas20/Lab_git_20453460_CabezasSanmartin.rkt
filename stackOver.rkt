#lang racket
;TDA stack
;(USUARIOS x PREGUNTAS x RESPUESTAS x USUARIOACTIVO X RECOMPENSAS)
;Selectores
;(getUsuarios stack)(getPreguntas stack)(getRespuestas stack)(getUsuarioActivo stack)(sigUsuariosStack stack)(getRecompensas stack)
(define getUsuarios car) (define getPreguntas cadr)(define getRespuestas caddr)(define getUsuarioActivo cadddr)

(define getRecompensas (lambda(stack)
                         (cdr(cdr(cdr(cdr stack))))))

(define sigUsuariosStack (lambda(stack)
                           (list (getSigUsuario(getUsuarios stack))(getPreguntas stack)(getRespuestas stack)(getUsuarioActivo stack))))
;Modificador
;(addUsuarioActivo stack usuario)
(define addUsuarioActivo (lambda(stack usuario)
                           (list (getUsuarios stack)(getPreguntas stack)(getRespuestas stack) usuario(getRecompensas stack))))
(define removeUsuarioActivo (lambda(stack)
                           (list (getUsuarios stack)(getPreguntas stack)(getRespuestas stack) null (getRecompensas stack))))

;TDA fecha(DIA x MES x AÑO)
;Constructor (fecha dia mes año)
(define fecha (lambda(dia mes año)
                (list dia mes año)))

;TDA recompensas (IdRespuesta x UsuarioRecompensa x recompensa x UsuarioResponde)
;Constructor (recompensa idRespuesta usuarioRecompensa recompensa)
(define recompensa (lambda(idRespuesta usuarioRecompensa recompensa)
                     (list idRespuesta usuarioRecompensa recompensa null)))

;Selectores (idRecompensa recompensa)(usuarioRecompensa recompensa)(getRecompensa recompensa)(usuarioResponde recompensa)
(define getIdRecompensa car)
(define getUsuarioRecompensa cadr)
(define getRecompensa caddr)
(define getUsuarioResponde cadddr)
;Modificadores (agregarUsuarioResponde recompensa username)
(define addUsuarioResponde (lambda(recompensa username)
                                 (list (getIdRecompensa recompensa)(getUsuarioRecompensa recompensa)(getRecompensa recompensa)username)))


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

;Modificadores
;(addPregunta stack pregunta);saca el estado activo del usuario
(define addPregunta (lambda(stack pregunta)
                      (list(getUsuarios stack)(cons pregunta (getPreguntas stack))(getRespuestas stack) null)))
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

;----------------------------------------USUARIO--------------------------------------------
(define reputacionVacia 0)
(define idVacio null)

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
                                   "no"))
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
  (cons(list(idPregunta(primeraPregunta preguntas))(autorPregunta(primeraPregunta preguntas))(fechaPregunta(primeraPregunta preguntas))(getPregunta(primeraPregunta preguntas))
       (tagsPregunta(primeraPregunta preguntas))(idRespuestas(primeraPregunta preguntas))1)(sigPregunta preguntas))
  (cons(primeraPregunta preguntas)(activarRecompensa(sigPregunta preguntas)id)))))

;añade recompensa en el stack
(define addRecompensa (lambda(stack usernameActivo id recompensaUsuario)
(list (restaRelativa (getUsuarios stack)usernameActivo recompensaUsuario)(activarRecompensa (getPreguntas stack) id)
  (getRespuestas stack) null (list(recompensa id usernameActivo recompensaUsuario)(getRecompensas stack)))))

(define reward (lambda(stack)
                 (lambda(idPregunta)
                   (lambda(recompensa)
                     (if(pair?(getUsuarioActivo stack))
                        (if( <= recompensa (getReputacionRelativa(getUsuarioActivo stack)))
                           (addRecompensa stack (getUsername(getUsuarioActivo stack)) idPregunta recompensa)
                           (removeUsuarioActivo stack))
                        (removeUsuarioActivo stack))))))


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

