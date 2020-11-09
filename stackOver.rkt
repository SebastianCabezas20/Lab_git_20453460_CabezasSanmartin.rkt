#lang racket
;TDA stack
;(USUARIOS x PREGUNTAS x RESPUESTAS x USUARIOACTIVO)
;Selectores
;(getUsuarios stack)(getPreguntas stack)(getRespuestas stack)(getUsuarioActivo stack)(sigUsuariosStack stack)
(define getUsuarios car) (define getPreguntas cadr)(define getRespuestas caddr)(define getUsuarioActivo cadddr)

(define sigUsuariosStack (lambda(stack)
                           (list (getSigUsuario(getUsuarios stack))(getPreguntas)(getRespuestas)(getUsuarioActivo))))
;Modificador
;(addUsuarioActivo stack usuario)
(define addUsuarioActivo (lambda(stack usuario)
                           (list (getUsuarios stack)(getPreguntas)(getRespuestas)(setActividad usuario))))

;TDA fecha(DIA x MES x AÑO)
;Constructor (fecha dia mes año)
(define fecha (lambda(dia mes año)
                (list dia mes año)))

;TDA Usuario
;((USERNAME x PASS x REPUTACION x ACTIVIDAD)ID)
;Constructor (usuarioNuevo "username" pass)

;Pertenencia

;Selectores (getUsername usuario)(getPass usuario)(getReputacion usuario)(getActividad usuario)(getIds usuario)
;(define getIds ids) (getPrimerId ids) (getSigId ids) (getId ids id);busca en la lista de ids

;Modificadores
;(setActividad usuario)(addId id ids)(removeId id ids)

;TDA Usuarios
;(USUARIO x USUARIO x USUARIO)

;Selectores
;(getPrimerUsuario usuarios)(getSigUsuario usuarios)(getUsuario usuarios username)

;modificadores
;(agregarUsuario )

;----------------------------------------USUARIO--------------------------------------------
(define reputacionVacia 0)
(define idVacio null)

;Constructor
(define usuarioNuevo (lambda(username pass)
                    (cons(list username pass reputacionVacia "inactivo") idVacio)))
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
                          (cons(list (getUsername usuario)(getPass usuario)(getReputacion usuario)"inactivo")(getIds usuario))
                          (cons(list (getUsername usuario)(getPass usuario)(getReputacion usuario)"activo")(getIds usuario)))))
;(define addId )
;(define removeId)

;------------------------------------- STACK USUARIOS---------------------------------------------

(define stackUsuariosVacia null)
;Constructores 
;Selectores

(define getPrimerUsuario car)

(define getSigUsuario cdr)

(define getUsuario (lambda(stackUsuarios username)
                     (if(equal? stackUsuarios stackUsuariosVacia);si llega a null
                        stackUsuariosVacia
                        (if(equal? (getUsername(getPrimerUsuario stackUsuarios)) username)
                           (getPrimerUsuario stackUsuarios);usuario que estamos buscando lo retornamos
                           (getUsuario (getSigUsuario stackUsuarios) username)))));seguimos buscando 

;modificadores
(define agregarUsuario cons)

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
                (if(null?(getUsuarios stack))
                         stack
                         (if(and(equal?(getUsername(getPrimerUsuario(getUsuarios stack)))username)(equal?(getPass(getPrimerUsuario(getUsuarios stack)))pass))
                            (if(equal? operation "ask")
                               (lambda(pregunta)"pregunta")
                               "no")
                         (login (sigUsuariosStack stack)username pass operation)))))

;------------------ASK

(define stackPreguntas null)
(define stackRespuestas null)
(define stackUsuarios (list (usuarioNuevo "primero" 1234)(usuarioNuevo "segundo" 5678)(usuarioNuevo "tercero" 45)));stack de usuarios
(define stackOver (list stackUsuarios stackPreguntas stackRespuestas null))
(register stackOver "sebastian" 1234)

"---"
(login stackOver "primero" 1234 "ask")
"------------"
;(register stackUsuarios "sebastian" 123)

