#lang racket


(provide(all-defined-out))
(define usuarioInactivo null)

;(USUARIOS x PREGUNTAS x RESPUESTAS x USUARIOACTIVO X RECOMPENSAS)
;Selectores
;(getUsuarios stack)(getPreguntas stack)(getRespuestas stack)(getUsuarioActivo stack)(getRecompensas stack)(getPrimerUsuario usuarios)(getSigUsuario usuarios)
;(primeraPregunta pregunta)(sigPregunta pregunta)(primeraRespuesta pregunta)(sigRespuesta pregunta)

;realiza la seleccion de la lista de usuarios
(define getUsuarios car)

;realiza la seleccion de la lista de preguntas
(define getPreguntas cadr)

;realiza la seleccion de la lista de respuestas
(define getRespuestas caddr)

;realiza la seleccion del usuario activo
(define getUsuarioActivo cadddr)

;realiza la seleccion de la lista de usuarios
(define getRecompensas (lambda(stack)
                         (car(cdr(cdr(cdr(cdr stack)))))))

;realiza la seleccion de los siguientes usuarios en la lista
(define sigUsuariosStack (lambda(stack)
                           (list (getSigUsuario(getUsuarios stack))(getPreguntas stack)(getRespuestas stack)(getUsuarioActivo stack)(getRecompensas stack))))

(define getPrimerUsuario car)

(define getSigUsuario cdr)


;modificadores
;(addUsuarioActivo stack usuario) (sigUsuariosStack stack)(removeUsuarioActivo)(addPregunta stack pregunta)

;añade usuario activo en el stack
(define addUsuarioActivo (lambda(stack usuario)
                           (list (getUsuarios stack)(getPreguntas stack)(getRespuestas stack) usuario(getRecompensas stack))))

;remueve usuario activo en el stack
(define removeUsuarioActivo (lambda(stack)
                           (list (getUsuarios stack)(getPreguntas stack)(getRespuestas stack)null (getRecompensas stack))))


;añade pregunta al stack
(define addPregunta (lambda(stack pregunta)
                      (list(getUsuarios stack)(cons pregunta (getPreguntas stack))(getRespuestas stack) usuarioInactivo (getRecompensas stack))))

;otras funciones
;(contador preguntas)
;realiza la cuenta de una cierta lista
(define contador (lambda(lista)
                   (if(null? lista)
                      0
                      (+ (contador(cdr lista))1))))



