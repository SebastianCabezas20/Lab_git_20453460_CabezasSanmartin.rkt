#lang racket


(provide(all-defined-out))
(define usuarioInactivo null)

;(USUARIOS x PREGUNTAS x RESPUESTAS x USUARIOACTIVO X RECOMPENSAS)
;Selectores
;(getUsuarios stack)(getPreguntas stack)(getRespuestas stack)(getUsuarioActivo stack)(getRecompensas stack)(getPrimerUsuario usuarios)(getSigUsuario usuarios)
;(primeraPregunta pregunta)(sigPregunta pregunta)(primeraRespuesta pregunta)(sigRespuesta pregunta)
(define getUsuarios car)
(define getPreguntas cadr)
(define getRespuestas caddr)
(define getUsuarioActivo cadddr)

(define getRecompensas (lambda(stack)
                         (car(cdr(cdr(cdr(cdr stack)))))))
(define getPrimerUsuario car)

(define getSigUsuario cdr)

(define primeraPregunta car)
(define sigPregunta cdr)
(define primeraRespuesta car)
(define sigRespuesta cdr)

;modificadores
;(addUsuarioActivo stack usuario) (sigUsuariosStack stack)(removeUsuarioActivo)(addPregunta stack pregunta)
(define addUsuarioActivo (lambda(stack usuario)
                           (list (getUsuarios stack)(getPreguntas stack)(getRespuestas stack) usuario(getRecompensas stack))))
(define removeUsuarioActivo (lambda(stack)
                           (list (getUsuarios stack)(getPreguntas stack)(getRespuestas stack)null (getRecompensas stack))))

(define sigUsuariosStack (lambda(stack)
                           (list (getSigUsuario(getUsuarios stack))(getPreguntas stack)(getRespuestas stack)(getUsuarioActivo stack)(getRecompensas stack))))

;saca el estado activo del usuario
(define addPregunta (lambda(stack pregunta)
                      (list(getUsuarios stack)(cons pregunta (getPreguntas stack))(getRespuestas stack) usuarioInactivo (getRecompensas stack))))

;otras funciones
;(contador preguntas)
(define contador (lambda(preguntas)
                   (if(null? preguntas)
                      0
                      (+ (contador(sigPregunta preguntas))1))))



