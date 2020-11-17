#lang racket


(provide (all-defined-out))
;TDA respuestas ((ID x AUTOR x IDPREGUNTA x FECHA x RESPUESTA) x (TAGS)x ESTADO x V.positivo x V.negativo) 0=NO ACEPTADA 1 =ACEPTADA
;Constructor (respuesta id autor idPregunta fecha respuesta tags)
(define respuesta (lambda(id autor idPregunta fecha respuesta et1 et2 et3)
                    (list(list id autor idPregunta fecha respuesta)(list et1 et2 et3)0 1 2)))
;Selectores(idRespuesta respuesta)(autorRespuesta respuesta)(idPRespuesta respuesta)(fechaRespuesta respuesta)(getRespuesta respuesta)(tagsRespuesta respuesta)
;(estadoPregunta pregunta)(votoPositivoRespuesta pregunta)(votoNegativoRespuesta pregunta)
(define idRespuesta (lambda(respuesta)
                      (car(car respuesta))))
(define autorRespuesta (lambda(respuesta)
                      (cadr(car respuesta))))
(define idPRespuesta (lambda(respuesta)
                      (caddr(car respuesta))))
(define fechaRespuesta (lambda(respuesta)
                      (cadddr(car respuesta))))
(define getRespuesta (lambda(respuesta)
                      (cadr(cdddr(car respuesta)))))
(define tagsRespuesta cadr)
(define estadoRespuesta caddr)
(define votoPosRespuesta cadddr)
(define votoNegRespuesta(lambda(respuesta)
                               (cadr(cdddr respuesta))))


;(primerosDatosRespuesta pregunta)
(define primerosDatosRespuesta car)



