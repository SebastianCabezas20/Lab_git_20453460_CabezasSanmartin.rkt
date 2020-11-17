#lang racket

(provide (all-defined-out))
;TDA Pregunta ((ID x AUTOR x FECHA x PREGUNTA) x (TAGS) x (ID RESPUESTAS) x REWARD x V.positivo x V.negativo x NUMEROVISUALIZACIONES x ESTADO)
;Constructor (pregunta id autor fecha pregunta tags)
(define pregunta (lambda(id autor fecha pregunta tag1 tag2 tag3)
                   (list (list id autor fecha pregunta)(list tag1 tag2 tag3) null 0 3 4 0 0)))
;Selectores
;(idPregunta pregunta)(autorPregunta pregunta)(fechaPregunta pregunta)(Pregunta pregunta)(tagsPregunta pregunta)
;(idRespuestas pregunta)(reward pregunta)(votosPosPregunta pregunta)(votosNegPregunta pregunta)
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
(define votosPosPregunta (lambda(pregunta)
                         (cadr(cdddr pregunta))))
(define votosNegPregunta (lambda(pregunta)
                         (caddr(cdddr pregunta))))
(define numeroVisual (lambda(pregunta)
                         (cadddr(cdddr pregunta))))
(define estadoPregunta (lambda(pregunta)
                         (cadr(cdddr(cdddr pregunta)))))


;(primerTag pregunta)(segundoTag pregunta)(tercerTag pregunta)
;(primerosDatosPregunta pregunta)(ultimosDatosPregunta pregunta)

(define primerTag car)
(define segundoTag cadr)
(define tercerTag caddr)

(define primerosDatosPregunta car)
(define ultimosDatosPregunta(lambda(pregunta)
                              (cdddr(cdr pregunta))))

;Modificadores

;(addIdPregunta pregunta idRespuesta)
(define addIdPregunta (lambda(pregunta idResp)
 (list(primerosDatosPregunta pregunta)(tagsPregunta pregunta)(list idResp(idRespuestas pregunta))0 (votosPosPregunta pregunta)
   (votosNegPregunta pregunta)(numeroVisual pregunta)(estadoPregunta pregunta))))




