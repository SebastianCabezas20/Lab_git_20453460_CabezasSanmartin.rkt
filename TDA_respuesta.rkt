#lang racket


(provide (all-defined-out))

;TDA respuestas ((ID x AUTOR x IDPREGUNTA x FECHA x RESPUESTA) x (TAGS)x ESTADO x V.positivo x V.negativo) 0=NO ACEPTADA 1 =ACEPTADA

;Descr: permite la construcción de una respuesta
;Constructor (respuesta id autor idPregunta fecha respuesta tags) 
(define respuesta (lambda(id autor idPregunta fecha respuesta et1 et2 et3)
                    (list(list id autor idPregunta fecha respuesta)(list et1 et2 et3)0 1 2)))

;Selectores
;(idRespuesta respuesta)(autorRespuesta respuesta)(idPRespuesta respuesta)(fechaRespuesta respuesta)(getRespuesta respuesta)(tagsRespuesta respuesta)
;(estadoPregunta pregunta)(votoPositivoRespuesta pregunta)(votoNegativoRespuesta pregunta)

;Descr: seleccionar el id de la respuesta
(define idRespuesta (lambda(respuesta)
                      (car(car respuesta))))

;Descr: permite seleccionar el autor de la respuesta
(define autorRespuesta (lambda(respuesta)
                      (cadr(car respuesta))))

;Descr: permite seleccionar el id de la pregunta que responde la respuesta
(define idPRespuesta (lambda(respuesta)
                      (caddr(car respuesta))))
;Descr: permite seleccionar la fecha de la respuesta
(define fechaRespuesta (lambda(respuesta)
                      (cadddr(car respuesta))))
;Descr: permite seleccionar la respuesta
(define getRespuesta (lambda(respuesta)
                      (cadr(cdddr(car respuesta)))))
;Descr: permite seleccionar los tags de la respuesta
(define tagsRespuesta cadr)

;Descr: permite seleccionar el estado de la respuesta
(define estadoRespuesta caddr)

;Descr: permite seleccionar los votos positivos de la respuesta
(define votoPosRespuesta cadddr)

;Descr: permite seleccionar los votos negativos de la respuesta
(define votoNegRespuesta(lambda(respuesta)
                               (cadr(cdddr respuesta))))
;Descr: permite seleccionar el primer id de una lista de id respuestas
(define primerId car)

;Descr: permite seleccionar los siguientes id de una lista de id respuestas
(define sigId cdr)

;Descr: permite obtener los primeros datos de la respuesta
;(primerosDatosRespuesta pregunta)
(define primerosDatosRespuesta car)

(define primeraRespuesta car)
(define sigRespuesta cdr)

;modificadores

#|Dom: lista de respuestas x entero(id respuesta) 
  Rec: lista de respuestas
  Descr: cambia e estado de la respuesta a aceptada
  recursion: natural|#
(define cambiarEstado(lambda(respuestas idR)
                       (if(equal?(idRespuesta(primeraRespuesta respuestas)) idR)
       (cons(list(primerosDatosRespuesta(primeraRespuesta respuestas))(tagsRespuesta(primeraRespuesta respuestas))1(votoPosRespuesta(primeraRespuesta respuestas))
                 (votoNegRespuesta(primeraRespuesta respuestas)))(sigRespuesta respuestas))
       (cons(primeraRespuesta respuestas)(cambiarEstado(sigRespuesta respuestas)idR)))))



