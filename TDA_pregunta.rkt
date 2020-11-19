#lang racket

(provide (all-defined-out))
;TDA Pregunta ((ID x AUTOR x FECHA x PREGUNTA) x (TAGS) x (ID RESPUESTAS) x REWARD x V.positivo x V.negativo x ESTADO)

;Descr: permite construir una pregunta
;Constructor (pregunta id autor fecha pregunta tags)
(define pregunta (lambda(id autor fecha pregunta tag1 tag2 tag3)
                   (list (list id autor fecha pregunta)(list tag1 tag2 tag3) null 0 3 4 0)))
;Selectores
;(idPregunta pregunta)(autorPregunta pregunta)(fechaPregunta pregunta)(Pregunta pregunta)(tagsPregunta pregunta)
;(idRespuestas pregunta)(reward pregunta)(votosPosPregunta pregunta)(votosNegPregunta pregunta)

;Descr: permite seleccionar los tags de la pregunta
(define tagsPregunta cadr)

;Descr: permite seleccionar el id de la pregunta
(define idPregunta (lambda(pregunta)
                     (car(car pregunta))))

;Descr: permite seleccionar el autor de la pregunta
(define autorPregunta (lambda(pregunta)
                        (cadr(car pregunta))))

;Descr: permite seleccionar la fecha de la pregunta
(define fechaPregunta (lambda(pregunta)
                        (caddr(car pregunta))))

;Descr: permite seleccionar la pregunta
(define getPregunta (lambda(pregunta)
                      (cadddr(car pregunta))))

;Descr: permite seleccionar los id de respuestas a esa pregunta 
(define idRespuestas caddr)

;Descr: permite seleccionar la recompensa de la pregunta
(define getReward cadddr)

;Descr: permite seleccionar los votos positivos de la pregunta
(define votosPosPregunta (lambda(pregunta)
                         (cadr(cdddr pregunta))))

;Descr: permite seleccionar los votos negativos de la pregunta
(define votosNegPregunta (lambda(pregunta)
                         (caddr(cdddr pregunta))))

;Descr: permite seleccionar el estado de la pregunta
(define estadoPregunta (lambda(pregunta)
                        (cadddr(cdddr pregunta))))


;(primerTag pregunta)(segundoTag pregunta)(tercerTag pregunta)
;(primerosDatosPregunta pregunta)(ultimosDatosPregunta pregunta)

;Descr: permite seleccionar el primer tag de la pregunta
(define primerTag car)

;Descr: permite seleccionar el segundo tag de la pregunta
(define segundoTag cadr)

;Descr: permite seleccionar el tercer tag de la pregunta
(define tercerTag caddr)

;Descr: permite seleccionar los primeros datos de la pregunta
(define primerosDatosPregunta car)

;Descr: permite seleccionar la primera pregunta
(define primeraPregunta car)

;Descr: permite seleccionar la siguiente pregunta
(define sigPregunta cdr)

#|Dom: lista de preguntas
  Rec: lista de preguntas
  Descr: selecciona pregunta a para añadir voto positivo
  recursion: natural, se necesita la lista completa|#
;Buscar la pregunta para votar positivamente una pregunta
(define votarPositivoPregunta(lambda(preguntas idP)
                       (if(null? preguntas)
                          null
                          (if(equal?(idPregunta(primeraPregunta preguntas)) idP)
                             (cons(accionVotarPositivoP(primeraPregunta preguntas))(sigPregunta preguntas))
                             (cons(primeraPregunta preguntas)(votarPositivoPregunta(sigPregunta preguntas) idP))))))
#|Dom: lista de preguntas
  Rec: lista de preguntas
  Descr: selecciona pregunta a para añadir voto negativo
  recursion: natural, se necesita la lista completa|#
(define votarNegativoPregunta(lambda(preguntas idP)
                       (if(null? preguntas)
                          null
                          (if(equal?(idPregunta(primeraPregunta preguntas)) idP)
                             (cons(accionVotarNegativoP(primeraPregunta preguntas))(sigPregunta preguntas))
                             (cons(primeraPregunta preguntas)(votarNegativoPregunta(sigPregunta preguntas) idP))))))

;Modificadores

;(addIdPregunta pregunta idRespuesta)
;Descr: permite añadir un id de una respuesta a la lista de id de respuestas
(define addIdPregunta (lambda(pregunta idResp)
 (list(primerosDatosPregunta pregunta)(tagsPregunta pregunta)(cons idResp(idRespuestas pregunta))0 (votosPosPregunta pregunta)
   (votosNegPregunta pregunta)0)))

#|Dom: lista de preguntas x  entero(id pregunta) x entero(id respuesta) 
  Rec: lista de preguntas
  Descr: selecciona la pregunta a la cual se añadira el id de una determinada respuesta
  recursion: natural, se necesita la lista completa|#
(define addIdP (lambda(preguntas idPreg idResp)
                  (if(null? preguntas)
                     null
                     (if(equal?(idPregunta(primeraPregunta preguntas))idPreg)
                        (cons(addIdPregunta (primeraPregunta preguntas)idResp)(sigPregunta preguntas))
                        (cons(primeraPregunta preguntas)(addIdP(sigPregunta preguntas)idPreg idResp))))))

#|Dom: pregunta 
  Rec: pregunta
  Descr: suma un voto negativo pregunta
  |#
(define accionVotarNegativoP (lambda(pregunta)
      (list(primerosDatosPregunta pregunta)(tagsPregunta pregunta)(idRespuestas pregunta)(getReward pregunta)(votosPosPregunta pregunta)
          (+(votosNegPregunta pregunta)1)(estadoPregunta pregunta))))

#|Dom: pregunta 
  Rec: pregunta
  Descr: suma un voto positivo a pregunta
  |#
;realiza la accion de sumar voto positivo
(define accionVotarPositivoP (lambda(pregunta)
      (list(primerosDatosPregunta pregunta)(tagsPregunta pregunta)(idRespuestas pregunta)(getReward pregunta)(+(votosPosPregunta pregunta)1)
          (votosNegPregunta pregunta)(estadoPregunta pregunta))))






