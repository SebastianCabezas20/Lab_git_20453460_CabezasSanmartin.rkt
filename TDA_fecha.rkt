#lang racket
(provide(all-defined-out))
;TDA fecha(DIA x MES x AÑO)
;Constructor (fecha dia mes año)
;realiza la construccion de la fecha
(define fecha (lambda(dia mes año)
                (list dia mes año)))
;Selectores

;realiza la seleccion del dia en la fecha 
(define getDia car)

;realiza la seleccion del mes en la fecha 
(define getMes cadr)

;realiza la seleccion del año en la fecha 
(define getAño caddr)