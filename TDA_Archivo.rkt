#lang racket

;Definicion de TDAs
;TDA Archivos:
;Representación: [Filename, contenido]
;Constructor:
(define (archivo Nombre contenido)
  (if(and (fileName? Nombre)(string? contenido)(<= (string-length contenido) 0))
     (list Nombre contenido)
     '())
  )

;pertenencia:
(define (archivo? A)
  (and (list? A)
       (not(null? A))
       (fileName? (car a))
       (string? (cadr A))
       (> (string-length (cadr A)) 0)
       (null? (caddr A)))

;Selectores:
(define (getName A)
  (if (archivo? A)
      (car A)
      '()))

(define (getContent A)
  (if (archivo? A)
      (cadr A)
      '()))