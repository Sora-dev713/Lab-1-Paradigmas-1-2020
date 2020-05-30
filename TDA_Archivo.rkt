#lang racket
;---- TDAs Requeridos --------+
(require "TDA_FileName.rkt")


;Definicion de TDAs
;TDA Archivo:
;Representación: [Filename, contenido]
;Constructor:
;Dominio: FileName X String
;Recorrido: Archivo
(define (archivo FN contenido)
  (if(and (fileName? FN)(string? contenido)(<= (string-length contenido) 0))
     (list FN contenido)
     '())
  )
;----------------------------------------------------------------
;Funcion de Pertenencia:
;Dominio:
;Recorrido:
(define (archivo? A)
  (and (list? A)
       (not(null? A))
       (fileName? (car A))
       (string? (cadr A))
       (> (string-length (cadr A)) 0)
       (null? (caddr A))))

;----------------------------------------------------------------
;Selectores:
;getFName: Obtiene el nombre del archivo
;Dominio: Archivo
;Recorrido: FileName
(define (getFName A)
  (if (archivo? A)
      (car A)
      '()))

;getContent: Obtiene el contenido del Archivo
;Dominio: Archivo
;Recorrido: String
(define (getContent A)
  (if (archivo? A)
      (cadr A)
      '()))

;-----------------------------------------------------------------
(provide (all-defined-out))
