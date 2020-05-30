#lang racket

;------------------------TDA FileName-------------------------------

;Representacion: String = "NombreArchivo"."Extension"

;-----------------------------------------------------------------
;Constructor

;Dominio: String X String
;Recorrido: FileName

(define (fileName Nombre Extension)
  (if (and (string? Nombre)(<= (string-length Nombre) 0)
            (string? Extension)(<= (string-length Extension) 0))
      (string-join (list Nombre Extension) ".")
      '()))

;-----------------------------------------------------------------
;Funcion Pertenencia

;Dominio: FileName
;Recorrido: Boolean

(define (fileName? F)
  (if (and (string? F)
           (> (string-length (car (string-split F "."))) 0)
           (> (string-length (cadr (string-split F "."))) 0))
      #t
      #f))
;-----------------------------------------------------------------
(provide (all-defined-out))