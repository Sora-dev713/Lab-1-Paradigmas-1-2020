#lang racket
;---- TDAs Requeridos --------+
(require "TDA_Archivo.rkt")
;-----------------------------+
;-----------------------------------------------------------------
;TDA ListArch (Lista Archivos)
;Representación:
;ListArch = Null |
;           Archivo X ListArch

;Dominio: Archivo X ListArch
;Recorrido: listArch
(define (listArch A LsAr)
  (if (and (archivo? A)(listArch? LsAr))
      (cons A LsAr)
      '()))


;Funcion Pertenencia
;Dominio: ListArch
;Recorrido: Boolean
(define (listArch? L)
  (if (null? L)
      #t
      (if(and (list? L)(archivo? (car L))(null? (cdr L)))
             #t
             (if (archivo? (car L))
                 (listArch? (cdr L))
                 #f))))

;-----------------------------------------------------------------
(provide (all-defined-out))