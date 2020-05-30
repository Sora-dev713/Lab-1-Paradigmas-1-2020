#lang racket
;---- TDAs Requeridos --------+
(require "TDA_ListArch.rkt")
(require "TDA_FileName.rkt")
(require "TDA_Archivo.rkt")
;-----------------------------+
;TDA FileList

;Representación:
;FileList = Null |
;           FileName X FileList

;-----------------------------------------------------------------

;Constructor

;Dominio: FileName X FileList
;Recorrido: FileList
(define (fileList F FL)
  (if (and (fileName? F) (fileList? FL))
      (cons F FL)
      '()))

;-----------------------------------------------------------------

;Funcion de pertenencia

;Dominio: FileList
;Recorrido: Boolan
(define (fileList? F)
  (if(or(and (list? F)(fileName? (car F))(null? (cdr F)))(null? fileList))
     #t
     (if (fileName? (car F))
         (fileList? (cdr F))
         #f)))

;-----------------------------------------------------------------



;-----------------------------------------------------------------
(provide (all-defined-out))