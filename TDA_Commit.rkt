#lang racket

(require "TDA_FileList.rkt")

;------------------------TDA Commit-------------------------------

;Representacion: [FileList, "Comentario"]

;-----------------------------------------------------------------
;Constructor

;Domino: FileList X String
;Constructor: Commit
(define (CrearCommit L Comment)
  (if(and(fileList? L)(string? Comment))
     (list L comment)
     '()))
;-----------------------------------------------------------------
;Funcion de Pertenencia

;Domino: Commit
;Recorrido: Boolean
(define (commit? C)
  (if(and(list? C)(fileList? (car C))(string? (cadr C))(null? (cddr C)))
     #t
     #f))

;-----------------------------------------------------------------
(provide (all-defined-out))