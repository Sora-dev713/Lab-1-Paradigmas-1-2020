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
     (list L Comment)
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
;Selectores
;Funcion getCFiles: Obtiene los archivos del Commit
;Dominio: Commit
;Recorrido: FileList
(define (getCFiles C)
  (if (commit? C)
      (car C)
      '()))

;Funcion getComment: Obtiene los comentarios del Commit
;Dominio: Commit
;Recorrido: String
(define (getComment C)
  (if (commit? C)
      (cadr C)
      '()))
;-----------------------------------------------------------------
;Funciones Auxiliares:


;CommitToString
(define (commitToString C)
  (if (commit? C)
      (string-append "Commit: \n->Archivos: " (FlToString (getCFiles C)) "\n->Comentario: " (getComment C) "\n")
      "ERROR"))



;-----------------------------------------------------------------
(provide (all-defined-out))