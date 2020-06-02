#lang racket
;---- TDAs Requeridos --------+
(require "TDA_FileList.rkt")
;-----------------------------+

;----------------------- TDA Commit ------------------------------

;Representacion: [FileList, "Comentario"]

;-----------------------------------------------------------------
;Constructor

;Domino: FileList X String
;Constructor: Commit

(define (crearCommit L Comment)
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
;Funciones Selectoras
;+----------------------------------------+
;getCFiles

;Desc: Obtiene los archivos del Commit

;Dominio: Commit
;Recorrido: FileList

(define (getCFiles C)
  (if (commit? C)
      (car C)
      '()))

;+----------------------------------------+
;getComment

;Desc: Obtiene los comentarios del Commit

;Dominio: Commit
;Recorrido: String

(define (getComment C)
  (if (commit? C)
      (cadr C)
      '()))
;+----------------------------------------+
;-----------------------------------------------------------------
;--------------------- Operadores Auxiliares ---------------------
;-----------------------------------------------------------------
;commitToString

;Descripcion: Funcion que convierte un Commit en un String representativo
;             con la misma información.

;Dominio: Commit
;Recorrido: String

(define (commitToString C)
  (if (commit? C)
      (string-append "Commit: \n-->Archivos: " (FlToString (getCFiles C)) "-->Comentario: " (getComment C) "\n")
      "ERROR"))


;-----------------------------------------------------------------
(provide (all-defined-out))