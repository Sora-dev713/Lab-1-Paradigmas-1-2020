#lang racket
;---- TDAs Requeridos --------+
(require "TDA_FileList.rkt")
(require "TDA_Commit.rkt")
;-----------------------------+
;TDA CommitList

;Representación:
;CommitList = Null |
;           Commit X CommitList

;Constructor

;Dominio:Commit X CommitList
;Recorrido: CommitList
(define (commitList C Cl)
  (if (and (commit? C) (commitList? Cl))
      (cons C Cl)
      '()))

;Funcion de pertenencia

;Dominio: FileList
;Recorrido: Boolean
;Recursion: Lineal
(define (commitList? Cl)
  (if (null? Cl)
      #t
      (if(and (list? Cl)(commit? (car Cl))(null? (cdr Cl)))
         #t
         (if (commit? (car Cl))
             (commitList? (cdr Cl))
             #f))))



;UnifyCFiles
;Funcion que obtiene una lista de los archivos de todos los commits
;Dominio: CommitList X FileList
;Recurrido: FileList
;Recursion: Cola
(define (unifyCFiles Cl Fl)
  (if (and (commitList? Cl)(fileList? Fl))
      (if (null? Cl)
          Fl
          (unifyCFiles (cdr Cl) (mergeFiles (getCFiles (car Cl)) Fl)))
      '()))


;-----------------------------------------------------------------
(provide (all-defined-out))    