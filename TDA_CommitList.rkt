#lang racket
;---- TDAs Requeridos --------+
(require "TDA_FileList.rkt")
(require "TDA_Commit.rkt")
;-----------------------------+

;---------------------- TDA CommitList ---------------------------

;Representación:
;CommitList = Null |
;           Commit X CommitList

;-----------------------------------------------------------------
;Constructor

;Dominio:Commit X CommitList
;Recorrido: CommitList

(define (commitList C Cl)
  (if (and (commit? C) (commitList? Cl))
      (cons C Cl)
      '()))

;-----------------------------------------------------------------
;Funcion de pertenencia

;Dominio: FileList
;Recorrido: Boolean
;Recursion: de Cola

(define (commitList? Cl)
  (if (null? Cl)
      #t
      (if(and (list? Cl)(commit? (car Cl))(null? (cdr Cl)))
         #t
         (if (commit? (car Cl))
             (commitList? (cdr Cl))
             #f))))

;-----------------------------------------------------------------
;--------------------- Operadores Auxiliares ---------------------
;-----------------------------------------------------------------
;UnifyCFiles

;Descripcion: Funcion que obtiene una lista de los archivos
;             de todos los commits

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
;CommitListToString

;Descripcion: funcion que mediante llamados recursivos, pasa a String toda una
;             lista de commits, colocandole su numero respectivo.

;Dominio: CommitList X Entero
;Recorrido: String
;Tipo de Recursion: Natural.

(define (CommitListToString Cl num)
  (if(and(commitList? Cl)(null? (cdr Cl)))
     (string-append "->" (number->string num)"° "(commitToString (car Cl)));Caso por si no hay commits
     (if (and(commitList? Cl)(integer? num))
         (string-append "->" (number->string num)"° "(commitToString (car Cl)) (CommitListToString (cdr Cl) (+ num 1)));caso por si hay 1 o mas commits
         "ERROR\n")));En caso de no entregar un CommitList, se da un mensaje de error

;-----------------------------------------------------------------
(provide (all-defined-out))    