#lang racket
;---- TDAs Requeridos --------+
(require "TDA_FileList.rkt")
(require "TDA_Workspace.rkt")
;-----------------------------+


;------------------------- TDA Index -----------------------------

;Represetacion: ["Index", Lista_Archivos]

;-----------------------------------------------------------------
;Costructor

;Dominio: FileList
;Recorrido: Index

(define (Index A)
  (if (fileList? A)
      (list "Index" A)
      (list "Index" '())))

;-----------------------------------------------------------------
;Funcion de pertenencia

;Domio: Index
;Recorrido:Boolean

(define (index? I)
  (and(list? I)
      (not(null? I))
      (eq? "Index" (car I))
      (fileList? (cadr I))
      (null? (cddr I))))

;-----------------------------------------------------------------
;Funcion selectora

;getNameI
;Dominio: Index
;Recorrido: String
(define (getNameI I)
  (if(index? I)
     (car I)
     '()))

;getListI
;Dominio: Index
;Recorrido: FileList
(define (getListI I)
  (if(index? I)
     (cadr I)
     '()))
;-----------------------------------------------------------------
;Funciones Modificadoras

;setListI
;Dominio: Index X FileList
;Recorrido: Index
(define (setFList I L)
  (if (and(index? I)(fileList? L))
      (cons (car I)(cons L '()))
      '()))
;-----------------------------------------------------------------
;--------------------Funciones Auxiliares-------------------------
;-----------------------------------------------------------------

;PrepareFiles
;Funcion que permite verificar que un archivo este en el workspace
;Dominio: FileList X Workspace X FileList
;Recorrido: FileList
;Tipo de Recursion: de cola
(define (PrepareFiles L W L2)
  (if (fileList? L)
      (if (null? L)
          L2
          (if (and (workspace? W) (fileList? L2))
              (if (checkName (car L)(getWFileList W))
                  (PrepareFiles (cdr L) W (cons (car L) L2))
                  (PrepareFiles (cdr L) W L2))
              '()))
      '()))
;-----------------------------------------------------------------
;IndexToString

;Desc: Funcion que permite el representar el Index como un string
;Dominio: Index
;Recorrido: String
(define (indexToString I)
  (if (index? I)
      (if (null? (getListI I))
          "Index: \n->Archivos: Vacio.\n"
          (string-append "Index: \n->Archivos: " (FlToString (getListI I))))
         "ERROR\n"))
;-----------------------------------------------------------------
(provide (all-defined-out))