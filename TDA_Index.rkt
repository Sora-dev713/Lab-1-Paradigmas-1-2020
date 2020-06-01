#lang racket
(require "TDA_FileList.rkt")
(require "TDA_Workspace.rkt")

;TDA Index
;Represetacion: ["Index", Lista_Archivos]
;costructor
;Dominio: Archivo/S
;Recorrido: Index
(define (Index A)
  (if (fileList? A)
      (list "Index" A)
      (list "Index" '())))

;Funcion de pertenencia
;Domio: Index
;Recorrido:
(define (index? I)
  (and(list? I)
      (not(null? I))
      (eq? "Index" (car I))
      (fileList? (cadr I))
      (null? (cddr I))))

;Funcion selectora Nombre Index
;Dominio: Index
;recorrido: String
(define (getNameI I)
  (if(index? I)
     (car I)
     '()))

;Funcion selectoraArchivos (index)
;Dominio: Index
;recorrido: ListaArchivos
(define (getListI I)
  (if(index? I)
     (cadr I)
     '()))

;Funciones Modificadoras
(define (setFList I L)
  (if (and(index? I)(fileList? L))
      (cons (car I)(cons L '()))
      '()))

;---------------------------Funciones Auxiliares-------------------------------


;PrepareFiles
;Funcion que permite verificar que un archivo este en el workspace
;Dominio: FileList X Workspace X FileList
;Recorrido: FileList
;tipo de Recursion: de cola
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
(provide (all-defined-out))