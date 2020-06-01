#lang racket
;---- TDAs Requeridos --------+
(require "TDA_FileList.rkt")
;-----------------------------+
;TDA Workspace
;Represetacion: ["Workspace", Lista_Archivos]
;costructor
;Dominio: Archivo/S
;Recorrido: Workspace
(define (createWorkspace A)
  (if (fileList? A)
      (list "Workspace" A)
      '()))
;-----------------------------------------------------------------
;Funcion de pertenencia
;Domio: Workspace
;Recorrido: Boolean
(define (workspace? W)
  (and(list? W)
      (not(null? W))
      (eq? "Workspace" (car W))
      (fileList? (cadr W))
      (null? (cddr W))
      )
  )
;-----------------------------------------------------------------
;Funciones selectoras
;Funcion GetNameW
;Dominio: Workspace
;Recorrido String
(define (getWFiles W)
  (if (workspace? W)
      (car W)
      '()))

;Funcion GetListArcW
;dominio: Workspace
;Recorrido: Lista Archivos
(define (getWFileList W)
  (if (workspace? W)
      (cadr W)
      '()))
;-----------------------------------------------------------------
;Funciones Modificadoras
;Funcion setWFiles
;Dominio: Workspace X FileList
;Recorrido: Workspace
(define (setWFiles W Fl)
  (if (and(workspace? W)(fileList? Fl))
      (createWorkspace Fl)
      (createWorkspace '())))

;-----------------------------------------------------------------
;Extra:
;addFilestoW
(define (addFilesToW W Fl)
  (setWFiles W (fileAppend (getWFileList W) (Fl))))



;-----------------------------------------------------------------
(provide (all-defined-out))