#lang racket
;---- TDAs Requeridos --------+
(require "TDA_FileList.rkt")
;-----------------------------+

;----------------------- TDA Workspace ---------------------------

;Represetacion: ["Workspace", Lista_Archivos]

;-----------------------------------------------------------------
;Costructor

;Dominio: FileList
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
;+----------------------------------------+
;getNameW

;Desc: Funcion que entrega el Nombre del Workspace

;Dominio: Workspace
;Recorrido String

(define (getWName W)
  (if (workspace? W)
      (car W)
      '()))
;+----------------------------------------+
;getWFileList

;Desc: Funcion que entrega el FileList del Workspace

;Dominio: Workspace
;Recorrido: FileList

(define (getWFileList W)
  (if (workspace? W)
      (cadr W)
      '()))
;-----------------------------------------------------------------
;Funcion Modificadora

;setWFiles

;Desc: Funcion que Reemplaza la FileList del Workspace por la ingresada

;Dominio: Workspace X FileList
;Recorrido: Workspace

(define (setWFiles W Fl)
  (if (and(workspace? W)(fileList? Fl))
      (createWorkspace Fl)
      (createWorkspace '())))

;-----------------------------------------------------------------
;--------------------- Operadores Auxiliares ---------------------
;-----------------------------------------------------------------
;addFilestoW

;Desc: Funcion que Retorna un Workspace con su FileList combinado con
;      el ingresado

;Dominio: Workspace X FileList
;Recorrido: Workspace

(define (addFilesToW W Fl)
  (setWFiles W (fileAppend (getWFileList W) Fl)))

;-----------------------------------------------------------------
;WorkspaceToString

;Desc: Funcion que entrega un Workspace en forma de String

;Dominio: Workspace
;Recorrido: String

(define (workspaceToString W)
  (if (workspace? W)
      (if (null? (getWFileList W))
          "Workspace: \n->Archivos: Vacio.\n"
          (string-append "Workspace: \n->Archivos: " (FlToString (getWFileList W))))
         "ERROR\n"))

;-----------------------------------------------------------------
(provide (all-defined-out))