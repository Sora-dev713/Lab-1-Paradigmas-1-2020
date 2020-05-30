#lang racket
;TDA Workspace
;Represetacion: ["Workspace", Lista_Archivos]
;costructor
;Dominio: Archivo/S
;Recorrido: Workspace
(define (createWorskpace A)
  (if (listArch? A)
      (list "Workspace" A)
      (if (archivo? A)
          (list "Workspace" (cons A '()))
          '())))

;Funcion de pertenencia
;Domio: Workspace
;Recorrido: Boolean
(define (workspace? W)
  (and(list? W)(not(null? W))(eq? "Workspace" (car W))(listArch? (cadr W))(null? (cddr W))))

;Funciones selectoras
;Funcion GetNameW
;Dominio: Workspace
;Recorrido String
(define (getNameW W)
  (if (workspace? W)
      (car W)
      '()))

;Funcion GetListArcW
;dominio: Workspace
;Recorrido: Lista Archivos
(define (getListaArcW W)
  (if (workspace? W)
      (cadr W)
      '()))

;-----------------------------------------------------------------
(provide (all-defined-out))