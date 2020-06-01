#lang racket
;---- TDAs Requeridos --------+
(require "TDA_Workspace.rkt")
(require "TDA_Index.rkt")
(require "TDA_Repo.rkt")
;-----------------------------+
;-----------------------
;TDAZonas
;representacion: [Workspace, Index, Local Repository, Remote Repository]
;Dominio: Workspace X Index X Repositorio X Repositorio X Registro
;Recorrido: Zonas
(define makeZonas (lambda (W I L R Reg)
                (if (and(workspace? W)
                        (index? I)
                        (repository? L)(equal? (getNameR L) "Local Repository")
                        (repository? R)(equal? (getNameR R) "Remote Repository")
                        (list? Reg))
                    (list W I L R Reg)
                    '())))

;Funcion de Pertenencia:
;Dominio: Zonas
;Recorrido: Boolean
(define zonas? (lambda (Z)
                 (and(workspace? (car Z))
                     (index? (cadr Z))
                     (equal? (getNameR (caddr Z)) "Local Repository")
                     (equal? (getNameR (cadddr Z)) "Remote Repository")
                     (list? (cadddr (cdr Z)))
                     (null? (cdddr (cddr Z))))))

;Funciones Selectoras
;getWorkspace
(define (getWorkspace Z)
  (if (zonas? Z)
      (car Z)
      '()))

;getIndex
(define (getIndex Z)
  (if (zonas? Z)
      (cadr Z)
      '()))

;getLocRepo
(define (getLocRepo Z)
  (if (zonas? Z)
      (caddr Z)
      '()))

;getRemRepo
(define (getRemRepo Z)
  (if (zonas? Z)
      (cadddr Z)
      '()))

;getRemRepo
(define (getReg Z)
  (if (zonas? Z)
      (cadddr (cdr Z))
      '()))


;Modificadores:

(define (setWorkspace Z W)
  (if (and (zonas? Z)(workspace? W))
      (makeZonas W (getIndex Z)(getLocRepo Z)(getRemRepo Z) (getReg Z))
      (if (zonas? Z)
          Z
          '())
      )
  )

(define (setIndex Z I)
  (if (and (zonas? Z)(index? I))
      (makeZonas (getWorkspace Z) I(getLocRepo Z)(getRemRepo Z)(getReg Z))
      (if (zonas? Z)
          Z
          '())
      )
  )

(define (setLocRepo Z L)
  (if (and (zonas? Z)(repository? L)(equal? (getNameR L) "Local Repository"))
      (makeZonas (getWorkspace Z) (getIndex Z) L (getRemRepo Z)(getReg Z))
      (if (zonas? Z)
          Z
          '())
      )
  )

(define (setRemRepo Z R)
  (if (and (zonas? Z)(repository? R)(equal? (getNameR R) "Remote Repository"))
      (makeZonas (getWorkspace Z) (getIndex Z) (getLocRepo Z) R(getReg Z))
      (if (zonas? Z)
          Z
          '())
      )
  )
;-----------------------------------------------------------------
(define (clearIndex Z)
  (if (zonas? Z)
      (setIndex Z (setFList (getIndex Z) '()))
      '()))

(define makeTestZonas (list
                       (list "Workspace" (list "a.rkt" "b.txt" "c.d"))
                       (list "Index" '())
                       (list "Local Repository" '())
                       (list "Remote Repository" '())
                       '()))
;-----------------------------------------------------------------
(provide (all-defined-out))