#lang racket
;TDAZonas
;representacion: [Workspace, Index, Local Repository, Remote Repository]
;Dominio: Workspace X Index X Repositorio X Repositorio
;Recorrido: Zonas
(define Zonas (lambda (W I L R)
                (if (and(workspace? W)
                        (index? I)
                        (repo? L)(equal? (getNameR L) "Local Repository")
                        (repo? R)(equal? (getNameR R) "Remote Repository"))
                    (list W I L R)
                    '())))
;Funcion de Pertenencia:
;Dominio: Zonas
;Recorrido: Boolean
(define zonas? (lambda (Z)
                 (and(workspace? (car Z))
                     (index? (cadr Z))
                     (equal? (getNameR (caddr Z)) "Local Repository")
                     (equal? (getNameR (cadddr Z)) "Remote Repository")
                     (null? (cadddr (cdr Z))))))

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
