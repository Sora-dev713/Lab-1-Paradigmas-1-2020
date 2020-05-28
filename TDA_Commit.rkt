#lang racket
;TDA Commit:
;Representacion: [[Archivos], "Comentario"]
;Constructor:
(define (CrearCommit L comment)
  (if(and(listArch? L)(string? comment))
     (list L comment)
     '()))

;pertenencia
(define (commit? C)
  (if(and(list? C)(listArch? (car C))(string? (cadr C))(null? (cddr C)))
     #t
     #f))