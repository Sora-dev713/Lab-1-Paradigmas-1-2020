#lang racket
;Auxiliar: Lista de archivos
(define (listArch? L)
  (if(and (list? L)(archivo? (car L))(null? (cdr L)))
     #t
     (if (archivo? (car L))
         (listArch? (cdr L))
         #f)))
;Auxiliar: ListaNombres
(if(and (list? L)(string? (car? )(null? (cdr L)))
     #t
     (if (archivo? (car L))
         (listArch? (cdr L))
         #f)))