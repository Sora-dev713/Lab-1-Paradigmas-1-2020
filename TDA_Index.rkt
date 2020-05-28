#lang racket
;TDA Index
;Represetacion: ["Index", Lista_Archivos]
;costructor
;Dominio: Archivo/S
;Recorrido: Index
(define (createIndex A)
  (if (listArch? A)
      (list "Index" A)
      (if (archivo? A)
          (list "Index" (cons A '()))
          '())))

;Funcion de pertenencia
;Domio: Index
;Recorrido:
(define (index? I)
  (and(list? I)(not(null? I))(eq? "Index" (car I))(listArch? (cadr I))(null? (cddr I))))

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
  (if (and(index? I)(listArch? L))
      (cons (car I)(cons L '()))
      '()))