#lang racket
;Definicion de TDAs
;TDA Archivos:
;Representación: "Nombre_Archivo.extensión"
;Constructor:
(define (archivo Nombre extension)
  (if(and (string? Nombre)(string? extension)(<= (string-length extension) 3))
     (string-join (list Nombre extension) ".")
     '())
  )

;pertenencia:
(define (archivo? A)
  (if(and(string? A)(<= (string-length (cadr (string-split A "."))) 3))
     #t
     #f))
;---------------------------------
;Auxiliar: Lista de archivos
(define (listArch? L)
  (if(and (list? L)(archivo? (car L))(null? (cdr L)))
     #t
     (if (archivo? (car L))
         (listArch? (cdr L))
         #f)))
;----------------------------------
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
;----------------------------------------------------
;TDA Zona
