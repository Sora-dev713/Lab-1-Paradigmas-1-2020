#lang racket
;Definicion de TDAs
;TDA Archivos:
;Representación: "Nombre_Archivo.extensión"
;Constructor:
(define (archivo Nombre contenido)
  (if(and (string? Nombre)(string? contenido)(<= (string-length contenido) 0))
     (list Nombre contenido)
     '())
  )

;pertenencia:
(define (archivo? A)
  (and (list? A)
       (not(null? A))
       (string? (car A))
       (> (string-length (car (string-split A "."))) 0)
       (> (string-length (cadr (string-split A "."))) 0)
       (> (string-length (cadr A)) 0)))

;Selectores:
(define (getName A)
  (if (archivo? A)
      (car A)
      '()))

(define (getContent A)
  (if (archivo? A)
      (cadr A)
      '()))
;----------------0
     
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


;----------------------------------------------------------------------------
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

;---------------------------Funciones Auxiliares-------------------------------
;Check Names
;Funcion que comprueba que un archivo ya este en la lista
(define (checkName N L)
  (if (null? (cdr L))
      (equal? N (getName (car L)))
      (or (equal? N (getName (car L)))(checkName N (cdr L)))



;MergeFiles
;funcion que mezcla los archivos desde 2 listas (A|B), privilegiando los de A
(define (mergeFiles A B)
  (if (and(listArch? A)(listArch? B))
      (unifyFiles A B)
      '()))
;UnifyFiles
;Funcion recursiva que unifica los archivos en una lista
(define (unifyFiles A B C)
  (if (or (null? B)(null? A))
      A
      (if (checkName (car A) B)
          (unifyFiles (cdr A) (cons B A)))


;----------------------------------------------------------------------
;TDA Repository
;Representacion: ["NombreReposiorio", Lista_Commits,Lista_Archivos]
;constructor:
;Dominio: Lista_Commits/Commit
;Recorrido: Repositorio

