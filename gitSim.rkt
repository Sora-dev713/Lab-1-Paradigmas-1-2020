#lang racket

;TDA Filename
;Representacion: String = "NombreArchivo"."Extension"
;Constructor
;Dominio:
;Recorrido:
(define (fileName Nombre Extension)
  (if (and? (string? Nombre)(<= (string-length Nombre) 0)
            (string? Extension)(<= (string-length Extension) 0))
      (string-join (list Nombre Extension) ".")
      '()))

;Funcion Pertenencia:
(define (fileName? F)
  (if (and (string? F)
           (> (string-length (car (string-split F "."))) 0)
           (> (string-length (cadr (string-split F "."))) 0))
      #t
      #f))

;----------------------------------------------

;Definicion de TDAs
;TDA Archivos:
;Representación: [Filename, contenido]
;Constructor:
(define (archivo Nombre contenido)
  (if(and (fileName? Nombre)(string? contenido)(<= (string-length contenido) 0))
     (list Nombre contenido)
     '())
  )

;pertenencia:
(define (archivo? A)
  (and (list? A)
       (not(null? A))
       (fileName? (car a))
       (string? (cadr A))
       (> (string-length (cadr A)) 0)
       (null? (caddr A)))

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
;Auxiliar: ListaNombres
(if(and (list? L)(string? (car? )(null? (cdr L)))
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

;----------------------------------------------------------------------
;TDA Repository
;Representacion: ["NombreReposiorio", Lista_Commits,Lista_Archivos]
;constructor:
;Dominio:StringXLista_Commits/Commit
;Recorrido: Repositorio
(define (Repository S LA C )
  (if (and(string? S)
          (or(equal? S "Local Repository")(equal? S "Remote Repository"))
          (or(listArch? LA)(null? LA))
          (or(commit? C)(null? C)))
      (list S LA (cons C '()))
      '()))

;Funcion de pertenencia:
;Dominio: Repositorio
;Recorrido: Boolean
(define (repo? R)
  (and (string? (car R))
       (or(equal? (car R) "Local Repository")(equal? (car R) "Remote Repository"))
       (or(listArch? (cadr R))(null? (cadr R)))
       (or(commit? (caddr R))(null? (caddr R)))
       (null? (cadddr R))))

;Funciones Selectoras:
;GetNameR
(define (getNameR R)
  (if (repo? R)
      (car R)
      '()))

;getCommits
(define (getCommit R)
  (if (repo? R)
      (cadr R)
      '()))

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

;---------------------------Funciones Auxiliares-------------------------------
;Check Names
;Funcion que comprueba que un archivo ya este en la lista
(define (checkName N L)
  (if (null? (cdr L))
      (equal? N (getName (car L)))
      (or ((equal? N (getName (car L)))(checkName N (cdr L))))))


;MergeFiles
;funcion que mezcla los archivos desde 2 listas (A|B), privilegiando los de A
      ;nota, A es la lista nueva, B es la antigua
(define (mergeFiles A B)
  (if (not(null? A))
      (if (not (null? B))
          (if (and(listArch? A)(listArch? B))
              (if (checkName (getName (car B)) A)
                  (mergeFiles A (cdr B))
                  (mergeFiles (append A (cons (car B) '())) (cdr B)))
              '())
          A)
      (if (not (null? B))
          (if (listArch? B)
              B
              '())
          '())))

;getFilesByName
(define (getFilesByName S L)
  (if (null? L)
      '()
      (if (and (listaArch? L)(string? S))
          (if (equal? S (getName (car L)))
              (car L)
              (getFilesByName S (cdr L)))
          '())))

;PrepareFiles
;Funcion que permite verificar que un archivo este en el workspace
;Dominio: ListaArchivos X Workspace X lista
;Recorrido: Lista Archivos
;tipo de Recursion: de cola
(define (PrepareFiles La W Laux)
  (if (null? La)
      L
      (if (and (workspace? W) (listaArch? La))
          (if (checkName (car La)(getListaArcW W))
              (PrepareFiles (cdr La) W (cons (getFileByName (car La) L)))
              (PrepareFiles (cdr La) W L))
          '())))

          
          
;--------------------------------------------------------------------------

;Funciones necesarias:
(define git (lambda (f) f))
;add
(define add (lambda (Lista)(lambda (zonas)
                             (if (and (listArch? Lista))
                                 (setIndex zonas (PrepareFiles Lista (getWorkspaceZ zonas) '()))
                                     (
              ))))
