#lang racket
;---- TDAs Requeridos --------+
(require "TDA_FileName.rkt")
(require "TDA_FileList.rkt")
(require "TDA_Commit.rkt")
(require "TDA_CommitList.rkt")
;-----------------------------+
;----------------------- TDA Repository --------------------------

;Representacion: ["NombreReposiorio", CommitList]
;-----------------------------------------------------------------
;Constructor:

;Dominio:String X Commit List
;Recorrido: Repository

(define (repository S C)
  (if (and(string? S)
          (or(equal? S "Local Repository")(equal? S "Remote Repository"))
          (commitList? C))
      (list S C)
      '()))

;-----------------------------------------------------------------
;Funcion de pertenencia:

;Dominio: Repositorio
;Recorrido: Boolean
(define (repository? R)
  (and (string? (car R))
       (or(equal? (car R) "Local Repository")(equal? (car R) "Remote Repository"))
       (commitList? (cadr R))
       (null? (cddr R))))

;-----------------------------------------------------------------
;Funciones Selectoras:
;GetNameR
;Dominio: Repository
;Recorrido: String
(define (getNameR R)
  (if (repository? R)
      (car R)
      '()))

;getCommitL
;Dominio: Repository
;Recorrido: CommitList
(define (getCommitL R)
  (if (repository? R)
      (cadr R)
      '()))
;-----------------------------------------------------------------
;Funciones modificadoras:

;setComList
;Dominio: Repository X CommitList
;Recorrido: Repository
(define (setComList R C)
  (if (and(repository? R)(commitList? C))
      (list (getNameR R) C)
      '()))
;-----------------------------------------------------------------
;--------------------- Operadores Auxiliares ---------------------
;-----------------------------------------------------------------
;addCommit
;Desc: Funcion que añade un nuevo Commit al CommitList
;Dominio: Repository X Commit
;Recorrido: Repository
(define (addCommit R C)
  (if (repository? R)
      (if (commit? C)
          (setComList R (append (list C) (getCommitL R)))
          R)
      '()))
;-----------------------------------------------------------------
;CopyToRem
;Funcion encargda de copiar el Local Repository en el Remote Repository
;Dominio: Repository X Repository
;Recorrido: Repository

(define (copyToRem L R)
  (if (and(repository? L) (repository? R))
      (setComList R (getCommitL L))
      '()))
;-----------------------------------------------------------------
;RepoToString
;Desc: Funcion encargada de representar el Repositorio como un string
;Dominio: Repository
;Recorrido: String
(define (RepoToString R)
  (if (repository? R)
      (if (null? (getCommitL R))
          (string-append (getNameR R) ": \n No Commits\n")
          (string-append (getNameR R) ": \n" (CommitListToString (getCommitL R) 1)))
       "ERROR\n"))
      
;-----------------------------------------------------------------
(provide (all-defined-out))