#lang racket
;---- TDAs Requeridos --------+
(require "TDA_FileName.rkt")
(require "TDA_FileList.rkt")
(require "TDA_Commit.rkt")
(require "TDA_CommitList.rkt")
;-----------------------------+

;TDA Repository
;Representacion: ["NombreReposiorio", Lista_Commits]
;constructor:
;Dominio:StringXLista_Commits
;Recorrido: Repositorio
(define (repository S C)
  (if (and(string? S)
          (or(equal? S "Local Repository")(equal? S "Remote Repository"))
          (commitList? C))
      (list S C)
      '()))

;Funcion de pertenencia:
;Dominio: Repositorio
;Recorrido: Boolean
(define (repository? R)
  (and (string? (car R))
       (or(equal? (car R) "Local Repository")(equal? (car R) "Remote Repository"))
       (commitList? (cadr R))
       (null? (cddr R))))

;Funciones Selectoras:
;GetNameR
(define (getNameR R)
  (if (repository? R)
      (car R)
      '()))

;getCommits
(define (getCommitL R)
  (if (repository? R)
      (cadr R)
      '()))
;-----------------------------------------------------------------
;Funciones modificadoras:

(define (setComList R C)
  (if (and(repository? R)(commitList? C))
      (list (getNameR R) C)
      '()))
;-----------------------------------------------------------------
(define (addCommit R C)
  (if (repository? R)
      (if (commit? C)
          (setComList R (append (list C) (getCommitL R)))
          R)
      '()))
  
          
      
;-----------------------------------------------------------------
(provide (all-defined-out))