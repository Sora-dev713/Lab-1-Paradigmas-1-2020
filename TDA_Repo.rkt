#lang racket
;---- TDAs Requeridos --------+
(require "TDA_FileName.rkt")
(require "TDA_FileList.rkt")
(require "TDA_CommitList.rkt")
;-----------------------------+

;TDA Repository
;Representacion: ["NombreReposiorio", Lista_Commits]
;constructor:
;Dominio:StringXLista_Commits
;Recorrido: Repositorio
(define (Repository S C)
  (if (and(string? S)
          (or(equal? S "Local Repository")(equal? S "Remote Repository"))
          (commitList? C))
      (list S C)
      '()))

;Funcion de pertenencia:
;Dominio: Repositorio
;Recorrido: Boolean
(define (repo? R)
  (and (string? (car R))
       (or(equal? (car R) "Local Repository")(equal? (car R) "Remote Repository"))
       (commitList? (cadr R))
       (null? (caddr R))))

;Funciones Selectoras:
;GetNameR
(define (getNameR R)
  (if (repo? R)
      (car R)
      '()))

;getCommits
(define (getCommitL R)
  (if (repo? R)
      (cadr R)
      '()))

;-----------------------------------------------------------------
(provide (all-defined-out))