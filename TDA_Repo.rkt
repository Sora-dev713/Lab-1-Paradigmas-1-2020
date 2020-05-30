#lang racket
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

;-----------------------------------------------------------------
(provide (all-defined-out))