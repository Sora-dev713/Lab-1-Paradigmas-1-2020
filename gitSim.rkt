#lang racket




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
