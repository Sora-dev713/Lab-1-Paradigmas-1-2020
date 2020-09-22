#lang racket
;---- TDAs Requeridos --------+
(require "TDA_Workspace.rkt")
(require "TDA_Index.rkt")
(require "TDA_Repo.rkt")
;-----------------------------+
;------------------------- TDA Zonas ----------------------------

;representacion: [Workspace, Index, Local Repository, Remote Repository]

;-----------------------------------------------------------------
;Constructor:

;Dominio: Workspace X Index X Repositorio X Repositorio X Registro
;Recorrido: Zonas
(define makeZonas (lambda (W I L R Reg)
                (if (and(workspace? W)
                        (index? I)
                        (repository? L)(equal? (getNameR L) "Local Repository")
                        (repository? R)(equal? (getNameR R) "Remote Repository")
                        (list? Reg))
                    (list W I L R Reg)
                    '())))
;-----------------------------------------------------------------
;Funcion de Pertenencia:
;Dominio: Zonas
;Recorrido: Boolean
(define zonas? (lambda (Z)
                 (and(workspace? (car Z))
                     (index? (cadr Z))
                     (equal? (getNameR (caddr Z)) "Local Repository")
                     (equal? (getNameR (cadddr Z)) "Remote Repository")
                     (list? (cadddr (cdr Z)))
                     (null? (cdddr (cddr Z))))))
;-----------------------------------------------------------------
;Funciones Selectoras

;getWorkspace
;Dominio: Zonas
;Recorrido: Workspace
(define (getWorkspace Z)
  (if (zonas? Z)
      (car Z)
      '()))

;getIndex
;Dominio: Zonas
;Recorrido: Index
(define (getIndex Z)
  (if (zonas? Z)
      (cadr Z)
      '()))

;getLocRepo
;Dominio: Zonas
;Recorrido: Repository
(define (getLocRepo Z)
  (if (zonas? Z)
      (caddr Z)
      '()))

;getRemRepo
;Dominio: Zonas
;Recorrido: Repository
(define (getRemRepo Z)
  (if (zonas? Z)
      (cadddr Z)
      '()))

;getRemRepo
;Dominio: Zonas
;Recorrido: Registro (lista de Strings)
(define (getReg Z)
  (if (zonas? Z)
      (cadddr (cdr Z))
      '()))

;-----------------------------------------------------------------
;Modificadores:

;setWorkspace
;Dominio: Zonas X Workspace
;Recorrido: Zonas
(define (setWorkspace Z W)
  (if (and (zonas? Z)(workspace? W))
      (makeZonas W (getIndex Z)(getLocRepo Z)(getRemRepo Z) (getReg Z))
      (if (zonas? Z)
          Z
          '())
      )
  )

;setIndex
;Dominio: Zonas X Index
;Recorrido: Zonas
(define (setIndex Z I)
  (if (and (zonas? Z)(index? I))
      (makeZonas (getWorkspace Z) I(getLocRepo Z)(getRemRepo Z)(getReg Z))
      (if (zonas? Z)
          Z
          '())
      )
  )

;SetLocRepo
;Dominio: Zonas X Repository 
;Recorrido: Zonas
(define (setLocRepo Z L)
  (if (and (zonas? Z)(repository? L)(equal? (getNameR L) "Local Repository"))
      (makeZonas (getWorkspace Z) (getIndex Z) L (getRemRepo Z)(getReg Z))
      (if (zonas? Z)
          Z
          '())
      )
  )

;setRemRepo
;Dominio: Zonas X Repository
;Recorrido: Zonas
(define (setRemRepo Z R)
  (if (and (zonas? Z)(repository? R)(equal? (getNameR R) "Remote Repository"))
      (makeZonas (getWorkspace Z) (getIndex Z) (getLocRepo Z) R(getReg Z))
      (if (zonas? Z)
          Z
          '())
      )
  )

;setReg
;Dominio: Zonas X Registro
;Recorrido: Zonas
(define (setReg Z R)
  (if (and (zonas? Z)(list? R))
      (makeZonas(getWorkspace Z)(getIndex Z)(getLocRepo Z)(getRemRepo Z) R)
      (if (zonas? Z)
          Z
          '())
      )
  )
;-----------------------------------------------------------------
;--------------------Funciones Auxiliares-------------------------
;-----------------------------------------------------------------
;clearIndex
;Desc: Funcion que limpia el Index luego de un commit
;Dominio: Zonas
;Recorrido: Zonas
(define (clearIndex Z)
  (if (zonas? Z)
      (setIndex Z (setFList (getIndex Z) '()))
      '()))

;-----------------------------------------------------------------
;funcion isIndexEmpty
;Funcion que retorna un dato de tipo Boolean para saber si el Workspace
;esta o no vacio
(define (isWorkEmpty Z)
  (if(zonas? Z)
     (if (null? (getWFileList(getWorkspace Z)))
         #t
         #f)
     #f))
;-----------------------------------------------------------------
;funcion isIndexEmpty
;Funcion que retorna un dato de tipo Boolean para saber si el index
;esta o no vacio
;Dominio: Zonas
;Recorrido: Boolean
(define (isIndexEmpty Z)
  (if(zonas? Z)
     (if (null? (getListI(getIndex Z)))
         #t
         #f)
     #f))
;-----------------------------------------------------------------
;funcion isLocalEmpty
;Funcion que retorna un dato de tipo Boolean para saber si el Local
;Repository esta o no vacio
;Dominio: Zonas
;Recorrido: Boolean
(define (isLocalEmpty Z)
  (if(zonas? Z)
     (if (null? (getCommitL(getLocRepo Z)))
         #t
         #f)
     #f))
;-----------------------------------------------------------------
;funcion isRemoteEmpty
;Funcion que retorna un dato de tipo Boolean para saber si el Remote
;Repository esta o no vacio
;Dominio: Zonas
;Recorrido: Boolean
(define (isRemoteEmpty Z)
  (if(zonas? Z)
     (if (null? (getCommitL(getRemRepo Z)))
         #t
         #f)
     #f))
;-----------------------------------------------------------------
;Funcion canPush
;Funcion que verifica si es que se puede hacer o no el push
;Dominio: Zonas
;Recorrido: Boolean
(define (canPush Z)
  (if (not(isLocalEmpty Z))
      (if (not(equal? (getCommitNumbers (getRemRepo Z)) (getCommitNumbers (getLocRepo Z))))
          #t
          #f)
      #f))
          
;-------------------- Funciones para registro --------------------
;addRegister
;funcion encargada de añadir un nuevo string al registro
;Dominio: Zonas X String
;Recorrido: Zonas
(define (addRegister Z S)
  (if (zonas? Z)
      (if (string? S)
          (if (null?(getReg Z))
              (setReg Z (list S))
              (setReg Z (append (getReg Z) (list S))))
          Z)
      '())
      )

;-----------------------------------------------------------------
;RegisterToString

;Desc: Funcion encargada de representar el Registro como un string
;Dominio: ListaStrings
;Rec: String
(define (registerToString R)
  (if (list? R)
      (if (null? R)
          "Vacio."    
          (if (null? (cdr R))
              (string-append (car R) ". ")
              (string-append (car R) ", " (registerToString (cdr R)))))
      "ERROR"))
;-----------------------------------------------------------------
;-----------------------------------------------------------------
(provide (all-defined-out))