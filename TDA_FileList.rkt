#lang racket
;---- TDAs Requeridos --------+
(require "TDA_FileName.rkt")
;-----------------------------+

;------------------------- TDA FileList --------------------------
;Representación:

;FileList = Null |
;           FileName X FileList

;-----------------------------------------------------------------
;Constructor

;Dominio: FileName X FileList
;Recorrido: FileList

(define (fileList F FL)
  (if (and (fileName? F) (fileList? FL))
      (cons F FL)
      '()))

;-----------------------------------------------------------------

;Funcion de pertenencia

;Dominio: FileList
;Recorrido: Boolean
;Recursion: Lineal

(define (fileList? F)
  (if (null? F)
      #t
      (if(and (list? F)(fileName? (car F))(null? (cdr F)))
         #t
         (if (fileName? (car F))
             (fileList? (cdr F))
             #f))))

;-----------------------------------------------------------------
;----------------------Operadores Auxiliares----------------------
;-----------------------------------------------------------------
;CheckName

;Descripcion:Funcion que comprueba que un archivo ya este en la lista

;Dominio: FileName X FileList
;Recorrido: Boolean
;Tipo de Recursion: de Cola

(define (checkName N L)
  (if (fileList? L)
      (if (null? L)
         #f
         (if (fileName? N)
             (if (equal? N (car L))
                 #t
                 (checkName N (cdr L)))
             #f))
      #f))
;-----------------------------------------------------------------
;MergeFiles

;Descripcion: funcion que mezcla los archivos desde
;             2 listas (A|B), privilegiando los de A
;             nota: A es la lista nueva, B es la antigua

;Dominio: FileList X FileList
;Recorrido: FileList
;Tipo de Recursion: Cola.

(define (mergeFiles A B)
  (if (not(null? A));en el caso de que A no este vacia
      (if (not (null? B));se verifica si B tampoco lo esta para poder mezclar archivos
          (if (and(fileList? A)(fileList? B))
              (if (checkName (car B) A)
                  (mergeFiles A (cdr B))
                  (mergeFiles (append A (list (car B))) (cdr B)))
              '())
          A);En el caso que B este vacia se devuelve A
      (if (not (null? B));Si A esta vacia pero B no, se devuelve B
          (if (fileList? B)
              B
              '())
          '());Si ambas entan vacias, se devuelve una lista vacia.
      ))

;-----------------------------------------------------------------
;FileAppend

;Descripcion: Funcion que mezcla 2 listas de archivos.

;Dominio: FileList X FileList
;Recorrido: FileList
;Tipo de Recursion: Natural

(define fileAppend (lambda (F1 F2)
                     (if(fileList? F1)
                        (if (null? F1)
                            (if (fileList? F2)
                                (if (null? F2)
                                    '()
                                    (cons (car F2) (fileAppend F1 (cdr F2))))
                                '())
                            (cons (car F1) (fileAppend (cdr F1) F2)))
                        (if (fileList? F2)
                            (if (null? F2)
                                '()
                                (cons (car F2) (fileAppend F1 (cdr F2))))
                            '()))))
;-----------------------------------------------------------------
;FileFilter

;Descripcion: Funcion que filta una lista, removiendo los elementos 
;             existentes en una lista restrictiva.

;Dominio: FileList X FileList
;Recorrido:FileList
;Tipo de Recursion: Natural

(define (fileFilter Fl Rest)
  (if (and(fileList? Fl)(fileList? Rest))
      (if (null? Fl)
          '()
          (if (not (checkName (car Fl) Rest))
              (cons (car Fl) (fileFilter (cdr Fl) Rest))
               (fileFilter (cdr Fl) Rest)))
      '()))
          
  
;-----------------------------------------------------------------
;FilelistToString

;Descripcion: Funcion que genera un String representativo de una
;             lista de strings.

;Dominio: Filelist
;Recorrido: String
;Tipo de Recursion: Natural

(define FlToString (lambda (Fl)
                     (if (fileList? Fl)
                         (if(null? (cdr Fl))
                            (string-append (car Fl) ". \n")
                            (string-append (car Fl) ", "(FlToString (cdr Fl))))
                         "ERROR\n")))
                            

;-----------------------------------------------------------------
(provide (all-defined-out))