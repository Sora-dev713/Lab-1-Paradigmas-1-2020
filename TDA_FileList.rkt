#lang racket
;---- TDAs Requeridos --------+
(require "TDA_FileName.rkt")
;-----------------------------+
;TDA FileList

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
;Recorrido: Boolan
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
;Check Names
;Funcion que comprueba que un archivo ya este en la lista
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

;MergeFiles
;funcion que mezcla los archivos desde 2 listas (A|B), privilegiando los de A
      ;nota, A es la lista nueva, B es la antigua
;Dominio: FileList X FileList
;Recorrido: FileList
(define (mergeFiles A B)
  (if (not(null? A))
      (if (not (null? B))
          (if (and(fileList? A)(fileList? B))
              (if (checkName (car B) A)
                  (mergeFiles A (cdr B))
                  (mergeFiles (append A (list (car B))) (cdr B)))
              '())
          A)
      (if (not (null? B))
          (if (fileList? B)
              B
              '())
          '())))
;FileAppend
;Funcion que mezcla 2 listas de archivos
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

;FileFilter
;Dominio:
;Recorrido:
;Tipo de Recursion: Natural
(define (fileFilter Fl Rest)
  (if (and(fileList? Fl)(fileList? Rest))
      (if (null? Fl)
          '()
          (if (not (checkName (car Fl) Rest))
              (cons (car Fl) (fileFilter (cdr Fl) Rest))
               (fileFilter (cdr Fl) Rest)))
      '()))
          
  

;Filelist to String
;Dominio: Filelist
;Recorrido: String
(define FlToString (lambda (Fl)
                     (if (fileList? Fl)
                         (if(null? (cdr Fl))
                            (string-append (car Fl) ". ")
                            (string-append (car Fl) ", "(FlToString (cdr Fl))))
                         "ERROR")))
                            

;-----------------------------------------------------------------
(provide (all-defined-out))