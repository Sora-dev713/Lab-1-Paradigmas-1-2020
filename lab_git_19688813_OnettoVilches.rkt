#lang racket
;---- TDAs Requeridos --------+
(require "TDA_FileName.rkt")
(require "TDA_FileList.rkt")
(require "TDA_Index.rkt")
(require "TDA_Workspace.rkt")
(require "TDA_Repo.rkt")
(require "TDA_Commit.rkt")
(require "TDA_CommitList.rkt")
(require "TDA_Zonas.rkt")
;-----------------------------+

;-------------------------- Codigo Principal ------------------------------
;Nombre Autor: Bastian Onetto
;Seccion: A-1
;--------------------------------------------------------------------------
;-----------------Requerimientos Funcionales Obligatorios------------------
;--------------------------------------------------------------------------

;----------------------------------Git-------------------------------------

;Desc: Pilar fundamental del programa, indica la posibilidad de ejecutar los
;      comandos ingresados, segun los que se han especificado, dejando registro
;      del comando.

;Dominio: Comando X Argumento |
;         Comando X Argumento X Zonas
;(Argumento => FileList / String / Zonas)
;Recorrido: Zonas

(define git (lambda (comando)
              (lambda (args)
                (if (procedure? (comando args));Si es que la informacion hasta el momento corresponde a un procedure
                    (lambda (zonas); pedimos el ingreso de las Zonas
                      (if (and (string? args)(equal? comando commit)(zonas? zonas));Si los datos corresponden a un Commit
                          ((comando args)(addRegister zonas "Commit"))
                          (if(and (fileList? args)(equal? comando add)(zonas? zonas));Si los datos corresponden a un Add
                             ((comando args)(addRegister zonas "Add"))
                             zonas);Si no corresponde a niuno, se entregan las zonas sin cambio.
                          )
                      )
                    ;En el caso de que no seaun procedure pero una funcion por si sola
                    (if (and (zonas? args)(equal? comando pull));Si los datos corresponden a un Pull
                        (comando (addRegister args "Pull"))
                        (if (and (zonas? args)(equal? comando push));Si los datos corresponden a un Push
                            (comando (addRegister args "Push"))
                            args))))));Si no, se entregan los argumento de vuelta, que por lo general, son zonas.

;Ejemplos de uso:
;((git add)((list(list "Workspace" (list "a.rkt" "b.txt" "c.d"))
;               (list "Index" '())
;               (list "Local Repository" '())
;               (list "Remote Repository" '())
;               '())))
;siguiendo el ejemplo anterior
;(((git commit) "Commit de Prueba") ((git add)((list(list "Workspace" (list "a.rkt" "b.txt" "c.d"))(list "Index" '())(list "Local Repository" '())(list "Remote Repository" '())'()))))
;
;Siguiendo nuevamente el ejemplo anterior:
;((git push) (((git commit) "Commit de Prueba") ((git add)((list(list "Workspace" (list "a.rkt" "b.txt" "c.d"))(list "Index" '())(list "Local Repository" '())(list "Remote Repository" '())'())))))
;--------------------------------------------------------------------------

;----------------------------------Add-------------------------------------

;Descripcion: Funcion encargada de agregar los archivos dados por el usuario
;             en el Index, siempre y cuando estos existan en el Workspace.

;Dominio: Zonas
;Recorrido: Zonas
;Tipo de Recursion: De cola (mergeFiles y PrepareFiles)

(define add (lambda (Lista)(lambda (zonas)
                             (if (zonas? zonas)
                                 (if (and (fileList? Lista))
                                     (setIndex zonas (Index (mergeFiles (getListI (getIndex zonas)) (PrepareFiles Lista (getWorkspace zonas) '()))))
                                     zonas)
                                 '()))))

;--------------------------------------------------------------------------
;----------------------------------Pull------------------------------------

;Descripcion: Funcion encargada de obtener todos los archivos que se han
;             guardado en el Remote Repository y guardarlos como parte del
;             Workspace.

;Dominio: Zonas
;Recorrido: Zonas
;Tipo de Recursion: Natural (addFilesToW y fileFilter)
(define pull (lambda (zonas)
               (if (zonas? zonas)
                   (setWorkspace zonas (addFilesToW (getWorkspace zonas) (fileFilter (unifyCFiles (getCommitL (getRemRepo zonas)) '())
                                                           (getWFileList (getWorkspace zonas)))))
                   '())))
;--------------------------------------------------------------------------
;---------------------------------Commit-----------------------------------

;Descripcion: Funcion encargada de generar un Commit a partir de los archivos en Index y
;             un comentario dejado por el usuario, el cual se ira guardando en el Local
;             Repository.

;Dominio: String X Zonas
;Recorrido: Zonas

(define commit (lambda (mensaje)
                 (lambda (zonas)
                   (if (zonas? zonas)
                       (if (string? mensaje)
                           (clearIndex (setLocRepo zonas (addCommit (getLocRepo zonas) (crearCommit (getListI (getIndex zonas)) mensaje))))
                           zonas)
                       '())
                   )))


;--------------------------------------------------------------------------
;----------------------------------Push------------------------------------

;Desc: Funcion encargada de pasar los commits guardado en el Local Repository
;      hacia el Remote Repository.

;Dominio: Zonas
;Recorrido: Zonas

(define push (lambda (zonas)
              (if (zonas? zonas)
                   (setRemRepo zonas (copyToRem (getLocRepo zonas) (getRemRepo zonas)))
                    '())))
;--------------------------------------------------------------------------
;-----------------------------Zonas->String--------------------------------

;Desc: Funcion encargada de pasar a String todo el TDA Zonas en el estado actual
;      que se le entregue a la función.

;Dominio: Zonas
;Recorrido: String
(define zonas->string (lambda (zonas)
              (if (zonas? zonas)
                   (string-append "Zonas: \n"
                                  (workspaceToString (getWorkspace zonas))"\n"
                                  (indexToString(getIndex zonas))"\n"
                                  (RepoToString (getLocRepo zonas))"\n"
                                  (RepoToString (getRemRepo zonas))"\n"
                                  "Registro Comandos: "
                                  (registerToString (getReg zonas))
                                  "\n")
                    '())))
;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
;--------------------------------------------------------------------------