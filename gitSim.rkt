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

;--------------------------------------------------------------------------

;Funciones necesarias:
;(define git (lambda (Comando)
;              (lambda (Args)
 ;               (if (procedure? (comando args))
  ;                  (lambda (Zonas)
   ;                   (if ()))))))
;add
(define add (lambda (Lista)(lambda (zonas)
                             (if (zonas? zonas)
                                 (if (and (fileList? Lista))
                                     (setIndex zonas (Index (mergeFiles (getListI (getIndex zonas)) (PrepareFiles Lista (getWorkspace zonas) '()))))
                                     zonas)
                                 '()))))
        
;Pull
(define pull (lambda (zonas)
               (if (zonas? zonas)
                   (setWorkspace zonas
                                 (addFilesToW (getWorkspace zonas)
                                              (fileFilter (unifyCFiles (getCommitL (getRemRepo zonas)) '())
                                                           (getWFileList (getWorkspace zonas)))))
                   '())))
;Commit
(define commit (lambda (mensaje)
                 (lambda (zonas)
                   (if (zonas? zonas)
                       (if (string? mensaje)
                           (clearIndex (setLocRepo zonas (addCommit (getLocRepo zonas) (crearCommit (getListI (getIndex zonas)) mensaje))))
                           zonas)
                       '())
                   )))


;Push
;(define push (lambda (zonas)
             ; (if (zonas? zonas)
                   ;(