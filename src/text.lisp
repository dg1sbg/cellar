;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-
;;; =====================================================================

(declaim (optimize (speed 3) (compilation-speed 0) (safety 1) (debug 3)))

(in-package #:net.goenninger.lib.cellar)

(defv *language-info-registry* (make-hash-table))

(defstruct language-info
  ;; See: http://www-01.sil.org/iso639-3/codes.asp
  (iso-639-1-code :type 'string)
  (iso-639-3-code :type 'string) ;; <- Hash key!
  (name :type 'string))

(defun (setf language-info) (iso-639-1-code iso-639-3-code name)
  (let ((info (make-language-info :iso-639-1-code iso-639-1-code
				  :iso-639-3-code iso-639-3-code
				  :name name)))
    (setf (gethash iso-639-3-code *language-info-registry*) info)
    info))

(defun language-info (iso-639-3-code)
  (gethash iso-639-3-code *language-info-registry*))

(defun init-language-info-registry ()
  (setf language-info "de" "deu" "German")
  (setf language-info "en" "eng" "English")
  (setf language-info "fr" "fra" "French")
  (setf language-info "es" "spa" "Spanish")
  (setf language-info "ru" "rus" "Russian")
  (setf language-info "ja" "jpn" "Japanese")
  (setf language-info "zh" "zho" "Chinese")
  )

(init-language-info-registry)

(defstruct text
  (string :type 'babel:unicode-string)
  (external-format :type 'babel:external-format)
  (iso-639-3-language-code :type 'string)
  (read-only :type 'symbol))
