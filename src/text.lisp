;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-
;;; =====================================================================

(declaim (optimize (speed 3) (compilation-speed 0) (safety 1) (debug 3)))

(in-package #:net.goenninger.lib.cellar)

(defv *language-desc-registry* (make-hash-table))

(defstruct language-desc
  ;; See: http://www-01.sil.org/iso639-3/codes.asp
  (iso-639-1-code "" :type string)
  (iso-639-3-code "" :type string) ;; <- Hash key!
  (name           "" :type string))

(defun (setf language-info) (new-desc iso-639-3-code)
  (setf (gethash iso-639-3-code *language-desc-registry*) new-desc)
  (gethash iso-639-3-code *language-desc-registry*))

(defun language-info (iso-639-3-code)
  (gethash iso-639-3-code *language-desc-registry*))

(defun init-language-desc-registry ()
  (flet ((mld (iso-639-1-code iso-639-3-code name)
	   (declare (inline mld))
	   (make-language-desc
	    :iso-639-1-code iso-639-1-code
	    :iso-639-3-code iso-639-3-code
	    :name name)))
    (setf (language-info "deu") (mld "de" "deu" "German"))
    (setf (language-info "eng") (mld "en" "eng" "English"))
    (setf (language-info "fra") (mld "fr" "fra" "French"))
    (setf (language-info "spa") (mld "en" "spa" "Spanish"))
    (setf (language-info "rus") (mld "ru" "rus" "Russian"))
    (setf (language-info "jpn") (mld "ja" "jpn" "Japanese"))
    (setf (language-info "zho") (mld "zh" "zho" "Chinese")))
  (values))

(init-language-desc-registry)

(defstruct text
  (string                  ""  :type babel:unicode-string)
  (external-format         (babel:make-external-format :utf-16) :type babel:external-format)
  (iso-639-3-language-code ""  :type string)
  (read-only               nil :type symbol))
