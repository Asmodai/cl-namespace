;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-NAMESPACE; Base: 10; Lowercase: Yes -*-
;;;
;;; personal-namespace.lisp --- Personal namespace class
;;;
;;; Time-stamp: <Saturday Dec 10, 2011 00:59:21 asmodai>
;;; Revision:   3
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    09 Dec 2011 20:30:28
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;; {{{ License:
;;;
;;; This code is free software; you can redistribute it and/or modify
;;; it under the terms of the version 2.1 of the GNU Lesser General
;;; Public License as published by the Free Software Foundation, as
;;; clarified by the Franz preamble to the LGPL found in
;;; http://opensource.franz.com/preamble.html.
;;;
;;; This code is distributed in the hope that it will be useful, but
;;; without any warranty; without even the implied warranty of
;;; merchantability or fitness for a particular purpose.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; Version 2.1 of the GNU Lesser General Public License can be found
;;; at http://opensource.franz.com/license.html. If it is not present,
;;; you can access it from http://www.gnu.org/copyleft/lesser.txt
;;; (until superseded by a newer  version) or write to the Free
;;; Software Foundation, Inc., 59 Temple Place, Suite  330, Boston, MA
;;; 02111-1307  USA
;;;
;;; }}}
;;; {{{ Commentary:
;;;
;;; }}}

#-genera
(in-package #:cl-namespace)

;;; ==================================================================
;;; {{{ Parameters:

(defparameter *default-personal-change-window* 1
  "Changes before a dump is done.")

(defparameter *default-personal-version-limit* 2
  "Version limit for dump file.")

(defparameter *default-personal-file-modifier*
              (string-append "-"
                             +personal-type+
                             *personal-file-version-tag*))

;;; }}}
;;; ==================================================================

;;; ==================================================================
;;; {{{ Helper functions:

(defun default-personal-pathname (domain-name)
  (default-name-pathname domain-name
                         *default-personal-file-modifier*))

;;; }}}
;;; ==================================================================

;;; ==================================================================
;;; {{{ Personal namespace class definition:

(defclass personal-namespace (namespace-mixin)
  ((pending-changes
    :initform 0)
   (image
    :initform nil)))

;;; initargs:
;;;
;;;   :namespace-file-pathname
;;;   :auto-save-enabled
;;;   :changes-before-save
;;;
;;;   :domain-name  (CLOS)
;;;   :class-list   (CLOS)
;;;   :read-only    (CLOS)
;;;   :usage        (CLOS)
;;;
(defmethod initialize-instance
    ((instance personal-namespace) &rest init-args
     &key &allow-other-keys)
  (unless (stringp (cadr (member :domain-name init-args)))
    (error "Illegal namespace name ~A - a string is required."
           (cadr (member :domain-name init-args))))
  (let* ((domain-name (cadr (member :domain-name init-args)))
         (class-list (cadr (member :class-list init-args)))
         (read-only (cadr (member :read-only init-args)))
         (usage (cadr (member :usage init-args)))
         (pathname (or (cadr (member
                              :namespace-file-pathname
                              init-args))
                       (default-personal-pathname)))
         (auto-save (or (cadr (member :auto-save-enabled init-args))
                        t))
         (changes (or (cadr (member :changes-before-save init-args))
                      *default-personal-change-window*))
         (new-object (build-object
                      domain-name
                      :namespace
                      (list :type +personal-type+
                            :usage usage
                            :namespace-file-pathname pathname
                            :changes-before-save changes
                            :auto-save-enabled auto-save
                            :read-only read-only)))
         (update-domain-object t)
         (user-id "NAMESPACE"))
    (if (not (null (cadr (member :new init-args))))
        (progn
          ;; (create-directory ... )
          (when (probe-file pathname)
            (cmsg "WARNING: Personal namespace ~A is being ~@
                   initialized without using existing database ~@
                   files"
                  domain-name))
          (namespace-make-hash-table instance)
          (setf (slot-value instance 'image)
                (make-instance 'personal-namespace
                   :namespace (slot-value ns 'namespace))))
        (progn
          ;;; load files and update object... luls
          ))
    (when update-domain-object
      (namespace-internal-add-object instance new-object))))


;;; }}}
;;; ==================================================================




;;; Add the class to the list of namespace classes
(add-namespace-class +personal-type+ 'personal-namespace)

;;; personal-namespace.lisp ends here
