;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-NAMESPACE; Base: 10; Lowercase: Yes -*-
;;;
;;; management.lisp --- Namespace management
;;;
;;; Time-stamp: <Friday Dec  9, 2011 14:04:20 asmodai>
;;; Revision:   4
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    09 Dec 2011 11:01:50
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
;;; {{{ Namespace type/class pair functions:

(defsubst get-namespace-class (namespace-type)
  (second (assoc namespace-type *namespace-class-alist*)))

(defun add-namespace-class (namespace-type namespace-class)
  "Register a new namespace type identifier and its corresponding CLOS
class."
  (unless (equal (get-namespace-class namespace-type) namespace-class)
    (push (list namespace-type namespace-class)
          *namespace-class-alist*)))

;;; }}}
;;; ==================================================================

;;; ==================================================================
;;; {{{ System namespace (the root):

(defun %system-machine-instance ()
  (declare (optimize (speed 3)
                     (safety 0)
                     (space 3)
                     (debug 0)))
  (let* ((mi (machine-instance))
         (pos (string-search #\. mi)))
    (declare (type integer pos)
             (type simple-string mi))
    (if (> pos 0)
        (subseq mi 0 pos)
        mi)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (boundp '*system-name*)
             (fboundp '%system-machine-instance))
    (setf *system-name* (%system-machine-instance))))

(defun clear-system-namespace ()
  (if *system-namespace*
      (namespace-flush *system-namespace*)
      (namespace-manager-init :run-init t :force t)))

;;; }}}
;;; ==================================================================

;;; ==================================================================
;;; {{{ Initialization:

;;; Register the namespace initialization list as a valid
;;; initialization keyword
(push '(:namespace *namespace-initialization-list*)
      cl-startup:*initialization-keywords*)

(defun system-initialization-form ()
  "Initialization form for the main system namespace object."
  (when (null *system-namespace*)
    (setf *system-namespace*
          (new-namespace *system-name* :basic)))
  ;;
  ;; Register the system namespace
  (register-namespace *system-namespace* :beginning)
  ;;
  ;; Clear it to bootstrap the namespace initializations.
  (clear-system-namespace))

;;; Register the main system namespace init form
(add-initialization "Setup system namespace"
                    '(system-initialization-form)
                    :namespace)
;;; }}}
;;; ==================================================================

;;; ==================================================================
;;; {{{ Namespace manager functions:

(defun namespace-manager-init (&optional &key (run-init t)
                               (force nil))
  (discard-namespaces)
  ;;
  ;; If :FORCE is specified, clobber the system namespace.
  (when force
    (setf *system-namespace* nil))
  ;;
  ;; When :RUN-INIT is specified, we don't want to re-run the system
  ;;; namespace initialization...
  (unless run-init
    (system-initialization-form))
  ;;
  ;; ... we want to run any further namespace initialization forms
  ;; instead.
  (when run-init
    (cmsg "Initializing namespaces:")
    (initializations '*namespace-initeialization-list* :redo)
    (cmsg "Finished namespaces."))
  ;;
  ;; We're done, so return the list of all known namespaces.
  *all-known-namespaces*)

;;; }}}
;;; ==================================================================

;;; ==================================================================
;;; {{{ Namespace functions:

(defun clear-all-namespaces (&optional &key (notify t))
  "Clear all namespace instances of their data."
  (dolist (ns *all-known-namespaces*)
    (namespace-flush ns))
  (when notify
    (cmsg "All namespaces have been cleared.")))

(defun discard-namespaces ()
  "Discard all known namespaces and the namespace search list."
  (setf *all-known-namespaces* nil
        *namespace-search-list* nil))

(defmacro new-namespace (name type &optional &rest init-args)
  (let ((plist (nconc (list :domain-name name) init-args))
        class)
    (unless (setf class (get-namespace class type))
      (error "~A is not a known namespace class type." type))
    `(make-instance ',class ,@plist)))

;;; TODO: Fail to register if a namespace is already present.
(defun register-namespace (ns &optional (search-location :beginning))
  (push ns *all-known-namespaces*)
  (case search-location
    (:beginning
     (push ns *namespace-search-list*))
    (:end
     (let ((last (last *namespace-search-list*)))
       (cond ((null last)
              (setf *namespace-search-list* (list ns)))
             ((eq (car last) *system-namespace*)
              (rplaca last ns)
              (rplacd last (list *system-namespace*)))
             (:otherwise
              (rplacd last (list ns)))))))
  ns)

(defun add-namespace (name type &optional search-location
                      &rest init-args)
  (let ((ns (apply 'new-namespace name type init-args)))
    (register-namespace ns search-location)
    ns))

;;; }}}
;;; ==================================================================

;;; ==================================================================
;;; {{{ Namespace searching:

(defun find-domain-delimiter (name &optional &key (start 0) end
                              from-end)
  "Find the position of the domain name delimiter within a name."
  (when (stringp name)
    (or (position *domain-delimiter* name
                  :start start
                  :end end
                  :from-end from-end)
        (and *alternate-domain-delimiter*
             (position *alternate-domain-delimiter*
                       :start start
                       :end end
                       :from-end from-end)))))

(defun find-known-namespace (name &optional &key (start 0) end)
  "Find a namespace that is known about (i.e. it is in
*ALL-KNOWN-NAMESPACES*.)"
  (dolist (namespace *all-known-namespaces*)
    (when (string-equal (namespace-domain-name namespace)
                        name
                        :start2 start
                        :end2 end)
      (return namespace))))

;;; TODO: conditions please
(defun find-namespace (&optional name &key (if-does-not-exist :error))
  "Find the namespace object corresponding to a domain name (if
present) or return the default namespace (if NAME is NIL.)"
  (cond ((null name)
         (if (null *namespace-search-list*)
             (error "No known namespace defaults.")
             (car *namespace-search-list*)))
        ((or (stringp name)
             (symbolp name))
         (multiple-value-bind (ignore namespace)
             (find-closest-namespace name t)
           namespace))
        ((typep name 'basic-namespace)
         name)
        (:otherwise
         (error "~S is not a valid form for identifying a namespace"
                name))))

;;; }}}
;;; ==================================================================








;;; management.lisp ends here
