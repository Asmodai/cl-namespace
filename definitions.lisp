;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-NAMESPACE; Base: 10; Lowercase: Yes -*-
;;;
;;; definitions.lisp --- Various definitions.
;;;
;;; Time-stamp: <Friday Dec  9, 2011 20:09:57 asmodai>
;;; Revision:   4
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    09 Dec 2011 14:01:01
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
;;; {{{ Variables, parameters, and constants:

(defvar *system-namespace* nil
  "The system namespace.")

;;; This is defined in management.lisp
(defparameter *system-name* :unbound
    "The name of this system."))

(defparameter *domain-delimiter* #\.
  "The character used to delimit components of an absolutely-qualified
  name.")

(defparameter *alternate-domain-delimiter* #\|
  "Old-style TI Explorer domain delimiters.  Included should I attempt
to get this to work with Lisp machine namespaces.")

(defvar *all-known-namespace* nil
  "A list of all the currently-known namespace instances.")

(defvar *namespace-search-list* nil
  "An ordered list for relative names, used when searching
namespaces.")

(defvar *namespace-class-alist* nil
  "A list of namespace types and their associated CLOS classes.")

(defvar *namespace-initialization-list* nil
  "Initialization forms for namespace-related facilities.")

(defparameter *public-file-version-tag* ""
  "A tag appended to names of public namespace files.")

(defparameter *personal-file-version-tag* ""
  "A tag appended to names of personal namespace files.")

(defparameter *dont-cache* :*non-cacheable*
  "A typing shortcut flag for attributes that are not to be cached.")

(defparameter *alias-of* :*alias-of*
  "A typing shortcut flag for attributes that are aliases of other
  attributes.")

(defparameter *name-directory* "name-service"
  "A relative path in which namespace files are saved.")

(defparameter *name-file-extension* "lns"
  "The filename extension to use for namespace files.")

(defconstant +basic-type+ :basic
  "The most-basic of all namespace classes.")

(defconstant +personal-type+ :personal
  "A namespace class representing personal attributes - that is, user
settings and the like.")

(defconstant +server-type+ :namespace-server
  "A namespace class representing server attributes - that is,
settings for servers and the like.")

(defconstant +remote-type+ :public
  "A namespace class representing objects that can be accessed from
other namespace servers.")

(defconstant +symbolics-remote-type+ :symbolics
  "A namespace class representing a Symbolics Lisp Machine namespace
object.")

(defparameter *wildcard* :*)

(defparameter *wildany* #\*)

(defparameter *wildone* #\?)

(defparameter *wildset* (list *wildany* *wildone*))

;;; }}}
;;; ==================================================================

;;; ==================================================================
;;; {{{ Structures:

;;; The meta object for namespace objects.  Defines common fields used
;;; by most namespace-related structures.
(defstruct (meta-namespace-object :named)
  (timestamp nil)                       ; Timestamp of last change.
  (deleted nil))                        ; T if object is `deleted.'

;;; ------------------------------------------------------------------
;;; {{{ Namespace object:

;;; The basic namespace object.
(defstruct (namespace-object
            :named
            (:include meta-namespace-object)
            (:print-function namespace-object-print))
  name                                  ; Object name.
  class                                 ; Object class.
  (plist nil)                           ; Property list.
  (attributes nil))                     ; Attributes list.

(defun namespace-object-print (obj stream &optional junk)
  (declare (ignore junk)
           (optimize (speed 3)
                     (space 3)
                     (safety 0)
                     (debug 0)))
  (format stream "#<NAMESPACE-OBJECT ~S ~S ~S ~S ~S ~S>"
          (namespace-object-name obj)
          (namespace-object-class obj)
          (namespace-object-timestamp obj)
          (namespace-object-deleted obj)
          (namespace-object-plist obj)
          (namespace-object-attributes obj)))

(defun namespace-object-read (op name stream)
  (declare (ignore op name)
           (optimize (speed 3)
                     (space 3)
                     (safety 3)
                     (debug 0)))
  (let*((name (read stream t nil t))
        (class (read stream t nil t))
        (timestamp (read stream t nil t))
        (deleted (read stream t nil t))
        (plist (read stream t nil t))
        (attributes (read stream t nil t)))
    (prog1
      (make-namespace-object :timestamp timestamp
                             :deleted deleted
                             :name name
                             :class class
                             :plist plist
                             :attributes attributes)
      (read stream t nil t)
      (read-char stream))))

;;; }}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;; {{{ Namespace attribute:

;;; The attribute object
(defstruct (namespace-attribute
            :named
            (:include meta-namespace-object)
            (:print-function namespace-attribute-print))
  name                                  ; Attribute name.
  (type nil)                            ; Attribute type.
  (key nil)                             ; Attribute key.
  value)                                ; Attribute value.

(defun namespace-attribute-print (attr stream &optional junk)
  (declare (ignore junk)
           (optimize (speed 3)
                     (space 3)
                     (safety 0)
                     (debug 0)))
  (format stream "#<NAMESPACE-ATTRIBUTE ~S ~S ~S ~S ~S ~S>"
          (namespace-attribute-name attr)
          (namespace-attribute-type attr)
          (namespace-attribute-key attr)
          (namespace-attribute-timestamp attr)
          (namespace-attribute-deleted attr)
          (namespace-attribute-value attr)))

(defun namespace-attribute-read (op name stream)
  (declare (ignore op name)
           (optimize (speed 3)
                     (space 3)
                     (safety 3)
                     (debug 0)))
  (let* ((name (read stream t nil t))
         (type (read stream t nil t))
         (key (read stream t nil t))
         (timestamp (read stream t nil t))
         (deleted (read stream t nil t))
         (value (read stream t nil t)))
    (prog1
      (make-namespace-attribute :timestamp timestamp
                                :deleted deleted
                                :name name
                                :type type
                                :key key
                                :value value)
      (read stream t nil t)
      (read-char stream))))

;;; }}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;; {{{ Namespace group member:

(defstruct (namespace-group-member
            :named
            (:include meta-namespace-object)
            (:print-function namespace-group-member-print))
  value)

(defun namespace-group-member-print (obj stream &optional junk)
  (declare (ignore junk)
           (optimize (speed 3)
                     (space 3)
                     (safety 0)
                     (debug 0)))
  (format stream "#<NAMESPACE-GROUP-MEMBER ~S ~S ~S>"
          (namespace-group-member-timestamp obj)
          (namespace-group-member-deleted obj)
          (namespace-group-member-value obj)))

(defun namespace-group-member-read (op name stream)
  (declare (ignore op name)
           (optimize (speed 3)
                     (space 3)
                     (safety 3)
                     (debug 0)))
  (let* ((timestamp (read stream t nil t))
         (deleted (read stream t nil t))
         (value (read stream t nil t)))
    (prog1
      (make-namespace-group-member :timestamp timestamp
                                   :deleted deleted
                                   :value value)
      (read stream t nil t)
      (read-char stream))))

;;; }}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;; {{{ Namespace log:

(defstruct (namespace-log :named)
  (timestamp nil)
  (version-number 0)
  (originating-server nil)
  (server-major-version nil)
  (server-minor-version nil)
  (operation nil)
  (argument-list nil)
  (duplicate nil)
  (server-propagation-list nil))

;;; }}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;; {{{ `Personal' namespace:

(defstruct (dumped-personal :named)
  (namespace nil)
  (class-list nil))

;;; }}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;; {{{ `Server' namespace:

(defstruct (dumped-server
            :named
            (:include namespace-personal))
  (match-server nil)
  (match-major nil)
  (match-minor nil)
  ;;
  ;; Local only
  (local-audit-enabled-override nil)
  (local-audit-enabled nil)
  (local-save-enabled-override nil)
  (local-save-enabled nil))

;;; }}}
;;; ------------------------------------------------------------------

;;; }}}
;;; ==================================================================

;;; ==================================================================
;;; {{{ Property and attribute functions:

;;; Attach a property to the (hidden) plist of an object.
(defsubst object-put-property (object property value)
  (setf (get (namespace-object-plist object) property) value))

;;; Get a property from the (hidden) plist of an object.
(defsubst object-get-property (object property)
  (get (namespace-object-plist object) property))

;;; Returns T if an attribute is a group attribute.
(defsubst attribute-is-group (attribute)
  (eq (namespace-attribute-type attribute) :group))

;;; Returns T if the two objects are referenced in the same way
;;; (e.g. name and class have evivalency.)
(defsubst object-homonym (obj1 obj2)
  (and (key-compare (namespace-object-name obj1)
                    (namespace-object-name obj2))
       (key-compare (namespace-object-class obj1)
                    (namespace-object-class obj2))))

;;; Finds the attribute named NAME in the attribute list of OBJECT.
;;; Returns the attribute or NIL if the attribute does not exist.
;;; Ignores any deletion state the object may or may not have.
(defsubst find-attribute (object name)
  (when object
    (dolist (item (namespace-object-attributes object))
      (when (key-compare (namespace-attribute-name item) name)
        (return item)))))

;;; The name of the object for which OBJECT is an alias.
(defsubst object-alias-name (object)
  (get-attribute-value object *alias-of*))

;;; Returns T unless OBJECT has an attribute indicating that is is not
;;; to be cached.
(defsubst object-is-cacheable (object)
  (not (get-attribute-value object *dont-cache*)))

;;; The value of NAME in the domain for this namespace
(defsubst internal-attribute-value (ns name)
  (general-attribute-value
   (namespace-get-attribute ns
                            (namespace-domain-name ns)
                            :namespace
                            name)))

;;; }}}
;;; ==================================================================

;;; definitions.lisp ends here

