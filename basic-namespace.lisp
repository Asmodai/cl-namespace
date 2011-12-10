;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-NAMESPACE; Base: 10; Lowercase: Yes -*-
;;;
;;; basic-namespace.lisp --- Basic namespace class.
;;;
;;; Time-stamp: <Saturday Dec 10, 2011 00:58:59 asmodai>
;;; Revision:   4
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    09 Dec 2011 10:59:00
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
;;; {{{ Basic namespace:

(defclass basic-namespace ()
  ((domain-name
    :initarg :domain-name
    :initform nil
    :accessor namespace-domain-name)
   (class-list
    :initarg :class-list
    :initform nil)
   (timestamp
    :initform nil)
   (read-only
    :initarg :read-only
    :initform nil
    :accessor namespace-read-only)
   (usage-mode
    :initarg :usage
    :initform nil)))

;;; }}}
;;; ==================================================================

;;; ==================================================================
;;; {{{ Basic namespace methods:

(defgeneric namespace-make-hash-table (ns)
  (:method ((ns basic-namespace))
    (setf (slot-value ns 'namespace)
          (make-namespace-hash-table))))

(defgeneric namespace-mode (ns)
  (:method ((ns basic-namespace))
    :basic))

(defgeneric namespace-type (ns)
  (:method ((ns basic-namespace))
    +basic-type+))

(defgeneric namespace-usage (ns)
  (:method ((ns basic-namespace))
    (slot-value ns 'usage-mode)))

(defgeneric (setf namespace-usage) (mode ns)
  (:method (mode (ns basic-namespace))
    (setf (slot-value ns 'usage-mode) mode)))

(defgeneric namespace-domain-query (ns &optional name)
  (:method ((ns basic-namespace) &optional (name nil))
    (unless name
      (setq name (namespace-domain-name ns)))
    (namespace-view-object ns name :namespace)))

(defgeneric namespace-setup-from-object (ns &rest args)
  (:method ((ns basic-namespace) &rest args)
    (declare (ignore args))))

(defgeneric namespace-all-classes (ns)
  (:method ((ns basic-namespace))
    (slot-value ns 'class-list)))

(defgeneric namespace-add-class (ns class)
  (:method ((ns basic-namespace) class)
    (unless (member class (slot-value ns 'class-list)
                    :test #'key-compare)
      (push-end class (slot-value ns 'class-list)))))

;;;
;;; TODO: Should we add some sort of `namespace-local' method?
;;;

(defmethod namespace-clear-hidden-plists (ns)
  (:method ((ns basic-namespace))
    (let (new-class-list)
      (maphash #'(lambda (junk object)
                   (declare (ignore junk))
                   (setf (namespace-object-plist object) nil)
                   (unless (or (namespace-object-deleted object)
                               (member (namespace-object-class object)
                                       new-class-list
                                       :test #'key-compare))
                     (push (namespace-object-class object)
                           new-class-list)))
               (slot-value ns 'namespace))
      new-class-list)))

(defgeneric namespace-clear-timestamps (ns)
  (:method ((ns basic-namespace))
    (maphash #'(lambda (junk object)
                 (declare (ignore junk))
                 (set-object-time object :override 0))
             (slot-value ns 'namespace))))

(defgeneric namespace-flush (ns &optional class)
  (:method ((ns basic-namespace) &optional class)
    (if class
        (maphash #'(lambda (key obj cls tab)
                     (when (key-compare cls
                                        (namespace-object-class obj))
                       (remhash key tab)))
                 (slot-value ns 'namespace)
                 class
                 (slot-value ns 'namespace))
        (clrhash (slot-value ns 'namespace)))
    (or class :complete)))
                     
(defgeneric namespace-updates-ok (ns &rest args)
  (:method ((ns basic-namespace) &rest args)
    (declare (ignore args))
    (not (namespace-read-only ns))))

(defparameter *basic-update-in-seconds* t)
(defgeneric namespace-mark-object-updated (ns object)
  (:method ((ns basic-namespace) object)
    (if *basic-update-in-seconds*
        (object-put-property object :last-update (get-universal-time))
        (let ((old-value (object-get-property object :last-update)))
          (object-put-property object
                               :last-update
                               (if (numberp old-value)
                                   (1+ old-value)
                                   1))))))

(defgeneric namespace-add-object
    (ns object-name object-class 
        &optional attributes pre-delete check-time)
  (:method ((ns basic-namespace) object-name object-class
            &optional attributes (pre-delete nil) (check-time nil))
    (when (namespace-updates-ok ns)
      (let ((object (build-object object-name object-class attributes)))
        (when check-time
          (set-object-time object :override check-time))
        (namespace-internal-add-object ns
                                       object
                                       pre-delete
                                       check-time)))))

(defgeneric namespace-internal-add-object
    (ns object
        &optional pre-delete check-time)
  (:method ((ns basic-namespace) object
            &optional (pre-delete nil) (check-time nil))
    (let* ((old (namespace-find-object
                 ns
                 (namespace-object-name object)
                 (namespace-object-class object)))
           stamp
           applied)
      (if old
          (progn
            (when pre-delete
              (namespace-internal-delete-object ns
                                                old
                                                check-time
                                                pre-delete))
            (when (and check-time
                       (or (when (namespace-object-deleted object)
                             (setf stamp
                                   (namespace-object-timestamp object)))
                           (when (namespace-object-deleted old)
                             (setf stamp
                                   (namespace-object-timestamp old))))
                       (namespace-internal-delete ns
                                                  old
                                                  check-time
                                                  stamp))
              (setf applied t))
            (when (or (not check-time)
                      (timestamp-p old object))
              (mirror-object object old)
              (setf applied t))
            (when applied
              (namespace-mark-object-updated ns old)
              old))
          (progn
            (namespace-mark-object-updated ns object)
            (namespace-put-object ns object)
            object)))))
  
(defgeneric namespace-merge-attributes (ns src dst
                                           &optional check-time)
  (:method ((ns basic-namespace) src dst
            &optional check-time)
    (let (applied)
      (when (and src
                 (or check-time
                     (not (namespace-object-deleted src)))
                 dst)
        (loop for attribute in (namespace-object-attributes src)
              do (when (namespace-internal-add-attribute ns
                                                         dst
                                                         attribute
                                                         check-time)
                   (setf applied t))))
      applied)))
  
(defgeneric namespace-put-object (ns object)
  (:method ((ns basic-namespace) object)
    (namespace-add-class ns (namespace-object-class object))
    (let ((key (generate-hash-key (namespace-object-name object)
                                  (namespace-object-class object))))
      (puthash key object (slot-value ns 'namespace)))
    object))

(defgeneric namespace-get-object (ns name class)
  (:method ((ns basic-namespace) name class)
    (let ((object (namespace-view-object ns name class)))
      (when object
        (copy-entire-object object)))))

(defgeneric namespace-view-object (ns name class)
  (:method ((ns basic-namespace) name class)
    (let ((object (namespace-find-object ns name class)))
      (when (and object
                 (not (namespace-object-deleted object)))
        object))))

(defgeneric namespace-find-object (ns name class)
  (:method ((ns basic-namespace) name class)
    (let* ((key (generate-hash-key name class))
           (obj (gethash key (slot-value ns 'namespace))))
      (when (and obj
                 (null (object-get-property obj :last-update)))
        (object-put-property obj :last-update 0))
      obj)))

(defgeneric namespace-delete-object
    (ns name class
        &optional check-time timestamp synthesize)
  (:method ((ns basic-namespace) name class
            &optional (check-time nil) (timestamp 0) (synthesize t))
    (when (namespace-updates-ok ns)
      (let ((object (namespace-find-or-synthesize name
                                                  class
                                                  synthesize)))
        (when object
          (let ((result (namespace-internal-delete-object ns
                                                          object
                                                          check-time
                                                          timestamp)))
            (when result
              (namespace-mark-object-updated ns object))
            result))))))
  
(defgeneric namespace-internal-delete-object
    (ns object check-time timestamp)
  (:method ((ns basic-namespace) object check-time timestamp)
    (let (applied)
      (loop for attribute in (namespace-object-attributes object)
            do (when (namespace-internal-delete-attribute
                      ns
                      attribute
                      check-time
                      timestamp)
                 (setf applied t)))
      (when (or (not check-time)
                (timestamp-< (namespace-object-timestamp object)
                             timestamp))
        (setf (namespace-object-timestamp object) timestamp
              (namespace-object-deleted object) t
              applied t))
      applied)))

(defgeneric namespace-add-attribute
    (ns object-name class attribute-name attribute-value
        &optional group key check-time synthesize)
  (:method ((ns basic-namespace) object-name class attribute-name
            attribute-value 
            &optional group key (check-time nil) (synthesize t))
    (when (namespace-updates-ok ns)
      (let* ((object (namespace-find-or-synthesize ns
                                                   object-name
                                                   class
                                                   synthesize
                                                   nil
                                                   nil
                                                   nil
                                                   nil
                                                   :undelete
                                                   check-time))
             (attribute (when object
                          (build-attribute nil
                                           attribute-name
                                           attribute-value
                                           (when group
                                             :group
                                             nil)
                                           key))))
        (when attribute
          (when check-time
            (set-attribute-time attribute
                                :override
                                check-time))
          (let ((result (namespace-internal-add-attribute ns
                                                          object
                                                          attribute
                                                          check-time)))
            (when result
              (namespace-mark-object-updated ns object))
            result))))))
  
(defgeneric namespace-internal-add-attribute (ns object attribute
                                                 &optional check-time)
  (:method ((ns basic-mixin) object attribute &optional
            (check-time nil))
    (let* ((old-attr (find-attribute
                      object
                      (namespace-attribute-name attribute)))
           stamp
           applied
           (new-attr old-attr)
           (new-value (namespace-attribute-value attribute)))
      (if old-attr
          (progn
            (when (or check-time
                      (not (namespace-attribute-deleted attribute)))
              (when (attribute-is-group attribute)
                (setf (namespace-attribute-key old-attr)
                      (namespace-attribute-key attribute))
                (loop for member
                      in (namespace-attribute-value attribute)
                      do (when (namespace-internal-add-group-member
                                ns
                                old-attr
                                member
                                check-time)
                           (setf applied t)))
                (when
                    (and check-time
                         (or
                          (when (namespace-attribute-deleted attribute)
                            (setf
                             stamp
                             (namespace-attribute-timestamp attribute)))
                          (when (namespace-attribute-deleted old-attr)
                            (setf
                             stamp
                             (namespace-attribute-timestamp
                              old-attr))))
                         (namespace-internal-delete-attribute
                          ns
                          old-attr
                          check-time
                          stamp))
                  (setf applied t))
                (when (or (not check-time)
                          (timestamp-p old-attr attribute))
                  (mirror-attribute attribute old-attr)
                  (setf (namespace-attribute-value old-attr)
                        new-value)
                  (setf applied t))
                (when applied
                  new-attr)))
            (progn
              (push-end attribute (namespace-object-attributes object))
              attribute))))))

(defgeneric namespace-get-attribute (ns object-name class a-name
                                        &optional deleted-p)
  (:method ((ns basic-namespace) object-name class a-name
            &optional (deleted-p nil))
    (let ((object (namespace-find-object ns object-name class)))
      (when object
        (let ((attr (find-attribute object a-name)))
          (when attr
            (when (or deleted-p
                      (and (not (namespace-object-deleted object))
                           (not (namespace-attribute-deleted attr))))
              (copy-entire-attribute attr))))))))

(defgeneric namespace-delete-attribute
    (ns object-name class a-name
        &optional check-time timestamp synthesize)
  (:method ((ns basic-namespace) object-name class a-name
            &optional (check-time nil) (timestamp 0) (synthesize t))
    (when (namespace-updates-ok ns)
      (multiple-value-bind (attribute object)
          (namespace-find-or-synthesize ns
                                        object-name
                                        class
                                        synthesize
                                        a-name
                                        nil
                                        nil
                                        nil
                                        nil
                                        nil
                                        t)
        (when attribute
          (let ((result (namespace-internal-delete-attribute ns
                                                             attribute
                                                             check-time
                                                             timestamp)))
            (when result
              (namespace-mark-object-updated ns object))
            result))))))

(defgeneric namespace-internal-delete-attribute
    (ns attribute check-time timestamp)
  (:method ((ns basic-namespace) attribute check-time timestamp)
  (let (applied)
    (when (attribute-is-group attribute)
      (loop for member in (namespace-attribute-value attribute)
            do (when (namespace-internal-delete-group-member
                      ns
                      member
                      check-time
                      timestamp)
                 (setf applied t))))
    (when (or (not check-time)
              (timestamp-< (namespace-attribute-timestamp attribute)
                           timestamp))
      (setf (namespace-attribute-timestamp attribute) timestamp
            (namespace-attribute-deleted attribute) t
            applied t))
    applied))

(defgeneric namespace-add-group-member
    (ns name class a-name member-name key
        &optional check-time synthesize)
  (:method ((ns basic-namespace) name class a-name member-name key
            &optional (check-time nil) (synthesize t))
    (when (namespace-updates-ok ns)
      (multiple-value-bind (attribute object)
          (namespace-find-or-synthesize ns
                                        name
                                        class
                                        synthesize
                                        a-name
                                        :group
                                        nil
                                        nil
                                        :undelete
                                        check-time
                                        t)
        (when attribute
          (when key
            (setf (namespace-attribute-key attribute) key))
          (let ((member (build-group-member member-name)))
            (when check-time
              (set-member-time member :override check-time))
            (let ((result (namespace-internal-add-group-member
                           ns
                           attribute
                           member
                           check-time)))
              (when result
                (namespace-mark-object-updated object)
                result))))))))

(defgeneric namespace-internal-add-group-member
    (ns attribute member check-time)
  (:method ((ns basic-namespace) attribute member check-time)
    (let ((old-member (find-group-member
                       attribute
                       (namespace-group-member-value member))))
      (if old-member
          (when (or (and (not check-time)
                         (not (namespace-group-member-deleted member)))
                    (timestamp-p old-member member))
            (mirror-member member old-member :include-value)
            member)
          (progn
            (push-end member (namespace-attribute-value attribute))
            member)))))

(defgeneric namespace-delete-group-member
    (ns name class a-name member-name key
        &optional check-time timestamp synthesize)
  (:method ((ns basic-namespace) name class a-name member-name key
            &optional (check-time nil) (timestamp 0) (synthesize t))
    (when (namespace-updates-ok ns)
      (multiple-value-bind (member junk object)
          (namespace-find-or-synthesize ns
                                        name
                                        class
                                        synthesize
                                        a-name
                                        :group
                                        member-name
                                        key
                                        nil
                                        nil
                                        t
                                        t)
        (declare (ignore junk))
        (when member
          (let ((result (namespace-internal-delete-group-member
                         ns
                         member
                         check-time
                         timestamp)))
            (when result
              (namespace-mark-object-updated object)
              result)))))))

(defgeneric namespace-internal-delete-group-member
    (ns member check-time timestamp)
  (:method ((ns basic-namespace) member check-time timestamp)
    (when (or (not check-time)
              (timestamp-< (namespace-group-member-timestamp member)
                           timestamp))
      (setf (namespace-group-member-timestamp member) timestamp
            (namespace-group-member-deleted member) t))))

(defgeneric namespace-find-or-synthesize
    (ns name class
        &optional synthesize-ok a-name a-type member-name key undelete
        check-time lookup-attr lookup-member)
  (:method ((ns basic-namespace) name class
            &optional (synthesize-ok t) a-name a-type member-name key
            undelete check-time lookup-attr lookup-member)
    (let ((obj (or (namespace-find-object ns name class)
                   (and synthesize-ok
                        (namespace-put-object
                         (build-object name class nil)))))
          attr
          member)
      (when obj
        (when (and undelete
                   (namespace-object-deleted obj)
                   (or (null check-time)
                       (timestamp-< (namespace-object-timestamp obj)
                                    check-time)))
          (setf (namespace-object-timestamp obj) check-time
                (namespace-object-deleted obj) nil))
        (if (or a-name member-name lookup-attr lookup-member)
            (progn
              (when (setf attr (or (find-attribute obj a-name)
                                   (and (consp a-name)
                                        (eq (second a-name) :group)
                                        (find-attribute
                                         obj
                                         (car a-name)))
                                   (and synthesize-ok
                                        (build-attribute
                                         obj
                                         a-name
                                         nil
                                         a-type
                                         key))))
                (when (and undelete
                           (namespace-attribute-deleted attr)
                           (or (null check-time)
                               (timestamp-<
                                (namespace-attribute-timestamp attr)
                                check-time)))
                  (setf (namespace-attribute-timestamp attr) check-time
                        (namespace-attribute-deleted attr) nil))
                (if (or member-name lookup-member)
                    (progn
                      (unless (setf member (find-group-member attr
                                                              member-name
                                                              key))
                        (when synthesize-ok
                          (setf member (build-group-member member-name))
                          (push-end member
                                    (namespace-attribute-value attr))))
                      (values member attr obj))
                    (values attr obj))))
            obj)))))

(defgeneric namespace-expunge (ns &optional check-time timestamp)
  (:method ((ns basic-namespace)
            &optional (check-time nil) (timestamp 0))
    (declare (special timestamp check-time))
    (let ((names (slot-value ns 'namespace)))
      (declare (special names))
      (maphash
       #'(lambda (name object &rest junk)
           (declare (ignore junk))
           (if (and (namespace-object-deleted object)
                    (or (not check-time)
                        (< (namespace-object-timestamp object)
                           timestamp)))
               (remhash name names)
               (progn
                 (setf (namespace-object-attributes object)
                       (delete-if
                        #'(lambda (item)
                            (and (namespace-attribute-deleted item)
                                 (or (not check-time)
                                     (< (namespace-attribute-timestamp
                                         item)
                                        timestamp))))
                        (namespace-object-attributes object)))
                 (loop for attribute
                       in (namespace-object-attributes object)
                       do
                    (if (eq (namespace-attribute-type attribute) :group)
                        (setf (namespace-attribute-value attribute)
                              (delete-if
                               #'(lambda (member)
                                   (and
                                    (namespace-group-member-deleted
                                     member)
                                    (or (not check-time)
                                        (<
                                         (namespace-group-member-timestamp
                                          member)
                                         timestamp)))))
                              (namespace-attribute-value
                               attribute)))))))
       names))))

(defgeneric namespace-all-objects (ns &rest args)
  (:method ((ns basic-namespace) &rest args)
    (declare (ignore args))
    (maphash #'(lambda (junk object)
                 (declare (ignore junk))
                 (copy-entire-object object))
             (slot-value ns 'namespace))))

(defgeneric namespace-find-objects-from-properties
    (ns &optional name-pattern class first-only property-list test
        peek)
  (:method ((ns basic-namespace) &optional name-pattern class
            first-only property-list test peek)
    (namespace-extract-objects-from-properties ns
                                               name-pattern
                                               class
                                               first-only
                                               property-list
                                               test
                                               (if peek
                                                   :read-only
                                                   :copy))))

(defgeneric namespace-list-objects-from-properties
    (ns &optional name-pattern class first-only property-list brief
        test)
  (:method ((ns basic-namespace) &optional name-pattern class first-only
            property-list brief test)
    (namespace-extract-objects-from-properties ns
                                               name-pattern
                                               class
                                               first-only
                                               property-list
                                               test
                                               (if brief
                                                   :brief
                                                   :list))))

(defsubst %object-rep (object format)
  (case format
    (:brief
     (list (namespace-object-name object)
           (namespace-object-class object)))
    (:list
     (format-objects object))
    (:read-only
     object)
    (:otherwise
     (copy-entire-object object))))

(defgeneric namespace-extract-objects-from-properties
    (ns &optional name-pattern class first-only property-list test
        format)
  (:method ((ns basic-namespace) &optional name-pattern class
            first-only property-list test format)
    (let (result)
      (block method
        (cond
          ((and class
                (null test)
                (not (member class class-list :test #'key-compare))))
          ((and class
                (null test)
                (null property-list)
                (if (listp name-pattern)
                    (loop for name in name-pattern
                          do (when (name-is-wild name)
                               (return nil))
                          finally (return name-pattern))
                    (not (name-is-wild name-pattern))))
           (setf result
                 (for-single-or-list name name-pattern
                   when
                   (let ((object (namespace-find-object ns name class)))
                     (and object
                          (not (namespace-object-deleted object))
                          (%object-rep object format)))
                   collect it))
           (when first-only
             (setf result (car result))))
          (:otherwise
           (when (and (listp test)
                      (eq (car test) 'lambda))
             (let ((*standard-output* nil))
               (setf test (compile nil test))))
           (maphash
            #'(lambda (junk object class property-list)
                (when (and (not (namespace-object-deleted object))
                           (funcall (or test 'object-match)
                                    object
                                    name-pattern
                                    class
                                    property-list))
                  (if first-only
                      (return-from method (%object-rep object format))
                      (push (%object-rep object format) result))))
            (slot-value ns 'namespace)
            class
            property-list)))
        result))))

;;; }}}
;;; ==================================================================

;;; ==================================================================
;;; {{{ Namespace mixin class:

(defclass namespace-mixin (basic-namespace)
  ())

(defmethod namespace-type ((ns namespace-mixin))
  (internal-attribute-value ns :type))

(defmethod namespace-usage ((ns namespace-mixin))
  (internal-attribute-value ns :usage))

(defmethod (setf namespace-usage) (mode (ns namespace-mixin))
  (unless (key-compare mode (namespace-usage ns))
    (namespace-add-attribute (namespace-domain-name ns)
                             :namespace
                             :usage mode))
  mode)

(defmethod namespace-updates-ok ((ns namespace-mixin) &rest args)
  (let ((updates-ok (not (internal-attribute-value ns :read-only)))
        (name (first args))
        (class (second args)))
    (when (or (and (key-compare name (namespace-domain-name ns))
                   (key-compare class :namespace))
              updates-ok)
      ;; TODO: snapshots
      t))))

;;; }}}
;;; ==================================================================

;;; ==================================================================
;;; {{{ Namespace mixin methods:

(defmethod (setf namespace-usage) (mode (ns namespace-mixin))
  (error 'not-implemented
         :proc '(setf namespace-usage)
         :args (list (type-of ns)
                     (if (not (typep ns 'standard-class))
                         (if (symbolp ns)
                             (symbol-name ns)
                             ns)
                         ns))))

(macrolet ((!defmethod! (symbol &rest args)
             `(defmethod ,symbol ((ns namespace-mixin) ,@args)
                (error 'not-implemented
                       :proc ',symbol
                       :args (list (type-of ns)
                                   (if (not (typep
                                             ns
                                             'standard-class))
                                       (if (symbolp ns)
                                           (symbol-name ns)
                                           ns)
                                       ns))))))
  (!defmethod! namespace-make-hash-table)
  (!defmethod! namespace-mode)
  (!defmethod! namespace-domain-query &optional name)
  (!defmethod! namespace-setup-from-object &rest args)
  (!defmethod! namespace-all-classes)
  (!defmethod! namespace-add-class class)
  (!defmethod! namespace-clear-hidden-plists)
  (!defmethod! namespace-clear-timestamps)
  (!defmethod! namespace-flush)
  (!defmethod! namespace-mark-object-updated object)
  (!defmethod! namespace-add-object object-name object-class
                                    &optional attributes pre-delete
                                    check-time)
  (!defmethod! namespace-internal-add-object object &optional
                                             pre-delete check-time)
  (!defmethod! namespace-merge-attributes src dst &optional
                                          check-time)
  (!defmethod! namespace-put-object object)
  (!defmethod! namespace-get-object name class)
  (!defmethod! namespace-view-object name class)
  (!defmethod! namespace-find-object name class)
  (!defmethod! namespace-delete-object name class &optional
                                       check-time timestamp
                                       synthesize)
  (!defmethod! namespace-internal-delete-object object check-time
                                                timestamp)
  (!defmethod! namespace-add-attribute object-name class
                                       attribute-name
                                       attribute-value &optional
                                       group key check-time
                                       synthesize)
  (!defmethod! namespace-internal-add-attribute object attribute
                                                &optional
                                                check-time)
  (!defmethod! namespace-get-attribute object-name class a-name
                                       &optional deleted-p)
  (!defmethod! namespace-delete-attribute object-name class a-name
                                          &optional check-time
                                          timestamp synthesize)
  (!defmethod! namespace-internal-delete-attribute attribute
                                                   check-time
                                                   timestamp)
  (!defmethod! namespace-add-group-member name class a-name
                                          member-name key &optional
                                          check-time synthesize)
  (!defmethod! namespace-internal-add-group-member attribute
                                                   member
                                                   check-time)
  (!defmethod! namespace-delete-group-member name class a-name
                                             member-name key
                                             &optional check-time
                                             timestamp synthesize)
  (!defmethod! namespace-internal-delete-group-member member
                                                      check-time
                                                      timestamp)
  (!defmethod! namespace-find-or-synthesize name class &optional
                                            synthesize-ok a-name
                                            a-type member-name key
                                            undelete check-time
                                            lookup-attr
                                            lookup-member)
  (!defmethod! namespace-expunge &optional check-time timestamp)
  (!defmethod! namespace-all-objects &rest args)
  (!defmethod! namespace-find-objects-from-properties &optional
                                                      name-pattern
                                                      class
                                                      first-only
                                                      property-list
                                                      test peek)
  (!defmethod! namespace-list-objects-from-properties &optional
                                                      name-pattern
                                                      class
                                                      first-only
                                                      property-list
                                                      brief test)
  (!defmethod! namespace-extract-objects-from-properties &optional
                                                         name-pattern
                                                         class
                                                         first-only
                                                         property-list
                                                         test
                                                         format))

;;; }}}
;;; ==================================================================

;;; ==================================================================
;;; {{{ Utility functions:

(defun name-is-wild (name-pattern)
  (or (listp name-pattern)
      (eq name-pattern *wildcard*)
      (string-search-set *wildset* name-pattern)))

(defun object-match (object name-pattern class property-list)
  (and (or (null name-pattern)
           (eq name-pattern *wildcard*)
           (for-single-or-list indiv-pattern name-pattern
             with test-name = (namespace-object-name object)
             do (when (pattern-match indiv-pattern
                                     test-name
                                     *wildany*
                                     *wildone*)
                  (return t))))
       (or (null class)
           (key-compare class (namespace-object-class object)))
       (loop for (prop val) on property-list by #'cddr
             do (if (eq prop *wildcard*)
                    (progn
                      (unless
                          (loop for attribute
                                in (namespace-object-attributes
                                             object)
                                do
                             (when (attribute-match attribute val)
                               (return t)))
                        (return nil)))
                    (progn
                      (let ((attribute (find-attribute object prop)))
                        (unless (attribute-match attribute val)
                          (return nil)))))
                finally (return t))))

(defun attribute-match (attribute match-value)
  (let ((attribute-value (general-attribute-value attribute)))
    (or (and (null match-value)
             (null attribute-value))
        (and attribute
             (or (and attribute-value
                      (eq match-value *wildcard*))
                 (and (attribute-is-group attribute)
                      (or
                       (for-single-or-list match-element match-value
                         do (unless (member match-element
                                            attribute-value
                                            :test #'wild-key-compare)
                              (return nil))
                         finally (return t))
                       (and (consp match-value)
                            (member match-value
                                    attribute-value
                                    :test #'wild-key-compare))))
                 (wild-key-compare match-value attribute-value))))))

(defun pattern-match (pattern sample wild-any wild-one
                      &optional return-specs-flag
                      &aux specs)
  (if (not (and (stringp pattern)
                (stringp sample)))
      (equal pattern sample)
      (do ((p-ptr 0)
           (p-next)
           (p-char wild-one)
           (s-ptr -1)
           (set (list wild-any wild-one)))
          (())
        (setq p-next (string-search-set set pattern p-ptr))
        (cond ((>= s-ptr (length sample))
               (let ((old-s-ptr s-ptr))
                 (setq s-ptr
                       (search pattern (the string (string sample))
                               :from-end t
                               :end2 ()
                               :start2 s-ptr
                               :start1 p-ptr
                               :test #'char-equal))
                 (when return-specs-flag
                   (push (subseq sample old-s-ptr s-ptr) specs))))
              ((eq p-char wild-one)
               (and return-specs-flag (>= s-ptr 0)
                    (push (aref sample s-ptr) specs))
               (setq s-ptr
                     (and (string-equal sample
                                        pattern
                                        (1+ s-ptr)
                                        p-ptr
                                        (+ 1 s-ptr
                                           (- (or p-next
                                                  (length pattern))
                                              p-ptr))
                                        p-next)
                          (1+ s-ptr))))
              ((null p-next)
               (let ((old-s-ptr s-ptr))
                 (setq s-ptr
                       (search pattern (the string (string sample))
                               :from-end t
                               :end2 ()
                               :start2 s-ptr
                               :start1 p-ptr
                               :test #'char-equal))
                 (when return-specs-flag
                   (push (subseq sample old-s-ptr s-ptr) specs))))
              (t
               (let ((old-s-ptr s-ptr))
                 (setq s-ptr
                       (search pattern (the string (string sample))
                               :start2 s-ptr
                               :end2 ()
                               :start1 p-ptr
                               :end1 p-next
                               :test #'char-equal))
                 (when return-specs-flag
                   (push (subseq sample old-s-ptr s-ptr) specs)))))
        (unless s-ptr
          (return nil))
        (incf s-ptr (- (or p-next
                           (length pattern))
                       p-ptr))
        (unless p-next
          (return (and (= s-ptr (length sample))
                       (or (nreverse specs)
                           t))))
        (setq p-char (aref pattern p-next))
        (setq p-ptr (1+ p-next)))))

;;; }}}
;;; ==================================================================

;;; Add the class to the list of namespace classes
(add-namespace-class +basic-type+ 'basic-namespace)

;;; basic-namespace.lisp ends here
