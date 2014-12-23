(defpackage crane.utils
  (:use :cl :anaphora :iter :annot.doc)
  (:export :diff-plist
           :plist-keys
           :make-keyword
           :find-class-slot
           :find-slot
           :get-class-slot
           :get-slot)
  (:documentation "Various utilities for use in other parts of Crane."))
(in-package :crane.utils)
(annot:enable-annot-syntax)

@doc "Calculates the difference between two plists, returning the result as a
list of ([property] [old value] [new value])"
(defun diff-plist (plist-a plist-b &key (test #'eql))
  (remove-if
    #'null
    (loop for slot in plist-a by #'cddr appending
      (if slot
          (let ((val-a (getf plist-a slot))
                (val-b (getf plist-b slot)))
            (if (funcall test val-a val-b)
                (list nil)
                (list slot (list val-a val-b))))))))

@doc "Return the keys of a plist."
(defun plist-keys (plist)
  (iter (for key in plist by #'cddr)
        (collecting key)))

@doc "Reintern a symbol into the keyword package."
(defun make-keyword (symbol)
  (intern (symbol-name symbol) :keyword))

@doc "Find a slot by name"
(defun find-class-slot (class name)
  (aif (remove-if-not #'(lambda (slot-name)
                          (eql name
                               (closer-mop:slot-definition-name slot-name)))
                      (closer-mop:class-slots class))
       (first it)))

@doc "Find a slot by name"
(defun find-slot (obj name)
  (find-class-slot (class-of obj) name))

@doc "Find a slot in a class by keyword name"
(defun get-class-slot (class name)
  (find-class-slot class (intern (symbol-name name)
                                 (symbol-package (class-name class)))))

@doc "Find slot by keyword name"
(defun get-slot (obj name)
  (get-class-slot (class-of obj) name))
