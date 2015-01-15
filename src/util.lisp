(defpackage crane.util
  (:use :cl :anaphora :iter)
  (:export :diff-plist
           :plist-keys
           :make-keyword
           :find-class-slot
           :find-slot
           :get-class-slot
           :get-slot)
  (:documentation "Various utilities for use in other parts of Crane."))
(in-package :crane.util)

(defun diff-plist (plist-a plist-b &key (test #'eql))
  "Calculates the difference between two plists, returning the result as a list of ([property] [old value] [new value])"
  (remove-if
    #'null
    (loop for slot in plist-a by #'cddr appending
      (if slot
          (let ((val-a (getf plist-a slot))
                (val-b (getf plist-b slot)))
            (if (funcall test val-a val-b)
                (list nil)
                (list slot (list val-a val-b))))))))

(defun plist-keys (plist)
  "Return the keys of a plist."
  (iter (for key in plist by #'cddr)
        (collecting key)))

(defun make-keyword (symbol)
  "Reintern a symbol into the keyword package."
  (intern (symbol-name symbol) :keyword))

(defun find-class-slot (class name)
  "Find a slot by name"
  (aif (remove-if-not #'(lambda (slot-name)
                          (eql name
                               (closer-mop:slot-definition-name slot-name)))
                      (closer-mop:class-slots class))
       (first it)))

(defun find-slot (obj name)
  "Find a slot by name"
  (find-class-slot (class-of obj) name))

(defun get-class-slot (class name)
  "Find a slot in a class by keyword name"
  (find-class-slot class (intern (symbol-name name)
                                 (symbol-package (class-name class)))))

(defun get-slot (obj name)
  "Find slot by keyword name"
  (get-class-slot (class-of obj) name))
