(defpackage :crane.utils
  (:use :cl :anaphora :iter :cl-annot.doc))
(in-package :crane.utils)
(annot:enable-annot-syntax)

@export
(defun diff-plist (plist-a plist-b &key (test #'eql))
  "Calculates the difference between two plists, returning
the result as a list of ([property] [old value] [new value])"
  (remove-if-not
    #'consp
    (loop for slot in plist-a by #'cddr collecting
      (if slot
          (let ((val-a (getf plist-a slot))
                (val-b (getf plist-b slot)))
            (if (funcall test val-a val-b)
                t
                (list slot val-a val-b)))))))

@export
(defun slurp-file (path)
  ;; Credit: http://www.ymeme.com/slurping-a-file-common-lisp-83.html
  (with-open-file (stream path)
    (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq stream))
      seq)))

@export
(defun debugp ()
  (getf (envy:config *package*) :debug))

@export
(defun get-configuration ()
  (aif (getf (envy:config *package*) :crane)
       it
       (error 'crane.errors:no-configuration-error)))

@export
(defun get-config-value (key)
  (aif (getf (get-configuration) key)
       it
       (error 'crane.errors:configuration-error
              :key key
              :text "This key is not configured."
              key)))

@export
(defun plist-keys (plist)
  (iter (for key in plist by #'cddr)
        (collecting key)))
