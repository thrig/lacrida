(in-package :lacrida)

(defparameter *msg-buf* nil)
(defparameter +msg-buf-max+ 6)

(defmacro post-message (&rest args)
  `(progn
    (push (format nil ,@args) *msg-buf*)
    (when (> (list-length *msg-buf*) +msg-buf-max+)
      (rplacd (nthcdr (1- +msg-buf-max+) *msg-buf*) nil))
    (show-messages)))

(defun show-messages ()
  (loop :for msg :in *msg-buf*
        :for r :from (+ 18 (length *msg-buf*)) :downto 0
        :for v :from 254 :downto 0 :by 4
        :do (colorpair v 0) (emit-at-row r msg) (norm)))
