#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(ql:quickload :cl-minterm)

(defpackage :lacrida (:use :alexandria :cl :cl-minterm))
(in-package :lacrida)

; traditional rogue(6) keybindings
(defparameter *keymap*
  (alist-hash-table
   '((#\h . :move-ww) (#\j . :move-ss) (#\k . :move-nn) (#\l . :move-ee)
     (#\y . :move-nw) (#\u . :move-ne) (#\b . :move-sw) (#\n . :move-se)
     (#\q . :quit-game))
   :test 'eq))

; viewport concerns
(defconstant  +show-rows+  17)
(defconstant  +show-cols+  31)
(defconstant  +row-offset+ 9)
(defconstant  +col-offset+ 16)

(defparameter *world-map* nil)
(defconstant  +map-rows+  32)
(defconstant  +map-cols+  32)
(defconstant  +map-zzzs+  3)
(defparameter *hero-row*  nil)
(defparameter *hero-col*  nil)
(defparameter *hero-zzz*  0)

(defparameter *msg-buf* nil)
(defparameter +msg-buf-max+ 24)

(define-condition game-over (error)
  ((reason :initarg :reason :reader game-over-reason)))

(defmacro coinflip () `(zerop (random 2)))

(defmacro post-message (&rest args)
  `(progn
    (push (format nil ,@args) *msg-buf*)
    (when (> (list-length *msg-buf*) +msg-buf-max+)
      (rplacd (nthcdr (1- +msg-buf-max+) *msg-buf*) nil))
    (show-messages)))

(defmacro randomize ()
  `(progn (setq *random-state* (make-random-state t)) (values)))

(defparameter *animates* nil)

; or instead use a LMC style setup? but then how will animates have
; a ref to that?
(defstruct cell (type nil) (things nil))
(defparameter *wall* (make-cell :type :cell-wall))
(defparameter *floor* (make-cell :type :cell-floor))

(defstruct mineral genus species display name interact)
(defstruct (item (:include mineral)) stash)
(defstruct
    (animate
     (:print-function
      (lambda (ani stream depth)
        (declare (ignore depth))
        (format stream "#<ANI ~a (~a) cost ~a dead? ~a>" (animate-name ani)
                (animate-display ani) (animate-cost ani)
                (animate-dead ani))))
     (:include mineral))
  (cost 0 :type fixnum)
  (dead nil :type boolean)
  stash
  update)

(defstruct
    (point
     (:print-function
      (lambda (p stream depth)
        (declare (ignore depth))
        (format stream "#<point ~d,~d>" (point-x p) (point-y p)))))
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(defmacro move-player (&key (row 0) (col 0) (cost 0))
  (declare (fixnum row col cost))
  `(let ((newrow (+ *hero-row* ,row)) (newcol (+ *hero-col* ,col)))
     (post-message "at ~d ~d" newcol newrow)
     ; xxx no move if square is wall, fall if square is shaft otherwise ok
     (if (array-in-bounds-p *world-map* 1 newrow newcol)
       (progn
         ; xxx update hero in cell stack for this point (and remove from
         ; the old one) xxx implement that cell stack thingy
         (setf *hero-row* newrow)
         (setf *hero-col* newcol)
         (list :action-ok ,cost))
       (list :action-fail 0))))

(defmacro action-for (symb)
  `(case ,symb
     (:move-nn (move-player :row -1 :col  0 :cost 1000))
     (:move-ss (move-player :row  1 :col  0 :cost 1000))
     (:move-ee (move-player :row  0 :col  1 :cost 1000))
     (:move-ww (move-player :row  0 :col -1 :cost 1000))
     (:move-nw (move-player :row -1 :col -1 :cost 1414))
     (:move-ne (move-player :row -1 :col  1 :cost 1414))
     (:move-sw (move-player :row  1 :col -1 :cost 1414))
     (:move-se (move-player :row  1 :col  1 :cost 1414))
     (:quit-game (error 'game-over :reason :quit-game))
     (otherwise (list :action-fail 0))))

(defmacro symbol-for (key) `(gethash ,key *keymap*))

; slope math for the color fade is tuned for 6 or 24 steps as the
; messages either appear below the level map or on the entire terminal
(defun show-messages (&optional (start-row 19))
  (let* ((count (- +msg-buf-max+ start-row))
         (step (truncate (+ (* -1.26 count) 36))))
    (loop for r from start-row to +msg-buf-max+
          for v from 255 downto 75 by step
          for msg in *msg-buf*
          do (grey v) (emit-at-row r msg))))

(defun generate-world ()
  (setf *world-map* (make-array (list +map-zzzs+ +map-rows+ +map-cols+)
                                :element-type 'cell))
  ; put a floor down
  (let ((b (make-array
             (array-total-size *world-map*)
             :displaced-to *world-map*
             :element-type (array-element-type *world-map*))))
    (dotimes (n (array-total-size *world-map*))
      (setf (aref b n) *floor*)))
  ; world border dbg
  (loop for r from 0 to (1- +map-rows+) do
        (setf (aref *world-map* 0 r 0) *wall*)
        (setf (aref *world-map* 0 r (1- +map-cols+)) *wall*))
  (loop for c from 1 to (- +map-cols+ 2) do
        (setf (aref *world-map* 0 0 c) *wall*)
        (setf (aref *world-map* 0 (1- +map-rows+) c) *wall*))
  (setf *hero-row* 4)
  (setf *hero-col* 4))

(defun draw-bg-dbg ()
  (grey 75) (dotimes (r 24) (at 1 (1+ r)) (dotimes (c 80) (princ #\x))))

(defun draw-map ()
  (let ((srow (- *hero-row* +row-offset+)) (scol (- *hero-col* +col-offset+)))
    (post-message "start offset ~a ~a" scol srow)
    (color 250 250 5)
    (dotimes (r +show-rows+)
      (at 1 (1+ r))
      (dotimes (c +show-cols+)
        (let ((row (+ srow r)) (col (+ scol c)))
          (if (array-in-bounds-p *world-map* 1 row col)
              (let ((cell (aref *world-map* *hero-zzz* row col)))
                (cond ((eq (cell-type cell) :cell-floor) (princ #\.))
                      ((eq (cell-type cell) :cell-wall) (princ #\#))
                      (t (princ #\ ))))
              (princ #\ )))))))

(defun least-cost (animates)
  (loop :for ani :in animates
        :minimize (animate-cost ani) :into min
        :finally (return min)))

(defun update-hero ()
  (loop do (let ((key (getch)))
             (destructuring-bind
                 (status cost)
                 (action-for (symbol-for key))
               (when (eq :action-ok status)
                 (return-from update-hero cost))))))

(defun game-loop ()
  (randomize)
  (generate-world)
  (post-message "at ~d ~d" *hero-col* *hero-row*)
  (draw-bg-dbg)
  (draw-map)
  (push (make-animate :name "hero" :cost 0 :update #'update-hero) *animates*)
  (finish-output)
  (loop do (let ((min-cost (least-cost *animates*)))
             (dolist (ani *animates*)
               (let ((new-cost (- (animate-cost ani) min-cost)))
                 (setf (animate-cost ani)
                         (if (<= new-cost 0)
                             (funcall (animate-update ani))
                             new-cost))
                 (post-message "move cost ~d" (animate-cost ani))))
             (delete-if (lambda (ani) (animate-dead ani)) *animates*)
             (draw-map)
             (finish-output))))

(defun start-game ()
  (handler-case
   (unwind-protect
       (with-rawterm 0 0 (setup-screen +hide-cursor+ +hide-pointer+)
        (game-loop))
     (restore-screen +clear-right+))
   (game-over (msg)
    (let ((reason (game-over-reason msg)))
      (case reason
        (:quit-game (princ "Be seeing you..."))
        (otherwise (princ "Well, I guess it's over."))))))
  #+DARWIN
  (princ #\Return))

(start-game)
