(in-package :lacrida)

; traditional rogue(6) keybindings
(defparameter *keymap*
  (alist-hash-table
   '((#\h . :move-ww) (#\j . :move-ss) (#\k . :move-nn) (#\l . :move-ee)
     (#\y . :move-nw) (#\u . :move-ne) (#\b . :move-sw) (#\n . :move-se)
     (#\. . :move-nop) (#\g . :move-get) (#\Q . :quit-game))
   :test 'eq))

(define-condition game-over (error)
  ((reason :initarg :reason :reader game-over-reason)))

(define-condition done-path (error) nil)

(deftype uint8_t () '(unsigned-byte 8))

; ??? alexandria has a conflicting define-constant but SBCL yells that
; {+ALT-SCREEN+ is an already defined constant whose value "" is not
; equal to the provided initial value "" under EQL."} if I use it...
(defmacro mdefine-constant (name value &optional doc)
  `(defconstant ,name
     (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

; many of these came from cl-minterm which is a bit too minimal, so
; instead cl-charms is being used to setup the terminal and then these
; following to "paint" into that
;
; ANSI or XTerm Control Sequences - https://invisible-island.net/xterm/
(mdefine-constant +alt-screen+   #.(format nil "~C[?1049h" #\Esc))
(mdefine-constant +clear-screen+ #.(format nil "~C[1;1H~C[2J" #\Esc #\Esc))
(mdefine-constant +clear-left+   #.(format nil "~C[1K"   #\Esc))
(mdefine-constant +clear-line+   #.(format nil "~C[2K"   #\Esc))
(mdefine-constant +clear-right+  #.(format nil "~C[K"    #\Esc))
(mdefine-constant +hide-cursor+  #.(format nil "~C[?25l" #\Esc))
(mdefine-constant +hide-pointer+ #.(format nil "~C[>2p"  #\Esc))
(mdefine-constant +show-cursor+  #.(format nil "~C[?25h" #\Esc))
(mdefine-constant +term-norm+    #.(format nil "~C[m"    #\Esc))
(mdefine-constant +unalt-screen+ #.(format nil "~C[?1049l" #\Esc))

(defmacro at (col row)
  `(format t "~C[~d;~dH" #\Esc ,row ,col))
(defmacro at-col (col)
  `(format t "~C[~dG" #\Esc ,col))

; ISO-8613-3 colors, probably needs TERM=xterm-256color or similar
(defmacro colorpair (fg bg &optional (weight 0))
  `(format t "~C[~d;38;5;~d;48;5;~dm" #\Esc ,weight ,fg ,bg))
(defmacro norm ()
  `(princ +term-norm+))

(defmacro emit-at (col row string)
  `(progn (at ,col ,row)
          (princ ,string)))
(defmacro emit-at-col (col string)
  `(progn (at-col ,col)
          (princ ,string)))
(defmacro emit-at-row (row string)
  `(progn (at 1 ,row)
          (princ +clear-right+)
          (princ ,string)))

(defmacro coinflip () `(zerop (random 2)))
(defmacro onein (n) `(zerop (random ,n)))
(defmacro ninm (n m) `(< (random ,m) ,n))
(defmacro pick (from) `(nth (random (list-length ,from)) ,from))

(defmacro forever (&body body) `(do () (nil) ,@body))

; stuff that has move costs and turn up in the event system
(defparameter *animates* nil)

; TODO put this into the hero obj...
(defparameter *hero-row* nil)
(defparameter *hero-col* nil)
; NOTE hero FOV per shadowcast includes the @ so will grant (1- n)
; visible cells
(defparameter *hero-fov* 5)

(defparameter *mons-fov* 5)
; monster move speed (10 is player move speed)
(defparameter *golem-cost* 10)

(defparameter *scrumped* nil)
; how soon and then how much thereafter to spawn a Golem at the exit
; point if the player has stolen something (lower values == harder
; game); value is in energy units so 10 per turn
(defparameter *triggered* nil)
(defparameter *alarm-trigger* 40)
(defparameter *alarm-refresh* 1000)

(defparameter *exit-row* nil)
(defparameter *exit-col* nil)

(defun relocate-hero (newcol newrow)
  (setf *hero-col* newcol)
  (setf *hero-row* newrow))

(defparameter *item-locs* (make-hash-table :test #'equal))
(defparameter *mons-locs* (make-hash-table :test #'equal))
(defparameter *loot* nil)
(defparameter *wizard* nil)

(defun wizard-name () (pick '("Deldalf" "Ganfador")))
