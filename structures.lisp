(in-package :lacrida)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Map Cells - for different (or generic) needs

; generic cell, no coloring
; "moveok" is a KLUGE determine whether a monster can traverse the cell,
; which differs slightly from what the player can use
(defclass cell nil
  ((ch :initarg :ch :reader cell-ch :type character)
   (moveok :initarg :moveok :reader cell-moveok :type boolean)
   (opaque :initarg :opaque :reader cell-opaque :type boolean)
   (solid :initarg :solid :reader cell-solid :type boolean))
  (:documentation "a map cell"))

(defmethod initialize-instance :after ((cc cell) &key)
  (unless (slot-boundp cc 'ch) (setf (slot-value cc 'ch) #\.))
  (unless (slot-boundp cc 'moveok) (setf (slot-value cc 'moveok) t))
  (unless (slot-boundp cc 'opaque) (setf (slot-value cc 'opaque) nil))
  (unless (slot-boundp cc 'solid) (setf (slot-value cc 'solid) nil)))


; vault cells get a specific color
(defclass vault (cell)
  ((fg :initarg :fg :type uint8_t)
   (bg :initarg :bg :type uint8_t)
   (weight :initarg :weight :type uint8_t))
  (:documentation "a map vault cell"))

(defmethod initialize-instance :after ((vv vault) &key)
  (unless (slot-boundp vv 'fg) (setf (slot-value vv 'fg) 15))
  (unless (slot-boundp vv 'bg) (setf (slot-value vv 'bg) 0))
  (unless (slot-boundp vv 'weight) (setf (slot-value vv 'weight) 0)))


; plants block LOS variably (a patient player could cheese this?)
(defclass plant (vault)
  ((los :initarg :los :type uint8_t))
  (:documentation "a plant cell"))

(defmethod initialize-instance :after ((pp plant) &key)
  (unless (slot-boundp pp 'los) (setf (slot-value pp 'los) 25)))

(defmethod cell-opaque ((pp plant)) (ninm (slot-value pp 'los) 100))


(defgeneric display (cell)
  (:documentation "cell contents as a string"))

(defmethod display ((cc cell))
  (slot-value cc 'ch))

(defmethod display ((vv vault))
  (format nil "~C[~d;38;5;~d;48;5;~dm~a~C[m" #\Esc (slot-value vv 'weight)
          (slot-value vv 'fg) (slot-value vv 'bg) (slot-value vv 'ch) #\Esc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Items
;
; TODO make these inherit from "vault" or something even more generic
; probably so can share a "get a string display of whatever" method as
; that's the same code for vaults, items, monsters

(defclass item nil
  ((ch :initarg :ch :type character)
   (fg :initarg :fg :type uint8_t)
   (bg :initarg :bg :type uint8_t)
   (weight :initarg :weight :type uint8_t))
  (:documentation "generic item"))

(defmethod initialize-instance :after ((ii item) &key)
  (setf (slot-value ii 'fg) 220)
  (setf (slot-value ii 'bg) 0)
  (setf (slot-value ii 'weight) 1))

(defmethod display ((ii item))
  (format nil "~C[~d;38;5;~d;48;5;~dm~a~C[m" #\Esc (slot-value ii 'weight)
          (slot-value ii 'fg) (slot-value ii 'bg) (slot-value ii 'ch) #\Esc))

; food player can pick up
(defclass food (item)
  ((name :reader food-name)
   (score :reader food-score :type fixnum))
  (:documentation "food item"))

(defun random-food-item ()
  (let ((roll (random 100)))
    (cond ((= roll 0) (values "Staircake" 1000))
          ((< roll 11) (values "Truffle" 100))
          (t (values "Mushroom" 10)))))

(defmethod initialize-instance :after ((ff food) &key)
  (setf (slot-value ff 'ch) #\!)
  (multiple-value-bind
      (name score)
      (random-food-item)
    (setf (slot-value ff 'name) name)
    (setf (slot-value ff 'score) score)))

(defmacro make-food (col row)
  `(let ((item (make-instance 'food)))
     (setf (gethash (cons ,col ,row) *item-locs*) item)))

(defmethod item-interact ((ff food) newcol newrow cost)
  (post-message "You see here a ~a" (slot-value ff 'name))
  (relocate-hero newcol newrow)
  (values :action-ok 10))


; messages for the player to read
; TODO as yet unused, remove if not...
(defclass msg (item)
  ((text :initarg :text :reader msg-text))
  (:documentation "messsage item"))

(defmethod initialize-instance :after ((mm msg) &key)
  (setf (slot-value mm 'ch) #\?))

(defmacro make-message (col row text)
  `(let ((msg (make-instance 'msg :text ,text)))
     (setf (gethash (cons ,col ,row) *item-locs*) msg)))

(defmethod item-interact ((mm msg) newcol newrow cost)
  (post-message "You pause to peruse the message.")
  (post-message "~a" (slot-value mm 'text))
  (values :action-fail 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The World Map - more grandly stated than implemented

; viewport concerns
(defconstant +show-rows+  18)
(defconstant +show-cols+  31)
(defconstant +row-offset+ 9)
(defconstant +col-offset+ 16)

(defconstant +map-rows+ 42)
(defconstant +map-cols+ 42)
(defconstant +mratio+ (/ pi +map-rows+))

(defconstant +item-count+ 32)
(defconstant +mons-count+ 8)

(defparameter *floor* (make-instance 'vault :fg 28))
; TODO maybe have one fungus obj and it randomizes its color on access?
(defparameter *fungus*
  (list (make-instance 'plant :ch #\P :fg 223 :opaque t)
        (make-instance 'plant :ch #\P :fg 136 :opaque t)
        (make-instance 'plant :ch #\P :fg 142 :opaque t)
        (make-instance 'plant :ch #\GREEK_CAPITAL_LETTER_PSI :fg 76 :moveok
                       nil :opaque t :los 25)))
(defparameter *gate1*
  (make-instance 'vault :ch #\GREEK_CAPITAL_LETTER_PI :moveok t :fg 251))
(defparameter *lily* (make-instance 'vault :ch #\BLACK_CLUB_SUIT :fg 255))
(defparameter *rubble* (make-instance 'vault :ch #\, :fg 28))
(defparameter *wall*
  (make-instance 'vault :ch #\# :fg 251 :moveok nil :opaque t :solid t))
(defparameter *water*
  (make-instance 'vault :ch #\~ :fg 45 :moveok nil :solid t))

(defparameter *world-map* 
  (make-array (list +map-rows+ +map-cols+)
                                :element-type 'cell
                                :initial-element *wall*))
(defparameter *seen-map*
  (make-array (list +map-rows+ +map-cols+)
                                :element-type 'boolean
                                :initial-element nil))

(defparameter *old-view* (make-array (list +show-rows+ +show-cols+)
                                :initial-element nil))

(defparameter *view-port* (make-array (list +show-rows+ +show-cols+)
                                :initial-element nil))
(mdefine-constant +hero+ #.(format nil "~C[1;37;40m@~C[m" #\Esc #\Esc))
(defmacro heroic-viewport ()
  `(setf (aref *view-port* +row-offset+ +col-offset+) +hero+))

; NOTE calls for consistency try to use x,y (col,row) but the various
; maps are always in row,col (y,x) form
(defmacro map-display (col row)
  `(display (aref *world-map* ,row ,col)))

(defmacro map-in-bounds-p (col row)
  `(array-in-bounds-p *world-map* ,row ,col))

(defmacro map-open-p (col row)
  `(not (cell-solid (aref *world-map* ,row ,col))))

(defmacro map-opaque-p (col row)
  `(cell-opaque (aref *world-map* ,row ,col)))

(defmacro map-seen (col row)
  `(setf (aref *seen-map* ,row ,col) t))

(defmacro map-seen-p (col row)
  `(aref *seen-map* ,row ,col))

; for things seen but not directly visible of which we take a dim view
(defmacro map-view-display (col row)
  `(when
       (and (array-in-bounds-p *world-map* ,row ,col)
            (aref *seen-map* ,row ,col))
     (format nil "~C[38;5;236m~a~C[m" #\Esc
             (cell-ch (aref *world-map* ,row ,col)) #\Esc)))

; Chebyshev distance
(defun distance (x0 y0 x1 y1) (max (abs (- x1 x0)) (abs (- y1 y0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Vaults

(defparameter *vfloor* (make-instance 'vault :fg 28 :moveok nil))
(defparameter *celing*
  (make-instance 'vault :ch #\- :fg 250 :bg 0 :weight 1 :moveok nil :opaque t :solid t))
(defparameter *slashf*
  (make-instance 'vault :ch #\/ :fg 250 :bg 0 :weight 1 :moveok nil :opaque nil :solid t))
(defparameter *slashb*
  (make-instance 'vault :ch #\\ :fg 250 :bg 0 :weight 1 :moveok nil :opaque nil :solid t))
(defparameter *vert*
  (make-instance 'vault :ch #\| :fg 250 :bg 0 :weight 1 :moveok nil :opaque nil :solid t))
(defparameter *hollow*
  (make-instance 'vault :ch #\_ :fg 0 :bg 0 :moveok nil :opaque nil :solid nil))

(defparameter *dolmen*
  (make-array '(4 9) :initial-contents
              (list
               (list *vfloor* *vfloor* *vfloor* *vfloor* *vfloor* *vfloor* *vfloor*
                     *vfloor* *vfloor*)
               (list *vfloor* *vfloor* *celing* *celing* *celing* *celing*
                     *celing* *vfloor* *vfloor*)
               (list *vfloor* *vfloor* *slashf* *vert* *hollow* *vert* *slashb*
                     *vfloor* *vfloor*)
               (list *vfloor* *vfloor* *vfloor* *vfloor* *vfloor* *vfloor* *vfloor*
                     *vfloor* *vfloor*))))

(defparameter *pond*
  (make-array '(6 8) :initial-contents
              (list
               (list *vfloor* *vfloor* *vfloor* *vfloor* *vfloor* *vfloor*
                     *vfloor* *vfloor*)
               (list *vfloor* *vfloor* *water* *water* *water* *water*
                     *water* *vfloor*)
               (list *vfloor* *water* *water* *lily* *water* *water* *vfloor*
                     *vfloor*)
               (list *vfloor* *vfloor* *water* *water* *water* *vfloor*
                     *vfloor* *vfloor*)
               (list *vfloor* *water* *vfloor* *water* *water* *vfloor*
                     *water* *vfloor*)
               (list *vfloor* *vfloor* *vfloor* *vfloor* *vfloor* *vfloor*
                     *vfloor* *vfloor*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Map Agents - used to carve out wandering corridors, more or less
; borrowed from the ministry-of-silly-vaults code

(defparameter *total-moves* 0)
; percent of map that can be filled by agents walking out corridors
(defconstant +max-agent-moves+ (truncate (* 0.3 +map-rows+ +map-cols+)))

(defparameter *agent-seen*
  (make-array (list +map-rows+ +map-cols+) :element-type 'boolean
              :initial-element nil))

(defmacro run-amok (start update)
  (let ((label (gensym)) (queue (gensym)))
    `(prog ((,queue ,start))
      ,label
       (and (null ,queue) (return (< *total-moves* +max-agent-moves+)))
       (setq ,queue (mapcan ,update ,queue))
       (go ,label))))

(defstruct agent
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (hx 0 :type fixnum)
  (hy 1 :type fixnum)
  (moves 0 :type fixnum))

(defmacro agent-tee (agent)
  `(progn
    (rotatef (agent-hx ,agent) (agent-hy ,agent))
    (make-agent :x (agent-x ,agent)
                :y (agent-y ,agent)
                :hx (* (agent-hx ,agent) -1)
                :hy (* (agent-hy ,agent) -1))))

(defmacro move-agent (agent)
  `(progn
    (incf (agent-x ,agent) (agent-hx ,agent))
    (incf (agent-y ,agent) (agent-hy ,agent))))

(defun new-agents-at (x y)
  (mapcar (lambda (hx hy) (make-agent :x x :y y :hx hx :hy hy))
           '(-1 1 0 0) '(0 0 1 -1)))

(defmacro random-agent-turn (agent)
  `(progn
    (rotatef (agent-hx ,agent) (agent-hy ,agent))
    (when (coinflip)
      (setf (agent-hx ,agent) (* (agent-hx ,agent) -1))
      (setf (agent-hy ,agent) (* (agent-hy ,agent) -1)))))

(defmacro sidestep-agent (agent)
  `(progn
     (incf (agent-x ,agent) (agent-hy ,agent))
     (incf (agent-y ,agent) (agent-hx ,agent))
     (incf (agent-x ,agent) (* (agent-hx ,agent) -1))
     (incf (agent-y ,agent) (* (agent-hy ,agent) -1))))

(defun valid-agent (agent)
  (if (map-in-bounds-p (agent-y agent) (agent-x agent))
      (prog1 (not (aref *agent-seen* (agent-y agent) (agent-x agent)))
        (setf (aref *agent-seen* (agent-y agent) (agent-x agent)) t))
      nil))

(defun update-agent (agent)
  (move-agent agent)
  (when (and (< *total-moves* +max-agent-moves+) (valid-agent agent))
    (setf (aref *world-map* (agent-y agent) (agent-x agent)) *floor*)
    (incf (agent-moves agent))
    (incf *total-moves*)
    (let ((agents) (moves (agent-moves agent)))
      (if (> moves 3)
          (progn
           (if (onein 6)
               (progn
                (setf (agent-moves agent) 0)
                (push agent agents)
                (if (coinflip)
                    (random-agent-turn agent)
                    (push (agent-tee agent) agents)))
               (if (onein 12)
                   (setf agents
                           (nconc agents
                                  (new-agents-at (agent-x agent)
                                   (agent-y agent))))))
           (when (onein 3)
             (push agent agents)
             (sidestep-agent agent)))
          (push agent agents))
      agents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Animates

; TODO this needs a more generic form without row/col for invisible tasks
(defclass animate nil
  ((cost :initarg :cost :accessor animate-cost :type uint8_t)
   (update :initarg :update :reader animate-update :type function)
   (row :initarg :row :accessor animate-row :type uint8_t)
   (col :initarg :col :accessor animate-col :type uint8_t))
  (:documentation "an animate"))

(defmethod initialize-instance :after ((ani animate) &key)
  (unless (slot-boundp ani 'cost) (setf (slot-value ani 'cost) 0))
  (unless (slot-boundp ani 'update) (error "no update function provided")))

(defmethod display ((aa animate))
  (format nil "~C[1;38;5;~a;48;5;0mG~C[m" #\Esc
          (if *scrumped* 208 244)
          #\Esc))

(defmacro cast-display (x y)
  `(let ((loc (cons ,x ,y)))
     (if-let (mons (gethash loc *mons-locs*))
       (display mons)
       (if-let (item (gethash loc *item-locs*))
         (display item)
         (map-display ,x ,y)))))

(defun redraw-cell (col row is-veggie)
  (when (< (distance *hero-col* *hero-row* col row) *hero-fov*)
    (let ((offx (- col *hero-col*)) (offy (- row *hero-row*)))
      (unless (and (zerop offx) (zerop offy) is-veggie)
        (setf (aref *view-port* (+ offy +row-offset+) (+ offx +col-offset+))
                nil)
        (emit-at (+ offx +col-offset+) (+ offy +row-offset+)
         (cast-display col row))
        (finish-output)))))

; TODO really need a FOV object can say "hey something happened at cell
; x" then let that figure out if display or what
(defmethod animate-move ((aa animate) newcol newrow)
  (let ((col (slot-value aa 'col)) (row (slot-value aa 'row)))
    (remhash (cons col row) *mons-locs*)
    (redraw-cell col row nil))
  (setf (slot-value aa 'row) newrow)
  (setf (slot-value aa 'col) newcol)
  (setf (gethash (cons newcol newrow) *mons-locs*) aa)
  (redraw-cell newcol newrow nil))

(defparameter *monstmsg*
  '("A monstrous figure in the brutalist style."
    "The clay figure does not respond to your touch."
    "The clay figure is cold and solid."))
    "Who would even pay for a statue like this?"

; this may be too subtle a prompt?
(defmacro prompt (&rest args)
  `(block nil
     (unwind-protect
         (progn
          (post-message ,@args)
          (at +col-offset+ +row-offset+)
          (princ +show-cursor+)
          (finish-output)
          (forever
           (let ((key
                  (charms:get-char charms:*standard-window* :ignore-error t)))
             (case key
               (#\Y (return t))
               (#\N (return nil))
               (#\n (return nil))))))
       (princ +hide-cursor+))))

; ??? defconstant ... 10 blows up in SBCL with "The value ... is not of
; type FIXNUM when binding ..."
(defmacro move-player (&key (row 0) (col 0) (cost 10))
  (declare (fixnum row col cost))
  `(block move-player
     (let ((newrow (+ *hero-row* ,row)) (newcol (+ *hero-col* ,col)))
       (if (map-in-bounds-p newcol newrow)
         (let ((loc (cons newcol newrow)))
           (when-let (mons (gethash loc *mons-locs*))
             (if *scrumped*
               (progn
                 (post-message "You blunder into the Golem... (press any key)")
                 (finish-output)
                 (charms:get-char charms:*standard-window* :ignore-error t)
                 (error 'game-over :reason :killed))
               (progn
                 (post-message (pick *monstmsg*))
                 (return-from move-player (values :action-okay 10))))
             (error 'game-over :reason :killed))
           (when-let (item (gethash loc *item-locs*))
            (return-from move-player (item-interact item newcol newrow ,cost)))
           (when (map-open-p newcol newrow)
             (relocate-hero newcol newrow)
             (return-from move-player (values :action-ok ,cost))))
         (if (prompt "Leave the fair realm of ~a? [Y/n] " *wizard*)
           (error 'game-over :reason :escaped)
           (post-message "You decide against it."))))
     (values :action-fail 0)))

(defparameter *found*
  '("Found" "Obtained" "Came across" "Happend upon" "Acquired"))

(defmacro get-item ()
  `(let ((loc (cons *hero-col* *hero-row*)))
     (if-let (item (gethash loc *item-locs*))
       (progn
         (post-message "~a a ~a!" (pick *found*) (food-name item))
         (push item *loot*)
         (remhash loc *item-locs*)
         (setf *scrumped* t))
       (post-message "You grope around foolishly on the ground."))
     (values :action-ok 10)))

(defmacro action-for (symb)
  `(case ,symb
     (:move-nn (move-player :row -1 :col  0))
     (:move-ss (move-player :row  1 :col  0))
     (:move-ee (move-player :row  0 :col  1))
     (:move-ww (move-player :row  0 :col -1))
     (:move-nw (move-player :row -1 :col -1))
     (:move-ne (move-player :row -1 :col  1))
     (:move-sw (move-player :row  1 :col -1))
     (:move-se (move-player :row  1 :col  1))
     (:move-get (get-item))
     (:move-nop (values :action-ok 10))
     (:quit-game (error 'game-over :reason :quit-game))
     (otherwise (values :action-fail 0))))

(defmacro symbol-for (key) `(gethash ,key *keymap*))

(defun update-hero (&optional ani)
  (forever
    ; ??? screen blanks if terminal resized
    (let ((key (charms:get-char charms:*standard-window* :ignore-error t)))
      (multiple-value-bind
          (status cost)
          (action-for (symbol-for key))
        (finish-output)
        (when (eq :action-ok status)
          (shadowcast *hero-col* *hero-row* *hero-fov*)
          (finish-output)
          (return-from update-hero cost))))))

(defun make-hero () (make-instance 'animate :cost 0 :update #'update-hero))

; Bresenham line, but only for animate walks
(defun bline (ani linef x0 y0 x1 y1)
  (declare (fixnum x0 y0 x1 y1))
  (let* ((dx (abs (- x1 x0)))
         (sx (if (< x0 x1) 1 -1))
         (dy (abs (- y1 y0)))
         (sy (if (< y0 y1) 1 -1))
         (err (/ (if (> dx dy) dx (- dy)) 2))
         (first t))
    (declare (fixnum sx sy))
    (forever
      (unless first (funcall linef ani x0 y0))
      (setf first nil)
      (when (and (eq x0 x1) (eq y0 y1)) (return))
      (let ((e2 err))
        (when (> e2 (- dx)) (incf err (- dy)) (incf x0 sx))
        (when (< e2 dy) (incf err dx) (incf y0 sy))))))

(defun hunt-player (ani x y)
  (let ((cell (aref *world-map* y x)))
    (when (cell-moveok cell)
      (when (gethash (cons x y) *mons-locs*) (error 'done-path))
      (when (eq (cell-ch cell) #\P)
        ; TODO need message if this was nearby, or visible
        (setf (aref *world-map* y x) *rubble*)
        (redraw-cell x y t)
        (error 'done-path))
      (animate-move ani x y)
      (when (and (= x *hero-col*) (= y *hero-row*))
        (post-message "Ouch! That really hurt... (press any key)")
        (finish-output)
        (charms:get-char charms:*standard-window* :ignore-error t) ; DBG
        (error 'game-over :reason :killed))))
  (error 'done-path))

; ideally want a graph of the whole level but that will take time to
; implement, and besides Golem aren't nearly smart enough for that.
; maybe the Wizard needs more pets?
;
; TODO line function is really bad at pathing to player along certain
; diagonals, fix or instead follow a graph
(defun update-golem (ani)
  (let* ((col (animate-col ani))
         (row (animate-row ani))
         (d (distance col row *hero-col* *hero-row*)))
    (if (and *scrumped* (<= d *mons-fov*))
      (progn
        (handler-case
          (bline ani #'hunt-player col row *hero-col* *hero-row*)
          (done-path (msg)))
        *golem-cost*)
      ; sleep until player might be close enough to consider moving
      (* (1+ d) 10))))

(defmacro make-golem (col row)
  `(let ((mons
          (make-instance 'animate :row ,row :col ,col :cost (1+ (random 8))
                         :update #'update-golem)))
     (setf (gethash (cons ,col ,row) *mons-locs*) mons)
     mons))

(defun update-alarm (ani)
  (let ((d (distance *hero-col* *hero-row* *exit-col* *exit-row*)))
    (when
        (and *scrumped* (plusp d)
             (not (gethash (cons *exit-col* *exit-row*) *mons-locs*)))
      (setf *triggered* t)
      (push (make-golem *exit-col* *exit-row*) *animates*)
      (when (< d *hero-fov*) (redraw-cell *exit-col* *exit-row* nil))))
  (if *triggered* *alarm-refresh* *alarm-trigger*))
