(in-package :lacrida)

(defclass visible nil
  ((ch :initarg :ch :initform #\. :accessor display-ch :type character)
   (fg :initarg :fg :initform 15 :accessor display-fg :type uint8_t)
   (bg :initarg :bg :initform 0 :accessor display-bg :type uint8_t)
   (weight :initarg :weight :initform 0 :accessor display-weight :type uint8_t))
  (:documentation "something that can be visible on the map"))

(defmethod display ((vv visible))
  (format nil "~C[~d;38;5;~d;48;5;~dm~a~C[m" #\Esc (slot-value vv 'weight)
          (slot-value vv 'fg) (slot-value vv 'bg) (slot-value vv 'ch) #\Esc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Map Cells

; "moveok" is to determine whether a monster can traverse the cell;
; this differs from the player
(defclass cell (visible)
  ((moveok :initarg :moveok :initform t :reader cell-moveok :type boolean)
   (opaque :initarg :opaque :initform nil :reader cell-opaque :type boolean)
   (solid :initarg :solid :initform nil :reader cell-solid :type boolean))
  (:documentation "a map cell"))

; plants block LOS variably (a patient player could cheese this,
; assuming nothing is chasing after them)
(defclass plant (cell)
  ((los :initarg :los :initform 25 :type uint8_t))
  (:documentation "a plant cell"))

(defmethod cell-opaque ((pp plant)) (ninm (slot-value pp 'los) 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Food - the player can pick this up

(defclass food (visible)
  ((name :reader food-name)
   (score :reader food-score :type fixnum))
  (:documentation "food item"))

; TODO these probably should be individual classes if they had more
; unique properties about them?
(defun random-food-item ()
  (let ((roll (random 100)))
    (cond ((= roll 0) (values "Staircake" 1000))
          ((< roll 11) (values "Truffle" 100))
          (t (values "Mushroom" 10)))))

(defmethod initialize-instance :after ((ff food) &key)
  (multiple-value-bind
      (name score)
      (random-food-item)
    (setf (slot-value ff 'name) name)
    (setf (slot-value ff 'score) score)
    (when (= score 1000)
      (setf (display-ch ff) #\>)
      (setf (display-fg ff) 254)))) 

(defmacro make-food (col row)
  `(let ((item (make-instance 'food :ch #\! :fg 220 :weight 1)))
     (incf *max-score* (food-score item))
     (setf (gethash (cons ,col ,row) *item-locs*) item)))

(defmethod item-interact ((ff food) newcol newrow cost)
  (post-message "You see here a ~a" (slot-value ff 'name))
  (relocate-hero newcol newrow)
  (values :action-ok 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The World Map - more grandly stated than implemented

(defparameter *floor* (make-instance 'cell :fg 28))
; TODO maybe have one fungus obj and it randomizes its color on access?
(defparameter *fungus*
  (list (make-instance 'plant :ch #\P :fg 223 :opaque t)
        (make-instance 'plant :ch #\P :fg 136 :opaque t)
        (make-instance 'plant :ch #\P :fg 142 :opaque t)
        (make-instance 'plant :ch #\GREEK_CAPITAL_LETTER_PSI :fg 76 :moveok
                       nil :opaque t :los 25)))
(defparameter *gate1*
  (make-instance 'cell :ch #\GREEK_CAPITAL_LETTER_PI :moveok t :fg 251))
(defparameter *lily* (make-instance 'cell :ch (code-char 9827) :fg 255))
(defparameter *rubble* (make-instance 'cell :ch #\, :fg 28))
(defparameter *wall*
  (make-instance 'cell :ch #\# :fg 251 :moveok nil :opaque t :solid t))
(defparameter *water*
  (make-instance 'cell :ch #\~ :fg 45 :moveok nil :solid t))

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
             (display-ch (aref *world-map* ,row ,col)) #\Esc)))

; Chebyshev distance
(defun distance (x0 y0 x1 y1) (max (abs (- x1 x0)) (abs (- y1 y0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Vaults - premade cell sets that can be placed into the map

(defparameter *vfloor* (make-instance 'cell :fg 28 :moveok nil))
(defparameter *celing*
  (make-instance 'cell :ch #\- :fg 250 :bg 0 :weight 1 :moveok nil :opaque t :solid t))
(defparameter *slashf*
  (make-instance 'cell :ch #\/ :fg 250 :bg 0 :weight 1 :moveok nil :opaque nil :solid t))
(defparameter *slashb*
  (make-instance 'cell :ch #\\ :fg 250 :bg 0 :weight 1 :moveok nil :opaque nil :solid t))
(defparameter *vert*
  (make-instance 'cell :ch #\| :fg 250 :bg 0 :weight 1 :moveok nil :opaque nil :solid t))
(defparameter *hollow*
  (make-instance 'cell :ch #\_ :fg 0 :bg 0 :moveok nil :opaque nil :solid nil))

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
; borrowed from the ministry-of-silly-vaults corridors.lisp code

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
               (when (onein 12)
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
; Entities and Animates

(defclass entity ()
  ((cost :initarg :cost :initform 0 :accessor animate-cost :type uint16_t)
   (update :initarg :update :reader animate-update :type function))
  (:documentation "something that uses the energy system"))

(defmethod initialize-instance :after ((ee entity) &key)
  (unless (slot-boundp ee 'update) (error "no update function provided")))

(defclass animate (entity)
  ((row :initarg :row :accessor animate-row :type uint8_t)
   (col :initarg :col :accessor animate-col :type uint8_t))
  (:documentation "a visible entity"))

; with multiple animate types this would inherit from VISIBLE instead of
; begin golem-coded here
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Some Hero Stuff

; this may be too subtle a prompt?
(defmacro prompt (&rest args)
  `(block nil
     (unwind-protect
         (progn
          (post-message ,@args)
          (at +col-offset+ +row-offset+)
          (princ +show-cursor+)
          (finish-output)
          (loop
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
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (loop
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
    (loop
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
      (when (eq (display-ch cell) #\P)
        (setf (aref *world-map* y x) *rubble*)
        (redraw-cell x y t)
        ; monster always redrawn as otherwise would (maybe) need to redo
        ; FOV or line walk to see if player can now see the monster,
        ; simpler to just redraw it on the assumption the player somehow
        ; saw the move
        (redraw-cell (animate-col ani) (animate-row ani) t)
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
; diagonals, fix or instead follow a graph (but monsters getting stuck
; is mostly how the player can stay alive...)
(defun update-golem (ani)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
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
      (max *golem-cost* (* d 10)))))

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
