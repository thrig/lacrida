(in-package :lacrida)

; shadowcast accessory function
(defun castlight (startx starty radius row light-start light-end xx xy yx yy)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((blocked nil) (new-start 0.0))
    (loop :for j :from row :to radius :do
      (loop :with dy = (- j) :for dx :from dy :to 0 :do (block DIST-LOOP
        (let ((rslope (/ (+ dx 0.5) (- dy 0.5)))
              (lslope (/ (- dx 0.5) (+ dy 0.5))))
          (cond ((< light-start rslope) (return-from DIST-LOOP))
                ((> light-end lslope) (loop-finish)))
          (let* ((offx (+ (* dx xx) (* dy xy)))
                 (offy (+ (* dx yx) (* dy yy)))
                 (curx (+ startx offx))
                 (cury (+ starty offy)))
            (unless (map-in-bounds-p curx cury) (return-from DIST-LOOP))
            (when (< (max (abs dx) (abs dy)) radius)
              ; TODO does this hit any duplicate points? relevant?
              (map-seen curx cury)
              (setf (aref *view-port* (+ +row-offset+ offy)
                          (+ +col-offset+ offx))
                    (cast-display curx cury)))
            (when blocked
              (if (map-opaque-p curx cury)
                (progn
                  (setf new-start rslope)
                  (return-from DIST-LOOP))
                (progn
                  (setf blocked nil)
                  (setf light-start new-start)))
              (when (and (map-opaque-p curx cury) (< j radius))
                (setf blocked t)
                (unless (< light-start lslope)
                  (castlight startx starty radius (+ j 1)
                             light-start lslope xx xy yx yy))
                (setf new-start rslope)))))))
      (when blocked (loop-finish)))))

; what's in view, or previously seen but not visible (ground only),
; or blank
(defmacro visible-cell (offx offy row col)
  `(or (aref *view-port* ,row ,col)
       (map-view-display (+ ,offx ,col) (+ ,offy ,row))
       #\Space))

(defun shadowcast (startx starty radius)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (setf *old-view* (copy-array *view-port*))
  (setf *view-port*
          (make-array (list +show-rows+ +show-cols+) :initial-element nil))
  (heroic-viewport)
  (map-seen startx starty)
  (dolist
      (mult
       '((1 0 0 1) (0 1 1 0) (0 -1 1 0) (-1 0 0 1) (-1 0 0 -1) (0 -1 -1 0)
         (0 1 -1 0) (1 0 0 -1)))
    (apply #'castlight startx starty radius 1 1.0 0.0 mult))
  (let ((offx (- startx +col-offset+)) (offy (- starty +row-offset+)))
    (dotimes (r +show-rows+)
      (dotimes (c +show-cols+)
        (let ((show (visible-cell offx offy r c)))
          (unless (equal show (aref *old-view* r c))
            (emit-at c r show)))))))

; this, alas, needs to nuke any items/monsters now within the vault
; (Brogue by contrast does items last, after the level map is already
; built with rooms and so forth)
(defun place-vault (vault col row)
  (destructuring-bind (vrows vcols) (array-dimensions vault)
    (dotimes (r vrows)
      (dotimes (c vcols)
        (setf (aref *world-map* (+ row r) (+ col c))
              (aref vault r c))
        (let ((loc (cons c r)))
          (remhash loc *item-locs*)
          (remhash loc *mons-locs*))))))

; the random here adds some noise to the larger-scale trig outputs
(defmacro random-ground (r c off1 off2 mul1 mul2)
  `(let ((val
          (+ (cos (* (- ,r ,off1) +mratio+ (1+ (random 2))))
             (sin (* (- ,c ,off2) +mratio+ (1+ (random 3))))
             (cos (* (- ,r ,off2) +mratio+ ,mul1))
             (sin (* (- ,c ,off1) +mratio+ ,mul2)))))
     (cond ((> val 0.5) (pick *fungus*)) ((> val -0.9) *floor*) (t *water*))))

; fugnus/floor/water placement are randomized by sin functions scaled to
; the level map; this (if done right) makes clumps of similar cells
(defun ground-by-trig ()
  (let ((off1 (random +map-rows+))
        (off2 (random +map-rows+))
        (mul1 (+ 4 (random 7)))
        (mul2 (+ 4 (random 7))))
    (dotimes (r +map-rows+)
      (dotimes (c +map-cols+)
        (setf (aref *world-map* r c)
              (random-ground r c off1 off2 mul1 mul2))))))

; this also causes corridors to be drawn to make all items (and
; monsters!) reachable even if there is water around the item. maybe.
(defun monsters-and-items ()
  (let ((items +item-count+)
        (monst +mons-count+)
        (total (* (- +map-rows+ 2) (- +map-cols+ 2))))
    (loop :for r :from 1 :below (1- +map-rows+) :do
      (loop :for c :from 1 :below (1- +map-cols+) :do
        (when (< (random total) items)
          (make-food c r)
          (unless (aref *agent-seen* r c)
            (setf (aref *world-map* r c) *floor*)
            (run-amok (new-agents-at c r) #'update-agent))
          (decf items))
        (when (< (random total) monst)
          (push (make-golem c r) *animates*)
          (unless (aref *agent-seen* r c)
            (setf (aref *world-map* r c) *floor*)
            (run-amok (new-agents-at c r) #'update-agent))
          (decf monst))
        (decf total)))))

(defun border-wall ()
  (loop :for r :from 1 :below (1- +map-rows+) :do
    (setf (aref *world-map* r 0) *wall*)
    (setf (aref *world-map* r 1) *floor*)
    (setf (aref *world-map* r (- +map-cols+ 2)) *floor*)
    (setf (aref *world-map* r (1- +map-cols+)) *wall*))
  (loop :for c :from 1 :below (1- +map-cols+) :do
    (setf (aref *world-map* 0 c) *wall*)
    (setf (aref *world-map* 1 c) *floor*)
    (setf (aref *world-map* (- +map-rows+ 2) c) *floor*)
    (setf (aref *world-map* (1- +map-rows+) c) *wall*))
  (setf (aref *world-map* 0 0) *wall*)
  (setf (aref *world-map* 0 (1- +map-cols+)) *wall*)
  (setf (aref *world-map* (1- +map-rows+) 0) *wall*)
  (setf (aref *world-map* (1- +map-rows+) (1- +map-cols+)) *wall*))

; potential entry (and exit...) point
(defun random-edge-point ()
  (if (coinflip)
    (values (if (coinflip) 0 (1- +map-cols+)) (+ 2 (random (- +map-rows+ 4))))
    (values (+ 2 (random (- +map-cols+ 4))) (if (coinflip) 0 (1- +map-rows+)))))

; no monsters too close to the entry point at the start
; NOTE will run increasingly forever if there are too many monsters
(defun min-monster-dist (col row)
  (loop :for k :being :the :hash-key :in *mons-locs*
        :minimizing (distance col row (car k) (cdr k))))

(defun entry-gate ()
  (loop
    (multiple-value-bind (col row) (random-edge-point)
      (when (> (min-monster-dist col row) *hero-fov*)
        (setf (aref *world-map* row col) *gate1*)
        (push (make-hero) *animates*)
        (setf *hero-col* col)
        (setf *hero-row* row)
        (setf *exit-col* col)
        (setf *exit-row* row)
        ; KLUGE clear an area around the entrance so there's lower odds
        ; of a stuck exit-camping Golem that cannot be coaxed out due to
        ; the current line-walking code
        ; TODO need better way to ID walls by, well, ID and not the wall
        ; character, or instead to draw the border walls afterwards, but
        ; without that draw trashing our entry gate
        (loop :for rr :from (- row 3) :to (+ row 3) :do
          (loop :for cc :from (- col 3) :to (+ col 3) :do
            (when (map-in-bounds-p cc rr)
              (let ((cell (aref *world-map* rr cc)))
                (when (and
                        (not (eq #\# (display-ch cell)))
                        (not (cell-moveok (aref *world-map* rr cc))))
                  (setf (aref *world-map* rr cc) *floor*))))))
        (return)))))

(defun maybe-add-vault ()
  (when (onein 20)
    (let ((col (+ 10 (random (- +map-cols+ 20))))
          (row (+ 10 (random (- +map-rows+ 20)))))
      (place-vault (if (coinflip) *dolmen* *pond*) col row))))

(defun help-text ()
  (colorpair 250 0)
  (emit-at (1+ +show-cols+) 1 (format nil "Lacrida - The Hungry Halfling"))
  (emit-at (1+ +show-cols+) 3 (format nil "movement keys, and other commands:"))
  (emit-at (1+ +show-cols+) 5 (format nil "   y  k  u"))
  (emit-at (1+ +show-cols+) 6 (format nil "    \\ | /      g - get an item (!)"))
  (emit-at (1+ +show-cols+) 7 (format nil "  h - @ - l    Q - quit without saving"))
  (emit-at (1+ +show-cols+) 8 (format nil "    / | \\      . - wait a turn"))
  (emit-at (1+ +show-cols+) 9 (format nil "   b  j  n"))
  (emit-at (1+ +show-cols+) 11 (format nil "symbols include:"))
  (emit-at (1+ +show-cols+) 13 (format nil "  P - Fungus   # - Wall"))
  (emit-at (1+ +show-cols+) 14 (format nil "  ~a - Tree     ~a - Water" #\GREEK_CAPITAL_LETTER_PSI #\~))
  (emit-at (1+ +show-cols+) 15 (format nil "  . - Floor    , - Rubble"))
  (emit-at (1+ +show-cols+) 16 (format nil "  ! - Food     ~a - Entrance (and exit)" #\GREEK_CAPITAL_LETTER_PI))
  (norm)
  (post-message "Found an entrance to ~a's garden..." *wizard*)
  (post-message "There is a note written here:")
  (post-message "  \"Absolutely no scrumping\" -- ~a" *wizard*))

(defun generate-world ()
  (ground-by-trig)
  (monsters-and-items)
  (border-wall)
  (entry-gate)
  (maybe-add-vault)
  (help-text))

(defun draw-bg ()
  (dotimes (r 24) (at 1 (1+ r)) (dotimes (c 80) (princ #\Space))))

(defun least-cost (animates)
  (loop :for ani :in animates
        :minimize (animate-cost ani) :into min
        :finally (return min)))

(defun game-loop ()
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (setq *random-state* (make-random-state t))
  (setf *wizard* (wizard-name))
  (colorpair 0 0)
  (draw-bg)
  (norm)
  (generate-world)
  (push (make-instance 'entity :cost *alarm-trigger*
                       :update #'update-alarm) *animates*)
  (shadowcast *hero-col* *hero-row* *hero-fov*)
  (finish-output)
  (loop
    (let ((min-cost (least-cost *animates*)))
      (dolist (ani *animates*)
        (let ((new-cost (- (animate-cost ani) min-cost)))
          (setf (animate-cost ani)
                  (if (<= new-cost 0)
                      (funcall (animate-update ani) ani)
                      new-cost)))))))

(defun score () (loop :for x :in *loot* :sum (food-score x)))

(defun farewell ()
  (let ((score (score)))
    (format t "You exit the gardens of ~a with ~D points~a" *wizard*
            score (if (= score *max-score*) "...FLAWLESS VICTORY" #\.))))

(defun start-game ()
  (handler-case
   (unwind-protect
     (charms:with-curses ()
       (charms:disable-echoing)
       (charms:enable-raw-input)
       (princ +hide-cursor+)              ; ??? how call charms curs_set
        (multiple-value-bind (cols rows)
            (charms:window-dimensions charms:*standard-window*)
          (when (or (< rows 24) (< cols 80))
            (error 'game-over :reason :term-size)))
        (game-loop))
     (princ +show-cursor+))
   (game-over (msg)
    (progn
      (at-col 1)
      (princ +clear-right+)
      (case (game-over-reason msg)
        (:escaped (farewell))
        (:killed
          (princ "The Golem crushes you, and lives happily ever after. The End."))
        (:quit-game (princ "Be seeing you..."))
        (:term-size (princ "The terminal is too small."))
        (otherwise (princ "Well, I guess it is over??")))
      (fresh-line)))))
