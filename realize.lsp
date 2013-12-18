;; REALIZE supports converting LINE, CIRCLE, POLYLINE and POINT entities to
;; RS-274 GCODE for use with EMC.  
;; 
;; The layer of the drawing entity determines the drill or mill
;; description.  Entities on layer 0 are ignored.
;; 
;; The x and y work in the obvious way.  The z dimension determines the
;; cutting depth.  Therefore the z of all entities NOT on layer 0 must be
;; negative.
;; 
;; All cuts made with a particular tool are made in least-depth-first
;; order.
;; 
;; Tools are used in decreasing alphabetic order, so if you have paths on
;; layers named 250-MILL and 125-MILL, it will ask for the 1/4" before
;; the 1/8".
;; 
;; A text of the form "vfeed: [real number]" or "hfeed: [real number]" on
;; a tool layer will set the vertical/horizontal feed for that tool.
;; 
;; A text of the form "peck: [real number]" on a drill layer will set
;; the increment for a peck drill cycle.
;;
;; A text of the form "chip: [real number]" on a drill layer will set
;; the increment for a chip breaking drill cycle.
;;
;; A text of the form "tap: [real number]" on a drill layer will set
;; the thread pitch for a tap cycle.
;;
;; A text of the form "dwell: [real number]" on a drill layer will set
;; the dwell at the bottom of the drill cycle.
;; 
;; REALIZE is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2 of the License, or (at your
;; option) any later version.  REALIZE is distributed in the hope that it
;; will be useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See
;; the GNU General Public License for more details.  You should have
;; received a copy of the GNU General Public License along with REALIZE; if
;; not, write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA
;; 
;; REALIZE is Copyright © 2005,2006,2007,2008,2009 Chris Radek
;; chris@timeguy.com


(defun preamble ()
  (write-line "G64 G17 G20 G40 G54 G80 G90 G94" ofp)
  (write-line "; O<circle> sub [x] [y] [z] [rad] [hfeed] [vfeed] [safetyheight]" ofp)
  (write-line "; O<drill> sub [x] [y] [z] [vfeed] [safetyheight]" ofp)
  (write-line "" ofp))

(defun tool-change (layername description)
  (write-line "M9" ofp)
  (write-line (strcat "N" layername " T" layername " M6 G43; " 
                      (if (/= description "MILL") description "")
                      " "
                      (get-string layername "tool:" "?")) ofp)
;  (write-line (strcat "G0 Z" (rtos sh lunits 4)) ofp)
;  (write-line "M1" ofp)
  (write-line (strcat "S" (rtos (get-value layername "rpm:" 1000) lunits 0) " M3") ofp)

  (setq vfeed (get-value layername "vfeed:" 8.0))
  (princ (strcat "\nVertical feed for " layername 
                 " is " (rtos vfeed lunits 2)))
  (if (/= description "DRILL")
      (progn
        (setq hfeed (get-value layername "hfeed:" 8.0))
        (princ (strcat "\nHorizontal feed for " layername 
                       " is " (rtos hfeed lunits 2))))))

(defun to-ucs (p)
  (cons 10 (trans (cdr p) 0 1)))

(defun sanity-check (/ sane ent etype z layer)
  (setq sane t)
  (foreach ent sslist
           (setq etype (cdr (assoc 0 ent)))
           (if (or (= etype "POLYLINE") (= etype "POINT") (= etype "LINE") (= etype "CIRCLE")) 
               (progn
                 (if (= etype "POLYLINE") 
                     (setq z (cadddr (to-ucs (assoc 10 (entget (entnext (cdr (assoc -1 ent))))))))
                   (setq z (cadddr (to-ucs (assoc 10 ent)))))
                 (setq layer (cdr (assoc 8 ent)))
                 (if (and (/= layer "0") (>= z 0.0)) 
                     (progn
                       (princ (strcat "\n" etype " entity at z>=0.  Aborting."))
                       (princ)
                       (princ ent)
                       (princ (entget (entnext (cdr (assoc -1 ent)))))
                       (setq sane nil))))))
  sane)

(defun drill-holes-of-layer (s / ent etype layer x y z dwell peck chip tap)
  (setq dwell (get-value s "dwell:" 0))
  (setq peck (get-value s "peck:" 0))
  (setq chip (get-value s "chip:" 0))
  (setq tap (get-value s "tap:" 0))
  (write-line (strcat "G0 Z" (rtos sh lunits 4)) ofp)
  (write-line "M8" ofp)
  (foreach ent sslist
           (setq etype (cdr (assoc 0 ent)))
           (if (= etype "POINT") 
               (progn
                 (setq layer (cdr (assoc 8 ent)))
                 (if (= layer s) (progn
                                   (setq x (cadr (to-ucs (assoc 10 ent))))
                                   (setq y (caddr (to-ucs (assoc 10 ent))))
                                   (setq z (cadddr (to-ucs (assoc 10 ent))))
                                   (drill-point x y z dwell peck chip tap))))))
  (write-line (strcat "G0 Z" (rtos sh lunits 4)) ofp))

(defun cut-line (ent / begin end)
  ;; pocketing with HATCH works better if the direction alternates,
  ;; since the parallel lines all go in the same direction.
  (setq cut-line-backward (not cut-line-backward))
  (if cut-line-backward
      (setq begin (to-ucs (assoc 10 ent)) end (to-ucs (assoc 11 ent)))
    (setq begin (to-ucs (assoc 11 ent)) end (to-ucs (assoc 10 ent))))
  (start-cut (cadr begin) (caddr begin) (cadddr begin))
  (cut-to (cadr end) (caddr end) 0))
		
(defun cut-circle (ent / x y z rad)
  (setq   x (cadr (to-ucs (assoc 10 ent))) 
          y (caddr (to-ucs (assoc 10 ent)))
          z (cadddr (to-ucs (assoc 10 ent)))
          rad (cdr (assoc 40 ent)))
  (write-line (strcat "; O<circle> call [" 
                      (rtos x lunits 4) "] [" 
                      (rtos y lunits 4) "] ["
                      (rtos z lunits 4) "] ["
                      (rtos rad lunits 4) "] ["
                      (rtos hfeed lunits 2) "] ["
                      (rtos vfeed lunits 2) "] ["
                      (rtos sh lunits 4) "]") ofp)
  (setq y (+ y rad))
  (write-line (strcat "G0 Z" (rtos sh lunits 4)) ofp)
  (write-line 
   (strcat "G0 X" (rtos x lunits 4) 
           " Y" (rtos y lunits 4)) ofp)
  (if (> sh 0.1)
      (write-line "G0 Z0.1" ofp))
  (write-line 
   (strcat "G1 Z" (rtos z lunits 4) 
           " F" (rtos vfeed lunits 2)) ofp)
  (write-line
   (strcat "G3 X" (rtos x lunits 4) 
           " Y" (rtos y lunits 4)
           " I0 J-" (rtos rad lunits 4)
           " F" (rtos hfeed lunits 2)) ofp)
  (write-line "; end circle" ofp)
  (setq prev-x x prev-y y))

(defun cut-pline (ent / down close-pt ent x y z bulge last-bulge)
  (setq down nil)
  (setq last-bulge 0.0)
  (if (= (boole 1 (cdr (assoc 70 ent)) 1) 1)
      (setq close-pt (cdr (to-ucs (assoc 10 (entget (entnext (cdr (assoc -1 ent))))))))
    (setq close-pt nil))
  (while (and (setq ent (entget (entnext (cdr (assoc -1 ent)))))
              (= (cdr (assoc 0 ent)) "VERTEX"))
    (setq x (cadr (to-ucs (assoc 10 ent))))
    (setq y (caddr (to-ucs (assoc 10 ent))))
    (setq z (cadddr (to-ucs (assoc 10 ent))))
    (setq bulge (cdr (assoc 42 ent)))
    (if down
        (cut-to x y last-bulge)
      (start-cut x y z))
    (setq down t last-bulge bulge))
  (if close-pt (cut-to (car close-pt) (cadr close-pt) last-bulge)))
		

(defun cut-plines-of-layer (s / ent etype layer)
  (foreach ent sslist
           (setq etype (cdr (assoc 0 ent)))
           (if (= etype "POLYLINE") (progn
                                     (setq layer (cdr (assoc 8 ent)))
                                     (if (= layer s)
                                         (cut-pline ent)))))
  (write-line (strcat "G0 Z" (rtos sh lunits 4)) ofp))

(defun cut-lines-of-layer (s / ent etype layer)
  (foreach ent sslist
           (setq etype (cdr (assoc 0 ent)))
           (if (= etype "LINE") (progn
                                 (setq layer (cdr (assoc 8 ent)))
                                 (if (= layer s)
                                     (cut-line ent)))))
  (write-line (strcat "G0 Z" (rtos sh lunits 4)) ofp))

(defun cut-circles-of-layer (s / ent etype layer)
  (foreach ent sslist
           (setq etype (cdr (assoc 0 ent)))
           (if (= etype "CIRCLE") (progn
                                   (setq layer (cdr (assoc 8 ent)))
                                   (if (= layer s)
                                       (cut-circle ent)))))
  (write-line (strcat "G0 Z" (rtos sh lunits 4)) ofp))


(defun get-value (layer key default)
  (atof (get-string layer key (rtos default lunits 2))))

(defun get-string (layer key default / value keylen)
  (setq value default)
  (setq keylen (strlen key))
  (foreach ent sslist
           (if 	(and (= layer (cdr (assoc 8 ent)))
                     (= "TEXT" (cdr (assoc 0 ent)))
                     (= key (substr (cdr (assoc 1 ent)) 1 keylen)))
               (setq value (substr (cdr (assoc 1 ent)) (1+ keylen)))))
  value)

(defun get-drills (/ ent etype layer)
  (princ "\nSearching for drills...")
  (setq drills ())
  (foreach ent sslist
           (setq etype (cdr (assoc 0 ent)))
           (if (= etype "POINT") 
               (progn
                 (setq layer (cdr (assoc 8 ent)))
                 (if (and (/= layer "0") (not (member layer drills)))
                     (setq drills (cons layer drills)))))))

(defun get-mills (/ ent etype layer)
  (princ "\nSearching for mills...")
  (setq mills ())
  (foreach ent sslist
           (setq etype (cdr (assoc 0 ent)))
           (if (or (= etype "POLYLINE") 
                   (= etype "LINE") 
                   (= etype "CIRCLE")) (progn
                   (setq layer (cdr (assoc 8 ent)))
                   (if (and (/= layer "0") (not (member layer mills)))
                       (setq mills (cons layer mills)))))))

(defun init-depths ()
  (initget 6)                           ; not zero or neg
  (setq sh (getdist (strcat "\nSafety Height <"
                            (rtos (if (not (null oldsh))
                                      oldsh
                                    1.0) lunits 3)
                            ">: ")))
  (if (null sh)
      (setq sh (if (not (null oldsh))
                      oldsh
                    1.0))
    (setq oldsh sh)))

(defun save-interactive-options (name height / f)
  (setq f (open "c:\\realize.prf" "w"))
  (write-line (strcat "(setq oldfile \"" name "\")") f)
  (write-line (strcat "(setq oldsh " (rtos height lunits 4) ")") f)
  (close f))

(defun init-output ()
  (setq of (getstring (strcat "\nOutput file <"
                              (if oldfile 
                                  oldfile 
                                "H:/ACAD.NGC")
                              ">: ")))
  (if (eq "" of) 
      (setq of (if oldfile 
                   oldfile 
                 "h:/acad.ngc"))
    (setq oldfile of)))

;; G82 R0.040000 Z-0.080000 P200 X1.925000 Y0.687500
(defun drill-point (x y depth dwell peck chip tap / retract)
  (write-line (strcat "; O<drill> call ["
                      (rtos x lunits 4) "] ["
                      (rtos y lunits 4) "] ["
                      (rtos depth lunits 4) "] ["
                      (rtos vfeed lunits 2) "] ["
                      (rtos sh lunits 4) "]") ofp)
  (cond ((> sh 0.1)
         (setq retract 0.1)
         (write-line (strcat "G0 Z" (rtos sh lunits 4)) ofp)
         (write-line (strcat "G0 X" (rtos x lunits 4)
                             " Y"    (rtos y lunits 4)) ofp)
         (write-line "G0 Z0.1" ofp)
         (cond ((> dwell 0.0)
                (write-line (strcat 
                             "G82 R0.1"
                             " Z" (rtos depth lunits 4)
                             " P" (rtos dwell lunits 2) 
                             " F" (rtos vfeed lunits 2)) ofp))
               ((> peck 0.0)
                (write-line (strcat
                             "G83 R0.1"
                             " Z" (rtos depth lunits 4)
                             " Q" (rtos peck lunits 4)
                             " F" (rtos vfeed lunits 2)) ofp))
               ((> chip 0.0)
                (write-line (strcat
                             "G73 R0.1"
                             " Z" (rtos depth lunits 4)
                             " Q" (rtos peck lunits 4)
                             " F" (rtos vfeed lunits 2)) ofp))
               ((> tap 0.0)
                (write-line (strcat
                             "G33.1"
                             " Z" (rtos depth lunits 4)
                             " K" (rtos tap lunits 6)) ofp))
               (t 
                (write-line (strcat
                             "G81 R0.1"
                             " Z" (rtos depth lunits 4)
                             " F" (rtos vfeed lunits 2)) ofp))))
        
        (t
         (cond ((> dwell 0.0)
                (write-line (strcat 
                             "G82 R" (rtos sh lunits 4)
                             " Z" (rtos depth lunits 4)
                             " P" (rtos dwell lunits 2) 
                             " X" (rtos x lunits 4)
                             " Y" (rtos y lunits 4)
                             " F" (rtos vfeed lunits 2)) ofp))
               ((> peck 0.0)
                (write-line (strcat
                             "G83 R" (rtos sh lunits 4)
                             " X" (rtos x lunits 4)
                             " Y" (rtos y lunits 4)
                             " Z" (rtos depth lunits 4)
                             " Q" (rtos peck lunits 4)
                             " F" (rtos vfeed lunits 2)) ofp))
               ((> chip 0.0)
                (write-line (strcat
                             "G73 R" (rtos sh lunits 4)
                             " X" (rtos x lunits 4)
                             " Y" (rtos y lunits 4)
                             " Z" (rtos depth lunits 4)
                             " Q" (rtos peck lunits 4)
                             " F" (rtos vfeed lunits 2)) ofp))
               ((> tap 0.0)
                (write-line (strcat 
                             "G0 Z" (rtos sh lunits 4)) ofp)
                (write-line (strcat
                             "G0 X" (rtos x lunits 4)
                             " Y" (rtos y lunits 4)) ofp)
                (write-line (strcat
                             "G33.1"
                             " Z" (rtos depth lunits 4)
                             " K" (rtos tap lunits 6)) ofp))
               (t 
                (write-line (strcat
                             "G81 R" (rtos sh lunits 4)
                             " Z" (rtos depth lunits 4)
                             " X" (rtos x lunits 4)
                             " Y" (rtos y lunits 4)
                             " F" (rtos vfeed lunits 2)) ofp))))))

(defun start-cut (x y depth)
  (write-line (strcat "G0 Z" (rtos sh lunits 4)) ofp)
  (write-line "M8" ofp)
  (write-line (strcat "G0 X" (rtos x lunits 4) 
                      " Y"    (rtos y lunits 4))  ofp)
  (if (> sh 0.1)
      (write-line "G0 Z0.1" ofp))
  (write-line (strcat "G1 Z" (rtos depth lunits 4)
                      " F" (rtos vfeed lunits 2)) ofp)
  (setq prev-x x prev-y y))

(defun cut-to (x y bulge / cot cx cy rad arcdir i j)
  (if (/= 0.0 bulge)
      (progn
        (setq 
         cot (* 0.5 (- (/ 1.0 bulge) bulge))
         cx (/ (- (+ prev-x x) (* (- y prev-y) cot)) 2.0)
         cy (/ (+ (+ prev-y y) (* (- x prev-x) cot)) 2.0)
         rad (distance (list prev-x prev-y) (list cx cy))
         arcdir (if (< bulge 0.0) "G2" "G3")
         i (- cx prev-x) j (- cy prev-y))
        (write-line (strcat arcdir 
                            " X" (rtos x lunits 4)
                            " Y" (rtos y lunits 4) 
                            " I" (rtos i lunits 4) 
                            " J" (rtos j lunits 4)
                            " F" (rtos hfeed lunits 2)) ofp))
    (write-line (strcat "G1 X" (rtos x lunits 4) 
			" Y" (rtos y lunits 4)
			" F" (rtos hfeed lunits 2))  ofp))
  (setq prev-x x prev-y y))

(defun finish ()
  (write-line (strcat "G0 Z" (rtos sh lunits 4)) ofp)
  (write-line "M9" ofp)
  (write-line "T0 M6 G49\nM2" ofp)
  (close ofp))

(defun compare-entity-height (a b)
  (> (cadddr (to-ucs (assoc 10 a))) (cadddr (to-ucs (assoc 10 b)))))

(defun ins-sort (L cmp / M N O)
  (setq O L L (list (car O)))
  (while (setq M nil N L O (cdr O))
    (while (and O N (apply cmp (list (car N) (car O))))
      (setq M (append M (list (car N))) N (cdr N)))
    (setq N (cons (car O) N) L (append M N))) L)

(defun myerror (s)                      ; If an error (such as CTRL-C) occurs
                                        ; while this command is active...
  (if (/= s "Function cancelled")
      (princ (strcat "\nError: " s))
    )
  (setvar "cmdecho" ocmd)               ; Restore saved modes
  (setvar "blipmode" oblp)
  (setq *error* olderr)                 ; Restore old *error* handler
  (princ))

(defun C:REALIZE (/ olderr ocmd oblp ss i) 
  (setq olderr  *error*
	*error* myerror)
  (setq ocmd (getvar "cmdecho"))
  (setq oblp (getvar "blipmode"))
  (setvar "cmdecho" 0)

  (setq lunits (getvar "lunits"))
  (load "c:/realize.prf" "")
  (init-depths)
  (init-output)
  (save-interactive-options of sh)

  (setq ss (ssget))
  (princ "\nGetting selection...")
  (setq sslist ())
  (setq i 0)
  (repeat (sslength ss)
          (setq sslist (cons (entget (ssname ss i)) sslist))
          (setq i (1+ i)))

  (princ "\nSorting selection...")
  (setq sslist (ins-sort sslist 'compare-entity-height))

  (if (sanity-check) 
      (progn
        (setq ofp (open of "w"))
        (preamble)
        (get-drills)
        (if drills (progn
                     (setq drills (ins-sort drills '>))
                     (princ "\nDrills: ")
                     (princ drills)
                     (foreach d drills
                              (tool-change d "DRILL")
                              (drill-holes-of-layer d))))

        (get-mills)
        (if mills (progn
                    (setq mills (ins-sort mills '>))
                    (princ "\nMills: ")
                    (princ mills)
                    (foreach c mills
                             (tool-change c "MILL")
                             (cut-plines-of-layer c)
                             (cut-lines-of-layer c)
                             (cut-circles-of-layer c))))
        (finish)))

  (setvar "cmdecho" ocmd)
  (setvar "blipmode" oblp)
  (setq *error* olderr)                 ; Restore old *error* handler
  (princ)) 
