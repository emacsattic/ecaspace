;; ecaspace.el --- music creation tool built on Ecasound
;;   ___  ___ __ _ ___ _ __   __ _  ___ ___ 
;;  / _ \/ __/ _` / __| '_ \ / _` |/ __/ _ \
;; |  __/ (_| (_| \__ \ |_) | (_| | (_|  __/
;;  \___|\___\__,_|___/ .__/ \__,_|\___\___|
;;                    |_|                   
;; Copyright (C) 2007  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: multimedia
;; $Id: ecaspace.el,v 1.12 2007/01/26 22:19:02 dto Exp dto $

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package implements a front-end to Mario Lang's ecasound.el
;; (and therefore to Ecasound.)

;; The file ecasound.el is included with ecasound. You may download
;; ecasound from http://eca.cx

;; Terminology: An "ecaspace" is a library of sound projects and
;; recordings.  A session is a set of tracks. A track consists of an
;; ecasound input, a corresponding output, a set of operations to be
;; performed, and finally a collection of takes. A take identifies an
;; individual audio file. A "take set" is the set of new takes created
;; when the engine is started with record-enabled tracks.
;;

;;; Code:


;; (@* "general")


(require 'ecasound)
(require 'jack)
(require 'cell)

(defvar ecaspace-directory "~/ecaspace" 
"The directory where all sessions and audio data are kept.")


;; (@* "engine state")


(defvar ecaspace-state :stopped "One of nil, :stopped, :running, :crashed.")

(defvar ecaspace-record-enabled-p nil 
"When non-nil at start of playback, enable recording on all
tracks for which record-p is set.")

(defvar ecaspace-buffer nil "Buffer for interaction with ecasound.")


(defmacro with-ecasound (&rest body)
  `(with-current-buffer ecaspace-buffer
     ,@body))


(defun ecaspace-global-init ()
  "Start up ecasound daemon if necessary."
  (interactive)
  (when (null ecaspace-buffer)
    (setf ecaspace-buffer (eci-init))
    (setf ecaspace-state :stopped)))


(defun ecaspace-global-reinit ()
  (interactive)
  (setf ecaspace-buffer nil)
  (ecaspace-global-init))

  
(defun ecaspace-start ()
  "Begin a new take. If ecaspace-record-enabled-p is non-nil,
then record all tracks that are also record enabled."
  (interactive)
  (with-ecasound 
   (eci-start)
   ;;
   ;; FIXME: there has to be a better way of waiting until the engine is running
   ;;
   (while (not (string= "running" (eci-engine-status)))
     (sit-for 0))
   (ecaspace-session-to-jack)
   (setf ecaspace-state :running)))


(defun ecaspace-stop ()
  "Complete the current take. "
  (interactive)
  (with-ecasound (eci-cs-disconnect))
  (setf ecaspace-state :stopped)
  ;;
  ;; turn off global record enable. this means you must explicitly
  ;; enable recording for each take.
  (setf ecaspace-record-enabled-p nil)
  ;;
  ;; see (@> "sessions")
  (ecaspace-session-finish-take))
         

;; (@* "sessions")
;;                    _                 
;;  ___  ___  ___ ___(_) ___  _ __  ___ 
;; / __|/ _ \/ __/ __| |/ _ \| '_ \/ __|
;; \__ \  __/\__ \__ \ | (_) | | | \__ \
;; |___/\___||___/___/_|\___/|_| |_|___/
                                     


(defstruct ecaspace-session 
  name ; string name of session
  tracks ; set of track objects. see (@> "tracks")
  selected-track-name ; string name of selected track
  )


(defvar ecaspace-selected-session-name nil "String name of current session.")


(defvar ecaspace-selected-session nil "Object storing current session state.")


(defun ecaspace-session-directory (name)
  (concat (file-name-as-directory ecaspace-directory) name))


(defun ecaspace-selected-session-directory ()
  (if ecaspace-selected-session-name
      (ecaspace-session-directory ecaspace-selected-session-name)
    (error "No ecaspace session is selected.")))
  

(defun ecaspace-session-file (file-name)
  (concat (file-name-as-directory (ecaspace-selected-session-directory)) file-name))


(defun ecaspace-find-session (session)
  "Load session named SESSION from the ecaspace-directory.
If no such session exists, create it. Start ecasound if needed."
  (interactive "sSession name: ")
  (let ((session-dir (ecaspace-session-directory session)))
    (if (file-exists-p session-dir)
	(if (file-directory-p session-dir)
	    ;;
	    ;; open existing session
	    nil ;; FIXME: load some kind of setup file
	  (error "File exists, but is not a directory."))
      ;;
      ;; create new session
      (make-directory session-dir)
      (message "Created session in %s" session-dir)))
  ;;
  ;; now make it the current session
  (setf ecaspace-selected-session-name session)
  (setf ecaspace-selected-session (make-ecaspace-session :name session)))


(defun ecaspace-find-timestamped-session (name)
  (interactive "sSession name after timestamp: ")
  (let ((stamp (format-time-string "%Y-%m-%d-%H%M" (current-time))))
    (ecaspace-find-session (concat stamp "." name))))


(defun ecaspace-iospec-to-string (iospec track)
  "Transform iospecs into lists of ecasound formatted strings."
  (cond 
   ;; it's a plain ecasound string.
   ((stringp iospec) (list iospec))
   ;; it's a list of iospecs
   ((and (listp iospec) (listp (car-safe iospec)))
    (mapcar (lambda (io)
	      (ecaspace-iospec-to-string io track))
	    iospec))
   ;; it's a single iospec
   ((listp iospec) 
    (case (car iospec)
      ('jack (format "jack_generic,%s" (ecaspace-track-name track)))
      ('file (expand-file-name (second iospec)))
      ('loop (error "loop devices not handled yet"))
      ('master "jack_alsa")))))
       

(defun ecaspace-session-to-ecasound ()
  "Build current configuration as a chainsetup in ecasound."
  (interactive)
  (setf ecaspace-jack-queued-connections nil)
  (setf ecaspace-jack-port-suffix-number 1)
  (let ((session ecaspace-selected-session))
    (with-ecasound
     ;;
     ;; create new chainsetup
     (eci-cs-remove)
     (eci-cs-add "ecaspace")
     ;;
     ;; implement each track as an ecasound chain
     ;; see (@> "tracks")
     (mapc 'ecaspace-track-to-ecasound (ecaspace-session-tracks session)))))


(defun ecaspace-session-to-jack ()
  "Make any necessary jack connections."
  (interactive)
  (dolist (connection ecaspace-jack-queued-connections)
    (apply 'jack-connect connection)))
	    

(defun ecaspace-session-finish-take ()
  (interactive)
  (dolist (track (ecaspace-session-tracks ecaspace-selected-session))
    ;;
    ;; did we recently record anything? 
    (when (and ecaspace-record-enabled-p (ecaspace-track-record-p track))
      ;;
      ;; set update flag so that the cell sheet updates
      (setf (ecaspace-track-update-p track) t)
      ;; 
      ;; select the take we just recorded for playback
      (setf (ecaspace-track-selected-take track) (ecaspace-track-next-take track))
      ;;
      ;; prepare for next take
      (incf (ecaspace-track-next-take track)))))
	  

;; (@* "tracks")
;;  _                  _        
;; | |_ _ __ __ _  ___| | _____ 
;; | __| '__/ _` |/ __| |/ / __|
;; | |_| | | (_| | (__|   <\__ \
;;  \__|_|  \__,_|\___|_|\_\___/
                             

(defstruct ecaspace-track 
  name ; string name of track. used in filenames, so don't use a slash
  num-channels ; how many channels? usually 1 or 2
  input output ; sexps that are mapped to ecasound i/o strings
  operators ; list of strings in ecasound chainop syntax
  record-p ; whether to create a new take when recording
  monitor-p ; whether to hear the selected take (or current recording, when record-p is non-nil)
  selected-take ; which take plays when you listen back
  next-take ; number of next take to be recorded
  update-p ; whether any caches/widgets should be re-initialized from track data
  )


(defvar ecaspace-selected-track-name nil "Currently selected track name.")


(defvar ecaspace-selected-track nil "Currently selected track object to which operations apply.")


(defun ecaspace-track-take-file (track &optional take)
  "Given a TRACK and an optional TAKE number, produce the name of
the file where audio will be recorded for this take. If TAKE is
not given, assume the next take number."
  (ecaspace-session-file (format "%s.take-%d.wav"
				 (ecaspace-track-name track) 
				 (or take (ecaspace-track-next-take track)))))


(defun ecaspace-track-add (name)
  (interactive "sTrack name: ")
  (let ((track (make-ecaspace-track :name name
				    :record-p nil
				    :monitor-p t
				    :next-take 1
				    :num-channels 1
				    :selected-take 0)))
    (setf (ecaspace-session-tracks ecaspace-selected-session)
	  (cons track (ecaspace-session-tracks ecaspace-selected-session)))
    ;;
    ;; make it the selected track 
    (ecaspace-select-track name)))


(defun ecaspace-track-remove (track-name)
  (interactive "sTrack name: ")
  (setf (ecaspace-session-tracks ecaspace-selected-session)
	(remove-if (lambda (track)
		     (string= track-name (ecaspace-track-name track)))
		   (ecaspace-session-tracks ecaspace-selected-session))))


(defun ecaspace-track-get (track-name)
  (interactive)
  (find-if (lambda (track)
	     (string= track-name (ecaspace-track-name track))) 
	   (ecaspace-session-tracks ecaspace-selected-session)))


(defun ecaspace-select-track (track-name)
  (setf ecaspace-selected-tack-name track-name)
  (setf ecaspace-selected-track (ecaspace-track-get track-name)))


;; (@* "rendering tracks to ecasound and jack")


(defvar ecaspace-jack-queued-connections nil 
  "List of 4-tuples of strings (IN-CLIENT IN-PORT OUT-CLIENT
OUT-PORT)")


(defvar ecaspace-jack-port-suffix-number 1)


(defun ecaspace-queue-jack-input (prefix iospec &optional mono-p)
  (let* ((in-client (second iospec))
	 (in-port (third iospec))
	 (out-client "ecasound")
	 (out-port (concat prefix (format "_%d" ecaspace-jack-port-suffix-number))))
    (push (list in-client in-port out-client out-port)
	  ecaspace-jack-queued-connections)
    (incf ecaspace-jack-port-suffix-number (if mono-p 1 1))))


(defun ecaspace-track-to-ecasound (track)
  (with-ecasound
   (let ((inputs (ecaspace-track-input track))
	 (outputs (ecaspace-track-output track))
	 (channels (ecaspace-track-num-channels track)))
     ;;
     ;; set track format
     (eci-cs-set-audio-format (format "f32_le,%d,44100,i" channels))
     ;; 
     ;; are we monitoring?
     (when (ecaspace-track-monitor-p track)
       (eci-c-add (concat (ecaspace-track-name track) "-monitor"))
       ;;
       ;; decide what to monitor: input or selected take? 
       (if (and ecaspace-record-enabled-p (ecaspace-track-record-p track))
	   ;;
	   ;; monitoring the recorded input. this will need jack connections.
	   (progn 
	     (eci-ai-add (ecaspace-iospec-to-string (car inputs) track))
	     (dotimes (channel channels)
	       (ecaspace-queue-jack-input (ecaspace-track-name track) 
					  (nth channel inputs)
					  (= 1 channels))))
	 ;;
	 ;; monitoring the selected take.
	 (eci-ai-add (ecaspace-track-take-file track (ecaspace-track-selected-take track))))
       ;;
       ;; send to output
       (eci-ao-add (ecaspace-iospec-to-string outputs track)))
     ;;
     ;; are we recording? 
     (when (and ecaspace-record-enabled-p (ecaspace-track-record-p track))
       (eci-c-add (concat (ecaspace-track-name track) "-record"))
       (eci-ai-add (ecaspace-iospec-to-string (car inputs) track))
       (dotimes (channel channels)
	 (ecaspace-queue-jack-input (ecaspace-track-name track)
				    (nth channel inputs)
				    (= 1 channels)))
       (eci-ao-add (ecaspace-track-take-file track))))))
		      


;; (@* "user interface based on cell-mode")
;;   ____ _   _ ___ 
;;  / ___| | | |_ _|
;; | |  _| | | || | 
;; | |_| | |_| || | 
;;  \____|\___/|___|
                 

(mapcar 'cell-register-prototype 
	'(("track" :compute %ecaspace-track :face ecaspace-track-face)
	  ("record-p" :compute %ecaspace-record-p :face ecaspace-record-p-face)
	  ("monitor-p" :compute %ecaspace-monitor-p :face ecaspace-monitor-p-face)
	  ("selected-take" :compute %ecaspace-selected-take :face ecaspace-selected-take-face)
	  ("next-take" :compute %ecaspace-next-take :face ecaspace-next-take-face)
	  ("input" :compute %ecaspace-input :face ecaspace-io-face)
	  ("channels" :compute %ecaspace-channels :face ecaspace-track-face)
	  ("output" :compute %ecaspace-output :face ecaspace-io-face)))
	  

(defun ecaspace-session-update-sheet ()
  (interactive)
  (with-current-cell-sheet
   (mapcar (lambda (cell)
	     (funcall (cell-compute cell) cell :model))
	   (cell-sheet-list sheet))
   ;; clear update flags on tracks
   (dolist (track (ecaspace-session-tracks ecaspace-selected-session))
     (setf (ecaspace-track-update-p track) nil))
   ;; update the screen
   (cell-sheet-update)))


(defun ecaspace-go ()
  (interactive)
  (ecaspace-session-update-sheet)
  (ecaspace-session-to-ecasound))


; (global-set-key [(f11)] 'ecaspace-go)

(defun %ecaspace-track (cell message)
  (with-cell cell
	     (when (null state)
	       (setf label "No track name!"))
	     (case message
	       (:edit 
		(setf state (read-from-minibuffer "Track name: " state))
		(setf label state))
	       (:model 
		;; select the track
		(ecaspace-select-track state)
		;;
		;; create track if needed
		(when (not (ecaspace-track-get state))
		  (ecaspace-track-add state))))))
		      

(defun %ecaspace-record-p (cell message)
  (with-cell cell
	     (if state 
		 (setf label "record")
	       (setf label " -- "))
	     (case message
	       (:model (setf (ecaspace-track-record-p ecaspace-selected-track)
			     state))
	       (:click (setf state (if state nil t))))))


(defun %ecaspace-monitor-p (cell message)
  (with-cell cell
	     (if state 
		 (setf label "monitor")
	       (setf label " -- "))
	     (case message
	       (:model (setf (ecaspace-track-monitor-p ecaspace-selected-track)
			     state))
	       (:click (setf state (if state nil t))))))


(defun %ecaspace-selected-take (cell message)
  (with-cell cell
	     (if state
		 (setf label (format "take %d" state))
	       (setf state 1)
	       (setf label "1"))
	     (case message
	       (:edit 
		(let ((entry (read-from-minibuffer "Select take number: " (format "%d" state) nil :read)))
		  (when (numberp entry)
		    (setf state entry))))
	       (:model 
		;;
		;; should we set the selected track, or update the widget when the selected track
		;; has been changed automatically? 
		(if (ecaspace-track-update-p ecaspace-selected-track)
		    (setf state (ecaspace-track-selected-take ecaspace-selected-track))
		  (setf (ecaspace-track-selected-take ecaspace-selected-track) state))))))
	       

(defun %ecaspace-next-take (cell message)
  (with-cell cell
	     (if state
		 (setf label (format "take %d" state))
	       (setf label "no next take?"))
	     (case message
	       (:model (setf state (ecaspace-track-next-take ecaspace-selected-track))))))


(defun %ecaspace-input (cell message)
  (with-cell cell 
	     (case message
	       (:edit
		(setf state (read-from-minibuffer "Lisp expression: " (format "%S" state) nil :read)))
	       (:model 
		(setf (ecaspace-track-input ecaspace-selected-track) state)))	     
	     (setf label (format "%S" state))))


(defun %ecaspace-output (cell message)
  (with-cell cell 
	     (case message
	       (:edit
		(setf state (read-from-minibuffer "Lisp expression: " (format "%S" state) nil :read)))
	       (:model
		(setf (ecaspace-track-output ecaspace-selected-track) state)))
	     (setf label (format "%S" state))))


(defun %ecaspace-channels (cell message)
  (with-cell cell
	     (when (null state)
	       (setf state 1))
	     (setf label (format "%d" state))
	     (case message
	       (:edit 
		(setf state (read-from-minibuffer "Number of channels: " 
						  (format "%S" state) nil :read)))
	       (:model 
		(setf (ecaspace-track-num-channels ecaspace-selected-track) state)))))	      		
	       	  	     

(defface ecaspace-track-face '((t (:foreground "gray50" :background "white")))
  "Face for ecaspace controls.")

(defface ecaspace-record-p-face '((t (:foreground "yellow" :background "red")))
  "Face for ecaspace controls.")

(defface ecaspace-monitor-p-face '((t (:foreground "yellow" :background "forestgreen")))
  "Face for ecaspace controls.")

(defface ecaspace-selected-take-face '((t (:foreground "white" :background "gray20")))
  "Face for ecaspace controls.")

(defface ecaspace-next-take-face '((t (:foreground "red" :background "white")))
  "Face for ecaspace controls.")

(defface ecaspace-io-face '((t (:foreground "cyan" :background "navyblue")))
  "Face for ecaspace controls.")


;; (@* "ecaradio")
;;                               _ _       
;;   ___  ___ __ _ _ __ __ _  __| (_) ___  
;;  / _ \/ __/ _` | '__/ _` |/ _` | |/ _ \ 
;; |  __/ (_| (_| | | | (_| | (_| | | (_) |
;;  \___|\___\__,_|_|  \__,_|\__,_|_|\___/ 
;;
;; concatenative synthesis with large files
;; (programmable crossfade and static effects)                                        


;; (@* "quickie functions to set up a marking session")


(defvar ecaradio-interactive-mark-number 1)


(defun ecaradio-interactive-mark ()
  (interactive)
  (ecasound-set-mark (format "%d" ecaradio-interactive-mark-number))
  (incf ecaradio-interactive-mark-number))
		     

;(global-set-key [(f6)] 'ecaradio-interactive-mark)

(defun ecaradio-interactive-mark-go (file)
  (interactive "fFilename: ")
  (ecasound)
  (setf ecaradio-interactive-mark-number 1)
  (eci-cs-disconnect)
  (eci-cs-add "ecasound")
  (eci-c-add "player")
  (eci-ai-add file)
  (eci-ao-add "jack_alsa")
  (ecaradio-interactive-mark)
  (eci-start))


(defvar ecaradio-internal-markers nil)

(defun ecaradio-markers ()
  ecaradio-internal-markers)

(defun ecaradio-grab-markers ()
  (interactive)
  (setf ecaradio-internal-markers
	(mapcar 'cdr (reverse
		      (cdr (assoc nil ecasound-markers))))))


(defun ecaradio-get-marker-pos (marker-number)
  (nth marker-number (ecaradio-markers)))


;(insert (format "%S" (ecaradio-markers) ))
; (setf my-album-markers '(0.0 30.092 258.216 260.805 406.439 519.549 537.722 593.464  693.169 727.801 852.317 876.724 906.3 979.913 1013.109 1135.827 1151.123 1244.131 1326.129 1333.292 1462.999 1486.181 1510.487 1688.187 1862.909 2014.364 2019.953 2035.345 2171.151 2199.828 2306.156 2363.387 2387.963 2494.098 2577.641 2717.237 2822.172 2914.746 3264.947 3352.404))


;; (@* "splitting a file at its markers")


(defvar ecaradio-split-output-directory "/tmp")


(defun ecaradio-split-file-on-markers (file &optional markers)
  (interactive "FFilename: ")
  (let* ((marker-list (or markers (ecaradio-markers)))
	 (num-markers (- ecaradio-interactive-mark-number 1))
	 (current-marker 1))
    (while (< current-marker num-markers)
      (let ((begin (ecaradio-get-marker-pos current-marker))
	    (end (or (ecaradio-get-marker-pos (1+ current-marker)) nil)))
	(ecaradio-split-on-position begin end current-marker file))
      (incf current-marker))))


(defun ecaradio-split-on-position (begin end split-number file)
  (message "Ecaradio-split-on-position: %S %S %S %S" begin end split-number file)
  (eci-cs-disconnect)
  (eci-cs-add (format "ecaradio-split-%d" split-number))
  (eci-c-add "player")
  (eci-ai-add file)
  (eci-ao-add (concat (file-name-as-directory ecaradio-split-output-directory)
		      "ecaradio-split-"
		      (number-to-string split-number)
		      ".wav"))
  (eci-ai-index-select 1)
  (eci-ai-set-position begin)
  (when end (eci-cs-set-length (- end begin)))
  (eci-start)
  (sit-for 3)
  (while (string= (eci-engine-status) "running")
    (sit-for 1)))


;; (@* "rendering a crossfaded ecaradio session")
  

(defvar ecaradio-crossfade-length 10.0)


(defvar ecaradio-static-source nil "String filename of static source.")
(defvar ecaradio-static-amp-percentage 60)
(defvar ecaradio-use-static t)

(defun ecaradio-make-ewf (outfile &rest args)
  (destructuring-bind (&key source offset start-position length looping &allow-other-keys)
      args
    (with-temp-buffer 
      (when source (insert (format "source = %s\n" source)))
      (when offset (insert (format "offset = %f\n" offset)))
      (when start-position (insert (format "start-position = %f\n" start-position)))
      (when length (insert (format "length = %f\n" length)))
      (when looping (insert "looping = true"))
      (write-file outfile))))
      

(defun ecaradio-measure-files (tracklist)
  (eci-cs-disconnect)
  (eci-cs-add "ecaradio-measure")
  (let ((counter 1)
	(track-files (mapcar (lambda (track)
			       (getf track :file))
			     tracklist))
	(lengths nil))
    (dolist (file track-files)
      (eci-c-add (format "%d" counter))
      (incf counter)
      (eci-ai-add file)
      (eci-ao-add "null"))
    (eci-start)
    (sit-for 5)
    (dolist (file track-files)
      (eci-ai-select file)
      (push (eci-ai-get-length) lengths))
    (reverse lengths)))
    

      
(defun ecaradio-render (outfile tracklist &optional measurements)
  (let ((counter 0)
	(position 0.0)
	(xfade-position 0.0)
	(track-files (mapcar (lambda (track)
			       (getf track :file))
			     tracklist))
	(lengths (or measurements (ecaradio-measure-files tracklist))))
    (eci-cs-disconnect)
    (eci-cs-add "ecaradio-render")
    (dolist (file track-files)
      (let ((ewf-file (concat file ".ewf")))
	;;
	;; overlap tracks by half the crossfade length
	(setf xfade-position (- position (/ ecaradio-crossfade-length 2.0)))
	(ecaradio-make-ewf ewf-file :source file :offset (if (< xfade-position 0)
							     position
							   xfade-position))
	(eci-c-add (format "%d" counter))
	(eci-ai-add ewf-file)
	(eci-ao-add "null")
	;; do cross-fade
	(eci-cop-add "-ea:0")
	(let* ((fadein-start xfade-position)
	       (fadein-end (+ ecaradio-crossfade-length fadein-start))
	       (fadeout-start (+ xfade-position (- (nth counter lengths) ecaradio-crossfade-length)))
	       (fadeout-end (+ fadeout-start ecaradio-crossfade-length))
	       (static-ewf-file (format "%s-%d.ewf" ecaradio-static-source counter)))
	  (eci-ctrl-add (format "-klg:1,0,100,4,%f,0,%f,1.0,%f,1.0,%f,0.0" 
				fadein-start fadein-end fadeout-start fadeout-end))
	  ;; add static
	  (when ecaradio-use-static
	    (ecaradio-make-ewf static-ewf-file :source ecaradio-static-source :offset fadeout-start
			       :start-position (random 40)
			       :length ecaradio-crossfade-length)
	    (eci-c-add (format "static-%d" counter))
	    (eci-ai-add static-ewf-file)
	    (eci-ao-add "null")
	    (eci-cop-add "-erc:1,2")
	    (eci-cop-add "-ea:0")
	    (eci-ctrl-add (format "-klg:1,0,%f,4,%f,0,%f,1.0,%f,1.0,%f,0.0"
				  ecaradio-static-amp-percentage
				  fadeout-start (+ fadeout-start 2.5)
				  (+ fadeout-start 5.0) (+ fadeout-start 10.0)))
	  ))
	;;
	(setf position (- (+ position (nth counter lengths)) (/ ecaradio-crossfade-length 2.0)))
	(incf counter)))
    (eci-c-select-all)
    (eci-ao-add outfile)
    (eci-ao-select outfile)
    (eci-ao-attach)
    (eci-cs-option "-z:mixmode,sum")))
      
	
	

      







  


(provide 'ecaspace)
;;; ecaspace.el ends here
