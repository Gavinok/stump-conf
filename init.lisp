(in-package :stumpwm)

;;; Helper Functions

;; (defun show-key-seq (key seq val)
;;   (message (print-key-seq (reverse seq))))
;; (add-hook *key-press-hook* 'show-key-seq)

;; (float-window-move-resize
;;  (current-window)
;;  :x 90 :y 90
;;  :width 90
;;  :height 90)
;;; Theme
(setf *colors*
      '("#000000"   ;black
        "#BF6262"   ;red
        "#a1bf78"   ;green
        "#dbb774"   ;yellow
        "#7D8FA3"   ;blue
        "#ff99ff"   ;magenta
        "#53cdbd"   ;cyan
        "#b7bec9")) ;white

(update-color-map (current-screen))

;;; Basic Settings
(setf *mode-line-background-color* (car *colors*)
      *mode-line-foreground-color* (car (last *colors*))
      *mode-line-timeout* 1)

(setf *message-window-gravity* :center
      *input-window-gravity* :center
      *window-border-style* :thin
      *message-window-padding* 3
      *maxsize-border-width* 2
      *normal-border-width* 2
      *transient-border-width* 2
      stumpwm::*float-window-border* 1
      stumpwm::*float-window-title-height* 1)

;; Focus Follow Mouse
(setf *mouse-focus-policy* :sloppy)

;;; Startup Commands
(run-shell-command "xsetroot -cursor_name left_ptr")

;;; Bindings
(set-prefix-key (kbd "C-z"))


;; General Top Level Bindings
(define-key *top-map* (kbd "s-h") "move-focus left")
(define-key *top-map* (kbd "s-j") "move-focus down")
(define-key *top-map* (kbd "s-k") "move-focus up")
(define-key *top-map* (kbd "s-l") "move-focus right")
(define-key *top-map* (kbd "s-n") "pull-hidden-next")
(define-key *top-map* (kbd "s-p") "pull-hidden-previous")
(define-key *top-map* (kbd "s-TAB") "fnext")
(define-key *top-map* (kbd "s-ISO_Left_Tab") "fprev")

(setf *resize-increment* 25)
(define-key *top-map* (kbd "s-H") "move-window left")
(define-key *top-map* (kbd "s-J") "move-window down")
(define-key *top-map* (kbd "s-K") "move-window up")
(define-key *top-map* (kbd "s-L") "move-window right")
(define-key *top-map* (kbd "s-f") "fullscreen")

;;; Volume Stuff
(define-key *top-map* (kbd "s-C-a") "exec cm down 5")
(define-key *top-map* (kbd "s-C-f") "exec cm up 5")

;;; Terminal
(define-key *root-map* (kbd "y") "eval (term \"cm\")")
(define-key *root-map* (kbd "c") "term")
(defcommand term (&optional prg) ()
  (run-shell-command (if prg
                         (format nil "st -e ~A" prg)
                       "st")))

;; General Root Level Bindings
(define-key *root-map* (kbd "c") "exec st")
(define-key *root-map* (kbd "C-c") "exec st")
(define-key *root-map* (kbd "b") "windowlist")
(define-key *root-map* (kbd "e") "exec emacsclient -c -a 'emacs'")
(define-key *root-map* (kbd "C-e") "exec emacsclient -c -a 'emacs'")
(define-key *root-map* (kbd "w") "exec ducksearch")
(define-key *root-map* (kbd "r") "remove")
(define-key *root-map* (kbd "R") "iresize")
(define-key *root-map* (kbd "f") "fullscreen")
(define-key *root-map* (kbd "C-z") "send-raw-key")

;; floating
;; (define-key *root-map* (kbd "z") '*float-map*)
;; (defvar *float-map*
;;   (let ((m (make-sparse-keymap)))
;;     (define-key m (kbd "f") "float-this")
;;     (define-key m (kbd "u") "unfloat-this")
;;     m))

;;; Splits
(defcommand hsplit-and-focus () ()
  "create a new frame on the right and focus it."
  (hsplit)
  (move-focus :right))

(defcommand vsplit-and-focus () ()
  "create a new frame below and focus it."
  (vsplit)
  (move-focus :down))
(define-key *root-map* (kbd "v") "hsplit-and-focus")
(define-key *root-map* (kbd "s") "vsplit-and-focus")

;;; Mode-Line
(load-module "battery-portable")
(load-module "wifi")
;;Requires me to setup quicklisp

;; Get Fit
(defvar *reps* 0)
(defcommand add-reps (reps) ((:number "Enter reps: "))
  (setf *reps* (+ *reps* reps)))
(defcommand reset-reps () ()
  (setf *reps* 0))

(defvar *gym-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "a") "add-reps")
    (define-key m (kbd "r") "reset-reps")
    m))
(define-key *root-map* (kbd "ESC") '*gym-map*)

(setf *screen-mode-line-format*
      (list
       ;; Groups
       " ^7[^B^4%n^7^b]"
       ;; Pad to right
       "^>"
       '(:eval (if (> *reps* 0) (format nil "^1^B(Reps ~A)^n " *reps*)))
       ;; Date
       '(:eval (run-shell-command "date +\"%a, %b %d %I:%M%p\" | tr '\\n' ' '" t))
       " ^7[^n%B^7]^n "))

(mode-line)
;; (enable-mode-line (current-screen) (current-head) t)
;; turn on/off the mode line for the current head only.
(define-key *root-map* (kbd "B") "mode-line")

;; Font
(set-font "-xos4-terminus-medium-r-normal-*-14-*-*-*-*-*-*-*")

;;; Gaps
(load-module "swm-gaps")
(setf swm-gaps:*inner-gaps-size* 13
      swm-gaps:*outer-gaps-size* 7
      swm-gaps:*head-gaps-size* 7)
(when *initializing*
  (swm-gaps:toggle-gaps))
(define-key *groups-map* (kbd "g") "toggle-gaps")

;;; Remaps
(define-remapped-keys
  '(("(discord|Element)"
     ("C-a"   . "Home")
     ("C-e"   . "End")
     ("C-n"   . "Down")
     ("C-p"   . "Up")
     ("C-f"   . "Right")
     ("C-b"   . "Left")
     ("C-v"   . "Next")
     ("M-v"   . "Prior")
     ("M-w"   . "C-c")
     ("C-w"   . "C-x")
     ("C-y"   . "C-v")
     ("M-<"   . "Home")
     ("M->"   . "End")
     ("C-M-b" . "M-Left")
     ("C-M-f" . "M-Right")
     ("M-f"   . "C-Right")
     ("M-b"   . "C-Left")
     ("C-s"   . "C-f")
     ("C-k"   . ("C-S-End" "C-x"))
     ("C-j"   . "C-k"))))

;;; Keyboard Mouse Movement
(load-module "binwarp")
(binwarp:define-binwarp-mode *root-map* "X" (:map *root-map*)
    ((kbd "RET") "ratclick 1")
    ((kbd "SPC") "ratclick 3"))

;;; Undo And Redo Functionality
(load-module "winner-mode")
(define-key *root-map* (kbd "u") "winner-undo")
(define-key *root-map* (kbd "C-r") "winner-redo")
(add-hook *post-command-hook* (lambda (command)
                                (when (member command winner-mode:*default-commands*)
                                  (winner-mode:dump-group-to-file))))

(defun is-emacs-p (win)
  (string-equal (window-class win) "Emacs"))

(defun eval-emacslisp (expression)
  (run-shell-command (concat "emacsclient -e '" expression "'") t))

(defun emacs-winmove (direction)
  (eval-emacslisp (concat "(windmove-" direction ")")))

(emacs-winmove "left")

(eval-emacslisp "(window-left (window-normalize-window nil t))")

(window-normalize-window nil t)

(concat "exec ")
