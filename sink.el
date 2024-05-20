;;; sink.el --- Receive messages from the plan9 plumber -*- lexical-binding: t -*-

;; Homepage: https://github.com/alcah/sink.el

;; Package-Version: 1.0.0

;; Package-Requires: ((emacs "25.1"))

;; This is free and unencumbered software released into the public domain.

;;; Commentary:
;; sink-mode is a global minor mode enabling Emacs to recieve and respond to
;; messages from the plan9 plumber via the latter's ports interface.

;;; Code:
(defun sink-plumb (string)
  "Send STRING to the plumber."
  (interactive "M")
  (shell-command (format "plumb -s emacs -w %s %s"
                         (shell-quote-argument default-directory)
                         (shell-quote-argument string))))
(defun sink-plumb-dwim (start end)
  "Send either the active region, or the filename at point to the plumber."
  (interactive "r")
  (sink-plumb (if (region-active-p)
                  (buffer-substring-no-properties start end)
                (thing-at-point 'filename))))

(defun sink-default-edit-handler (pmsg)
  "Open file described by PMSG for editing."
  (let ((buf (find-file-noselect (sink-pmsg-data pmsg))))
    (pop-to-buffer buf)
    (when (sink-pmsg-attr-addr pmsg)
      (goto-char (point-min))
      (forward-line (1- (sink-pmsg-attr-addr pmsg))))))

(defvar sink--tracked-ports '()
  "List of processes `sink-mode' is currently tracking.")
(defvar sink-port-alist '(("edit" . sink-default-edit-handler))
  "List of port-name . function pairs.
While `sink-mode' is active function will be evaluated whenever a
message is received on port-name.

port-name is a string specifying a plumber port without leading
namespaces.  eg. \"edit\"

function is a unary function accepting a pmsg argument.

Note: sink-mode reads sink-port-alist when activated and needs to
be restarted if this variable is modified")

(defun sink-pmsg (src dst wdir type attr ndata data)
  "Return a new pmsg structure."
  (list src dst wdir type attr ndata data))
(defun sink-pmsg-src (msg)
  "Source of message."
  (car msg))
(defun sink-pmsg-dst (msg)
  "Destination port."
  (cadr msg))
(defun sink-pmsg-wdir (msg)
  "Working directory."
  (nth 2 msg))
(defun sink-pmsg-type (msg)
  "Type of data."
  (nth 3 msg))
(defun sink-pmsg-attr (msg)
  "Attribute list."
  (nth 4 msg))
(defun sink-pmsg-ndata (msg)
  "Number of data bytes."
  (nth 5 msg))
(defun sink-pmsg-data (msg)
  "Message data."
  (nth 6 msg))

(defun sink-pmsg-attr-addr (pmsg)
  "Return the addr attribute of PMSG as an integer.
Else nil if addr does not exist or is unspecified."
  (let ((addr (assoc "addr" (sink-pmsg-attr pmsg))))
    (when (and addr (not (string-empty-p (cdr addr))))
      (string-to-number (cdr addr)))))

(defun sink--attr->pair (str)
  "Return a pair of attributes from STR."
  (let ((attr (split-string str "=")))
    (cons (car attr) (cadr attr))))

(defun sink--string->pmsg (str)
  "Parse STR into a pmsg."
  (let ((smsg (split-string str "\n")))
    (sink-pmsg (pop smsg)
              (pop smsg)
              (pop smsg)
              (pop smsg)
              (mapcar #'sink--attr->pair (split-string (pop smsg) " "))
              (string-to-number (pop smsg))
              (string-join smsg "\n"))))

;; TODO: Output from asynchronous processes is not guaranteed to arrive as
;; complete, discrete messages. Large messages may be split over 2 or more calls
;; to filter.  Could attach an input buffer to each port.
(defun sink--make-filter (func)
  "Return a process filter function for 9p output.
Resulting function coerces msg string to a pmsg structure and
passes it to FUNC"
  (lambda (_process msg)
    (funcall func (sink--string->pmsg msg))))

(defun sink--close-port (proc)
  "Kill PROC."
  (when (process-live-p proc)
    (interrupt-process proc)))

(defun sink--open-port (ppair)
  "return a new 9p process from the given port pair
ppair is name . function"
  (make-process :name (concat "9p " (car ppair)) :buffer 'nil
                :command (list "9p" "read"
                               (concat "plumb/" (car ppair)))
                :connection-type 'pipe
                :filter (sink--make-filter (cdr ppair))))

;;;###autoload
(define-minor-mode sink-mode
  "Toggle sink-mode.\n
While sink-mode is active Emacs will listen and respond to
messages from the plan9 plumber"
  :group 'sink
  :init-value nil
  :lighter " ╚╦"
  :keymap nil
  :global t
  (if sink-mode
      (progn (mapc #'sink--close-port sink--tracked-ports)
             (setq sink--tracked-ports (mapcar #'sink--open-port sink-port-alist)))
    (setq sink--tracked-ports (mapcar #'sink-close-port sink--tracked-ports))))

(provide 'sink)

;;; sink.el ends here
