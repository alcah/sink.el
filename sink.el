;;; sink.el --- Receive messages from the plan9 plumber -*- lexical-binding: t -*-

;; Homepage: https://github.com/alcah/sink.el

;; Package-Version: 1.0.0

;; Package-Requires: ((emacs "25.1"))

;; This is free and unencumbered software released into the public domain.

;;; Commentary:
;; sink-mode is a global minor mode enabling emacs to recieve and respond to
;; messages from the plan9 plumber via the latter's ports interface.

;;; Code:

(defun sink-plumb-dwim (start end)
  "Send either the active region, or the thing at point to the plumber"
  (interactive "r")
  (shell-command (format "plumb -s emacs -w %s '%s'"
                         default-directory
                         (if (region-active-p)
                             (buffer-substring start end)
                           (thing-at-point 'filename)))))

(defun sink-default-edit-handler (pmsg)
  "open file for editing at given line"
  (pop-to-buffer (plumbmsg-data pmsg))
  (when (plumbmsg-attr-addr pmsg)
    (goto-char (point-min))
    (forward-line (1- (plumbmsg-attr-addr pmsg)))))

(defvar sink--tracked-ports '()
  "list of processes sink-mode is currently tracking")
(defvar sink-port-alist '(("edit" . sink-default-edit-handler))
  "list of port-name . function pairs.
While sink-mode is active function will be evaluated whenever a
message is received on port-name.

port-name is a string specifying a plumber port without leading
namespaces.  eg. \"edit\"

function is a unary function accepting a plumbmsg argument

Note: sink-mode reads sink-port-alist when activated and needs to
be restarted if modified")

(defun plumbmsg (src dst wdir type attr ndata data)
  "return a new plumbmsg structure"
  (list src dst wdir type attr ndata data))
(defun plumbmsg-src (msg)
  "source of message"
  (car msg))
(defun plumbmsg-dst (msg)
  "destination port"
  (cadr msg))
(defun plumbmsg-wdir (msg)
  "working directory"
  (nth 2 msg))
(defun plumbmsg-type (msg)
  "type of data"
  (nth 3 msg))
(defun plumbmsg-attr (msg)
  "attribute list"
  (nth 4 msg))
(defun plumbmsg-ndata (msg)
  "number of data bytes"
  (nth 5 msg))
(defun plumbmsg-data (msg)
  "message data"
  (nth 6 msg))

(defun plumbmsg-attr-addr (pmsg)
  "return the addr attribute of pmsg as an integer
else nil if addr does not exist or is unspecified"
  (let ((addr (assoc "addr" (plumbmsg-attr pmsg))))
    (when (and addr (not (string-empty-p (cdr addr))))
      (string-to-number (cdr addr)))))

(defun sink--attr->pair (str)
  "Return a pair of attributes from the given string"
  (let ((attr (split-string str "=")))
    (cons (car attr) (cadr attr))))

(defun sink--string->plumbmsg (msg)
  "Return a plumbmsg from the given msg string"
  (let ((smsg (split-string msg "\n")))
    (plumbmsg (pop smsg)
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
  "return a 2-arg filter function for 9p output.
Returned function coerces msg string to a plumbmsg structure and
passes it to func"
  (lambda (process msg)
    (funcall func (sink--string->plumbmsg msg))))

(defun sink--close-port (proc)
  "kill the given process"
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
  "Toggle sink-mode\n
When sink-mode is active Emacs will listen
and respond to messages from the plan9 plumber"
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
