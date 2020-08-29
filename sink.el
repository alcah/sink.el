;; sink.el -- receive messages from the plan9 plumber -*- lexical-binding: t -*-

;; This is free and unencumbered software released into the public domain.

;;; code

(require 'subr-x)

(defvar sink-use-frames t
  "set to nil to open new buffers in windows rather than frames.
Used in the default edit handler")
(defvar sink--tracked-ports '()
  "list of processes sink-mode is currently tracking")
(defvar sink-port-alist '(("edit" . sink-default-edit-handler))
  "list of port-name . function pairs.
While sink-mode is active function will be evaluated whenever a
message is received on port-name")

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

(defun sink-pmsg-addr (pmsg)
  "return the addr attribute of pmsg as an integer
else nil if addr does not exist or is unspecified"
  (let ((addr (assoc "addr" (plumbmsg-attr pmsg))))
    (when (and addr (not (string-empty-p (cdr addr))))
      (string-to-number (cdr addr)))))

;; TODO: Error checking
;; Output from asynchronous processes is not guaranteed to arrive in
;; discrete chunks. Large messages may be split over 2 or more calls
;; to filter. Attach input buffer to each port?

;; NOTE: make-filter returns a function with func bound in its local
;; environment as a way of avoiding a lookup that would tie the code
;; to the state of any particular variable -- in this case port-alist.
(defun sink--make-filter (func)
  "return a 2-arg filter function for 9p output.
Returned function coerces msg string to a plumbmsg structure and
passes it to func"
  (lambda (process msg)
    (let ((plumbmsg (sink--string->plumbmsg msg)))
      (funcall func plumbmsg))))


(defun sink-close-port (proc)
  "kill the given process"
  (when (process-live-p proc)
    (interrupt-process proc)))

(defun sink-open-port (ppair)
  "return a new 9p process from the given port pair
ppair is name . function"
  (make-process :name (concat "9p " (car ppair)) :buffer 'nil
                :command (list "9p" "read"
                               (concat "plumb/" (car ppair)))
                :connection-type 'pipe
                :filter (sink--make-filter (cdr ppair))))

(define-minor-mode sink-mode
  "Toggle sink-mode\n
When sink-mode is active Emacs will listen
and respond to messages from the plan9 plumber"
  :init-value nil
  :lighter " ╚╦"
  :keymap nil
  :global t
  (if sink-mode
      (progn (mapc #'sink-close-port sink--tracked-ports)
             (setq sink--tracked-ports (mapcar #'sink-open-port sink-port-alist)))
    (setq sink--tracked-ports (mapcar #'sink-close-port sink--tracked-ports))))

(defun sink-default-edit-handler (pmsg)
  "default handler for messages on edit port"
  (let* ((fname (plumbmsg-data pmsg))
         (buf (get-file-buffer fname))
         (win (get-buffer-window buf t)))
    (if (and buf win)
        (select-window win)
      (if sink-use-frames
          (find-file-other-frame fname)
        (find-file-other-window fname)))
    (when (sink-pmsg-addr pmsg)
      (goto-line (sink-pmsg-addr pmsg)))))

(provide 'sink)

;;; sink.el ends here
