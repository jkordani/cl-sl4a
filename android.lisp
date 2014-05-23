;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(in-package :cl-android)

(defun my-getenv (name &optional default)
  #+CMU
  (let ((x (assoc name ext:*environment-list*
		  :test #'string=)))
    (if x (cdr x) default))
  #-CMU
  (or
   #+Allegro (sys:getenv name)
   #+(or CLISP ECL) (ext:getenv name)
   #+SBCL (sb-unix::posix-getenv name)
   #+LISPWORKS (lispworks:environment-variable name)
   #+ccl (ccl:getenv name)
   default))

;; (defvar *host* (my-getenv "AP_HOST"))
;; (defvar *port* (parse-integer (my-getenv "AP_PORT")))
;; (defvar *handshake* (my-getenv "AP_HANDSHAKE"))




(setq json::+json-lisp-symbol-tokens+
      '(("true" . t)
	;; By default cl-json encodes nil as "null", sl4a requires "[]"
	("[]" . nil)
	("null" . nil)
	("false" . nil)))



;; (setq *host* "localhost")
;; (setq *port* 60061)

(defvar *host* nil) 
(defvar *port* nil)

(defparameter *socket* nil)
(defparameter *stream* nil)

(defun target-device (ip port)
  ;; IP a string, port to running SL4A service
  (setq *host* ip)
  (setq *port* port))

(defun target-connect ()
  ;; opens socket to target
  (unless 
      (and *host* *port*) 
    (error "use target-device to pick an sl4a server!"))
  (setq *socket* (usocket:socket-connect *host* *port*))
  (setq *stream* (usocket:socket-stream *socket*)))

(defun target-close ()
  (usocket:socket-close *socket*)
  (setq *host* nil)
  (setq *port* nil))

;;(json:set-decoder-simple-clos-semantics)
(json:set-decoder-simple-list-semantics)
(setq json:*json-symbols-package* :android)

(defparameter *index* 0)

(defun execute (method &rest params)
  (format *stream*
	  (json:encode-json-to-string
	   (list (cons 'CL-ANDROID::ID *index*)
		 (cons 'METHOD method)
		 (cons 'CL-ANDROID::PARAMS
		       (when params params)))))

  (incf *index*)
  
  (terpri *stream*)
  (force-output *stream*)

  (let* ((response (read-line *stream*))
	 (decoded-response (with-input-from-string
			       (s response)
			     (json:decode-json s)))
	 (result (cdr (assoc 'CL-ANDROID::RESULT decoded-response)))
	 (err (cdr (assoc 'ERROR decoded-response))))
    (when err
      (let ((condition (make-condition 'simple-error
				       :format-control err)))
	(error condition)))
    result))


(defun test-dialog ()
  (execute "dialogCreateAlert" "Alert" "Alert Content")
  (execute "dialogSetPositiveButtonText" "Yes")
  (execute "dialogSetNegativeButtonText" "No")
  (execute "dialogSetNeutralButtonText" "Cancel")
  (execute "dialogShow")
  (execute "dialogGetResponse"))

(defun test-toast ()
  (execute "makeToast" "w00t!"))

(defun test-getinput ()
  (execute "dialogGetInput" "bob" "morane" "dsff"))
