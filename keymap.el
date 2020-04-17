;; Author: Lee
;; https://github.com/loyalpartner/emacs-keymap
;; MIT License

;; Copyright (c) 2020 lee

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; TODO: 解析 Control x
(defcustom keymap-buffer-name "*keymap*"
  "The name of the buffer keymap")

(defvar keymap-modify-alist
  '((meta . "M-")
    (ctrl . "C-")
    (super . "s-")
    (ctrlmeta . "C-M-")
    (ctrlsuper . "C-s-")
    (metasuper . "M-s-")))

(defvar keymap-printable-chars
  (seq-concatenate 'list
   (number-sequence ?a ?z)
   (number-sequence ?A ?z)
   (number-sequence ?0 ?9)
   (number-sequence ? ?@ )
   (number-sequence ?\[ ?`)
   (number-sequence ?{ ?~)
   ))

(defun keymap-get-keybinding-alist (command)
  (cond ((not (listp command)) command )
	((= (length command) 3) (keymap-get-keybinding-alist (nth 1 command)))
	(t (cdr command))))

(defun keymap--to-string (command &optional keyname)
  ""
  (cond ((null command) ; command maybe nil, command name , mode keymap
	 (concat keyname " " (symbol-name command) "\n"))
	((not (listp command))
	 (concat keyname " " (symbol-name command) "\n"))
	(t
	 (seq-reduce
	  #'concat (mapcar
		    (lambda (keybingding)
		      (keymap--to-string (cdr keybingding)
					 (concat keyname " "
						 (key-description
						  (char-to-string (car keybingding))))))
		    (keymap-get-keybinding-alist command))
	  ""))))

(defun keymap-to-string (key)
  (keymap--to-string (key-binding (kbd key)) key))


(defun keymap-global-keys-to-string (modify-key)
  (let* ((mod-string (alist-get modify-key keymap-modify-alist))
	 (keys (mapcar (lambda (rawkey) (concat mod-string (char-to-string rawkey)))
		       keymap-printable-chars)))

    (seq-reduce #'concat
		(mapcar #'keymap-to-string keys)
		"")))

(defun keymap-list-keys (modify-key)
  (with-current-buffer (get-buffer-create keymap-buffer-name)
       (setq buffer-read-only nil)
       (erase-buffer)
       (insert (keymap-global-keys-to-string modify-key))
       (beginning-of-buffer)
       (pop-to-buffer keymap-buffer-name)))

(defun keymap-list-meta-keys ()
  (interactive)
  (keymap-list-keys 'meta))

(defun keymap-list-ctrl-keys ()
  (interactive)
  (keymap-list-keys 'ctrl))

(defun keymap-list-super-keys ()
  (interactive)
  (keymap-list-keys 'super))
