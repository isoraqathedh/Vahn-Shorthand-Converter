(defpackage info.isoraqathedh.vahn-expander
  (:use :cl :plump-parser :cl-slice)
  (:nicknames :vahn-expander))

(in-package :vahn-expander)
(define-matcher slash (is #\/))

(defun alist->hash-table (alist &key (test 'eql))
  (loop with out = (make-hash-table :test test)
        for (key . val) in alist
        do (setf (gethash key out) val)
        finally (return out)))

(defparameter *logograms*
  (alist->hash-table '((#\K . "kah") (#\R . "rar") (#\T . "tor") (#\P . "poo") (#\S . "suh")
                       (#\D . "deu") (#\F . "fee") (#\G . "gih") (#\H . "hai") (#\J . "jeh")
                       (#\L . "laiy")(#\Z . "zoiy")(#\V . "vah") (#\B . "bar") (#\N . "nor")
                       (#\M . "moo")))
  "A hash table of all characters which combine through portmanteau from the first letter")

(defparameter *dual-logograms*
  (alist->hash-table '((#\A . "jarehr")
                       (#\U . "saruhr")
                       (#\O . "paroor")))
  "A hash table of all characters which combine through portmanteau from the first 3 letters")

(defparameter *postfix-ideograms*
  (alist->hash-table '((#\k . "k")    (#\d . "th")   (#\a . "wa")   (#\n . "n")   (#\m . "m")
                       (#\w . "w")    (#\l . "l")    (#\g . "ngi")  (#\t . "t")   (#\q . "ng")
                       (#\y . "ya")   (#\Y . "ya")   (#\c . "chi")  (#\C . "chi") (#\o . "ngol")
                       (#\b . "ngay") (#\i . "ngah") (#\r . "ngor")))
  "A hash table of all characters which combine at the end of the word, or at their position of being typed.")

(defparameter *infix-ideograms*
  (alist->hash-table '((#\h . "h") (#\x . "l")))
  "A hash table of all characters which combine after the initial of a three character cluster")

(defun at-end-p (needle haystack)
  "Determines if needle is at the end of haystack or not."
  (equal (search needle haystack :from-end t) (- (length haystack) (length needle))))

(defun get-word-props (current-value)
  (list :match-case      (loop for i-key being the hash-keys of *logograms*
                               for i-val being the hash-values of *logograms*
                               when (at-end-p i-val current-value)
                                 return (length i-val))
        :dual-match-case (loop for i-key being the hash-keys of *dual-logograms*
                               for i-val being the hash-values of *dual-logograms*
                               when (at-end-p i-val current-value)
                                 return (length i-val))
        :end-match-case  (loop for i-key being the hash-keys of *logograms*
                               for i-val being the hash-values of *logograms*
                               when (at-end-p (subseq i-val 1) (subseq current-value 1))
                                 return (1- (length i-val)))))

(defun expand-vahn (abbreviated-vahn)
  (with-output-to-string (out)
    (with-input-from-string (abbreviated-vahn)
      (loop with this-word = (make-array 10 :element-type 'base-char :fill-pointer 0 :adjustable t)
                                        ; Make a buffer string with an initial length of 10.
            for current-char = (read-char abbreviated-vahn nil :end-of-string)
            for match-case =      (loop for i being the hash-values of *logograms*
                                        when (at-end-p i this-word)
                                          return (length i))
            for dual-match-case = (loop for i being the hash-values of *logograms*
                                        when (at-end-p i this-word)
                                          return (length i))
            for end-match-case =  (loop for i-val being the hash-values of *logograms*
                                        when (at-end-p (subseq i-val 1) (subseq current-value 1))
                                          return (1- (length i-val)))
            do (flet ((flush (current-string)
                          (princ current-string out))
                      (clear-current-string ()
                        (setf this-word (make-array 10 :element-type 'base-char :fill-pointer 0 :adjustable t)))
                      (pop-many (&optional (n 1))
                        (loop repeat n
                              until (zerop (length this-word))
                              collect (vector-pop this-word)))
                      (push-into-buffer (thing)
                        (etypecase thing
                          (string (loop for i across thing do (vector-push-extend i this-word)))
                          (characer (vector-push-extend thing this-word)))))
                 (cond
                   ((char-equal this-word #\/)            ; Read a slash, now get into a special read-slash mode.
                    (when dual-match-case
                      (pop-many (- match-case 3)))
                    (push-into-buffer #\/)                ; Then push the slash into the buffer.
                    ;; Now, read as many characters as necessary and push them directly into the buffer, no parsing necessary.
                    (loop for char = (read-char abbreviated-vahn)
                          do (push-into-buffer char)
                          while (char/= char #\/)))
                   ((gethash current-char *logograms*)
                    (cond (dual-match-case ; Should there be a match for a double logogram at the end of the word
                           (pop-many (- dual-match-case 3)) ; Remove all but three characters in the matched word
                           (push-into-buffer (gethash current-char *logograms*))) ; Then put in the found one.
                          ((string-equal (gethash current-char *logograms*) (subseq 0 match-case)) ; Now if it matches the same thing in the buffer
                           (push-into-buffer (gethash current-char *logograms*))) ; Just put another copy into it
                          (t
                           (pop-many (- match-case 1)) ; Remove all but one character from the current buffer
                           (push-into-buffer (subseq (gethash current-char *logograms*) 1))))) ; And then add the missing ones.
                   ((gethash current-char *dual-logograms*)
                    (push-into-buffer
                     (subseq
                      (gethash current-char *dual-logograms*)
                      (if (member (peek-char nil abbreviated-vahn nil) (list nil #\Space))
                          3        ; Add the second half of the logogram if we're at the end of a wordd
                          0))))    ; Add the whole thing if not.
                   ((gethash current-char *postfix-ideograms*)
                    (when dual-match-case
                      (pop-many (- dual-match-case 3)))
                    (push-into-buffer (gethash current-char *postfix-ideograms*)))
                   ((gethash current-char *infix-ideograms*)
                    (unless end-match-case
                      (error "Invalid use of \"h\""))
                    ;; Slip the "h" in the middle. (ouch.)
                    (let ((final-word (concatenate 'string
                                                   (subseq this-word 0 end-match-case)
                                                   (gethash current-char *infix-ideograms*)
                                                   (subseq this-word end-match-case))))
                      (clear-current-string)
                      (push-into-buffer final-word)))
                   ((char-equal current-char #\.)) ; Do nothing for .
                   ((or (find current-char " ~")   ; These are end-of-word markers
                        (eql current-char :end-of-string))
                    (when (characterp current-char)
                      (push-into-buffer current-char)) ; Add the final character in
                    ;; Then flush and clear.
                    (flush this-word)
                    (clear-current-string))))
            until (eql current-char :end-of-string)))))
        
