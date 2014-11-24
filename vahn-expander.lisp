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

;; (defun read-vahn-word (string-stream &key logogram-at-end-of-input dual-logogram-at-end-of-input half-logogram-at-end-of-input)
;;   (loop while (peek)
;;         if (char-equal (peek) #\/)
;;           ;; Read the things between the slashes as-is
;;           do (consume)
;;              (format string-stream "~a" (consume-until :slash)) 
;;              (consume)
             
;;           ))

;; (defun expand-vahn-word (word)
;;   (with-output-to-string (string-stream)
;;     (with-lexer-environment (word)
;;       (read-vahn-word string-stream
;;                       :logogram-at-end-of-input
;;                       (loop for i being the hash-values of *logograms*
;;                               thereis (and (equal i (slice word (cons (- (length i)) nil)))
;;                                            (length i)))
;;                       :dual-logogram-at-end-of-input
;;                       (loop for i being the hash-values of *dual-logograms*
;;                               thereis (and (equal i (slice word (cons (- (length i)) nil)))
;;                                            (length i)))
;;                       :half-logogram-at-end-of-input
;;                       (loop for i being the hash-values of *logograms*
;;                               thereis (and (equal (slice i (cons 1 nil))
;;                                                   (slice word (cons (- -1 (length i)) nil)))
;;                                            (1- (length i))))))))
