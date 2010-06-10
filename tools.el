;;
;; Various tool functions & macros
;;

(defmacro comment (&rest body)
  "Ignores body, yields nil"
  nil)

;;;;;;;
;; Math

(defun mean (values)
  (/ (reduce '+ values)
     (float (length values))))

(defun square (x)
  (* x x))

(defun variance (values)
  (- (->> values (mapcar 'square) mean)
     (square (mean values))))

;;;;;;;;;;;;
;; Sequences

(defun sequence (maybe-seq)
  "Returns the value wrapped in a sequence if it is not a sequence already."
  (if (sequencep maybe-seq) maybe-seq
    (list maybe-seq)))

(defun random-elt (sequence)
  (elt sequence
       (-> sequence length random)))

(defun seq-difference (lseq rseq)
  (remove-if (lambda (element) (find element rseq :test 'equal))
             lseq))

;;;;;;;;;;
;; Strings

(defun string-empty-p (str)
  (if str
      (string= "" str)
    t))

(defun string-not-empty-p (str)
  (not (string-empty-p str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Undestructive alist functions

(defun alist-get (alist key &optional default)
  (or (assoc-default key alist)
      default))

(defun alist-remove (alist key)
  "Doesn't change the original alist, returns a new one instead."
  (remove-if (lambda (x) (equal key (car x)))
             alist))

(defun alist-set (alist key value)
  "Doesn't change the original alist, returns a new one instead."
  (cons (cons key value) (alist-remove alist key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure's Trush operators

(defmacro -> (result &rest body)
  (dolist (form body result)
    (setq form (sequence form)
          result (append (list (car form) result)
                         (cdr form)))))

(defmacro ->> (result &rest body)
  (dolist (form body result)
    (setq form (sequence form)
          result (append form (list result)))))

;;;;;;;;;;;;;;;;;;;;
;; Functional tools

(defmacro partial (f &rest args)
  `(lambda (&rest more)
     (apply ',f ,@args more)))

(defmacro lexdef (name args &rest body)
  "Defun with lexically-scoped parameters. Could also be called lexical-defun."
  `(defun ,name ,args
     (lexical-let ,(->> args
                        (remove-if (partial equal '&rest))
                        (mapcar (lambda (arg) (list arg arg))))
       ,@body)))

(provide 'tools)
