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

(defun string-blank-p (str)
  (if (string-empty-p str)
      t
    (not (null (string-match "^\\(?:\s*\n\\)*$" str)))))

(defun string-not-blank-p (str)
  (not (string-blank-p str)))

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


;; (defun -> (&rest args)
;;   (reduce (lambda (x y) (y x))
;;           (sequence args)))

(defmacro -> (x &optional form &rest more)
  (cond ((not (null more))
         `(-> (-> ,x ,form) ,@more))
        ((not (null form))
         (if (sequencep form)
             `(,(first form) ,x ,@(rest form))
           (list form x)))
        (t x)))

(defmacro ->> (x form &rest more)
  (cond ((not (null more)) `(->> (->> ,x ,form) ,@more))
        (t (if (sequencep form)
               `(,(first form) ,@(rest form) ,x)
             (list form x)))))

(defmacro -?> (x form &rest more)
  (cond ((not (null more)) `(-?> (-?> ,x ,form) ,@more))
        (t (if (sequencep form)
               `(if (null ,x) nil
                  (,(first form) ,x ,@(rest form)))
             `(if (null ,x) nil
                ,(list form x))))))

(defmacro -?>> (x form &rest more)
  (cond ((not (null more)) `(-?>> (-?>> ,x ,form) ,@more))
        (t (if (sequencep form)
               `(if (null ,x) nil
                  (,(first form) ,@(rest form) ,x))
             `(if (null ,x) nil
                ,(list form x))))))

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
