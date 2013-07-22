(require 'dash)
(require 'tools)
(require 'ert)

;;;;;;;;;;;;
;; Sequences

(ert-deftest seq-returns-the-sequence-itself ()
  (should (equal '(1)
                 (sequence '(1))))
  (should (equal [1]
                 (sequence [1])))
  (should (equal "1"
                 (sequence "1"))))

(ert-deftest seq-turns-non-sequences-into-a-list ()
  (should (equal '(1)
                 (sequence 1)))
  (should (equal '(toto)
                 (sequence 'toto))))

(ert-deftest I-can-substract-two-vectors ()
  (should (equal ["ABC"]
                (seq-difference ["ABC" "DEF"] ["DEF"]))))

(ert-deftest retrieving-a-random-element ()
  (should (equal "ABC"
                (random-elt ["ABC"]))))

(ert-deftest reducing-with-a-lambda ()
  (should (equal 5
                 (reduce (lambda (x y) (+ x y)) [2 3]))))

;;;;;;;;;;;;;
;; Functional

(ert-deftest currying-the-plus-function ()
  (should (equal 5
                 (funcall (-partial '+ 2) 3))))

(ert-deftest using-curry-in-map ()
  (should (equal  [2 4 6]
                  (map 'vector (-partial '* 2) [1 2 3]))))

(ert-deftest using-curry-in-filter ()
  (should (equal [2 3]
                (remove-if-not (-partial '< 1) [1 2 3]))))

(lexdef lexdef-test (x)
        (lambda () x))

(defun defun-test (x)
  (lambda () x))

(ert-deftest lexdef-has-lexical-scoping ()
  (let ((lexical (lexdef-test 1))
        (dynamic (defun-test 1))
        (x 2))
    (assert (equal 1
                   (funcall lexical))
    (assert (equal 2
                   (funcall dynamic))))))

;;;;;;;;;;;;;;
;; Trush tests

(ert-deftest trush-x-is-x ()
  (should (equal 1
                 (-> 1))))

(ert-deftest applying-trush-to-one-form ()
  (should (equal 5.0
                 (-> 25 sqrt))))

(ert-deftest applying-trush-to-two-forms ()
  (should (equal '(- (-> 25 sqrt) 2)
                 (macroexpand '(-> 25 sqrt (- 2)))))
  (should (equal '(- (sqrt 25) 2)
                 (macroexpand-all '(-> 25 sqrt (- 2))))))

(ert-deftest chaining-trush-combinators ()
  (should (equal "-2.0"
                 (-> 25 sqrt (->> (- 3) number-to-string)))))

(ert-deftest threading-three-forms ()
  (should (equal '(* 6 (+ 4 5 (- 2 3 x)))
                 (macroexpand-all '(->> x (- 2 3) (+ 4 5) (* 6))))))

;; (ert-deftest trush-with-a-lambda ()
;;   (assert (equal 10
;;               5 (lambda (x) x + x)))))

(ert-deftest nil-safe-trush-tests ()
  (should (equal 5.0
                 (-?> 25 sqrt)))
  (should (equal nil
                 (-?> nil sqrt)))
  (should (equal 5.0
                 (-?> 25 (sqrt))))
  (should (equal nil
                 (-?> nil (sqrt))))
  (should (equal 3.0
                 (-?> 25 sqrt (- 2))))
  (should (equal nil
                 (-?> 25 null (- 2)))))

(ert-deftest nil-safe-trush-at-end-tests ()
  (should (equal 5.0
                 (-?>> 25 sqrt)))
  (should (equal nil
                 (-?>> nil sqrt)))
  (should (equal 5.0
                 (-?>> 25 (sqrt))))
  (should (equal nil
                 (-?>> nil (sqrt))))
  (should (equal -3.0
                 (-?>> 25 sqrt (- 2))))
  (should (equal nil
                 (-?>> 25 null (- 2)))))

(ert-deftest comment-returns-nil ()
  (should (equal nil
                 (comment 3))))

(ert-deftest comment-does-nothing ()
  (let ((x 1))
    (setq x 2)
    (comment
     (setq x 3))
    (should (equal 2 x))))

;;;;;;;;
;; Maths

(ert-deftest variance-of-a-const-list-returns-0 ()
  (->> [[1 1 1]
        [2 2 2 2 2]
        [3 3 3 3 3 3 3]]
       (mapcar (lambda (v)
                 (should (equal 0.0 (variance v)))))))

(ert-deftest variance-of-a-simple-list ()
  (should (< (- (/ 2 3.0) (variance [1 2 3]))
             0.00000000001)))

;;;
;; Alist functions

(ert-deftest I-can-get-a-value-by-key-in-an-alist ()
  (let ((h '((a . 1)(b . 2))))
    (should (equal 1
                   (alist-get h 'a)))
    (should (equal 2
                   (alist-get h 'b)))
    (should (equal nil
                   (alist-get h 'z)))
    (should (equal 26
                   (alist-get h 'z 26)))))

(ert-deftest I-can-remove-a-value-in-an-alist ()
  (let ((h '((a . 1)(b . 2)(a . 3))))
    (should (equal nil
                   (-> h (alist-remove 'a) (alist-get 'a))))))

(ert-deftest I-can-set-a-value-in-an-alist ()
  (let ((h '((a . 1)(b . 2)(a . 3))))
    (should (equal '((a . 5)(b . 2))
                   (-> h (alist-set 'a 5))))
    (should (equal '((a . 1)(b . 2)(a . 3))
                   h))))

;;;;;;;;;;
;; Strings

(ert-deftest string-equal-can-compare-strings-to-symbols ()
  (should (string= 'hello "hello"))
  (should-error (string= 1 "1")))

(ert-deftest an-empty-string-is-empty--a-non-empty-string-is-not-empty ()
  (should (not (string-empty-p "hello")))
  (should (not (string-empty-p " ")))
  (should (string-empty-p (copy-seq "")))
  (should (string-empty-p nil))
  (should-error (string-empty-p 1)))

(ert-deftest I-can-tell-if-a-sring-is-empty ()
  (should (string-not-empty-p "hello"))
  (should (not (string-not-empty-p "")))
  (should (not (string-not-empty-p nil)))
  (should-error (string-not-empty-p 1)))

(ert-deftest symbols-are-non-empty ()
  (should (not (string-empty-p 'hello))))

(ert-deftest an-empty-string-or-a-string-containing-only-whitespaces-is-blank ()
  (should (string-blank-p ""))
  (should (string-blank-p nil))
  (should (string-blank-p " \t\n")))

(ert-deftest a-string-containing-non-whitespace-chars-is-not-blank ()
  (should (string-not-blank-p "h"))
  (should (string-not-blank-p "hello"))
  (should (string-not-blank-p " hello"))
  (should (string-not-blank-p "hello "))
  (should (string-not-blank-p " hello ")))

;; Test for s-blank?, from the 's library
(ert-deftest an-s-blank-string-is-nil-or-empty()
  (should (s-blank? (copy-seq "")))
  (should (s-blank? nil))
  (should (not (s-blank? " "))) ;; This is unfortunate...
  )

;;;;;;;;;;;;;;;;;;;;;
;; Destructuring bind

(ert-deftest destructuring-nested-lists ()
  (destructuring-bind (a (b c)) '(1 (2 3))
    (should (equal 1 a))
    (should (equal 2 b))
    (should (equal 3 c))))

(ert-deftest destructuring-alists-overlooks-keys ()
  (destructuring-bind ((x . a)(y . b)) '((y . 2)(x . 1))
    (should (equal 2 a))
    (should (equal 'y x))))

(ert-deftest destructuring-can-use-nested-keywords ()
  (destructuring-bind (a (x &key y z)) '(1 (2 :z 4 :y 3))
    (should (equal 1 a))
    (should (equal 2 x))
    (should (equal 3 y))
    (should (equal 4 z))))

(ert-deftest destructuring-can-t-use-hashtables ()
  (let ((h (make-hash-table)))
    (puthash :a 1 h)
    (should-error (destructuring-bind (&key a) h))))

(ert-deftest destructuring-can-t-use-vectors ()
    (should-error (progn
                    (destructuring-bind [a] [1]
                      a)))
    (should-error (progn
                    (destructuring-bind (a [b]) (1 [2])
                      b))))

(defun destructest (a (b c))
  (vector a b c))

(ert-deftest defun-can-t-destructure ()
  (should-error (destructest 1 '(2 3))))

(defun* destructest* (a (b c))
  (vector a b c))

(ert-deftest defun*-can-destructure ()
  (should (equal [1 2 3]
                 (destructest* 1 '(2 3)))))
