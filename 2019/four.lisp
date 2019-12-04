;;;; advent of code 2019 day 4 implementation in common lisp
;;;; takes the range as an argument to the program in the format `beginning-end`, inclusive

;;;; this makes the assumption that you are using sbcl, like i am

;; i don't feel like writing my own string separation function
(require 'uiop)

(defun validate-integer (i n)
  "validates an entire integer according to advent of code 2019 day 4"
  ;; i is the integer to validate
  ;; n is the number of digits in the integer
  (labels ((validate-digits (i n l p v s)
			  "validates an individual digit in the six-digit number. zero-indexed"
			   ;; i is the whole integer itself
			   ;; n is the number of digits in the integer
			   ;; l is the last digit
			   ;; p is the previously gotten component (directly after mod)
			   ;; v is t if there were at least one pair of consecutive matching digits
			   ;; s is the state, it tracks the index in the integer
			   (format t "~A ~A ~A ~A ~A ~A~C" i n l p v s #\linefeed)
			   (if (= s n)
			     (return-from validate-digits v))
			   (let ((leftovers (mod i (expt 10 (+ s 1)))))
			     (let ((digit (/ (- leftovers p) (expt 10 s))))
			       (cond ((= digit l) (validate-digits i n digit leftovers t (+ s 1)))
				     ((> digit l) (if (= s 0)
						    (validate-digits i n digit leftovers v (+ s 1))
						    nil))
				     ((< digit l) (validate-digits i n digit leftovers v (+ s 1))))))))
    ;; start the recursive loop
    (validate-digits i n 0 0 nil 0)))

(defun main ()
  (let ((range-as-string (uiop:split-string (second *posix-argv*) :separator "-")))
    (if (not range-as-string)
      (format t "no range was provided~C" #\linefeed)
      (format t "password-count=~A~C" (length (labels ((iterate-sequence (b e i s)
				 "iterates over the given integer sequence, and validates each integer with validate-integer
				 it returns a vector with each valid integer in it"
				 ;; b is the beginning of the sequence
				 ;; e is the end of the sequence
				 ;; i is the current offset in the sequence
				 ;; s is the vector containing each valid integer
				 (if (= (+ i b) (+ e 1))
				   (return-from iterate-sequence s))
				 (let ((c (+ i b)))
				   (if (validate-integer c 6)
				     (iterate-sequence b e (+ i 1) (concatenate 'vector #(c) s))
				     (iterate-sequence b e (+ i 1) s)))))
	(iterate-sequence (parse-integer (nth 0 range-as-string)) (parse-integer (nth 1 range-as-string)) 0 (vector)))) #\linefeed))))

(main)
