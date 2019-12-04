;;;; advent of code 2019 day 4 implementation in common lisp
;;;; takes the range as an argument to the program in the format `beginning-end`, inclusive

;;;; this makes the assumption that you are using sbcl, like i am

;; i don't feel like writing my own string separation function
(require 'uiop)

(defun validate-integer (i n)
  "validates an entire integer according to advent of code 2019 day 4 part 1"
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
			   (when (= s n)
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

(defun validate-integer-2 (i n)
  "validates an entire integer according to advent of code 2019 day 4 part 2"
  ;; i is the integer to validate
  ;; n is the number of digits in the integer
  (labels ((validate-digits-2 (i n l p s a)
			  "validates an individual digit in the six-digit number. zero-indexed"
			   ;; i is the whole integer itself
			   ;; n is the number of digits in the integer
			   ;; l is the last digit
			   ;; p is the previously gotten component (directly after mod)
			   ;; s is the state, it tracks the index in the integer
			   (when (= s n)
			     (return-from validate-digits-2 (reduce (lambda (r i) (if (= i 2) t r)) a :initial-value nil)))
			   (let ((leftovers (mod i (expt 10 (+ s 1)))))
			     (let ((digit (/ (- leftovers p) (expt 10 s))))
			       (setf (aref a digit) (+ (aref a digit) 1))
			       (if (> digit l)
				 (if (= s 0)
				   (validate-digits-2 i n digit leftovers (+ s 1) a)
				   nil)
				 (validate-digits-2 i n digit leftovers (+ s 1) a))))))
    ;; start the recursive loop
    (validate-digits-2 i n 0 0 0 (make-array 10 :initial-element 0))))

(defun iterate-sequence (b e i s v)
			 "iterates over the given integer sequence, and validates each integer with validate-integer
			 it returns a vector with each valid integer in it"
			 ;; b is the beginning of the sequence
			 ;; e is the end of the sequence
			 ;; i is the current offset in the sequence
			 ;; s is the amount of correct passwords
			 ;; v is the function to validate with
			 (when (= (+ i b) (+ e 1))
			   (return-from iterate-sequence s))
			 (let ((c (+ i b)))
			   (if (funcall v c 6)
			     (iterate-sequence b e (+ i 1) (+ s 1) v)
			     (iterate-sequence b e (+ i 1) s v))))
	
(defun main ()
  (let ((range-as-string (uiop:split-string (second *posix-argv*) :separator "-")))
    (if (not range-as-string)
      (format t "no range was provided~C" #\linefeed)
      (format t "password-count=~A, password-count (ignoring repititions >2)=~A~C"
	      (iterate-sequence
		(parse-integer (nth 0 range-as-string))
		(parse-integer (nth 1 range-as-string)) 0 0 'validate-integer)
	      (iterate-sequence
		(parse-integer (nth 0 range-as-string))
		(parse-integer (nth 1 range-as-string)) 0 0 'validate-integer-2)
	      #\linefeed))))

(main)
