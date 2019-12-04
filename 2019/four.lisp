;;;; advent of code 2019 day 4 implementation in common lisp
;;;; takes the range as an argument to the program in the format `beginning-end`, inclusive

(defun validate-integer (i n)
  "validates an entire integer according to advent of code 2019 day 4"
  ;; i is the integer to validate
  ;; n is the number of digits in the integer
  (defun validate-digits (i n l p v s)
    "validates an individual digit in the six-digit number. zero-indexed"
    ;; i is the whole integer itself
    ;; n is the number of digits in the integer
    ;; l is the last digit
    ;; p is the previously gotten component (directly after mod)
    ;; v is t if there were at least one pair of consecutive matching digits
    ;; s is the state, it tracks the index in the integer
    (format t "~A ~A ~A ~A ~A ~A~C" i n l p v s #\linefeed)
    (if (= (+ s 1) n)
      (return-from validate-digits v))
    (let ((leftovers (mod i (expt 10 (+ s 1)))))
      (let ((digit (/ (- leftovers p) (expt 10 s))))
	(cond ((= digit l) (validate-digits i n digit leftovers t (+ s 1)))
	      ((> digit l) (if (= s 0)
			     (validate-digits i n digit leftovers v (+ s 1))
			     nil))
	      ((< digit l) (validate-digits i n digit leftovers v (+ s 1)))))))

  ;; start the recursive loop
  (validate-digits i n 0 0 nil 0))


(print (validate-integer 111111 6))
