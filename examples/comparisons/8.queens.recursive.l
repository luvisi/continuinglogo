
(defun (test.position.helper i)
  (if (> i count) (op 'true))
  (if (= new (item i rest)) (op 'false))
  (if (= new (+ (item i rest) (+ (- count i) 1)))
      (op 'false))
  (if (= new (- (item i rest) (+ (- count i) 1)))
      (op 'false))
  (test.position.helper (+ 1 i)))

(defun (test.position new count rest)
    (test.position.helper 1))

(defun (8.queens)
  (8.queens.helper 0 8 (array 8)))

(defun (8.queens.iterator i)
    (if (= i total) stop)
    (if (test.position i n sofar)
        (begin
          (setitem (+ n 1) sofar i)
          (8.queens.helper (+ n 1) total sofar)))
    (8.queens.iterator (+ i 1)))

(defun (8.queens.helper n total sofar)
    (if (= n total) (begin (print sofar) stop))

    (8.queens.iterator 0))

;(trace '(8.queens 8.queens.helper test.position test.position.helper))

(8.queens)
(bye)

