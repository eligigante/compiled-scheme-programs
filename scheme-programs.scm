#lang scheme

(define (start)
  (let ((choice '()))
    (display "9487 Group 4 Scheme Projects\n")
    (display "1. Prelim")
    (newline)
    (display "2. Midterm")
    (newline)
    (display "3. Finals")
    (newline)
    (display "4. Exit")
    (newline)
    (display "Enter your choice (1-4): ")
    (set! choice (read))
    (case choice
      ((1) ())
      ((2) ())
      ((3) ())
      ((4) (display "Shutting Down") (exit))
      (else (display "Invalid choice. Please try again.\n")
        (start)))))

(display "Hello World)
(display "Finals Dice")
(display "Midterms Hans")

