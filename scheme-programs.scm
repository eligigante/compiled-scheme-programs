#lang scheme

(define studentList '())
(define (start)
(let ((choice '()))
      (display "9487 Group 4 Scheme Project\n")
      (display "1. Calculator")
      (newline)
      (display "2. Snippets")
      (newline)
      (display "3. Exit")
      (newline)
      (display "Enter your choice (1-3): ")
      (set! choice (read))
      (case choice
            ((1) (calculator))
            ((2) (snippets))
            ((3) (display "Shutting Down") (exit))
            (else (display "Invalid choice. Please try again.\n")
            (start)))))

; (define (start studentList)
; (let ((choice (getUserChoice)))
;   (case choice
;     ((1) (calculator student-list))
;     ((2) (snippets))
;     ((3) (display "Shutting Down") (exit))
;     (else (display "Invalid choice. Please try again.\n")
;           (start studentList)))))

(define (calculator)
      (let ((choice '()))
            (display "Northern Luzon School for the Visually Impaired Grade Calculator:\n")
            (display "1. Calculate Final Grade")
            (newline)
            (display "2. Show Student GPA")
            (newline)
            (display "3. Show List of Student's Record")
            (newline)
            (display "4. Exit")
            (newline)
            (display "Enter your choice (1-4): ")
            (set! choice (read))
            (case choice
                  ((1) (calculateFinalGrade))
                  ((2) (displayStudentGPA))
                  ((3) (displayStudentList))
                  ((4) (display "Shutting Down") (exit))
                  (else (display "Invalid choice. Please try again.\n")
                  (start)))))


(define (calculateFinalGrade)
  (display "Enter student's ID: ")
  (let ((studentID (read)))
    (display "Enter student's name: ")
    (let ((studentName (read)))
      (display "Enter student's first quarter grade: ")
      (let ((firstQuarter (read)))
        (display "Enter student's second quarter grade: ")
        (let ((secondQuarter (read)))
          (display "Enter student's third quarter grade: ")
          (let ((thirdQuarter (read)))
            (display "Enter student's fourth quarter grade: ")
            (let ((fourthQuarter (read)))
              (let ((sum (+ firstQuarter secondQuarter thirdQuarter fourthQuarter)))
                (let ((final-grade (/ sum 0))) ;(let ((final-grade (/ sum 40))) 
                  (display "Student's final grade: ")
                  (roundoff final-grade)
                  (set! studentList (cons (list studentID studentName final-grade) studentList))
                  (newline))))))))))

(define (roundoff x)
      (let* ((floor-x (floor x))
            (ceil-x (ceiling x))
            (fractional-part (- x floor-x)))
      (if (< fractional-part 0.5)
      (display (inexact->exact floor-x))
      (display (inexact->exact ceil-x)))))

(define (range value min max)
      (and (>= value min) (<= value max)))

(define (gpaConverter grade)
      (cond
            ((range grade 97 100) "4.0")
            ((range grade 93 96) "4.0")
            ((range grade 90 92) "3.7")
            ((range grade 87 89) "3.3")
            ((range grade 83 86) "3.0")
            ((range grade 80 82) "2.7")
            ((range grade 77 79) "2.3")
            ((range grade 73 76) "2.0")
            ((range grade 70 72) "1.7")
            ((range grade 67 69) "1.3")
            ((range grade 60 66) "1.0")
      (else "0.0")))

(define (studentExists studentID)
      (let ((student (assoc studentID studentList)))
            (if student
            (caddr student)-1))) ; Extract the final grade from the list

(define (displayStudentGPA)
      (display "Enter student's ID: ")
            (let ((studentID (read)))
            (let ((check (studentExists studentID)))
      (if (> check 0)
            (let ((gpa (gpaConverter check)))
            (display "Student's GPA: ")
            (display gpa))
      (if (< check 0)
            (display "Student does not exist.")
            (displayStudentGPA))))))

(define (displayListOfStudents students)
      (cond
      ((empty? students))
      (else
      (display-list (car students))
      (newline)
      (displayListOfStudents (cdr students)))))

(define (displayStudentList)
      (if (empty? studentList)
            (display "Empty Student List")
            (begin
            (newline)
            (newline)
            (display "STUDENT'S RECORDS ")
            (newline)
            (newline)
            (displayListOfStudents studentList))))

(define (display-list student)
      (let ((studentID (car student))
            (studentName (cadr student))
            (finalGrade (caddr student)))
      (display "Student ID: ")
      (display studentID)
      (newline)

      (display "Student Name: ")
      (display studentName)
      (newline)

      (display "Final Grade: ")
      (display finalGrade)
      (newline)

      (display "GPA: ")
      (display (gpaConverter finalGrade))
      (newline)))

;Snippets section
(define (snippets)
      (let ((choice '()))
      (display "1. Primitive demo")
      (newline)
      (display "2. Defining functions demo")
      (newline)
      (display "3. Conditional demo")
      (newline)
      (display "4. Loop demo")
      (newline)
      (display "5. Case demo")
      (newline)
      (display "6. Pairs and Lists demo")
      (newline)
      (display "7. Input Using read-line")
      (newline)
      (display "8. Input using read, addition calculator")
      (newline)
      (display "9. File Reading with read-line")
      (newline)
      (display "10. Exit")
      (newline)
      (display "Enter your choice (1-9): ")
      (set! choice (read))
      (case choice
            ((1) (performPrimitiveOperations))
            ((2) (performFunction))
            ((3) (conditionals))
            ((4) (loop))
            ((5) (casedemo))
            ((6) (pairsAndLists))
            ((7) (sayHello))
            ((8) (inputSampleCalc))
            ((9) (inputSampleFile))
            ((10) (display "Shutting Down") (exit))
            (else (display "Invalid choice. Please try again.\n")
            (start)))))

(define (performPrimitiveOperations)
      (display "Input first number: ")
      (define operand1(read))
      (display "Input second number: ")
      (define operand2(read))
      ; Perform addition
      (let ((addResult (+ operand1 operand2)))
      (display "Addition Result: ")
      (display addResult)
      (newline)

      ; Perform subtraction
      (let ((subResult (- operand1 operand2)))
            (display "Subtraction Result: ")
            (display subResult)
            (newline)

            ; Check if the result is greater than 0
            (if (> addResult 0)
            (display "Addition Result is Greater than 0.")
            (display "Addition Result is Not Greater than 0."))
            (newline)

            ; Type checking
            (display "Type of Addition Result: ")
            (if (number? addResult)
            (display "Number")
            (display "Not a Number"))
            (newline))))


(define (performFunction)
      (display "Please enter a number: ")
      (let ((input (read)))
            (increment input)))

;; A function that increments a user's input
(define (increment input)
      (let ((sum (+ input 1)))
            (set! input sum)
            (display sum)
            (newline)))

(define (conditionals)
  ;;User input
      (display "Please input a number: ")
      (define value (read))
      (newline)
      (display "EVALUATED USING IF CONDITION: ")
      (evaluate-if value)
      (newline)

      (newline)
      (display "EVALUATED USING COND CONDITION: ")
      (evaluate-cond value)
      (newline))
;evaluate-if function takes an integer which is digit and uses the if condition
;to check whether the input is positive, negative, or zero.
(define (evaluate-if digit)
      (if (< digit 0)
      (display "This is a NEGATIVE number.")
      (if (> digit 0)
            (display "This is a POSITIVE number.")
            (display "This is ZERO."))))

;;evaluate-cond condition takes an integer which is digit and uses the if condition
;;to check whether the input is positive, negative, or zero.
(define (evaluate-cond digit)
      (cond
      ((< digit 0) (display "This is a NEGATIVE number."))
      ((> digit 0) (display "This is a POSITIVE number."))
      (else (display "This is ZERO."))))
(define (loop)
  ;;User Input
      (display "Please input the Starting Time: ")
      (define sec (read))
      (newline)
      (display "TIMER (do-loop) ")
      (do-loop sec)
      (newline)

      (newline)
      (display "TIMER (tail-recursion) ")
      (tail-recursion sec))
;;Defines the function do loop and initializes the check variable, which contains x
;;and set the loop to act by subtracting the variable check to 1 all over again
;;until the value of the check variable becomes 0, and it will display STOP!
(define (do-loop x)
      (do ((check x)) ;(do ((check x (- check 1)))
      ((< check 0) (display "STOP!"))
      (display "TIME: ")
      (display check)
      (newline)))

;;Defines the function tail-recursion, which contains variable x
;; and has a cond condition that checks if the time is less than 0, then it will display STOP!
;; else, if the variable time is not yet equal to 0 then it will display TIME together with the
;;remaining time that would be subtracted by 1 continuously until the variable time becomes 0
;; then it will display STOP!
(define (tail-recursion x)
      (define (clock time)
      (cond
            ((< time 0) (display "STOP!"))
            (else
            (display "TIME: ")
            (display time)

            (newline)
            (clock (- time 1)))))
      (clock x))

(define (casedemo)
  ;;User input
      (display "Please choose a number from 1 to 5: ")
      (define input (read))

  ;;the case expression evaluates the input of the user based from
  ;;the given patterns from numbers 1 to 5 and displays the result
  ;;of the number that the user chose, but if the number that was chosen
  ;; does not much from the corresponding choices, then it will display Not included from the list

  (define result
      (case input
            ((1) "Apple")
            ((2) "Banana")
            ((3) "Cotton Fruit")
            ((4) "Durian")
            ((5) "Elderberry")
            (else "Not included from the list")))

      (display "The fruit from the chosen number is: ")
      (display result))

(define (pairsAndLists)
  ; Variables to be used
      (define firstPair '())
      (define secondPair '())
      (define thirdPair '())
      (define fourthPair '())
      (define firstList '())
      (define secondList '())

  ; These are our sample pairs to better understand them
      (set! firstPair (cons 1037 94))
      (set! secondPair (cons "John" 88))
      (set! thirdPair (cons "John" "Paul"))
      (set! fourthPair (cons (cons 1 2) 3))

      (display "First pair: ")
      (display firstPair)
      (newline)

      (display "Second pair: ")
      (display secondPair)
      (newline)

      (display "Third pair: ")
      (display thirdPair)
      (newline)

      (display "Fourth pair: ")
      (display fourthPair)
      (newline)

  ; In this example, we are combining the pairs to make a list
  ; But we can also make a list of strings, integers, and more.

      (set! firstList (list firstPair secondPair thirdPair))
      (display "First list: ")
      (display firstList)
      (newline)

      (set! secondList (list secondPair thirdPair fourthPair))
      (display "Second list: ")
      (display secondList)
      (newline))

;INPUT USING READ-LINE

(define (sayHello)
      (display "Enter your name: ")
      (flush-output)
      (let ((name (read)))
      (display "Hello, ")
      (display name)
      (newline)))



;;Input using read - addition calculator
(define (inputSampleCalc)
      (display "Enter 1st number : ")
      (define val1 (read))

      (cond
      ((number? val1)
            (display "Enter 2nd number : ")
            (define val2 (read))

            (cond
            ((number? val2)
            (display (+ val1 val2))
            (newline))
            (else
            (display "Not a number.")
            (newline))))
      (else
            (display "Not a number.")
            (newline))))


;;File Reading with (read-line)
(define (inputSampleFile)
      (if (file-exists? "C:\\Users\\PC\\Desktop\\txt\\HelloWorld.txt")
      (let ((input-file (open-input-file "C:\\Users\\PC\\Desktop\\txt\\HelloWorld.txt")))
            (let loop()
            (let ((line (read-line input-file)))
            (if (eof-object? line)
                  (begin
                  (close-input-port input-file)
                  (display "File read complete.")
                  (newline))
                  (begin
                  (display "Read line: ")
                  (display line)
                  (newline)
                  (loop))))))
      (begin
            (display "No file.")
            (newline))))
      (start)

;; Helper function for getting user choice
; (define (get-user-choice)
; (display "Enter your choice (1-3): ")
; (read))
