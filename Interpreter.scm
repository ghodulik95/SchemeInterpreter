;George Hodulik EECS 345 Project 1
(load "verySimpleParser.scm")

;My interpreter does not implement the "extra challenge" features.
;My general code convention:
;Parameter code indicates code that could have come from the parser, ie, one or more statements contained in a list
;Parameter statement indicates one statement, not contained in an extra list
;Ex. code: ((var x 5) (var y 3) (if (< x y) (return x) (return y)))
;    statement: (var x 5)
;A code is a list of statements
;An expression is an arithmetic operation, ie it is a value, a var, or aritmetic operations of values and/or vars.
;Ex. expressions: x, y, (+ x y), (* x (+ 4 y))

;takes code and environment and returns the environment or the environment of the next statememnt
(define Mstate
  (lambda (code environment)
    (cond
      ((null? code) environment)
      (else (Mstatement code environment)))))
                    
;takes code an an environment and acts upod the nextstatement
(define Mstatement
  (lambda (code environment)
    (cond
      ;if this is reached, then there was no return statement in the program
      ((null? code) environment)
      ;calls if statement if nextstatement is if: note, remaining statements is a parameter, since the if statement could have a return, or it could continue
      ((if? (nextstatement code)) (evaluate-if (nextstatement code) environment (remainingstatements code)))
      ;calls return if the nextstatement is an if
      ((return? (nextstatement code)) (return (nextstatement code) environment))
      ;calls assign if the nextstatement is an assignment
      ((=? (nextstatement code)) (Mstate (remainingstatements code) (assign (getvarname (nextstatement code)) (getvarvalue (nextstatement code)) environment environment 'assign)))
      ;calls declarevar if the next statement is a declare
      ((declaration? (nextstatement code)) (Mstate (remainingstatements code) (declarevar (nextstatement code) environment))))))

;gets the nextstatement from code. returns a statement
(define nextstatement
  (lambda (code)
    (car code)))

;gets the remaining assignments from code, returns a code
(define remainingstatements
  (lambda (code)
    (cdr code)))


;checks if a statement is an if statement
(define if?
  (lambda (statement)
    (cond
      ((eq? (car statement) 'if) #t) 
      (else #f))))

;checks if a staement is an assignment statement
(define =?
  (lambda (statement)
    (cond
      ((eq? (car statement) '=) #t) 
      (else #f))))

;checks if a statement is a declaration
(define declaration?
  (lambda (statement)
    (cond
      ((and (eq? (car statement) 'var) ) #t) ;(null? (cddr statement)) 
      (else #f))))

;checks if a statement is a return statement
(define return?
  (lambda (statement)
    (cond
      ((eq? (car statement) 'return) #t) 
      (else #f))))

;calls assignment when theres a declaration, with parameter assigntype of 'declaration
(define declarevar
  (lambda (statement environment)
    (cond
      ;if this was of the form (var x) with no value, call assign with value of 'error
      ;note (cddr statement) is used to see if there is a value given: this may need to be changed if parser changes
      ((null? (cddr statement)) (assign (getvarname statement) 'error environment environment 'declaration))
      ;else call assign with the value 
      (else (assign (getvarname statement) (getvarvalue statement) environment environment 'declaration)))))

;so far can do complicated assignments with one =, but can not do nested assignments with multiple ='s
;assigntype is 'declarataion if this is coming from a (var x expression) value
;assigntype is 'assign if it is just an (= x expression)
;note that there are two environment parameters: environment is searched through to find the var while old-environment does not change
;This is because an assignment value may need to be evaluated, and if it has other variables in the expression, eval-expression needs
;that environment to properly determine a value
(define assign
  (lambda (varname value environment original-environment assigntype)
    (cond
      ;if the environment is empty and this is an assignment, then the variable was not declared
      ((and (null? environment) (eq? assigntype 'assign)) (error "variable not declared"))
      ;if the environment is empty and this is a declaration w/o assignment value, then add the variable with value 'error
      ((and (null? environment) (eq? assigntype 'declaration) (eq? value 'error) (cons (list varname 'error) '())))
      ;if this variable has a non-error assignment value, evaluate the value and assign it
      ((and (null? environment) (eq? assigntype 'declaration)) (cons (list varname (eval-expression value original-environment pprefix)) '()))
      ;if the var is reached but this is a declaration, the variable is being redeclared
      ((and (eq? (caar environment) varname) (eq? assigntype 'declaration) (error "redefining of variable")))
      ;if the var is reached otherwise, update its value
      ((eq? (caar environment) varname) (cons (list varname (eval-expression value original-environment pprefix)) (cdr environment)))
      ;otherwise recurse
      (else (cons (car environment) (assign varname value (cdr environment) original-environment assigntype))))))

;gets varname from statement: expects statement in form of (var x value) where x is varname and value is a value or expression
(define getvarname
  (lambda (statement)
    (cadr statement)))

;gets value from statement: expects statement in form of (var x value) where x is varname and value is a value or expression
(define getvarvalue
  (lambda (statement)
    (caddr statement)))

;retuns the value of a variable, '() if it is not declared
(define lookup
  (lambda (var environment)
    (cond
      ((null? environment) '())
      ((eq? (caar environment) var) (cadar environment))
      (else (lookup var (cdr environment))))))

;returns the return expression in a return statement
(define returnexpression
  (lambda (statement)
    (cadr statement)))

;returns the value of the return expression
(define return
  (lambda (code environment)
    (make-english (eval-expression (returnexpression code) environment pprefix))))

;translates scheme words to "English," for now this is only for #t and #f to 'true and 'false
(define make-english
  (lambda (expression)
    (cond
      ((eq? expression #t) 'true)
      ((eq? expression #f) 'false)
      (else expression))))

;evaluates an if statement
(define evaluate-if
  (lambda (ifstatement environment remainingcode)
    (cond
      ;checks if the if statement is true, returns the state ofthe expression if it is
      ((eval-expression (ifcond ifstatement) environment pprefix) (Mstate (cons (nextstatement (ifexpr ifstatement)) remainingcode) environment))
      ;if the condition was false and there is no else, the continue with the remaining code
      ((null? (ifelse ifstatement)) (Mstate remainingcode environment))
      ;if there was an else, then Mstate with the else's if statement
      (else (Mstate (cons (nextstatement (ifelse ifstatement)) remainingcode) environment)))))

;returns the condition expression in the if statement
(define ifcond
  (lambda (ifstatement)
    (cadr ifstatement)))

;returns the statement of the if
(define ifexpr
  (lambda (ifstatement)
    (cddr ifstatement)))

;retunds the if of the else.  If there isn't one, this will be null
(define ifelse
  (lambda (ifstatement)
    (cdddr ifstatement)))

;This was built off of what was done in class.
;eval-expression will evaluate an expression, returning a number or #t or #f
;Note, this is Mvalue and Mboolean combined
; A MValue function that uses abstraction to
;allow expressions in prefix,
; postfix, or infix format
; Call as (eval-expression '(3 + 4) '() infix)
(define eval-expression
  (lambda (expression environment form)
    ((lambda (operator left-operand right-operand)
       (cond
         ;if the expression is 'error, that means an unitialized variable was being used (lookup returned 'error)
         ((eq? 'error expression) (error "error: Var not assigned a value"))
         ;if the expression is a number, return it
         ((number? expression) expression)
         ;if the expression is 'true, translate it to #t
         ((and (not (number? expression)) (eq? expression 'true)) #t)
         ;if the expression is 'false, translate it to #f
         ((and (not (number? expression)) (eq? expression 'false)) #f)
         ;if the expression is an undeclared var, return an error
         ((and (not (list? expression)) (not (number? expression)) (eq? (lookup expression environment) '()) (error "variable used before being declared")))
         ;if the expression is an uninitialized var, return an error
         ((and (not (list? expression)) (not (number? expression)) (eq? 'error (lookup expression environment))) (error "error: Var not assigned a value"))
         ;if the expression is an initialized var, return its value
         ((and (not (list? expression)) (not (number? expression))) (lookup expression environment))
         ;the next few lines perform arithmetic operations
         ((eq? '+ (operator expression)) (+ (eval-expression (left-operand expression) environment form) (eval-expression (right-operand expression) environment form)))
         ;THIS LINE BELOW MUST BE CHANGED IF PARSER IS NO LONGER PREFIX to allow negatives support ( like -(* 5 9))
         ((and (eq? '- (operator expression)) (null? (cddr expression))) (- 0 (eval-expression (cadr expression) environment form)))
         ((eq? '- (operator expression)) (- (eval-expression (left-operand expression) environment form) (eval-expression (right-operand expression) environment form)))
         ((eq? '* (operator expression)) (* (eval-expression (left-operand expression) environment form) (eval-expression (right-operand expression) environment form)))
         ((eq? '/ (operator expression)) (floor (/ (eval-expression (left-operand expression) environment form) (eval-expression (right-operand expression) environment form))))
         ((eq? '% (operator expression)) (modulo (eval-expression (left-operand expression) environment form) (eval-expression (right-operand expression) environment form)))
         ;check if eqv? is okay
         ;the next few lines perform boolean operators
         ((eq? '== (operator expression)) (eqv? (eval-expression (left-operand expression) environment form) (eval-expression (right-operand expression) environment form)))
         ((eq? '!= (operator expression)) (not (eq? (eval-expression (left-operand expression) environment form) (eval-expression (right-operand expression) environment form))))
         ((eq? '< (operator expression)) (< (eval-expression (left-operand expression) environment form) (eval-expression (right-operand expression) environment form)))
         ((eq? '> (operator expression)) (> (eval-expression (left-operand expression) environment form) (eval-expression (right-operand expression) environment form)))
         ((eq? '<= (operator expression)) (<= (eval-expression (left-operand expression) environment form) (eval-expression (right-operand expression) environment form)))
         ((eq? '>= (operator expression)) (>= (eval-expression (left-operand expression) environment form) (eval-expression (right-operand expression) environment form)))
         ((eq? '|| (operator expression)) (or (eval-expression (left-operand expression) environment form) (eval-expression (right-operand expression) environment form)))
         ((eq? '&& (operator expression)) (and (eval-expression (left-operand expression) environment form) (eval-expression (right-operand expression) environment form)))
         ;THIS LINE BELOW MUST BE CHANGED IF PARSER IS NO LONGER PREFIX to allow ! operator
         ((eq? '! (operator expression)) (not (eval-expression (left-operand expression) environment form) ))
         ;if this line is reached, it means an unknown/undefined operator was used
         (else (error "undefined operator used"))))
     (get-operator form) (get-left-operand form) (get-right-operand form))))

; retrieves the operator function from the format
(define get-operator
  (lambda (form)
    (car (form))))

; retrieves the left-operand function from the format   ;PROBLEMS HERE
(define get-left-operand
  (lambda (form)
    (cond
      ((null? (cddr (form))) 0)
      (else (cadr (form))))))

; retrieves the right-operand function from the format
(define get-right-operand
  (lambda (form)
    (cond
      ((null? (cddr (form))) (cadr (form)))
      (else (caddr (form))))))

; the format functions.  The functions are organized: (operator operand1 operand2)
; infix as the operator in the middle
(define infix
  (lambda ()
    (list cadr car caddr)))

; postfix has the operator at the end
(define postfix
  (lambda ()
    (list caddr car cadr)))

; prefix has the operator at the beginning
(define pprefix
  (lambda ()
    (list car cadr caddr)))

;iterprets code by calling Mstate
(define interpret
  (lambda (filename)
    (legitimize (Mstate (parser filename) '()))))

;legitimizes return value: ie, if it is not a number or boolean, that means a return statement was not reached
(define legitimize
  (lambda (returnvalue)
    (cond
      ((or (number? returnvalue) 
           (eq? 'true returnvalue)
           (eq? 'false returnvalue)
           ) returnvalue)
      (else (error "No return reached")))))
            

;Test function: will test a list of testfiles and return the list of their return values
(define doTests
  (lambda (tests)
    (cond
      ((null? tests) '())
      (else (cons (interpret (car tests)) (doTests (cdr tests)))))))

;(doTests 
 ;'("Test1/test1.txt" "Test1/test2.txt" "Test1/test3.txt" "Test1/test4.txt" "Test1/test5.txt" "Test1/test6.txt" 
  ;                   "Test1/test7.txt" "Test1/test8.txt" "Test1/test9.txt" "Test1/test10.txt")); "test11.txt" "test12.txt" "test13.txt" "test14.txt" "test15.txt" "test16.txt" "test17.txt" "test18.txt" "test19.txt" "test20.txt" "test21.txt" "test22.txt" "test23.txt" "test24.txt"))
 ;(150 -4 10 16 220 5 6 10 5 -39 error error error error true 100 false true 30 11 1106 12 16 72)
