;George Hodulik Project 2 EECS 345
(load "functionParser.scm")
;My interpreter DOES implement the extra challenge of call by reference, but does NOT do statements with side effects.
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
  (lambda (code environment return next break continue)
    (cond
      ((null? code) (next environment))
      (else (Mstatement (nextstatement code) environment return (lambda (env) (Mstate (remainingstatements code) env return
                                                                                      next break continue))
                                                                  break continue)))))
            
;takes code an an environment and acts upod the nextstatement
(define Mstatement
  (lambda (statement environment return next break continue)
    (cond
      ;if this is reached, perform the next action on the resulting environment
      ((null? statement) (next environment))
      ;If it is a function call
      ((funcall? statement) (begin (call-function (cadr statement) (cddr statement) environment) (next environment)));(next (Mstatefunc statement environment)))
      ;if this is a block, call Mstate on it, with a new layer to the environment, and be sure to cdr off that layer when next, break, or continue is called
      ((block? statement) (Mstate (remainingstatements statement) (addLayer environment) return (lambda (v) (next (cdr v))) (lambda (v) (break (cdr v))) (lambda (v) (continue (cdr v))) ))
      ;calls if statement if nextstatement is if: note, remaining statements is a parameter, since the if statement could have a return, or it could continue
      ((if? statement) (evaluate-if statement environment return next break continue))
      ;calls return if the nextstatement is a return
      ((return? statement) (Mreturn statement environment return))
      ;calls assign if the nextstatement is an assignment
      ((=? statement) (next (assign (getvarname statement) (getvarvalue statement) environment environment 'assign)))
      ;calls declarevar if the next statement is a declare
      ((declaration? statement) (next (declarevar statement environment)))
      ;if this is a break, call break
      ((break? statement) (break environment))
      ;if this is a continue, call continue
      ((continue? statement) (continue environment))
      ;if this is a while, call whileloop
      ((while? statement) (whileloop statement environment return next break continue)))))

(define Mstate-build
  (lambda (code environment)
    (cond
      ((null? code) environment)
      ((declaration? (nextstatement code)) (Mstate-build (cdr code) (declarevar (nextstatement code) environment)))
      ((function-decl? (nextstatement code)) (Mstate-build (cdr code) (Mstatefunc (nextstatement code) environment)))
      (else (error "Invalid operations outside of main function")))))

;gets the nextstatement from code. returns a statement
(define nextstatement
  (lambda (code)
    (car code)))

;gets the remaining assignments from code, returns a code
(define remainingstatements
  (lambda (code)
    (cdr code)))

;add a layer to an environment
(define addLayer
  (lambda (environment)
    (cons '() environment)))

(define funcall?
  (lambda (statement)
    (eq? (car statement) 'funcall)))

;checks if a statement is a function
(define function-decl?
  (lambda (statement)
    (eq? (car statement) 'function)))

;checks if a statement is a block
(define block?
  (lambda (statement)
    (eq? (car statement) 'begin)))

;checks if a statement is an if statement
(define if?
  (lambda (statement)
    (eq? (car statement) 'if)))

;checks if a staement is an assignment statement
(define =?
  (lambda (statement)
    (eq? (car statement) '=)))

;checks if a statement is a declaration
(define declaration?
  (lambda (statement)
    (eq? (car statement) 'var)))

;checks if a statement is a return statement
(define return?
  (lambda (statement)
      (eq? (car statement) 'return)))

;checks if a statement is a while
(define while?
  (lambda (statement)
    (cond
      ((eq? (car statement) 'while) #t)
      (else #f))))

;checks if a statement is a break
(define break?
  (lambda (statement)
    (eq? (car statement) 'break)))

;checks if a statement is a continue
(define continue?
  (lambda (statement)
    (eq? (car statement) 'continue)))
;The below functions build what needs to be added to the environment for a function : (fname fclosure)
(define functionname
  (lambda (statement)
    (cadr statement)))
(define functionbody
  (lambda (statement)
    (cadddr statement)))
(define functionformalparams
  (lambda (statement)
    (caddr statement)))

;conses the closure of the function to the environment when the function is written
(define Mstatefunc
  (lambda (statement environment)
    (addLayer (cons (fclosure statement) environment))))

;makes the closure of a function
(define fclosure
  (lambda (statement)
    (list (list (functionname statement) (box (list (functionformalparams statement) (functionbody statement) fenvironment))))))

;builds the environment based off the environment at call time: note the environment is cdred at the end so as to not include
;the environment from when the function is called.  Note that when there is an &, the get-box function is called rather 
;than eval-expression
(define fenvironment
  (lambda (fparams cparams environment)
    (cond
      ((and (null? fparams) (null? cparams)) (cdr environment))
      ((or (null? fparams) (null? cparams)) (error "Parameter mistmatch"))
      ((eq? (car fparams) '&) (cons (list (list (cadr fparams) (get-box (car cparams) environment))) (fenvironment (cddr fparams) (cdr cparams) environment)))
      (else (cons (list(list (car fparams) (box (eval-expression (car cparams) environment pprefix)))) (fenvironment (cdr fparams) (cdr cparams) environment))))))

;get-box and lookupBox are identical to lookup and lookupLayer, except they do not unbox the var's value
(define get-box
  (lambda (varname environment)
    (cond
      ((null? environment) (error "variable not declared"))
      ((not (null? (lookupBox varname (car environment)))) (lookupBox varname (car environment)))
      (else (get-box varname (cdr environment))))))

(define lookupBox
  (lambda (var environment)
    (cond
      ((null? environment) '())
      ((eq? (caar environment) var) (cadar environment))
      (else (lookupBox var (cdr environment))))))

;gets the condition of a while
(define whilecondition
  (lambda (statement)
    (cadr statement)))

;gets the body of a while
(define whilebody
  (lambda (statement)
    (caddr statement)))

;while loop
(define whileloop
 (lambda (statement environment return next break continue)
   (letrec ((loop (lambda (condition body environment next)
               ;if the condition is true, call Mstatement, with the approriate next, break, annd continue
                    ;else call next
               (if (eval-expression condition environment pprefix)
                  (Mstatement body environment return (lambda (env) (loop condition body env next)) (lambda (env) (next env)) 
                             (lambda (env) (loop condition body env next)))
                  (next environment)))))
       (loop (whilecondition statement) (whilebody statement) environment next))))

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
;There are two assign functions, assign goes through each layer in the environment and decides which one the assignment will take place
;Note assign takes an environment of the form (()) or ( ((x 3) (y 2)) ((z 2) (x 5)) ), or layers of environments
;whereas assignLayer takes an environment of the form () or ((x 3) (y 2)), or a single environment
(define assign
  (lambda (varname value environment original-environment assigntype)
    (cond
      ((and (null? environment) (eq? assigntype 'assign)) (error "variable not declared"))
      ((and (eq? assigntype 'declaration) (eq? (lookupLayer varname (car environment)) '())) 
       (cons (assignLayer varname value (car environment) original-environment assigntype) (cdr environment)))
      ((eq? assigntype 'declaration) (error "redefining of variable"))
      ((eq? (lookupLayer varname (car environment)) '()) (cons (car environment) (assign varname value (cdr environment) original-environment assigntype)))
      (else (cons (assignLayer varname value (car environment) original-environment assigntype) (cdr environment))))))
   
;assignLayer assigns a value for a specific layer
(define assignLayer
  (lambda (varname value environment original-environment assigntype)
    (cond
      ;if the environment is empty and this is an assignment, then the variable was not declared
      ((and (null? environment) (eq? assigntype 'assign)) (error "variable not declared"))
      ;if the environment is empty and this is a declaration w/o assignment value, then add the variable with value 'error
      ((and (null? environment) (eq? assigntype 'declaration) (eq? value 'error) (cons (list varname (box 'error)) '())))
      ;if this variable has a non-error assignment value and is a declaration, evaluate the value and assign it
      ((and (null? environment) (eq? assigntype 'declaration)) (cons (list varname (box (eval-expression value original-environment pprefix))) '()))
      ;if the var is reached but this is a declaration, the variable is being redeclared
      ((and (eq? (caar environment) varname) (eq? assigntype 'declaration) (error "redefining of variable")))
      ;if the var is reached otherwise, update its value
      ((eq? (caar environment) varname) (begin (set-box! (cadar environment) (eval-expression value original-environment pprefix)) environment))
      ;otherwise recurse
      (else (cons (car environment) (assignLayer varname value (cdr environment) original-environment assigntype))))))



;gets varname from statement: expects statement in form of (var x value) where x is varname and value is a value or expression
(define getvarname
  (lambda (statement)
    (cadr statement)))

;gets value from statement: expects statement in form of (var x value) where x is varname and value is a value or expression
(define getvarvalue
  (lambda (statement)
    (caddr statement)))

;retuns the value of a variable, '() if it is not declared
;Like assign and assignLayer, lookuLayer looks up a var in a particular layer of the environment, while lookup
;determines which layer of the environment the var will be looked up in
(define lookupLayer
  (lambda (var environment)
    (cond
      ((null? environment) '())
      ((eq? (caar environment) var) (unbox (cadar environment)))
      (else (lookupLayer var (cdr environment))))))
 
;lookup determines which layer of the environment (or the first appearance) contains var, and calls lookupList to get its value
(define lookup
  (lambda (var environment)
    (cond
      ((null? environment) (error "variable not declared"))
      ((not (null? (lookupLayer var (car environment)))) (lookupLayer var (car environment)))
      (else (lookup var (cdr environment))))))

;returns the return expression in a return statement
(define returnexpression
  (lambda (statement)
    (cadr statement)))

;returns the value of the return expression
(define Mreturn
  (lambda (code environment return)
    (return (make-english (eval-expression (returnexpression code) environment pprefix)))))

;translates scheme words to "English," for now this is only for #t and #f to 'true and 'false
(define make-english
  (lambda (expression)
    (cond
      ((eq? expression #t) 'true)
      ((eq? expression #f) 'false)
      (else expression))))

;evaluates an if statement
(define evaluate-if
  (lambda (ifstatement environment return next break continue)
    (cond
      ;checks if the if statement is true, returns the state ofthe expression if it is
      ((eval-expression (ifcond ifstatement) environment pprefix) (Mstatement (nextstatement (ifexpr ifstatement)) environment return next break continue))
      ;if the condition was false and there is no else, the continue with the remaining code
      ((null? (ifelse ifstatement)) (next environment))
      ;if there was an else, then Mstate with the else's if statement
      (else (Mstatement (nextstatement (ifelse ifstatement)) environment return next break continue)))))

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
         ((funcall? expression) (call-function (cadr expression) (cddr expression) environment))
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

; retrieves the left-operand function from the format   ;PROBLEMS HERE -- Actually I don't think there are...
(define get-left-operand
  (lambda (form)
    (cond
      ((null? (cddr (form))) 0)  ;This maybe should be commented out but I have not tested
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

(define newEnvironment
  '(()) )

;interprets code by calling Mstate
(define interpret
  (lambda (filename)
    ;(legitimize 
     (call-main (Mstate-build (parser filename) newEnvironment)))) ;return (lambda (v)  v) (lambda (v) v) (lambda (v) v))))))
     ;(call/cc (lambda (return)
       ;(call-main (Mstate (parser filename) newEnvironment return (lambda (v)  v) (lambda (v) v) (lambda (v) v))))))

;abstraction functions to get parts of the function closure
(define getformalparams
  (lambda (closure)
    (car closure)))

(define getfunctionbody
  (lambda (closure)
    (cadr closure)))

(define getenvbuildfunction
  (lambda (closure)
    (caddr closure)))

;call the main function - same as call function except parameter list is always empty and the name of the function is known
(define call-main
  (lambda (environment)
    (legitimize 
     (call/cc (lambda (return)
       (letrec ((f (lookup 'main environment)))
        (Mstate (getfunctionbody f) ((getenvbuildfunction f) (getformalparams f) '() environment) return (lambda (v) v) (lambda (v) v) (lambda (v) v))))))))

;calls a function
(define call-function
  (lambda (fname params environment)
     (call/cc (lambda (return2)
       (letrec ((f (lookup fname environment)))
        (Mstate (cadr f) (addLayer ((caddr f) (car f) params environment)) return2 (lambda (v) v) (lambda (v) v) (lambda (v) v)))))))

;legitimizes return value: ie, if it is not a number or boolean, that means a return statement was not reached
(define legitimize
  (lambda (returnvalue)
    (cond
      ((or (number? returnvalue) 
           (eq? 'true returnvalue)
           (eq? 'false returnvalue)
           ) returnvalue)
      (else (error "No return reached")))))
            

;(null? (car (lookup 'main (Mstate-build 
 ;(parser "test25.txt") 
 ;newEnvironment))))

;Test function: will test a list of testfiles and return the list of their return values
(define doTests
  (lambda (tests)
    (cond
      ((null? tests) '())
      (else (cons (interpret (car tests)) (doTests (cdr tests)))))))

;Below is test code.  There is no point in uncommenting the below code, since the file locations of test files will not be the
;same on computers other than mine.
;tests for part 2
;(doTests 
 ;'("test25.txt")); "test26.txt" 
 ;"test27.txt" "test28.txt" "test29.txt" "test30.txt" "test31.txt" ))
 ;              '("test34.txt")); "test33.txt" "test34.txt" 
 ;"test35.txt" 
 ;"test36.txt""test37.txt"))

;100 20 6 -1 789 2 164 error error error 12 32 21


;test for part 1
;(doTests 
 ;'("Test1/test1.txt"  "Test1/test2.txt" "Test1/test3.txt" "Test1/test4.txt" "Test1/test5.txt" "Test1/test6.txt" 
  ;                  "Test1/test7.txt" "Test1/test8.txt" "Test1/test9.txt" "Test1/test10.txt" "Test1/test15.txt" "Test1/test16.txt" 
   ;               "Test1/test17.txt" "Test1/test18.txt")); "test19.txt" "test20.txt" "test21.txt" "test22.txt" "test23.txt" "test24.txt"))
 ;(150 -4 10 16 220 5 6 10 5 -39 error error error error true 100 false true 30 11 1106 12 16 72)

 
;"test11.txt" "test12.txt" "test13.txt" "test14.txt" 
