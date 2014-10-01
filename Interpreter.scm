;George Hodulik Project 2 EECS 345
(load "classParser.scm")
;My interpreter does NOT implement method overloading
;My interpreter DOES implement the extra challenge of call by reference, but does NOT do statements with side effects.
;My general code convention:
;Parameter code indicates code that could have come from the parser, ie, one or more statements contained in a list
;Parameter statement indicates one statement, not contained in an extra list
;Ex. code: ((var x 5) (var y 3) (if (< x y) (return x) (return y)))
;    statement: (var x 5)
;A code is a list of statements
;An expression is an arithmetic operation, ie it is a value, a var, or aritmetic operations of values and/or vars.
;Ex. expressions: x, y, (+ x y), (* x (+ 4 y))
;All Mstate and Mvalue functions have the added parameters currclass (current class) parent (parent class) and globalEnv (the global environment containing all classes)
;lookup and assign were changed so that if the var is not in the current environment, the parent is checked, then the next parent, etc.


;takes code and environment and returns the environment or the environment of the next statememnt
(define Mstate
  (lambda (code environment return next break continue currclass parent globalEnv)
    (cond
      ((null? code) (next environment))
      (else (Mstatement (nextstatement code) environment return (lambda (env) (Mstate (remainingstatements code) env return
                                                                                      next break continue currclass parent globalEnv))
                                                                  break continue currclass parent globalEnv)))))
            
;takes code an an environment and acts upod the nextstatement
(define Mstatement
  (lambda (statement environment return next break continue currclass parent globalEnv) 
    (cond
      ;if this is reached, perform the next action on the resulting environment
      ((null? statement) (next environment))
      ;If it is a function call
      ((funcall? statement) (begin (call-function (cadr statement) (cddr statement) environment currclass parent globalEnv) (next environment)));(next (Mstatefunc statement environment)))
      ;if this is a block, call Mstate on it, with a new layer to the environment, and be sure to cdr off that layer when next, break, or continue is called
      ((block? statement) (Mstate (remainingstatements statement) (addLayer environment) return 
                                  (lambda (v) (next (cdr v))) (lambda (v) (break (cdr v))) (lambda (v) (continue (cdr v))) currclass parent globalEnv))
      ;calls if statement if nextstatement is if: note, remaining statements is a parameter, since the if statement could have a return, or it could continue
      ((if? statement) (evaluate-if statement environment return next break continue currclass parent globalEnv))
      ;calls return if the nextstatement is a return
      ((return? statement) (Mreturn statement environment return currclass parent globalEnv))
      ;calls assign if the nextstatement is an assignment
      ((=? statement) (next (assign (getvarname statement) (getvarvalue statement) environment environment 'assign currclass parent globalEnv)))
      ;calls declarevar if the next statement is a declare
      ((declaration? statement) (next (declarevar statement environment environment currclass parent globalEnv)))
      ;if this is a break, call break
      ((break? statement)  (break environment))
      ;if this is a continue, call continue
      ((continue? statement) (continue environment))
      ;if this is a while, call whileloop
      ((while? statement) (whileloop statement environment return next break continue currclass parent globalEnv)))))


(define cutofflayers
  (lambda (classbody)
    (car (cddar classbody))))

;looks up a class in an environment.  classname is usually in the form of a list containing one item ie '(A),
;but I think sometimes just an atom is passed.  I dealt with this lazily by allowing both cases.
;return values are of the form '((parent) (classdefinition))
(define lookupClass
  (lambda (classname globalenv)
    (cond
      ((null? globalenv) 'classNotFound)
      ((null? (car globalenv)) 'classNotFound)
      ((and (list? classname) (not (null? classname))) (lookupClass (car classname) globalenv))
      ((eq? classname (caar globalenv)) (list (cadar globalenv) (cutofflayers globalenv)))
      (else (lookupClass classname (cdr globalenv))))))

;builds a class given its name, parent body, and a global environment
;returns value of the form '(classname (parent) classdefinition) where the classdefinition is made basically from P3's function builder
;classdefinition of the form '( ((staticvar pairs)) ((nonstaticvar pairs)) body)  At the moment I don't think there is 
;much of a distinction between static and non static methods
(define Mstate-class-build
  (lambda (classname parent body globalEnv)
    (list classname parent (Mstate-build body '() newEnvironment '() classname parent globalEnv))))

;I've separated the variable declarations for a class into static vars and non static vars.
;however, because a declaration could inlude other variables or functions on the right hand side, I made this function to build the
;"evaluation environment" that will be used to evaluate the right hand side of a declaration. Also this environment is what is returned
(define evalenv
  (lambda (staticvars vars env)
    (cons staticvars (cons (car vars) env))))

;Basically the same Mstate-build from P3, except I've distinguished static vars and non static vars
;Right now, static and non static methods are recognized as differnent but are treated the same way.
(define Mstate-build
  (lambda (code environment vars staticvars currclass parent globalEnv)
    (cond
      ((null? code) (evalenv staticvars vars environment))
      ((declaration? (nextstatement code)) (Mstate-build (cdr code) environment (declarevar (nextstatement code) vars (evalenv staticvars vars environment) currclass parent globalEnv)
                                                         staticvars currclass parent globalEnv))
      ((staticdecl? (nextstatement code)) (Mstate-build (cdr code) environment vars (Mstate-static-vars (nextstatement code) staticvars 
                                                                                                        (evalenv staticvars vars environment) currclass parent globalEnv) currclass parent globalEnv))
      ((function-decl? (nextstatement code)) (Mstate-build (cdr code) (Mstatefunc (nextstatement code) environment) vars staticvars currclass parent globalEnv))
      ((static-function-decl? (nextstatement code)) (Mstate-build (cdr code) (Mstatefunc (nextstatement code) environment) vars staticvars currclass parent globalEnv))
      (else (error "Invalid operations outside of main function")))))

;checks if the statement isa static declaration
(define staticdecl?
  (lambda (statement)
    (eq? (car statement) 'static-var)))

;Builds the list of static vars.  
;Basically the same function as declarevar, but the environment expected for declarevar is slightly different
(define Mstate-static-vars
  (lambda (statement staticvars env currclass parent globalEnv)
    (if (null? (cddr statement))
        (cons (list (getvarname statement) (box 'error)) staticvars)
        (cons (list (getvarname statement) (box (eval-expression (getvarvalue statement) env pprefix currclass parent globalEnv))) staticvars))))

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

;checls if the statement is a function call
(define funcall?
  (lambda (statement)
    (eq? (car statement) 'funcall)))

;checks if a statement is a function
(define function-decl?
  (lambda (statement)
    (eq? (car statement) 'function)))

;checks if the statement is a static function declaration
(define static-function-decl?
  (lambda (statement)
    (eq? (car statement) 'static-function)))

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
    (cons (fclosure statement) environment)))

;makes the closure of a function
(define fclosure
  (lambda (statement)
    (list (list (functionname statement) (box (list (functionformalparams statement) (functionbody statement) fenvironment))))))

;builds the environment based off the environment at call time: note the environment is cdred at the end so as to not include
;the environment from when the function is called.  Note that when there is an &, the get-box function is called rather 
;than eval-expression
(define fenvironment
  (lambda (fparams cparams environment currclass parent globalEnv) 
    (cond
      ((and (null? fparams) (null? cparams)) (cdr environment))
      ((or (null? fparams) (null? cparams)) (error "Parameter mistmatch"))
      ((eq? (car fparams) '&) (cons (list (list (cadr fparams) (get-box (car cparams) environment))) (fenvironment (cddr fparams) (cdr cparams) environment)))
      (else (cons (list (car fparams) (box (eval-expression (car cparams) environment pprefix currclass parent globalEnv))) 
                  (fenvironment (cdr fparams) (cdr cparams) environment currclass parent globalEnv))))))

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
 (lambda (statement environment return next break continue currclass parent globalEnv)
   (letrec ((loop (lambda (condition body environment next)
               ;if the condition is true, call Mstatement, with the approriate next, break, annd continue
                    ;else call next
               (if (eval-expression condition environment pprefix currclass parent globalEnv)
                  (Mstatement body environment return (lambda (env) (loop condition body env next)) (lambda (env) (next env)) 
                             (lambda (env) (loop condition body env next)) currclass parent globalEnv)
                  (next environment)))))
       (loop (whilecondition statement) (whilebody statement) environment next))))

;calls assignment when theres a declaration, with parameter assigntype of 'declaration
(define declarevar
  (lambda (statement newenvironment oldenvironment currclass parent globalEnv)
    (cond
      ;if this was of the form (var x) with no value, call assign with value of 'error
      ;note (cddr statement) is used to see if there is a value given: this may need to be changed if parser changes
      ((null? (cddr statement)) (assign (getvarname statement) 'error newenvironment oldenvironment 'declaration currclass parent globalEnv))
      ;else call assign with the value 
      (else (assign (getvarname statement) (getvarvalue statement) newenvironment oldenvironment 'declaration currclass parent globalEnv)))))

(define getstaticvars
  (lambda (classdef)
    (list (car (cadr classdef)))))

(define lookupSuper
  (lambda (super currclass parent globalEnv)
     (cond
      ((null? globalEnv) (error "No parent"))
      ((null? (car globalEnv)) (error "No parent"))
      ((and (list? super) (eq? (caddr super) 'super)) (lookupClass (findparent parent globalEnv) globalEnv))
      ((and (list? super) (list? (caddr super))) (lookupSuper (caadr super) parent (findparent parent globalEnv) globalEnv))
      ((eq? 'super super) (lookupClass parent globalEnv))
      (else (lookupSuper (cadr super) currclass parent globalEnv)))))

(define findparent
  (lambda (class globalEnv)
    (car (lookupClass class globalEnv))))

(define findclassbody
  (lambda (class globalEnv)
    (cadr (lookupClass class globalEnv))))

(define getnonstaticvars
  (lambda (class globalEnv)
    (cadr (findclassbody class globalEnv))))
      

;so far can do complicated assignments with one =, but can not do nested assignments with multiple ='s
;assigntype is 'declarataion if this is coming from a (var x expression) value
;assigntype is 'assign if it is just an (= x expression)
;note that there are two environment parameters: environment is searched through to find the var while old-environment does not change
;This is because an assignment value may need to be evaluated, and if it has other variables in the expression, eval-expression needs
;that environment to properly determine a valuesuper
;There are two assign functions, assign goes through each layer in the environment and decides which one the assignment will take place
;Note assign takes an environment of the form (()) or ( ((x 3) (y 2)) ((z 2) (x 5)) ), or layers of environments
;whereas assignLayer takes an environment of the form () or ((x 3) (y 2)), or a single environment
(define assign
  (lambda (varname value environment original-environment assigntype currclass parent globalEnv)
    (cond
      ((and (dot-expression? varname) (not (super? varname)) (not (eq? (lookupClass (cadr varname) globalEnv) 'classNotFound)))
       (assign (caddr varname) value (findstaticvars (cadr varname) globalEnv) original-environment assigntype currclass parent globalEnv))
      ((and (dot-expression? varname) (not (super? varname))) (error "Class was not found"))
      ((and (dot-expression? varname) (super? varname) (not (null? parent)))
       (assign (caddr varname) value (getstaticvars (lookupSuper varname currclass parent globalEnv)) original-environment assigntype currclass parent globalEnv))
      ((and (dot-expression? varname) (super? varname) (null? parent)) (error "No parent"))
      ((and (null? environment) (eq? assigntype 'assign) (null? parent)) (error "variable not declared"))
      ((and (null? environment) (eq? assigntype 'assign)) (assign varname value (findclassbody (car parent) globalEnv)
                                                                  original-environment assigntype (car parent) 
                                                                  (findparent (car parent) globalEnv) globalEnv))
      ((and (eq? assigntype 'declaration) (eq? (lookupLayer varname (car environment)) '())) 
       (cons (assignLayer varname value (car environment) original-environment assigntype currclass parent globalEnv) (cdr environment)))
      ((eq? assigntype 'declaration) (error "redefining of variable"))
      ((eq? (lookupLayer varname (car environment)) '()) (cons (car environment) (assign varname value (cdr environment) 
                                                                                         original-environment assigntype currclass parent globalEnv)))
     (else (cons (assignLayer varname value (car environment) original-environment assigntype currclass parent globalEnv) (cdr environment))))))

  ;assignLayer assigns a value for a specific layer
(define assignLayer
  (lambda (varname value environment original-environment assigntype currclass parent globalEnv)
    (cond
      ;if the environment is empty and this is an assignment, then the variable was not declared
      ((and (null? environment) (eq? assigntype 'assign)) (error "variable not declared"))
      ;if the environment is empty and this is a declaration w/o assignment value, then add the variable with value 'error
      ((and (null? environment) (eq? assigntype 'declaration) (eq? value 'error) (cons (list varname (box 'error)) '())))
      ;if this variable has a non-error assignment value and is a declaration, evaluate the value and assign it
      ((and (null? environment) (eq? assigntype 'declaration)) (cons (list varname (box (eval-expression value original-environment pprefix currclass parent globalEnv))) '()))
      ;if the var is reached but this is a declaration, the variable is being redeclared
      ((and (eq? (caar environment) varname) (eq? assigntype 'declaration) (error "redefining of variable")))
      ;if the var is reached otherwise, update its value
      ((eq? (caar environment) varname) (begin (set-box! (cadar environment) (eval-expression value original-environment pprefix currclass parent globalEnv)) environment))
      ;otherwise recurse
      (else (cons (car environment) (assignLayer varname value (cdr environment) original-environment assigntype currclass parent globalEnv))))))



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
      ((and (null? (car environment)) (not (null? (cdr environment)))) (lookupLayer var (car (cdr environment))))
      ((eq? (caar environment) var) (unbox (cadar environment)))
      (else (lookupLayer var (cdr environment))))))

;lookup determines which layer of the environment (or the first appearance) contains var, and calls lookupList to get its value
(define lookup
  (lambda (var environment isdot parent globalEnv) 
    (cond
      ((and (dot-expression? var) (super? (cadr var)) ) (lookup (caddr var) (getstaticvars (lookupSuper var 'error parent globalEnv))
                                               #t (car (lookupSuper var 'error parent globalEnv)) globalEnv))
      ((dot-expression? var) (lookup (getfunctionname var) (findstaticvars (cadr var) globalEnv) #t 
                                     (findparent (cadr var) globalEnv) globalEnv))
      ((and (null? environment) isdot (not (null? parent))) (lookup var (findstaticvars (car parent) globalEnv)
                                               #t (findparent parent globalEnv) globalEnv))
      ((null? environment) (error "variable not declared"))
      ((not (null? (lookupLayer var (car environment)))) (lookupLayer var (car environment)))
      (else (lookup var (cdr environment) isdot parent globalEnv)))))

(define findstaticvars
  (lambda (class globalEnv)
    (getstaticvars (lookupClass class globalEnv))))

;returns the return expression in a return statement
(define returnexpression
  (lambda (statement)
    (cadr statement)))

;returns the value of the return expression
(define Mreturn
  (lambda (code environment return currclass parent globalEnv)
    (return (make-english (eval-expression (returnexpression code) environment pprefix currclass parent globalEnv)))))

;translates scheme words to "English," for now this is only for #t and #f to 'true and 'false
(define make-english
  (lambda (expression)
    (cond
      ((eq? expression #t) 'true)
      ((eq? expression #f) 'false)
      (else expression))))

;evaluates an if statement
(define evaluate-if
  (lambda (ifstatement environment return next break continue currclass parent globalEnv)
    (cond
      ;checks if the if statement is true, returns the state ofthe expression if it is
      ((eval-expression (ifcond ifstatement) environment pprefix currclass parent globalEnv) (Mstatement (nextstatement (ifexpr ifstatement)) environment return next break continue currclass parent globalEnv))
      ;if the condition was false and there is no else, the continue with the remaining code
      ((null? (ifelse ifstatement)) (next environment))
      ;if there was an else, then Mstate with the else's if statement
      (else (Mstatement (nextstatement (ifelse ifstatement)) environment return next break continue currclass parent globalEnv)))))

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
  (lambda (expression environment form currclass parent globalEnv)
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
         ((and (not (list? expression)) (not (number? expression)) (eq? (lookup expression environment #t parent globalEnv) '())) (error "variable used before being declared"))
         ;if the expression is an uninitialized var, return an error
         ((and (not (list? expression)) (not (number? expression)) (eq? 'error (lookup expression environment #t parent globalEnv))) (error "error: Var not assigned a value"))
         ;if the expression is an initialized var, return its value
         ((and (not (list? expression)) (not (number? expression))) (lookup expression environment #t parent globalEnv))
         ;the next few lines perform arithmetic operations
         ((dot-expression? expression) (lookup expression environment #t parent globalEnv))
         ((funcall? expression) (call-function (cadr expression) (cddr expression) environment currclass parent globalEnv))
         ((eq? '+ (operator expression)) (+ (eval-expression (left-operand expression) environment form currclass parent globalEnv)
                                            (eval-expression (right-operand expression) environment form currclass parent globalEnv)))
         ;THIS LINE BELOW MUST BE CHANGED IF PARSER IS NO LONGER PREFIX to allow negatives support ( like -(* 5 9))
         ((and (eq? '- (operator expression)) (null? (cddr expression))) (- 0 (eval-expression (cadr expression) environment form currclass parent globalEnv)))
         ((eq? '- (operator expression)) (- (eval-expression (left-operand expression) environment form currclass parent globalEnv)
                                            (eval-expression (right-operand expression) environment form currclass parent globalEnv)))
         ((eq? '* (operator expression)) (* (eval-expression (left-operand expression) environment form currclass parent globalEnv)
                                            (eval-expression (right-operand expression) environment form currclass parent globalEnv)))
         ((eq? '/ (operator expression)) (floor (/ (eval-expression (left-operand expression) environment form currclass parent globalEnv)
                                                   (eval-expression (right-operand expression) environment form currclass parent globalEnv))))
         ((eq? '% (operator expression)) (modulo (eval-expression (left-operand expression) environment form currclass parent globalEnv)
                                                 (eval-expression (right-operand expression) environment form currclass parent globalEnv)))
         ;check if eqv? is okay
         ;the next few lines perform boolean operators
         ((eq? '== (operator expression)) (eqv? (eval-expression (left-operand expression) environment form currclass parent globalEnv)
                                                (eval-expression (right-operand expression) environment form currclass parent globalEnv)))
         ((eq? '!= (operator expression)) (not (eq? (eval-expression (left-operand expression) environment form currclass parent globalEnv)
                                                    (eval-expression (right-operand expression) environment form currclass parent globalEnv))))
         ((eq? '< (operator expression)) (< (eval-expression (left-operand expression) environment form currclass parent globalEnv)
                                            (eval-expression (right-operand expression) environment form currclass parent globalEnv)))
         ((eq? '> (operator expression)) (> (eval-expression (left-operand expression) environment form currclass parent globalEnv)
                                            (eval-expression (right-operand expression) environment form currclass parent globalEnv)))
         ((eq? '<= (operator expression)) (<= (eval-expression (left-operand expression) environment form currclass parent globalEnv)
                                              (eval-expression (right-operand expression) environment form currclass parent globalEnv)))
         ((eq? '>= (operator expression)) (>= (eval-expression (left-operand expression) environment form currclass parent globalEnv)
                                              (eval-expression (right-operand expression) environment form currclass parent globalEnv)))
         ((eq? '|| (operator expression)) (or (eval-expression (left-operand expression) environment form currclass parent globalEnv)
                                              (eval-expression (right-operand expression) environment form currclass parent globalEnv)))
         ((eq? '&& (operator expression)) (and (eval-expression (left-operand expression) environment form currclass parent globalEnv)
                                               (eval-expression (right-operand expression) environment form currclass parent globalEnv)))
         ;THIS LINE BELOW MUST BE CHANGED IF PARSER IS NO LONGER PREFIX to allow ! operator
         ((eq? '! (operator expression)) (not (eval-expression (left-operand expression) environment form currclass parent globalEnv) ))
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

;interprets code by calling the main of the given vlass
(define interpret
  (lambda (filename classname)
    ;(legitimize 
    (letrec ((globalEnvironment (buildGlobalEnv (parser filename) newEnvironment))
             (class (lookupClass (string->symbol classname) globalEnvironment)))
      (call-main (string->symbol classname) (car class) (cadr class) globalEnvironment))))
                 ;(call-main (Mstate-build (parser filename) newEnvironment)))) ;return (lambda (v)  v) (lambda (v) v) (lambda (v) v))))))
     ;(call/cc (lambda (return)
       ;(call-main (Mstate (parser filename) newEnvironment return (lambda (v)  v) (lambda (v) v) (lambda (v) v))))))

;abstraction methods to get information about a class from the parser
(define getclassname
  (lambda (statement)
    (cadr statement)))
  
(define getparent
  (lambda (statement)
    (if (not (null? (caddr statement)))
        (list (cadr (caddr statement)))
        '())))

(define getclassbody
  (lambda (statement)
    (cadddr statement)))
 
;builds the global environment
(define buildGlobalEnv
  (lambda (filecontent env)
    (cond
      ((null? filecontent) env)
      (else (buildGlobalEnv (remainingstatements filecontent) 
                            (cons (Mstate-class-build (getclassname (nextstatement filecontent)) 
                                      (getparent (nextstatement filecontent)) 
                                      (getclassbody (nextstatement filecontent))
                                      env) env  )))))) 
                  

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
  (lambda (currclass parent environment globalEnv)
    (legitimize 
     (call/cc (lambda (return)
       (letrec ((f (lookup 'main environment #f parent globalEnv)))
        (Mstate (getfunctionbody f) (addLayer environment)
                return (lambda (v) v) (lambda (v) v) (lambda (v) v) currclass parent globalEnv)))))))

;checks if the statement is a dot expression
(define dot-expression?
  (lambda (value)
    (cond
      ((list? value) (eq? 'dot (car value)))
      (else #f))))

;checks if a name is a classname in the global environment
(define className?
  (lambda (name globalEnv)
    (cond
      ((eq? name 'super) #f)
      (else (not (eq? 'classNotFound (lookupClass name globalEnv)))))))

;checks if a name is super
(define super?
  (lambda (name)
    (if (dot-expression? name)
        (super? (cadr name))
        (eq? name 'super))))

;given a function call, gets the function call environment
;if it is a dot expression, the corresponding class environment is found and returned
;else, it is checked if the local environment has the function, and if not, checks the parent(s)
(define getfunctioncallenv
  (lambda (fname parent environment origenv globalEnv)
    (cond
      ((and (dot-expression? fname) (className? (cadr fname) globalEnv))
       (getfunctioncallenv (getfunctionname fname) (findparent (cadr fname) globalEnv) 
                          (findclassbody (cadr fname) globalEnv) (findclassbody (cadr fname) globalEnv) globalEnv))
      ((and (dot-expression? fname) (super? (cadr fname)))
       (getfunctioncallenv (getfunctionname fname) (findparent (car parent) globalEnv) (findclassbody (car parent) globalEnv)
                           (findclassbody (car parent) globalEnv) globalEnv))
      ((null? environment) (getfunctioncallenv fname (if (eq? 'classNotFound (lookupClass parent globalEnv))
                                                                        (error "There is no parent to this class")
                                                                        (findparent parent globalEnv))
                                                              (if (eq? 'classNotFound (findparent parent globalEnv))
                                                                        (error "There is no parent to this class")
                                                                        (findclassbody parent globalEnv))
                                                              (if (eq? 'classNotFound (findparent parent globalEnv))
                                                                        (error "There is no parent to this class")
                                                                        (findclassbody parent globalEnv))
                                                              globalEnv))
      ((null? (lookupLayer fname (car environment))) (getfunctioncallenv fname parent (cdr environment) origenv globalEnv))
      (else origenv))))

;abstration to get the function name
(define getfunctionname
  (lambda (expr)
    (if (dot-expression? expr)
        (caddr expr)
        expr)))
                                                              

;calls a function
;first gets correct environment where the function is located, then gets the function closure from that environment
;then calls the function using the environment built by the closure consed to the function's environment
(define call-function
  (lambda (fname params environment currclass parent globalEnv)
     (call/cc (lambda (return2)
       (letrec ((functioncallenv (getfunctioncallenv fname parent environment environment globalEnv))
                (f (lookup (getfunctionname fname) functioncallenv #f parent globalEnv)))
         (Mstate (cadr f) (addLayer (cons (car (list ((caddr f) (car f) params environment currclass parent globalEnv))) functioncallenv)) return2 (lambda (v) v) 
                  (lambda (v) v) (lambda (v) v) currclass parent globalEnv)
          environment)))))
  
;legitimizes return value: ie, if it is not a number or boolean, that means a return statement was not reached
(define legitimize
  (lambda (returnvalue)
    (cond
      ((or (number? returnvalue) 
           (eq? 'true returnvalue)
           (eq? 'false returnvalue)
           ) returnvalue)
      (else (error "No return reached")))))

;uncomment to run tests
;(display "1A")(interpret "P4test1.txt" "B")
;(display "2A")(interpret "P4test2.txt" "A")
;(display "3A")(interpret "P4test3.txt" "A")
;(display "4A")(interpret "P4test4.txt" "A")
;(display "5A")(interpret "P4test5.txt" "A")
;(display "5B")(interpret "P4test5.txt" "B")
;(display "6A")(interpret "P4test6.txt" "A")
;(display "6B")(interpret "P4test6.txt" "B")
;(display "7A")(interpret "P4test7.txt" "A")
;(display "7B")(interpret "P4test7.txt" "B")
;(display "8B")(interpret "P4test8.txt" "B")
;(display "9B")(interpret "P4test9.txt" "B") ;supposed to return error
;(display "9C")(interpret "P4test9.txt" "C")
;(display "10")(interpret "P4test10.txt" "Square")
;(display "11")(interpret "P4test11.txt" "A")

;Test function: will test a list of testfiles and return the list of their return values
;(define doTests
 ; (lambda (filname parent)
  ;  (cond
   ;   ((null? tests) '())
    ;  (else (cons (interpret (car tests)) (doTests (cdr tests)))))))
