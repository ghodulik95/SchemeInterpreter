;George Hodulik Project 5 EECS 345
(load "classParser.scm")

;PROJECT 5 ---
;My interpreter does not work for everything, but it works for many if not most things.
;Of our sample tests, 1,2,3,4,5,6,9,11,12,14,15 work.
;Try/catch works
;object instantiation works
;any combination of dot expressions can be called on objects and they should work: ie c.m().g.s() and x.y.z.f1().f2().g should both work
;calling this and super works sometimes but not all of the time.  Part of this is due to the way I handled super, which I
;explained at the buildInstanceEnv2 function.
;-----

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
  (lambda (code environment return next break continue currclass parent globalEnv throw)
    (cond
      ((null? code) (next environment))
      (else (Mstatement (nextstatement code) environment return (lambda (env) (Mstate (remainingstatements code) env return
                                                                                      next break continue currclass parent globalEnv throw))
                                                                  break continue currclass parent globalEnv throw)))))
            
;takes code an an environment and acts upod the nextstatement
(define Mstatement
  (lambda (statement environment return next break continue currclass parent globalEnv throw)
    (cond
      ;if this is reached, perform the next action on the resulting environment
      ((null? statement) (next environment))
      ;If it is a function call
      ((funcall? statement) (begin (call-function (cadr statement) (cddr statement) environment currclass parent globalEnv throw) (next environment)));(next (Mstatefunc statement environment)))
      ;if this is a block, call Mstate on it, with a new layer to the environment, and be sure to cdr off that layer when next, break, or continue is called
      ((block? statement) (Mstate (remainingstatements statement) (addLayer environment) return 
                                  (lambda (v) (next (cdr v))) (lambda (v) (break (cdr v))) (lambda (v) (continue (cdr v))) currclass parent globalEnv throw))
      ;if this is a try, call the try then call next on the environment afterward.  This ensures the integrety of the environment
      ((try? statement) (begin (call-try statement environment return next break continue currclass parent globalEnv throw) (next environment)))
      ;calls if statement if nextstatement is if: note, remaining statements is a parameter, since the if statement could have a return, or it could continue
      ((if? statement) (evaluate-if statement environment return next break continue currclass parent globalEnv throw))
      ;calls return if the nextstatement is a return
      ((return? statement) (Mreturn statement environment return currclass parent globalEnv throw))
      ;calls assign if the nextstatement is an assignment
      ((=? statement) (next (begin (assign (getvarname statement) (getvarvalue statement) environment environment 'assign currclass parent globalEnv throw)
                                   environment)))
                      
                      ;calls declarevar if the next statement is a declare
      ((declaration? statement) (next (declarevar statement environment environment currclass parent globalEnv throw)))
      ;if this is a break, call break
      ((break? statement)  (break environment))
      ;if this is a continue, call continue
      ((continue? statement) (continue environment))
      ;if this is a throw, evaluate the throw value (this should be an object, but could be a number. eval-expression will handle both)
      ((throw? statement) (throw (eval-expression (cadr statement) environment pprefix currclass parent globalEnv throw)))
      ;if this is a while, call whileloop
      ((while? statement) (whileloop statement environment return next break continue currclass parent globalEnv throw)))))

;abstraction for try block
(define throw?
  (lambda (statement)
    (eq? 'throw (car statement))))

(define gettryblock
  (lambda (statement)
    (cadr statement)))

(define getcatchblock
  (lambda (statement)
    (if (null? (caddr statement))
        '()
        (cadr (cdaddr statement)))))

(define getcatchparam
  (lambda (statement)
    (if (null? (caddr statement))
        '()
        (caar (cdaddr statement)))))

(define getfinblock
  (lambda (statement)
    (if (null? (cadddr statement))
        '()
        (cadr (cadddr statement)))))
     
;This is where a try block is handled.  Note that I do not need to add layers to the environment really because of the nature of blocks
;In Mstate, next will be called on the original environment, so it doesnt matter how new variables are added in the try because
;any changes to the original environment will be kept because of blocks.
(define call-try
  (lambda (statement environment return next break continue currclass parent globalEnv throw)
   (call/cc (lambda (jump)
              (Mstate (gettryblock statement) (addLayer environment) return (lambda (env) (next (Mstate (getfinblock statement) environment
                                                                                                  return next break continue currclass parent globalEnv throw)))
                      (lambda (env) (break (Mstate (getfinblock statement) environment
                                                                        return next break continue currclass parent globalEnv throw)))
                      (lambda (env) (continue (Mstate (getfinblock statement) environment
                                                                        return next break continue currclass parent globalEnv throw)))
                      currclass parent globalEnv 
                      (lambda (caught) (jump (Mstate (getcatchblock statement) (addBinding (getcatchparam statement) caught environment) 
                                                     return (lambda (env) (next (Mstate (getfinblock statement) environment
                                                                        return next break continue currclass parent globalEnv throw)))
                      break continue currclass parent globalEnv throw)))
                      )))))
;adds the thrown object to the environment        
(define addBinding
  (lambda (varname obj env)
    (cons (list (list varname (box obj))) env))) 
                    
(define try?
  (lambda (statement)
    (eq? 'try (car statement))))

;abstraction: for some reason extra layers were building up
(define cutofflayers
  (lambda (classbody)
    (car (cddar classbody))))

;looks up a class in an environment.  classname is usually in the form of a list containing one item ie '(A),
;but I think sometimes just an atom is passed.  I dealt with this lazily by allowing both cases.
;return values are of the form '((parent) (classdefinition))
(define lookupClass
  (lambda (classname globalEnv) 
    (cond
      ((null? globalEnv) 'classNotFound)
      ((null? (car globalEnv)) 'classNotFound)
      ((and (list? classname) (not (null? classname))) (lookupClass (car classname) globalEnv))
      ((eq? classname (caar globalEnv)) (list (cadar globalEnv) (cutofflayers globalEnv)))
      (else (lookupClass classname (cdr globalEnv))))))

;builds a class given its name, parent body, and a global environment
;returns value of the form '(classname (parent) classdefinition) where the classdefinition is made basically from P3's function builder
;classdefinition of the form '( ((staticvar pairs)) ((nonstaticvar pairs)) body)  At the moment I don't think there is 
;much of a distinction between static and non static methods
(define Mstate-class-build
  (lambda (classname parent body globalEnv)
    (list classname parent (Mstate-build body '() newEnvironment '() classname parent globalEnv (lambda (v) v)))))

;I've separated the variable declarations for a class into static vars and non static vars.
;however, because a declaration could inlude other variables or functions on the right hand side, I made this function to build the
;"evaluation environment" that will be used to evaluate the right hand side of a declaration. Also this environment is what is returned
(define evalenv
  (lambda (staticvars vars env)
    (cons staticvars (cons (car vars) env))))

;Basically the same Mstate-build from P3, except I've distinguished static vars and non static vars
;Right now, static and non static methods are recognized as differnent but are treated the same way.
(define Mstate-build
  (lambda (code environment vars staticvars currclass parent globalEnv throw)
    (cond
      ((null? code) (evalenv staticvars vars environment))
      ((declaration? (nextstatement code)) (Mstate-build (cdr code) environment (declarevar (nextstatement code) vars (evalenv staticvars vars environment) currclass parent globalEnv throw)
                                                         staticvars currclass parent globalEnv throw))
      ((staticdecl? (nextstatement code)) (Mstate-build (cdr code) environment vars (Mstate-static-vars (nextstatement code) staticvars 
                                                                                                        (evalenv staticvars vars environment) currclass parent globalEnv throw) currclass parent globalEnv throw))
      ((function-decl? (nextstatement code)) (Mstate-build (cdr code) (Mstatefunc (nextstatement code) environment) vars staticvars currclass parent globalEnv throw))
      ((static-function-decl? (nextstatement code)) (Mstate-build (cdr code) (Mstatefunc (nextstatement code) environment) vars staticvars currclass parent globalEnv throw))
      (else (error "Invalid operations outside of main function")))))

;checks if the statement isa static declaration
(define staticdecl?
  (lambda (statement)
    (eq? (car statement) 'static-var)))

;Builds the list of static vars.  
;Basically the same function as declarevar, but the environment expected for declarevar is slightly different
(define Mstate-static-vars
  (lambda (statement staticvars env currclass parent globalEnv throw)
    (if (null? (cddr statement))
        (cons (list (getvarname statement) (box 'error)) staticvars)
        (cons (list (getvarname statement) (box (eval-expression (getvarvalue statement) env pprefix currclass parent globalEnv throw))) staticvars))))

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
    (cond
      ((list? statement) (eq? (car statement) 'funcall))
      (else #f))))

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
  (lambda (fparams cparams environment currclass parent globalEnv throw) 
    (cond
      ((and (null? fparams) (null? cparams)) (cdr environment))
      ((or (null? fparams) (null? cparams)) (error "Parameter mistmatch"))
      ((eq? (car fparams) '&) (cons (list (list (cadr fparams) (get-box (car cparams) environment))) (fenvironment (cddr fparams) (cdr cparams) environment currclass parent globalEnv throw)))
      (else (cons (list (car fparams) (box (eval-expression (car cparams) environment pprefix currclass parent globalEnv throw))) 
                  (fenvironment (cdr fparams) (cdr cparams) environment currclass parent globalEnv throw))))))

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
 (lambda (statement environment return next break continue currclass parent globalEnv throw)
   (letrec ((loop (lambda (condition body environment next throw2)
               ;if the condition is true, call Mstatement, with the approriate next, break, annd continue
                    ;else call next
               (if (eval-expression condition environment pprefix currclass parent globalEnv throw2)
                  (Mstatement body environment return (lambda (env) (loop condition body env next throw2)) (lambda (env) (next env)) 
                             (lambda (env) (loop condition body env next throw2)) currclass parent globalEnv throw2)
                  (next environment)))))
       (loop (whilecondition statement) (whilebody statement) environment next throw))))

;calls assignment when theres a declaration, with parameter assigntype of 'declaration
(define declarevar
  (lambda (statement newenvironment oldenvironment currclass parent globalEnv throw)
    (cond
      ;if this was of the form (var x) with no value, call assign with value of 'error
      ;note (cddr statement) is used to see if there is a value given: this may need to be changed if parser changes
      ((null? (cddr statement)) (assign (getvarname statement) 'error newenvironment oldenvironment 'declaration currclass parent globalEnv throw))
      ;else call assign with the value 
      (else (assign (getvarname statement) (getvarvalue statement) newenvironment oldenvironment 'declaration currclass parent globalEnv throw)))))

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

(define findclassmethods
  (lambda (class globalEnv)
    (caddr (findclassbody class globalEnv))))

(define findclassinstmethods
  (lambda (class globalEnv)
    (if (null? class)
        '()
        (append (delayer (cddadr (lookupClass class globalEnv)) '()) (findclassinstmethods (findparent class globalEnv) globalEnv)))))
       
  

(define delayer
  (lambda (l acc)
    (cond
      ((null? l) acc)
      (else (delayer (cdr l) (cons (caar l) acc))))))

(define getnonstaticvars
  (lambda (class globalEnv)
    (cadr (findclassbody class globalEnv))))

;this gets the default values of nonstatic variables in a class: this is for building an object closure
;these values will be linked to their corresponding variables when the object instance environment must be called
(define getnonstaticvarsvalues
  (lambda (class globalEnv)
    (letrec ((vars (getnonstaticvars class globalEnv))
             (loop (lambda (varlist currclass)
                     (cond
                       ((and (null? varlist) (null? (findparent currclass globalEnv))) '())
                       ((null? varlist) (getnonstaticvarsvalues (findparent currclass globalEnv) globalEnv))
                       (else (cons (box (unbox (cadar varlist))) (loop (cdr varlist) currclass)))))))
      (loop vars class))))

;this gets the names of nonstatic variables in a class: for building an object environment (not the closure)
(define getnonstaticvarsnames
  (lambda (class globalEnv)
    (letrec ((vars (getnonstaticvars class globalEnv))
             (loop (lambda (varlist currclass)
                     (cond
                       ((and (null? varlist) (null? (findparent currclass globalEnv))) '())
                       ((null? varlist) (getnonstaticvarsnames (findparent currclass globalEnv) globalEnv))
                       (else (cons (caar varlist) (loop (cdr varlist) currclass)))))))
      (loop vars class))))
 
;this gets the static variables of a class for an object closure
(define findstaticvarsRecurse
  (lambda (class globalEnv)
    (if (null? (findparent class globalEnv))
        (findstaticvars class globalEnv)
        (cons (car (findstaticvars class globalEnv)) (findstaticvarsRecurse (findparent class globalEnv) globalEnv)))))

;This is what is called for the new operator. It builds an object closure
(define buildObject
  (lambda (class globalEnv)
    (list class (getnonstaticvarsvalues class globalEnv) (findstaticvarsRecurse class globalEnv) (findclassinstmethods class globalEnv))))

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
  (lambda (varname value environment original-environment assigntype currclass parent globalEnv throw)
    (cond
      ((and (dot-expression? varname) (not (super? varname)) (not (eq? (lookupClass (cadr varname) globalEnv) 'classNotFound)))
       (assign (caddr varname) value (findstaticvars (cadr varname) globalEnv) original-environment assigntype currclass parent globalEnv throw))
      ((and (dot-expression? varname) (isObjectDot? varname globalEnv)) 
       (assign (caddr varname) value (buildInstanceEnv (cadr varname) environment globalEnv (getInstanceClass (cadr varname) environment globalEnv))
                                               original-environment assigntype '() '() globalEnv throw))
      ((and (dot-expression? varname) (super? varname) (not (null? parent)))
       (assign (caddr varname) value (getstaticvars (lookupSuper varname currclass parent globalEnv throw)) original-environment assigntype currclass parent globalEnv throw))
      ((and (not (null? environment)) (not (null? (cdr environment))) (null? (car environment))) (assign varname value (cdr environment) original-environment assigntype currclass parent globalEnv throw))
      ((and (dot-expression? varname) (super? varname) (null? parent)) (error "No parent"))
      ((and (null? environment) (eq? assigntype 'assign) (null? parent)) (error "variable not declared"))
      ((and (null? environment) (eq? assigntype 'assign)) (assign varname value (findclassbody (car parent) globalEnv throw)
                                                                  original-environment assigntype (car parent) 
                                                                  (findparent (car parent) globalEnv throw) globalEnv throw))
      ((and (eq? assigntype 'declaration) (eq? (lookupLayer varname (car environment)) '())) 
       (cons (assignLayer varname value (car environment) original-environment assigntype currclass parent globalEnv throw) (cdr environment)))
      ((eq? assigntype 'declaration) (error "redefining of variable"))
      ((eq? (lookupLayer varname (car environment)) '()) (cons (car environment) (assign varname value (cdr environment) 
                                                                                         original-environment assigntype currclass parent globalEnv throw)))
      (else (cons (assignLayer varname value (car environment) original-environment assigntype currclass parent globalEnv throw) (cdr environment))))))

  ;assignLayer assigns a value for a specific layer
(define assignLayer
  (lambda (varname value environment original-environment assigntype currclass parent globalEnv throw)
    (cond
      ;if the environment is empty and this is an assignment, then the variable was not declared
      ((and (null? environment) (eq? assigntype 'assign)) (error "variable not declared"))
      ;if the environment is empty and this is a declaration w/o assignment value, then add the variable with value 'error
      ((and (null? environment) (eq? assigntype 'declaration) (eq? value 'error) (cons (list varname (box 'error)) '())))
      ;if this variable has a non-error assignment value and is a declaration, evaluate the value and assign it
      ((and (null? environment) (eq? assigntype 'declaration)) (cons (list varname (box (eval-expression value original-environment pprefix currclass parent globalEnv throw))) '()))
     ; ((and (not (null? environment)) (not (null? (cdr environment))) (null? (car environment)))
      ;if the var is reached but this is a declaration, the variable is being redeclared
      ((and (not (null? environment)) (not (null? (cdr environment))) (null? (car environment)))
       (assignLayer varname value (cdr environment) original-environment assigntype currclass parent globalEnv throw))
      ((and (eq? assigntype 'declaration) (eq? (caar environment) varname)) (error "redefining of variable"))
      ;if the var is reached otherwise, update its value
      ((eq? (caar environment) varname) (begin (set-box! (cadar environment) (eval-expression value original-environment pprefix currclass parent globalEnv throw)) environment))
      ;otherwise recurse
      (else (cons (car environment) (assignLayer varname value (cdr environment) original-environment assigntype currclass parent globalEnv throw))))))



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
      ((null? (car environment)) '())
      ((eq? (caar environment) var) (unbox (cadar environment)))
      (else (lookupLayer var (cdr environment))))))

(define hasVar?
  (lambda (varname environment)
    (cond
      ((null? environment) #f)
      ((null? (lookupLayer varname (car environment))) (hasVar? varname (cdr environment)))
      (else #t))))


;fname params environment currclass parent globalEnv throw
;lookup determines which layer of the environment (or the first appearance) contains var, and calls lookupList to get its value
(define lookup
  (lambda (var environment isdot parent globalEnv throw)
    (cond
      ;if the var is a function call, return whatever the function returns
      ((funcall? var) (call-function (cadr var) (cddr var) environment '() parent globalEnv throw))
      ;((box? environment) (lookup var (buildInstanceEnv2 environment globalEnv) isdot parent globalEnv throw))
      ;If the var is a dot expression with a function call, look up the values in the object instance of the returned object of the function
      ((and (dot-expression? var) (funcall? (cadr var))) (lookup (caddr var) 
                                                                 (buildInstanceEnv2 (call-function (cadadr var) (cddadr var)
                                                                                                  environment '() parent globalEnv throw)
                                                                                   globalEnv)
                                                                 isdot parent globalEnv throw))
      ;if the dot expression is super, call this.super instead (not sure if this works or is used)
      ((and (dot-expression? var) (super? (cadr var)) (hasVar? 'this environment)) 
       (lookupInObj (caddr var) (cadr var) environment globalEnv throw))
      ((and (dot-expression? var) (super? (cadr var)) ) (lookup (caddr var) (getstaticvars (lookupSuper var 'error parent globalEnv))
                                               #t (car (lookupSuper var 'error parent globalEnv)) globalEnv throw))
      ;If the dot expression has an object on the far left, evalueate with lookupInObj
      ((and (dot-expression? var) (isObjectDot? var globalEnv)) (lookupInObj (caddr var) (cadr var) environment globalEnv throw #t))
      ((dot-expression? var) (lookup (getfunctionname var) (findstaticvars (cadr var) globalEnv ) #t 
                                     (findparent (cadr var) globalEnv) globalEnv throw))
      ((and (null? environment) isdot (not (null? parent))) (lookup var (findstaticvars (car parent) globalEnv)
                                               #t (findparent parent globalEnv) globalEnv throw))
      ((null? environment) (error "variable not declared"))
      ((not (null? (lookupLayer var (car environment)))) (lookupLayer var (car environment)))
      (else (lookup var (cdr environment) isdot parent globalEnv throw)))))

;returns true if a var is an object
(define isObject?
  (lambda (var globalEnv)
    (if (list? var)
        #f
        (and (eq? 'classNotFound (lookupClass var globalEnv))))))

;returns true if the given closure is of type Object (as opposed to function, number, or boolean)
(define isObjType?
  (lambda (l)
    (cond
      ((box? l) (isObjectType? (unbox l)))
      (else (and (list? l) (not (list? (car l))))))))

 
;This handles looking up items in an object.
;This is called for function calls or dot expressions involving objects
(define lookupInObj
  (lambda (var obj env globalEnv throw isFirst)
    (cond
      ((and (funcall? obj) (dot-expression? (cadr obj))) 
       (lookupInObj var (car (cdadr obj)) 
                    (buildInstanceEnv2
                     (call-function (cadr(cdadr obj)) (cddr obj) 
                                    (buildInstanceEnv obj env globalEnv (getInstanceClass obj env globalEnv))
                                    '() '() globalEnv throw)
                     globalEnv) globalEnv throw #f))
      ((and (boolean? var) (not var) (list? obj) (not (funcall? obj))) (lookupInObj (caddr obj) (cadr obj) env globalEnv throw #f))
      ((and (boolean? var) (not var) (list? obj)) (lookupInObj (caddr obj) (cadr obj) env globalEnv throw #f))
      ((and (boolean? var) (not var)) (lookupInObj 'this obj (buildInstanceEnv obj env globalEnv (getInstanceClass obj env globalEnv))
                                                   globalEnv throw #f) )
      ((and (isObject? obj globalEnv) isFirst (not (isObjType? (lookup var (buildInstanceEnv obj env globalEnv (getInstanceClass obj env globalEnv)) #t '() globalEnv throw))))
       (lookup var (buildInstanceEnv obj env globalEnv (getInstanceClass obj env globalEnv)) #t '() globalEnv throw))
      ((and (isObject? obj globalEnv) isFirst)
       (buildInstanceEnv2 (lookup var (buildInstanceEnv obj env globalEnv (getInstanceClass obj env globalEnv)) #t '() globalEnv throw)
                          globalEnv))
      ((and (isObject? obj globalEnv) (isObjType? (lookup var env #t '() globalEnv throw)))
       (buildInstanceEnv2 (lookup var env #t '() globalEnv throw) globalEnv))
      ((isObject? obj globalEnv) (lookup var env #t '() globalEnv throw))
      (else (lookupInObj var (getnextobj obj) (lookupInObj (getnextobj obj) (cadr obj) env globalEnv throw #t) globalEnv throw #f)))))

;abstraction                                         
(define getnextobj
  (lambda (obj)
    (caddr obj)))

;gets what class a given object is.  This was important so that the parent could be found and then super could be made
(define getInstanceClass
  (lambda (obj env globalEnv) 
    (cond
      ((list? obj) (getInstanceClass (cadr obj) env globalEnv))
      (else (car (lookup obj env #t '() globalEnv (lambda (v) v)))))))

;This builds the instance environment of a function given its closure and the global Environment
;this is different from buildInstanceEnv which builds the instance environment by first getting the object closure.
;Having the flexibility between both made coding a little easier. Now that I'm done though, probably only one is necessary.
;Note that the way I handled this and super is by simply making them objects in the instance environement. So this.function()
;would be handled exactly the same as localObject.function().  Likewise with super, and this is recursive so that all parents
;have a super instance in their nested super.  The integrity of the variables is kept because of blocks.
;NOTE that I know the way super is handled here is not fully correct because is you called say super.f(), and from within f()
;there was a this, the this would not necessarily reach the variables they are supposed to (ie overwritten methods or default values).
;I did ATTEMPT to make this possible with the changeclass method and by cdring nonstatic variable in the buildNonStatic function.
;I will * these attempts.
(define buildInstanceEnv2
  (lambda (objectClosure globalEnv)
    (letrec ((obj objectClosure)
             (nonstaticvarsvalues (cadr obj))
             (class (car obj))
             (staticvars (caddr obj))
             (body (cadddr obj))) 
       (list (list (list 'this (box obj))) (buildNonStatic (getnonstaticvarsnames class globalEnv) nonstaticvarsvalues)
            staticvars body (if (null? (findparent class globalEnv))
                                '()
                                (list (list 'super (box (buildInstanceEnv2 (changeclass objectClosure globalEnv) globalEnv ))  )))))))
;this function changes the listed class in an objects closure so that when making the supers the variables link correctly
(define changeclass
  (lambda (objclosure globalEnv)   ;*
    (letrec ((class (car objclosure))
             (parent (findparent class globalEnv)))
      (cons parent (cdr objclosure)))))

;this function builds an instance environment given the object, local environment, global environment, and its class
;It works very much the same way as buildInstanceEnv2, except it finds the objects closure on its own, and you give it
;the class (so that when you make a super, you pass the parent class as the class).
(define buildInstanceEnv
  (lambda (obj env globalEnv class) 
    (letrec ((objectClosure (lookup obj env #t '() globalEnv (lambda (v) v)))
             ;(class (car objectClosure))
             (nonstaticvarsvalues (cadr objectClosure))
             (staticvars (caddr objectClosure))
             (body (cadddr objectClosure)))
      (list (list (list 'this (box objectClosure))) (buildNonStatic (getnonstaticvarsnames class globalEnv) nonstaticvarsvalues)
            staticvars body (if (null? (findparent class globalEnv))
                                '()
                                (list (list 'super (box (buildInstanceEnv obj env globalEnv (findparent class globalEnv))))))))))
;this function links nonstatic variables to their values when buillding object instance variables.
(define  buildNonStatic
  (lambda (varnames varvalues)
    (cond
      ((and (null? varnames) (null? varvalues)) '())
      ((or (null? varnames) (null? varvalues)) '())
      ((not (eqv? (length varnames) (length varvalues))) (buildNonStatic varnames (cdr varvalues))) ; *
      ;* Note that for this to work more correctly, I would not only cdr these values, but move what I am cdring off
      ;to the back, so that the variables could still be reached if they are not overwritten
      (else (cons (list (car varnames) (car varvalues)) (buildNonStatic (cdr varnames) (cdr varvalues)))))))
          
;this returns true if a dot expression has an object on its farthest left side.
(define isObjectDot?
  (lambda (dotexpr globalEnv)
    (cond
      ((list? (cadr dotexpr)) (isObjectDot? (cadr dotexpr) globalEnv))
      (else (not (className? (cadr dotexpr) globalEnv))))))
;this gets the static variables of a given class.
(define findstaticvars
  (lambda (class globalEnv)
    (getstaticvars (lookupClass class globalEnv))))

;returns the return expression in a return statement
(define returnexpression
  (lambda (statement)
    (cadr statement)))

;returns the value of the return expression
(define Mreturn
  (lambda (code environment return currclass parent globalEnv throw)
    (return (make-english (eval-expression (returnexpression code) environment pprefix currclass parent globalEnv throw)))))

;translates scheme words to "English," for now this is only for #t and #f to 'true and 'false
(define make-english
  (lambda (expression)
    (cond
      ((eq? expression #t) 'true)
      ((eq? expression #f) 'false)
      (else expression))))

;evaluates an if statement
(define evaluate-if
  (lambda (ifstatement environment return next break continue currclass parent globalEnv throw)
    (cond
      ;checks if the if statement is true, returns the state ofthe expression if it is
      ((eval-expression (ifcond ifstatement) environment pprefix currclass parent globalEnv throw) (Mstatement (nextstatement (ifexpr ifstatement)) environment return next break continue currclass parent globalEnv throw))
      ;if the condition was false and there is no else, the continue with the remaining code
      ((null? (ifelse ifstatement)) (next environment))
      ;if there was an else, then Mstate with the else's if statement
      (else (Mstatement (nextstatement (ifelse ifstatement)) environment return next break continue currclass parent globalEnv throw)))))

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

(define new?
  (lambda (expr)
    (and (list? expr) (eq? 'new (car expr)))))

;This was built off of what was done in class.
;eval-expression will evaluate an expression, returning a number or #t or #f
;Note, this is Mvalue and Mboolean combined
; A MValue function that uses abstraction to
;allow expressions in prefix,
; postfix, or infix format
; Call as (eval-expression '(3 + 4) '() infix)
(define eval-expression
  (lambda (expression environment form currclass parent globalEnv throw) 
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
         ((and (not (list? expression)) (not (number? expression)) (eq? (lookup expression environment #t parent globalEnv throw) '())) (error "variable used before being declared"))
         ;if the expression is an uninitialized var, return an error
         ((and (not (list? expression)) (not (number? expression)) (eq? 'error (lookup expression environment #t parent globalEnv throw))) (error "error: Var not assigned a value"))
         ;if the expression is an initialized var, return its value
         ((and (not (list? expression)) (not (number? expression))) (lookup expression environment #t parent globalEnv throw))
         ;the next few lines perform arithmetic operations
         ((new? expression)  (buildObject (cadr expression) globalEnv))
         ;lookup is called for all dot expressions
         ((dot-expression? expression) (lookup expression environment #t parent globalEnv throw))
         ((funcall? expression) (call-function (cadr expression) (cddr expression) environment currclass parent globalEnv throw))
         ((eq? '+ (operator expression)) (+ (eval-expression (left-operand expression) environment form currclass parent globalEnv throw)
                                            (eval-expression (right-operand expression) environment form currclass parent globalEnv throw)))
         ;THIS LINE BELOW MUST BE CHANGED IF PARSER IS NO LONGER PREFIX to allow negatives support ( like -(* 5 9))
         ((and (eq? '- (operator expression)) (null? (cddr expression))) (- 0 (eval-expression (cadr expression) environment form currclass parent globalEnv throw)))
         ((eq? '- (operator expression)) (- (eval-expression (left-operand expression) environment form currclass parent globalEnv throw)
                                            (eval-expression (right-operand expression) environment form currclass parent globalEnv throw)))
         ((eq? '* (operator expression)) (* (eval-expression (left-operand expression) environment form currclass parent globalEnv throw)
                                            (eval-expression (right-operand expression) environment form currclass parent globalEnv throw)))
         ((eq? '/ (operator expression)) (floor (/ (eval-expression (left-operand expression) environment form currclass parent globalEnv throw)
                                                   (eval-expression (right-operand expression) environment form currclass parent globalEnv throw))))
         ((eq? '% (operator expression)) (modulo (eval-expression (left-operand expression) environment form currclass parent globalEnv throw)
                                                 (eval-expression (right-operand expression) environment form currclass parent globalEnv throw)))
         ;check if eqv? is okay
         ;the next few lines perform boolean operators
         ((eq? '== (operator expression)) (eqv? (eval-expression (left-operand expression) environment form currclass parent globalEnv throw)
                                                (eval-expression (right-operand expression) environment form currclass parent globalEnv throw)))
         ((eq? '!= (operator expression)) (not (eq? (eval-expression (left-operand expression) environment form currclass parent globalEnv throw)
                                                    (eval-expression (right-operand expression) environment form currclass parent globalEnv throw))))
         ((eq? '< (operator expression)) (< (eval-expression (left-operand expression) environment form currclass parent globalEnv throw)
                                            (eval-expression (right-operand expression) environment form currclass parent globalEnv throw)))
         ((eq? '> (operator expression)) (> (eval-expression (left-operand expression) environment form currclass parent globalEnv throw)
                                            (eval-expression (right-operand expression) environment form currclass parent globalEnv throw)))
         ((eq? '<= (operator expression)) (<= (eval-expression (left-operand expression) environment form currclass parent globalEnv throw)
                                              (eval-expression (right-operand expression) environment form currclass parent globalEnv throw)))
         ((eq? '>= (operator expression)) (>= (eval-expression (left-operand expression) environment form currclass parent globalEnv throw)
                                              (eval-expression (right-operand expression) environment form currclass parent globalEnv throw)))
         ((eq? '|| (operator expression)) (or (eval-expression (left-operand expression) environment form currclass parent globalEnv throw)
                                              (eval-expression (right-operand expression) environment form currclass parent globalEnv throw)))
         ((eq? '&& (operator expression)) (and (eval-expression (left-operand expression) environment form currclass parent globalEnv throw)
                                               (eval-expression (right-operand expression) environment form currclass parent globalEnv throw)))
         ;THIS LINE BELOW MUST BE CHANGED IF PARSER IS NO LONGER PREFIX to allow ! operator
         ((eq? '! (operator expression)) (not (eval-expression (left-operand expression) environment form currclass parent globalEnv throw) ))
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
       (letrec ((f (lookup 'main environment #f parent globalEnv (lambda (v) v))))
        (Mstate (getfunctionbody f) (addLayer environment)
                return (lambda (v) v) (lambda (v) v) (lambda (v) v) currclass parent globalEnv (lambda (v) v))))))))

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

(define insertthis
  (lambda (dotexpr)
    (if (list? dotexpr)
        (list 'dot (insertthis (cadr dotexpr)) (caddr dotexpr))
        (list 'dot 'this dotexpr))))

;given a function call, gets the function call environment
;dot expressions of objects are recursively handled.
;if it is a dot expression, the corresponding class environment is found and returned
;else, it is checked if the local environment has the function, and if not, checks the parent(s)
(define getfunctioncallenv
  (lambda (fname parent environment origenv globalEnv throw)
    (cond
      ((and (dot-expression? fname) (className? (cadr fname) globalEnv))
       (getfunctioncallenv (getfunctionname fname) (findparent (cadr fname) globalEnv) 
                          (findclassbody (cadr fname) globalEnv) (findclassbody (cadr fname) globalEnv) globalEnv throw))
       ((and (dot-expression? fname) (isObjectDot? fname globalEnv) (funcall? (cadr fname)))
       (buildInstanceEnv2 (call-function (cadadr fname) (cddadr fname) environment '() parent globalEnv throw) globalEnv)) 
      ((and (dot-expression? fname) (isObjectDot? fname globalEnv))
       (buildInstanceEnv (cadr fname) environment globalEnv (getInstanceClass (cadr fname) environment globalEnv)))
      ((and (dot-expression? fname) (super? (cadr fname)))
       (getfunctioncallenv (getfunctionname fname) (findparent (car parent) globalEnv) (findclassbody (car parent) globalEnv )
                           (findclassbody (car parent) globalEnv) globalEnv throw))
      
      ((null? environment) (getfunctioncallenv fname (if (eq? 'classNotFound (lookupClass parent globalEnv))
                                                                        (error "There is no parent to this class")
                                                                        (findparent parent globalEnv))
                                                              (if (eq? 'classNotFound (findparent parent globalEnv))
                                                                        (error "There is no parent to this class")
                                                                        (findclassbody parent globalEnv))
                                                              (if (eq? 'classNotFound (findparent parent globalEnv))
                                                                        (error "There is no parent to this class")
                                                                        (findclassbody parent globalEnv))
                                                              globalEnv throw))
      ((null? (lookupLayer fname (car environment))) (getfunctioncallenv fname parent (cdr environment) origenv globalEnv throw))
      (else origenv))))

;abstraction to get the function name
(define getfunctionname
  (lambda (expr)
    (if (dot-expression? expr)
        (caddr expr)
        expr)))

;calls a function
;first gets correct environment where the function is located, then gets the function closure from that environment
;then calls the function using the environment built by the closure consed to the function's environment
(define call-function 
  (lambda (fname params environment currclass parent globalEnv throw)
     (call/cc (lambda (return2)
       (letrec ((functioncallenv (getfunctioncallenv fname parent environment environment globalEnv throw))
                (f (lookup (getfunctionname fname) functioncallenv #f parent globalEnv throw)))
         (Mstate (cadr f) (addLayer (cons (car (list ((caddr f) (car f) params environment currclass parent globalEnv throw))) functioncallenv)) return2 (lambda (v) v) 
                  (lambda (v) v) (lambda (v) v) currclass parent globalEnv throw)
          environment)))))
  
;legitimizes return value: ie, if it is not a number or boolean, that means a return statement was not reached
(define legitimize
  (lambda (returnvalue)
    (cond
      ((or (number? returnvalue) 
           (eq? 'true returnvalue)
           (eq? 'false returnvalue)
           ) returnvalue)
      (else (error "No return reached")))))  ;returnvalue))));for debugging

;uncomment to run tests
;(display "1A")(interpret "test1.txt" "A")
;(display "2A")(interpret "test2.txt" "A")
;(display "3A")(interpret "test3.txt" "A")
;(display "4A")(interpret "test4.txt" "A")
;(display "5A")(interpret "test5.txt" "A")
;(display "6A")(interpret "test6.txt" "A")
;(display "7C")(interpret "test7.txt" "C");doesn't work
;(display "8")(interpret "test8.txt" "Square") ;doesn't work
;(display "9")(interpret "test9.txt" "List")
;(display "10")(interpret "test10.txt" "Primes") ;doesn't work
;(display "11")(interpret "test11.txt" "A")
;(display "12")(interpret "test12.txt" "A")
;(display "13")(interpret "test13.txt" "A") ;doesn't work
;(display "14")(interpret "test14.txt" "A")
;(display "15")(interpret "test15.txt" "A")

;Test function: will test a list of testfiles and return the list of their return values
;(define doTests
 ; (lambda (filname parent)
  ;  (cond
   ;   ((null? tests) '())
    ;  (else (cons (interpret (car tests)) (doTests (cdr tests)))))))
