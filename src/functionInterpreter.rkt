;Author: Shiva Mohan
;Additional authors: Xiaoyao Ni, Martin Bui

#lang racket
(require "functionParser.rkt")


; the launching function of our interpreter
(define interpret
  (lambda (filename)
    (define finalState (Mstate (parser filename) emptystate returnContinuation returnContinuation returnContinuation (lambda (v) (error "invalid throw")) #t))
    ;finalState))
    (Mfuncall '(funcall main) finalState returnContinuation returnContinuation returnContinuation (lambda (v) (error "invalid throw")) #t)))
    ;(Mstate (parser filename) emptystate returnContinuation returnContinuation returnContinuation (lambda (v) (error "invalid throw")))))
    


; adds a layer to the front of the current state when entering a block
; state = '((variable) (value)) , '( (() variable) ( () value) )
(define addLayer
  (lambda (state)
    (cons (cons '() (varlist state)) (cons (cons '() (valuelist state)) '()))))

; removes the layer when exiting a block 
(define removeLayer
  (lambda (state)
    (if (equal? (cons (rest (varlist state)) (cons (rest (valuelist state)) '())) emptystate)
        (error "invalid use of break or continue")
        (cons (rest (varlist state)) (cons (rest (valuelist state)) '())))))


; main function that takes each type of statement to its corresponding function 
(define Mstate    
  (lambda (statement state break next cont throw value)
    (cond
      ((eq?  '() statement)                   state) 
      ((eq?  (stmtType statement) 'var)       (call/cc (lambda (k) (Mstate (rest statement) (Mdeclare (first statement) state) k next cont throw value))))
      ((eq?  (stmtType statement) '=)         (call/cc (lambda (k) (Mstate (rest statement) (Massign (first statement)  state) k next cont throw value))))
      ((eq?  (stmtType statement) 'return)    (if value (break   (Mreturn (val (first statement)) state)) state))
      ((eq?  (stmtType statement) 'if)        (call/cc (lambda (k) (Mstate (rest statement) (Mif (first statement) state break next cont throw value) k next cont throw value))))
      ((eq?  (stmtType statement) 'while)     (call/cc (lambda (k) (Mstate (rest statement) (call/cc (lambda (v) (Mwhile (first statement) state break v cont throw value))) k next cont throw value))))
      ((eq?  (stmtType statement) 'begin)     (call/cc (lambda (k) (Mstate (rest statement) (Mbegin (rest (first statement)) (addLayer state) break next cont throw value) k next cont throw value))))
      ((eq?  (stmtType statement) 'try)       (call/cc (lambda (k) (Mstate (rest statement) (Mtry (first statement) state break next cont throw value) k next cont throw value))))
      ((eq?  (stmtType statement) 'catch)     (call/cc (lambda (k) (Mstate (catch (first statement)) state k next cont throw value))))
      ((eq?  (stmtType statement) 'continue)  (cont    (removeLayer state)))
      ((eq?  (stmtType statement) 'finally)   (call/cc (lambda (k) (Mstate (finally statement) state k next cont throw value))))
      ((eq?  (stmtType statement) 'break)     (next    (removeLayer state)))
      ((eq?  (stmtType statement) 'throw)     (throw   (cons (Mboolean (val (first statement)) state) state)))
      ((eq?  (stmtType statement) 'function)  (Mstate  (rest statement) (Mfunction (car statement) state) break next cont throw value))
      ((eq?  (stmtType statement) 'funcall)   (define funcallResult (call/cc (lambda (k) (Mfuncall (first statement) state k next cont k value))))
                                              (if (isState funcallResult)
                                                  (call/cc (lambda (k) (Mstate (rest statement) funcallResult k next cont throw value)))
                                                  (call/cc (lambda (k) (Mstate (rest statement) (call/cc (lambda (k) (Mfuncall (first statement) state k next cont k #f))) k next cont throw value)))))
      (else 				      (error   "invalid statement")))))




       
;abstraction used
(define val cadr)
(define finally cadar)

; declares a variable, returns the updated state 
(define Mdeclare                              
  (lambda (statement state) 
    (cond
      ((not (null? (varlist state)))
       (cond
         ((and (list? (first (varlist state))) (null? (rest (rest statement))))    (cons  (cons (cons (condition statement) (first (varlist state))) (rest (varlist state))) (cons (cons (cons '? (nestedListVariable state)) (nestedListValue state)) '())))
         ((and (list? (first (varlist state))) (value? (body statement) state))    (cons  (cons (cons (condition statement) (first (varlist state))) (rest (varlist state))) (cons (cons (cons (Mboolean (body statement) state) (nestedListVariable state)) (nestedListValue state)) '())))
         ((null? (rest (rest statement)))                                          (cons  (cons (first (rest statement)) (varlist state)) (cons (cons '? (valuelist state)) '())))
         ((value? (body statement) state)                                          (cons  (cons (condition statement) (varlist state)) (cons (cons (Mboolean (body statement) state) (valuelist state)) '())))
         (else                                                                     (error "variable not assigned"))))
      ((null? (rest (rest statement)))        (cons  (cons (first (rest statement)) (varlist state)) (cons (cons '? (valuelist state)) '())))
      ((value? (body statement) state)        (cons  (cons (condition statement) (varlist state)) (cons (cons (Mboolean (body statement) state) (valuelist state)) '())))
      ((null? (body statement))               (error "invalid assignment"))
      (else                                   (error "variable not assigned")))))

;abstraction used
(define nestedListVariable caadr)
(define nestedListValue cdadr)

; assigns a value to a variable, returns the updated state
(define Massign 
  (lambda (statement state)
    (cond
     ((null? statement)                                                             (state))
     ; checks if the variable is a variable, value is actually an appropriate value, and that this is actually the Massign function
     ((and (variable? (condition statement) state) (value? (body statement) state)) (update (condition statement) (body statement) state))                                                                   
     (else                                                                          (error "variable not declared/out of scope")))))

; if statement, returns the updated state  
(define Mif
  (lambda (statement state break next cont throw value)
    (cond
      ((Mboolean (condition statement) state)                                                             (Mstate (cons (body statement) '()) state break next cont throw value))           
      ((and (not (Mboolean (condition statement) state)) (not (null? (second (rest statement)))))         (Mstate (cons (body (rest statement)) '()) state break next cont throw value))    
      (else                                                                                               state))))

;abstraction
(define second cddr)

; while statement, returns the updated state  
(define Mwhile
  (lambda (statement state break next cont throw value)
    (cond
      ((Mboolean (condition statement) state)  (Mwhile statement (call/cc (lambda (x) (Mstate (cons (body statement) '()) state break next x throw value))) break next cont throw value))
      (else                                    state))))

; returns a value for a statement 
(define Mreturn 
  (lambda (statement state)
    (cond
      ((null? statement)        '())
      (else                     (define output (Mboolean statement state)) (if (boolean? output)
                                                                               (if output 'true 'false)
                                                                               output)))))
      
; creates blocks and adds layers to the state 
(define Mbegin
  (lambda (statement state break next cont throw value)
    (cond
      ((null? statement) (removeLayer state))
      (else              (Mbegin (rest statement) (Mstate (cons (first statement) '()) state break next cont throw value) break next cont throw value)))))

; calls different type of try functions depending on if there is catch and finally statements 
(define Mtry
  (lambda (statement state break next cont throw value)
    (cond
      ((and (hasCatch? statement) (hasFinally? statement)) (MtryCatchFinally statement state break next cont throw value))
      ((hasCatch? statement)                               (MtryCatch statement state break next cont throw value))
      ((hasFinally? statement)                             (MtryFinally statement state break next cont throw value))
      (else                                                (error "Invalid try statement")))))


; executes try with catch and finally
(define MtryCatchFinally
  (lambda (statement state break next cont throw value)
    (letrec ((e (call/cc (lambda (k) (Mstate (try statement) state break next cont k value)))))
      (cond
        ((atom? (first e))   (Mstate (final statement) (Mstate (cons (catch statement) '()) (Mdeclare (MakeVariable statement (first e)) (rest e)) break next cont throw value) break next cont throw value))
        (else                (Mstate (final statement) e break next cont throw value))))))

; executes try with catch 
(define MtryCatch
  (lambda (statement state break next cont throw value)
    (letrec ((e (call/cc (lambda (k) (Mstate (try statement) state break next cont k value)))))  ; if exeception caught, returns exception value to e, along with the respective state (environment). Otherwise, e is the final state (environment)
      (cond
        ((atom? (first e))   (Mstate (cons (catch statement) '()) (Mdeclare (MakeVariable statement (first e)) (rest e)) break next cont throw value))
        (else                e)))))  


; executes try with finally  
(define MtryFinally
  (lambda (statement state break next cont throw value)
     (Mstate (final statement) (Mstate (try statement) state break next cont throw value) break next cont throw value)))

; a helper function that checks if there is a catch statement 
(define hasCatch?
  (lambda (statement)
    (if (< (length statement) 3)
        (error "try statement is incorrect")
        (search? 'catch statement))))

; a helper function that checks if there is a finally statement 
(define hasFinally?
  (lambda (statement)
    (if (< (length statement) 3)
        (error "try statement is incorrect")
        (search? 'finally statement))))

(define hasReturn?
  (lambda (lis)
    (cond
      ((null? lis) #f)
      ((list? (car lis)) (or (hasReturn? (car lis)) (hasReturn? (cdr lis))))
      ((eq? (car lis) 'return) #t)
      (else (hasReturn? (cdr lis))))))
      
                           
;abstraction used
(define exception (lambda (statement) (caar (cdaddr statement))))
(define MakeVariable (lambda (var value) (cons 'var (cons (exception var) (cons value '())))))
(define final cdddr)
(define catch caddr)
(define try cadr)

(define functionClosure
  (lambda (formalParam funcBody activeBindings)
    (box (list formalParam funcBody activeBindings))))

(define Mfunction
  (lambda (function state)
    (cond
      ((null? (functionBody function)) state)
      (else (list (cons (functionName function) (varlist state)) (cons (functionClosure (functionParameters function) (functionBody function) (activeBindings (functionParameters function) state)) (valuelist state)))))))


(define activeBindings
  (lambda (formalParameters state) ;used to be formalParameters
    (cond
      ((null? formalParameters) state) 
      (else (activeBindings (rest formalParameters) (Mdeclare (list 'var (first formalParameters)) state))))))


(define functionName cadr)
(define functionParameters caddr)
(define functionBody cadddr)

; active bind = '((a b) (? ?))
; function = (funcall fib (- a 2))
; actual param = (- a 2)
; formal param = ((a) (?))
; update 
(define Mfuncall
  (lambda (functionCall state break next cont throw value)
    (define closure (unbox (getValue (functionName functionCall) state)))
    (define newEnvironment (bind functionCall closure state))
    (if (= (length (first closure)) (length (functionValues functionCall)))
        (Mstate (functionCallBody closure) newEnvironment break next cont throw value)
        (error "Mismatched parameters and arguments"))))

(define functionValues cddr)
(define functionCallBody cadr)

; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

; HELPER FUNCTIONS USED:

; helper function that checks if it is a variable in the state 
(define variable?
  (lambda (statement state)
    (cond
      ((null? statement)                   (error "variable cannot be null"))
      ((search? statement (varlist state)) #t)
      (else                                #f))))


; boolean function which returns true if the given expression evaluates to a number(value) else returns false. Modeled after M-integer
(define value?          
  (lambda (expression state)
    (cond
      ((number? expression)                                                                                                       #t)
      ((variable? expression state)                                                                                               #t)
      ((or (eq? #t expression) (eq? #f expression))                                                                               #t)
      ((or (eq? 'false expression) (eq? 'true expression))                                                                        #t) 
      ((or (number?  (Mboolean expression state)) (or (eq? (Mboolean expression state) #f) (eq? (Mboolean expression state) #t))) #t)
      (else                                                                                                                       #f))))

; gets the value of a variable from the state 
(define getValue
  (lambda (variable state)
    (get-number (search-number variable (flatten-cps (varlist state) returnContinuation)) 1 (flatten-cps (valuelist state) returnContinuation))))

; evaluates any integer expression (provided by professor Connamacher)
(define Minteger        
  (lambda (expression state)
    (cond
      ((number? expression)                 expression)
      ((variable? expression state)         (getValue expression state))
      ((atom? expression)                   (error "using before declaring"))
      ((eq? (operator expression) '+)       (+ (Minteger (leftoperand expression) state) (Minteger (rightoperand expression) state)))
      ((and (eq? (operator expression) '-)  (null? (second expression))) (- (Minteger (leftoperand expression) state) (* 2 (Minteger (leftoperand expression) state))))  ; for unary operator
      ((eq? (operator expression) '-)       (- (Minteger (leftoperand expression) state) (Minteger (rightoperand expression) state)))
      ((eq? (operator expression) '*)       (* (Minteger (leftoperand expression) state) (Minteger (rightoperand expression) state)))
      ((eq? (operator expression) '/)       (quotient (Minteger (leftoperand expression) state) (Minteger (rightoperand expression) state)))
      ((eq? (operator expression) '%)       (remainder (Minteger (leftoperand expression) state) (Minteger (rightoperand expression) state)))
      ((eq? (operator expression) '^)       (expt (Minteger (leftoperand expression) state) (Minteger (rightoperand expression) state)))
      ((eq? (operator expression) 'funcall) (call/cc (lambda (k) (Mfuncall expression state k returnContinuation returnContinuation returnContinuation #t))))
      (else                                 (error "bad-operator")))))

; evaluates boolean expressions 
(define Mboolean 
  (lambda (expression state)
    (cond
      ((number? expression)                 expression)
      ((variable? expression state)         (getValue expression state))
      ((eq? expression #t)                  #t)
      ((eq? expression #f)                  #f)
      ((eq? expression 'true)               #t)
      ((eq? expression 'false)              #f)
      ((box?  expression)                   expression)
      ((atom? expression)                   (error "using before declaring"))
      ((eq? (operator expression) 'funcall) (call/cc (lambda (k) (Mfuncall expression state k returnContinuation returnContinuation returnContinuation #t))))
      ((eq? (operator expression) '+)       (Minteger expression state))
      ((eq? (operator expression) '-)       (Minteger expression state))
      ((eq? (operator expression) '*)       (Minteger expression state))
      ((eq? (operator expression) '/)       (Minteger expression state))
      ((eq? (operator expression) '%)       (Minteger expression state))
      ((eq? (operator expression) '>=)      (>=(Mboolean(leftoperand expression) state) (Mboolean (rightoperand expression) state)))
      ((eq? (operator expression) '>)       (> (Mboolean(leftoperand expression) state) (Mboolean (rightoperand expression) state)))
      ((eq? (operator expression) '<=)      (<=(Mboolean(leftoperand expression) state) (Mboolean (rightoperand expression) state)))
      ((eq? (operator expression) '<)       (< (Mboolean(leftoperand expression) state) (Mboolean (rightoperand expression) state)))
      ((eq? (operator expression) '==)      (eq? (Mboolean(leftoperand expression) state) (Mboolean (rightoperand expression) state)))
      ((eq? (operator expression) '!=)      (not (eq? (Mboolean(leftoperand expression) state) (Mboolean (rightoperand expression) state))))
      ((eq? (operator expression) '||)      (define left (Mboolean(leftoperand expression) state)) (define right (Mboolean(rightoperand expression) state))
                                            (or (or left (eq? left 'true)) (or right (eq? right 'true))))
      ;((eq? (operator expression) '||)      (or  (eq? (Mboolean(leftoperand expression) state) #t) (eq? (Mboolean (rightoperand expression) state) #t)))
      ((eq? (operator expression) '&&)      (define left (Mboolean(leftoperand expression) state)) (define right (Mboolean(rightoperand expression) state))
                                            (and (or left (eq? left 'true)) (or right (eq? right 'true))))
      ((eq? (operator expression) '!)       (define left (Mboolean(leftoperand expression) state))
                                            (if (eq? left 'false)
                                                #t
                                                (not left)))
      ;((eq? (operator expression) 'funcall) (call/cc (lambda (k) (Mfuncall expression state k returnContinuation returnContinuation returnContinuation))))
      (else                            (error "bad-operator")))))

; boolean function which searches for an atom "a" in a list "lis"
;state = '( ( (x y) z jk) ( (0 0) 0 0)) ..... search (x (car (car state))
(define search?         
  (lambda (a lis)
    (cond
      ((or    (null? lis))                 #f)
      ((pair? (first lis))                 (or (search? a (first lis)) (search? a (rest lis))))
      ((eq? a (first lis))                 #t)
      (else                                (search? a (rest lis))))))

; returns the number corresponding to the location of an element "a" in a list "lis"
(define search-number   
  (lambda (a lis)
    (cond
      ((null? lis)         1)
      ((pair? (first lis)) (+ (search-number a (first lis)) (search-number a (rest lis))))
      ((eq? a (first lis)) 1)
      (else                (+ 1 (search-number a (rest lis)))))))

; helper function for update and getValue that flattens a nested list in cps
(define flatten-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return '()))
      ((list? (first lis)) (flatten-cps (first lis)
                                      (lambda (v1) (flatten-cps (rest lis)
                                                                (lambda (v2) (append-cps v1 v2 return))))))
      (else (flatten-cps (rest lis) (lambda (v) (return (cons (first lis) v))))))))

; helper function for flatten-cps that appends 2 lists in cps
(define append-cps
  (lambda (l1 l2 return)
    (if (null? l1)
        (return l2)
        (append-cps (rest l1) l2 (lambda (v) (return (cons (first l1) v)))))))

; returns the corresponding element of the number in the list  
(define get-number     
  (lambda (num current_num lis)
    (cond
      ((null? lis)                        (error "variable not assigned value"))
      ((eq? num current_num)              (if (or (null? (first lis)) (eq? '? (first lis)))
                                              (error "variable not assigned value") 
                                              (first lis)))
      (else                               (get-number num (+ 1 current_num) (rest lis))))))


; updates the atom "value" at the "num"th location in a list "lis"
;(eg: num=2, value=3, list='(1 5 6) will return '(1 3 6))
(define add-at-number-cps   
  (lambda (num current_num value lis state return) ;current_num stores the current location of the pointer in the list
    (cond
      ((null? lis)           (return current_num '()))
      ((list? (first lis))   (add-at-number-cps num current_num value (first lis) state
                                             (lambda (v1 v2) (add-at-number-cps num v1 value (rest lis) state
                                                                             (lambda (v3 v4) (return v3 (cons v2 v4)))))))
      ((eq? num current_num) (return (+ 1 current_num) (cons (Mboolean value state) (rest lis))))  
      (else                  (add-at-number-cps num (+ 1 current_num) value (rest lis) state (lambda (v1 v2) (return v1 (cons (first lis) v2))))))))

(define hasReturn
  (lambda (statement)
    (if (isState statement)
        #f
        #t)))

(define isState
  (lambda (state)
    (if (not (atom? state))
        (if (and (and (list? (car state)) (list? (cadr state))) (null? (cddr state)))
            #t
            #f)
        #f)))

(define bind
  (lambda (functionCall closure state)
    (bindParameters (first closure) (functionValues functionCall) (change (caar (functionValues closure)) state) state)))


(define bindParameters
  (lambda (parameters values environment state)
    (cond
      ((null? parameters) environment)
      ((null? values) environment)
      (else (bindParameters (rest parameters) (rest values) (Massign (list '= (first parameters) (Mboolean (first values) state)) environment) state)))))

(define change
  (lambda (lis state)
    (cond
      ((null? lis) (list (reverse* (car state)) (reverse* (cadr state))))
      (else (if (hasx? (car lis) (car state))
                (change (cdr lis) state)
                (change (cdr lis) (Mdeclare (list 'var (car lis)) state)))))))

(define reverse-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return lis))
      ((list? (car lis)) (reverse-cps (car lis)
                                      (lambda (v) (return (append-cps (cdr lis) (cons v '()) (lambda (v) v))))))
      (else (reverse-cps (cdr lis)
                         (lambda (v) (return (append-cps v (cons (car lis) '()) (lambda (v) v)))))))))

(define reverse*
  (lambda (lis)
    (reverse-cps lis (lambda (v) v))))


(define hasx?
  (lambda (x lis)
    (cond
      ((null? lis) #f)
      ((eq? (first lis) x) #t)
      (else (hasx? x (rest lis))))))


; Our state uses 2 lists to monitor the variables and values.
; This function finds the "variable" in the variable list and updates its "value" in the corresponding value list.
(define update         
  (lambda (variable value state)
    (cond
      ((or (null? variable) (null? value)) (error "variable/value cannot be null"))
      (else                                (cons (varlist state) (cons (add-at-number-cps (search-number variable (flatten-cps (varlist state) returnContinuation)) 1 value (valuelist state) state (lambda (v1 v2) v2)) '()))))))


; Other commonly used abstraction:
(define operator     (lambda (expression) (car expression)))
(define leftoperand  cadr)
(define rightoperand caddr)
(define condition    (lambda (statement) (cadr statement)))
(define body         (lambda (statement) (caddr statement)))
(define (atom? x)    (not (pair? x)))
(define stmtType     caar)
(define varlist      (lambda (state) (car state)))
(define valuelist    (lambda (state) (cadr state)))
(define emptystate   '(()()))
(define rest         cdr)
(define first        car)
(define returnContinuation (lambda (v) v))
