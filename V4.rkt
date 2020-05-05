;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname V4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|
Ben McClure

URL: https://www.radford.edu/~itec380//2019fall-ibarland/Homeworks/V6.html


#|
>>>>>>>>>>>>>>>>>>>>>>>>PROLOG<<<<<<<<<<<<<<<<<<<<<<<
last([Item],Item).
last([F|R],Item) :- last(R,Item).

nextToLast([Item,_],Item).
nextToLast([_|R],Item) :- nextToLast(R,Item).

lastTwoReversed([],[]).
lastTwoReversed([X,Y],[Y,X]).
lastTwoReversed(List,ListOf2) :- last(List,L), nextToLast(List,NL), [L,NL]=ListOf2.

reverseLastTwo([A,B],[B,A]).
reverseLastTwo([A,B,C|X],[A|Y]) :- reverseLastTwo([B,C|X],Y).
>>>>>>>>>>>>>>>>>>>>>>>>PROLOG<<<<<<<<<<<<<<<<<<<<<<<
|#


;; A V4 implementation.
@see https://www.radford.edu/itec380/2019fall-ibarland/Homeworks/Project/V0.html
@see https://www.radford.edu/itec380/2019fall-ibarland/Homeworks/V2.html
@see https://www.radford.edu/itec380/2019fall-ibarland/Homeworks/V4.html


@author ibarland
@version 2019-Nov-28

@license CC-BY -- share/adapt this file freely, but include attribution, thx.
    https://creativecommons.org/licenses/by/4.0/ 
    https://creativecommons.org/licenses/by/4.0/legalcode 
Including a link to the *original* file satisifies "appropriate attribution".
@cite https://www.radford.edu/itec380/2019fall-ibarland/Homeworks/Project/V0.rkt
@cite https://learn.radford.edu/d2l/home/146899
|#

(require "student-extras.rkt")
(require "scanner.rkt")
(provide (all-defined-out))
#|
  Expr       ::= Num | Paren | BinOp | IfZero
  Paren      ::= (: Expr :)
  BinOp      ::= # Op  Expr Expr #
  Op         ::= yasss | yoink | yeet
  IfZero     ::= ?0 Expr Expr Expr :!                  ;>>>V2
  IfDiv      ::= ?| Expr Expr thn Expr els Expr :|     ;>>>V2
  LetExpr    ::= let Id := Expr {: Expr :}             ;>>>V2
  FuncExpr   ::= fnc Id --> Expr                       ;>>>V4                    
  FuncApplyExpr ::= app Expr wth Expr                  ;>>>V4                    

|#

; An Expr is:
;  - a number
;  - (make-paren [Expr])
;  - (make-binop [Op] [Expr] [Expr])
;  - (make-if-zero  [Expr] [Expr] [Expr])
;  - (make-if-div [Expr] [Expr] [Expr])       ;>>>V1
;  - string                                   ;>>>V2                    
;  - (make-let-expr   [String] [Expr] [Expr]) ;>>>V2                   
;  - (make-func-expr [Id] [Expr])                ;>>>V4                   
;  - (make-func-apply-expr [Expr] [Expr])        ;>>>V4                   

(define OPS (list "yasss" "yoink" "yeet" "yeesh")) ;>>>V1
; An Op is: (one-of OPS)


(define-struct binop (op left right))
(define-struct paren (e))
(define-struct if-zero (test zero-ans pos-ans))
(define-struct if-div (divisor dividend evenly unevenly)) ;>>> V1                    
; A string represents an V2 identifier                    
(define-struct let-expr   (id rhs body))     ;>>>V2                    
(define-struct func-expr (param body env))       ;>>>V4                   
(define-struct func-apply-expr (fun arg))    ;>>>V4



;a 'binding' is:
;       a list containing two elements: Id, Val
;  '("x" 17)
;  '("y" 0)

; an 'environment' is a list of bindings
;  '()
; '( '("x" 17) )
; '( '("x" 17) '("y" 0) )



; Examples of Expr:
34
(make-paren 34)
(make-binop "yasss" 3 4)
(make-binop "yasss" (make-paren 34) (make-binop "yeet" 3 4))
(make-if-zero 3 7 9)
(make-if-zero (make-paren 1)
              (make-binop "yasss" (make-paren 34) (make-binop "yeet" 3 4))
              (make-if-zero 0 7 9))


; string->expr : string -> Expr
; given a string, return the parse-tree for the V0 expression at its front.
;
(define (string->expr prog)
  (parse! (create-scanner prog)))


; parse! : scanner -> Expr
; given a scanner, consume one V0 expression off the front of it
; and
; return the corresponding parse-tree.
;
(define (parse! s)
  ; Recursive-descent parsing:

  (cond [(number? (peek s)) (pop! s)]
        [(string=? "(" (peek s))
         (let* {[_ (check-token= (pop! s) "(")] ; Starts with "(:"; consume the '('
                [_ (check-token= (pop! s) ":")] ; ...and the ':' too.
                [the-inside-expr (parse! s)]
                [_ (check-token= (pop! s) ":")] ; consume the ':' and ')'
                [_ (check-token= (pop! s) ")")] 
                }
           (make-paren the-inside-expr))]
        [(string=? "#" (peek s))
         (let* {[_ (check-token= (pop! s) "#")]
                [op     (pop! s)]
                [_ (if (not (member? op OPS))
                       (error 'parse "Unknown op " op)
                       'keep-on-going)]
                [lefty  (parse! s)]
                [righty (parse! s)]
                [_ (check-token= (pop! s) "#")]
                }
           (make-binop op lefty righty))]
        [(string=? "?" (peek s))
         (let* {[_ (check-token= (pop! s) "?")] ; throw away the opening "?" of "?0" OR "?|"                    
                [type-of-if (pop! s)]           ;>>>V1                    
                }
           (cond [(and (number? type-of-if) (zero? type-of-if)) ;>>>V1
                  (let* {[the-test (parse! s)]
                         [the-zero-ans (parse! s)]
                         [the-pos-ans  (parse! s)]
                         [_ (check-token= (pop! s) ":")] ; throw away ':' of ":!"
                         [_ (check-token= (pop! s) "!")] ; throw away '!' of ":!"
                         }
                    (make-if-zero the-test the-zero-ans the-pos-ans))]
                 [(string=? "|" type-of-if)                ;>>>V1                    
                  (let* {[divisor  (parse! s)]             ;>>>V1                    
                         [dividend (parse! s)]             ;>>>V1                    
                         [_ (check-token= (pop! s) "thn")] ;>>>V1                    
                         [evenly (parse! s)]               ;>>>V1                    
                         [_ (check-token= (pop! s) "els")] ;>>>V1                    
                         [unevenly  (parse! s)]            ;>>>V1                    
                         [_ (check-token= (pop! s) ":")]   ; throw away ':' of ":!" ;>>>V1                    
                         [_ (check-token= (pop! s) "|")]   ; throw away '|' of ":|" ;>>>V1                    
                         }                    
                    (make-if-div divisor dividend evenly unevenly))] ;>>>V1                    
                 [else (error 'parse "unknown token after '?': got " type-of-if)]))]
        [(string=? "let" (peek s)) ;>>>V2                    
         (let* {[_ (check-token= (pop! s) "let")] ;>>>V2                    
                [id     (pop! s)]                ;>>>V2                    
                [_ (check-token= (pop! s) ":")] ;>>>V2                    
                [_ (check-token= (pop! s) "=")] ;>>>V2                    
                [rhs (parse! s)]                ;>>>V2                    
                [_ (check-token= (pop! s) "{")] ;>>>V2                    
                [_ (check-token= (pop! s) ":")] ;>>>V2                    
                [body (parse! s)]               ;>>>V2                    
                [_ (check-token= (pop! s) ":")] ;>>>V2                    
                [_ (check-token= (pop! s) "}")] ;>>>V2                    
                }                               ;>>>V2
           (make-let-expr id rhs body))]        ;>>>V2                    
        [(string=? "faq" (peek s))                 ;>>>V4                    
         (let* {[_ (check-token= (pop! s) "faq")] ;>>>V4                   
                [id (pop! s)]                     ;>>>V4                   
                [_ (check-token= (pop! s) "mek")] ;>>>V4                   
                [body (parse! s)]                 ;>>>V4
                [env #f]             ;>>>V5
                }                                 ;>>>V4                   
           (make-func-expr id body env))]             ;>>>V4                   
        [(string=? "mod" (peek s))                ;>>>V4                   
         (let* {[_ (check-token= (pop! s) "mod")] ;>>>V4                   
                [arg (parse! s)]                  ;>>>V4                   
                [_ (check-token= (pop! s) "wtf")] ;>>>V4                   
                [func (parse! s)]                 ;>>>V4
                }                                 ;>>>V4                   
           (make-func-apply-expr func arg))]      ;>>>V4                   
        
        [(string? (peek s)) (pop! s)]   ; must be an identifier  ;>>>V2
        [else (error 'parse! (format "syntax error -- something has gone awry!  Seeing ~v" (peek s)))]))
  

; eval : Expr -> Num
; Return the value which this Expr evaluates to.
; In V0, the only type of value is a Num.
;
(define (eval e)
  (eval-with-env e '())
  )


#|
V5 Tests for eval-with-env
|#

;1: What does the V5 program #yasss y 3# evaluate to, with the environment where x is bound to 5 and y is bound to 7?
(check-expect (eval-with-env (make-binop "yasss" "y" 3) (list (list "x" 5) (list "y" 7))) 10)

;2: What does the V5 program #yasss y 3# evaluate to, with the environment where x is bound to 5 and y is bound to 99?
(check-expect (eval-with-env (make-binop "yasss" "y" 3) (list (list "x" 5) (list "y" 99))) 102)

;3: What does the V5 program let y := 5 {: #yasss y 3# :} evaluate to, with the environment where x is bound to 5 and y is bound to 7?
(check-expect (eval-with-env (parse! (create-scanner "let y := 5 {: #yasss y 3# :}")) (list (list "x" 5) (list "y" 7))) 8)

;4: What does the V5 program let y := #yeet x 2# {: #yasss y 3# :} evaluate to, with the environment where x is bound to 5 and y is bound to 7?
(check-expect (eval-with-env (parse! (create-scanner "let y := #yeet x 2# {: #yasss y 3# :}")) (list (list "x" 5) (list "y" 7))) 13)


(check-expect (eval-with-env (parse! (create-scanner "let m := 100
                                          {: let addM := faq x mek #yasss x m#
                                             {: #yasss
                                                let m := 5 {: mod 3 wtf addM :}
                                                let m := 4 {: mod 3 wtf addM :}
                                                #
                                              :}
                                          :}"))
                             '())
              15)


; eval-with-env : Expr, environment -> Num
; Return the value which this Expr evaluates to.
; In V0, the only type of value is a Num.
;
(define (eval-with-env e env)
  (cond [(number? e) e]
        [(paren? e) (eval-with-env (paren-e e) env)]
        [(binop? e) (let* {[the-op    (binop-op e)]
                           [left-val  (eval-with-env (binop-left  e) env)]
                           [right-val (eval-with-env (binop-right e) env)]
                           }
                      (eval-binop the-op left-val right-val))]
        [(if-zero? e) (if (zero? (eval-with-env (if-zero-test e) env))
                          (eval-with-env (if-zero-zero-ans e) env)
                          (eval-with-env (if-zero-pos-ans e) env))]
        [(if-div? e) (if (zero? (my-mod (eval-with-env (if-div-dividend e) env) (eval-with-env (if-div-divisor e) env)))                   
                         (eval-with-env (if-div-evenly e) env)                    
                         (eval-with-env (if-div-unevenly e) env))]                    
        ;[(string? e) (error 'eval-with-env "Undeclared identifier: ~v" e)]  ;>>>V2
        [(string? e) (let*{
                           [ep (assoc e (reverse env))] ;>>>reverse, because you want the most recent one >>>V5
                           }
                       (if (boolean? ep)
                           e
                           (list-ref ep 1)))] ;>>>>V5
        [(let-expr? e)                                             ;>>>V2
         (let* {[v0 (eval-with-env (let-expr-rhs e) env)]                       ;>>>V2
                [b (list (let-expr-id e) v0)] ;>>>V5 (new binding)
                [e′ (eval-with-env (let-expr-body e) (append env (list b)))] ; >>>>V5                
                }                    ;>>>V2
           (eval-with-env e′ env))]
        [(func-expr? e) (make-func-expr (func-expr-param e) (func-expr-body e) env)] ; Functions eval to themselves.                 ;>>>V4     >>>V6                   
        [(func-apply-expr? e) (let* {[funk (eval-with-env (func-apply-expr-fun e) env)] ;>>>V4                   
                                     [f-param (func-expr-param funk)]      ;>>>V4 
                                     [f-body  (func-expr-body  funk)]      ;>>>V4                   
                                     [arg  (eval-with-env (func-apply-expr-arg e) env)] ;>>>V4
                                     [b (list f-param arg)]                       ;>>>V5 new binding
                                     [ep (eval-with-env f-body (append env (list b)))] ;>>>V5   translation of (subst f-param arg f-body)     
                                     }                                     ;>>>V4                   
                                (eval-with-env ep env))]        ;>>>V4 >>>V5                  
         
        [else (error 'eval-with-env "unknown type of expr: " (expr->string e))]))                    


; eval-binop : op num num -> num
; Implement the binary operators.
(define (eval-binop op l r)
  (cond [(string=? op "yasss") (+ l r)]
        [(string=? op "yoink") (- l r)]
        [(string=? op "yeet") (* l r)]
        [(string=? op "yeesh") (my-mod l r)]        ;>>>V1                    
        [else (error 'eval-with-env "Unimplemented op " op)]
        ))

; my-mod: real, real -> real                    
; x mod y, where the result is always between 0 (inclusive) and y (exclusive)                    
(define (my-mod x y)                    
   (* y (- (/ x y) (floor (/ x y)))))                    
(check-expect (my-mod 8 2) 0)                    
(check-expect (my-mod 8 3) 2)                    
(check-expect (my-mod 8.1 3) 2.1)                    
(check-expect (my-mod 8.1 2) 0.1)                    
(check-expect (my-mod 8 16) 8)                    

(check-expect (my-mod  8  3) 2)                    
(check-expect (my-mod  8 -3) -1)                    
(check-expect (my-mod -8  3)  1)                    
(check-expect (my-mod -8 -3) -2)                    


  

(check-expect (eval-binop "yasss" 3 2) 5)
(check-expect (eval-binop "yoink" 3 2) 1)
(check-expect (eval-binop "yeet"  3 2) 6)


; expr->string : Expr -> string
; Return a string-representation of `e`.
;
(define (expr->string e)
  (cond [(number? e) (number->string (if (integer? e) e (exact->inexact e)))]
        [(paren? e) (string-append "(:" (expr->string (paren-e e)) ":)")]
        [(binop? e) (string-append "#"
                                   (binop-op e)
                                   " "
                                   (expr->string (binop-left e))
                                   " "
                                   (expr->string (binop-right e))
                                   "#"
                                   )]
        [(if-zero? e) (string-append "?0 "
                                     (expr->string (if-zero-test e))
                                     " "
                                     (expr->string (if-zero-zero-ans e))
                                     " "
                                     (expr->string (if-zero-pos-ans e))
                                     " :!"
                                     )]
        [(if-div? e) (string-append "?| "                    
                                    (expr->string (if-div-divisor e))                    
                                    " "                    
                                    (expr->string (if-div-dividend e))                    
                                    " thn "                    
                                    (expr->string (if-div-evenly e))                    
                                    " els "                    
                                    (expr->string (if-div-unevenly e))                    
                                    " :|"                    
                                    )]                    
        [(string? e) e] ; identifier ;>>>V2                    
        [(let-expr? e) (string-append ;>>>V2                   
                           "let "                     
                           (let-expr-id e)                     
                           " := "                     
                           (expr->string (let-expr-rhs e))                    
                           " {:"                     
                           (expr->string (let-expr-body e))                     
                           ":}"                      
                           )]                      
        [(func-expr? e) (string-append  ;>>>V4                   
                           "faq " 
                           (func-expr-param e)                   
                           " mek "                   
                           (expr->string (func-expr-body e))
                           )]                   
        [(func-apply-expr? e) (string-append ;>>>V4                   
                           "mod "                     
                           (expr->string (func-apply-expr-arg e))                     
                           " wtf "                     
                           (expr->string (func-apply-expr-fun e))                    
                           )]                      
        [else (error 'expr->string "unknown type of expr: " e)]))


; subst : string number expr -> expr                    
; Return `e` but with any occurrences of `id` replaced with `v`.                    
;>>>V2                    
;                    
(define (subst id v e)                                      
  (cond [(number? e) e]                                      
        [(paren? e) (make-paren (subst id v (paren-e e)))]                                      
        [(binop? e) (make-binop (binop-op e)                                       
                                (subst id v (binop-left e))                    
                                (subst id v (binop-right e)))]                    
        [(if-zero? e) (make-if-zero (subst id v (if-zero-test e))                    
                                    (subst id v (if-zero-zero-ans e))                    
                                    (subst id v (if-zero-pos-ans  e)))]                    
        [(if-div? e) (make-if-div (subst id v (if-div-divisor e))                    
                                  (subst id v (if-div-dividend e))                    
                                  (subst id v (if-div-evenly e))                    
                                  (subst id v (if-div-unevenly e)))]                       
        [(let-expr? e)   (make-let-expr (let-expr-id e) ;>>>V3                       
                                        ; We know the above is an id, not *any* Expr.                    
                                        (subst id v (let-expr-rhs e))                       
                                        (if (string=? id (let-expr-id e)) ;>>>V3
                                            (let-expr-body e) ;>>>V3 our `let`s `id` field is shadowing `id`s substitution.
                                            (subst id v (let-expr-body e))))]                     
        [(string? e) (if (string=? e id) v e)] ;The only line of "real" work!                    
        [(func-apply-expr? e) (make-func-apply-expr (subst id v (func-apply-expr-fun e)) ;>>>V4(rote)                   
                                                    (subst id v (func-apply-expr-arg e)))];>>>V4(rote)                   
        [(func-expr? e) (if (string=? (func-expr-param e) id)  ;>>>V4 w shadowing                   
                            e ; Our `param` is shadowing `id`s substitution ;>>>V4 w shadowing                   
                            (make-func-expr (func-expr-param e) ;>>>V4(rote)                   
                                            (subst id v (func-expr-body e))))] ;>>>V4(rote)                   
        [else (error 'expr->string "unknown internal format?!: ~v" e)]                    
        ))                    

;>>>V2 tests for 'subst'                   
(check-expect (subst "x" 9 (string->expr "3"))   (string->expr "3") )                   
(check-expect (subst "x" 9 (string->expr "x"))   (string->expr "9") )                   
(check-expect (subst "z" 7 (string->expr "x"))   (string->expr "x") )                   
(check-expect (subst "z" 7 (string->expr "#yasss 4 z#"))   (string->expr "#yasss 4 7#"))                   
(check-expect (subst "z" 7 (string->expr "let x := z {:#yeet x z#:}"))                   
              (string->expr "let x := 7 {:#yeet x 7#:}"))                   

;>>>V4 from hw
(check-expect (subst "x" 3 (string->expr "(:let y := 4 {: #yasss x (:let x := 5 {: #yasss x y# :}:)# :} :)")) 
              (string->expr "(:let y := 4 {: #yasss 3 (:let x := 5 {: #yasss x y# :}:)# :} :)") )




; check-token= : (or/c string? number?) (or/c string? number?) -> (or/c string? number?)
; Verify that `actual-token` equals `expected-token`; throw an error if not.
; IF they are equal, just return `actual-token` (as a convenience-value).
;
(define (check-token= actual-token expected-token)
  (if (equal? actual-token expected-token)
      actual-token
      (error 'check-token= (format "Expected the token ~v, but got ~v." expected-token actual-token))))



