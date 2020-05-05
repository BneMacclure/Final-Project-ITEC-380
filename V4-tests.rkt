;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname V4-tests) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Note: this file is written in *advanced*-student
; (because the test-functions do sequences of I/O, rather than return values).
; Your program should be written in "intermediate student with lambda".


(require "V4.rkt") ;;; >>>V4
(require "scanner.rkt")
(require rackunit)
(require "student-extras.rkt") ; solely for `for-each`

;;;;;;;;;;;;;;;;;;; TEST CASES: V0 ;;;;;;;;;;;;;;;;

; Some expressions to test in a non-automated way:
(check-equal? (parse! (create-scanner "34")) 34)
(check-equal? (parse! (create-scanner "-34")) -34)
(check-equal? (string->expr "34") 34)
(check-equal? (string->expr "(:34:)") (make-paren 34))
(check-equal? (string->expr "#yasss 3 4#") (make-binop "yasss" 3 4))
(check-equal? (string->expr "#yasss (:34:) #yeet 3 4##")
              (make-binop "yasss" (make-paren 34) (make-binop "yeet" 3 4)))
(check-equal? (string->expr "?0 3 7 9 :!")
              (make-if-zero 3 7 9))
(check-equal? (string->expr "?0 (:34:) #yasss (:34:) #yeet 3 4## ?0 3 7 9 :! :!")
              (make-if-zero (make-paren 34)
                           (make-binop "yasss" (make-paren 34) (make-binop "yeet" 3 4))
                           (make-if-zero 3 7 9)))


(check-equal? (eval 34) 34)
(check-equal? (eval (string->expr "(:34:)")) 34)
(check-equal? (eval (string->expr "#yasss 3 4#"))  7)
(check-equal? (eval (string->expr "#yoink 3 4#")) -1)
(check-equal? (eval (string->expr "#yeet  3 4#")) 12)
(check-equal? (eval (string->expr "?0 3 4 5 :!")) 5)
(check-equal? (eval (string->expr "?0 0 4 5 :!")) 4)
(check-equal? (eval (string->expr "?0 #yoink 2 4# #yasss 1 2# #yasss 3 4# :!")) 7)
(check-equal? (eval (string->expr "?0 7 #yasss 3 4# 5 :!")) 5)

(check-equal? (expr->string (string->expr "34")) "34")
(check-equal? (expr->string (string->expr "(:34:)")) "(:34:)")
(check-equal? (expr->string (string->expr "#yasss 3 4#")) "#yasss 3 4#")
(check-equal? (expr->string (string->expr "#yoink 3 4#")) "#yoink 3 4#")
(check-equal? (expr->string (string->expr "  #  yeet 3    4   # ")) "#yeet 3 4#")
(check-equal? (expr->string (string->expr "?0 3 4 5 :!")) "?0 3 4 5 :!")
(check-equal? (expr->string (string->expr "?0 0 #yasss 3 4# 5 :!")) "?0 0 #yasss 3 4# 5 :!")

;; Add more specific tests here,
;; if you want things more specific that provided via adding to `all-tests` below.





(define e0 "43")
(define e1 "(:43:)")
(define e2 "#yasss 4 3#")
(define e3 "(:(:#yasss 4 (:3:)#:):)")
(define e4 "#yasss (:43:) #yeet 42 3##")


;;; we can automate checking that string->expr is the (right)inverses of expr->string:
(for-each (λ(e) (check-equal? (expr->string (string->expr e))
                              e))
          (list e0 e1 e2 e3 e4))
; `for-each` is like map except that it discards the result from each function-call;
; it is suitable for functions which are called solely for a side-effect.
; (`test-all` is such a function.)


;;; Though we also want to check that e0..e4 eval to 43,43,7,7,169 respectively.
(for-each (λ(e v) (check-equal? (eval (string->expr e)) v)) ; is the source-Expression; v for Value
          (list e0 e1 e2 e3  e4)
          (list 43 43  7  7 169))


;;; The above is a promising start, to automating tests.
;;; Okay, we'll generalize the above to a more complete test-harness.
;;; One thing, is that we don't want to have two parallel-lists;
;;; instead keep a list of pairs.

;;; Three sorts of tests we want to make, for many different exprs:
(check-equal? (string->expr "#yasss 4 3#") (make-binop "yasss" 4 3))
(check-equal? (eval (string->expr "#yasss 4 3#")) 7)
(check-equal? (expr->string (string->expr "#yasss 4 3#"))
              "#yasss 4 3#")




; Data Def'n:  a `S-example` is a list of length two or length three:
; '[str val]      (where val is the expected result `(eval (string->expr str))`, or
; '[str val expr] (as above, but `expr` is the internal (struct) representation of `(string->expr str)`).

; A list of S-examples;
; The last line of this file runs two-to-three tests on each S-example.
;
; BE AWARE of the comma preceding the constructors; it's necessary to actually call it.
; See explanation at http://www.radford.edu/~itec380/2017fall-ibarland/Lectures/backquote.html
;
(define all-tests
  `{("7" 7 7)
    ("(:3:)" 3 ,(make-paren 3))
    ("#yasss 3 4#" 7 ,(make-binop "yasss" 3 4))
    ("#yeet 3 4#" 12 ,(make-binop "yeet" 3 4 ))
    ("#yasss #yasss 3 4# #yeet 3 4##" 19)
    ("#yeet (:3:) (:#yasss 2 3#:)#" 15)
    ("?0 0 1 2 :!" 1 ,(make-if-zero 0 1 2))
    ("?0 1 1 2 :!" 2 ,(make-if-zero 1 1 2))
    ("?0 #yasss 3 -3# 1 2 :!" 1 ,(make-if-zero (make-binop "yasss" 3  -3) 1 2))
    ("?0 #yasss ?0 ?0 0 1 2 :! 3 4 :! -3# 1 2 :!"
     2
     ,(make-if-zero (make-binop "yasss" (make-if-zero (make-if-zero 0 1 2) 3 4) -3) 1 2))

  
    ;>>>V1  tests
    ; Uncomment these tests, once `yeesh`(mod) is implemented:
    ["#yeesh 3 4#" 3]
    ["#yeesh #yasss 5 6# 3#" 2]
    ["#yeesh 8.1 3#" 2.1]
    ["#yeesh 8 3.1#" 1.8]
    ["#yeesh -8.1 3#" 0.9]
    ["#yeesh -8 3.1#" 1.3]
    ["#yeesh 8.1 -3#" -0.9]
    ["#yeesh 8 -3.1#" -1.3]
    ["#yeesh -8.1 -3#" -2.1]
    ["#yeesh -8 -3.1#" -1.8]
    ["#yeesh 8 2#" 0]
    ["#yeesh -8 2#" 0]
    ["#yeesh 8 -2#" 0]
    ["#yeesh -8 -2#" 0]
    ["#yeesh 8 3#" 2]
    ["#yeesh -8 3#" 1]
    ["#yeesh 8 -3#" -1]
    ["#yeesh -8 -3#" -2]

    ;>>> V1
    ; YOU-MUST-CREATE-SOME-TESTS-FOR-IfDiv
    ["?|  2  6 thn 100 els 200 :|" 100 ,(make-if-div 2 6 100 200)]                   
    ["?| -2  6 thn 100 els 200 :|" 100]                   
    ["?|  2 -6 thn 100 els 200 :|" 100]                   
    ["?|  6  2 thn 100 els 200 :|" 200]                   
    ["?|  2  0 thn 100 els 200 :|" 100]                   
    ["?|  1  6 thn 100 els 200 :|" 100]                   
                   
    ["?| #yasss 1 1# #yasss 3 3# thn #yasss 50 50# els #yasss 100 100#:|" 100]                   
    ["#yasss ?| 2 6 thn 100 els 200 :| 33#" 133]                   
    ["?| 2 ?| 1 1 thn 6 els 3:| thn 100 els 200 :|" 100]                   
                   
    ;>>>V2                   
    ["let x := 5 {:7:}" 7]                   
    ["let x := 5 {:x:}" 5]                   
    ["let x := 5 {:#yoink x 2#:}" 3]                   
    ["#yoink 7 let x := 5 {:#yoink x 2#:}#" 4]                   
    ["let x := 5 {:let y := 7 {:#yasss x y#:}:}" 12]                   
   
    ;>>>V3 -- shadowing
    ["let x := 5 {:let x := 9 {:7:}:}" 7]                   
    ["let x := 5 {:let x := 9 {:x:}:}" 9]                   
    ["let x := 5 {:#yasss let x := 9 {:x:} x#:}" 14]                   
    ["let x := 5 {:let y := 7 {: let x := #yasss y 100# {: #yoink x 1#:}:}:}" 106]                   
    ["let x := 5 {:let y := 7 {: let x := #yasss x 100# {: #yoink x 1#:}:}:}" 104]                   

    ;>>>V4 -- functions and func-apply
    ["faq x mek 17" ,(make-func-expr "x" 17) ,(make-func-expr "x" 17)]                   
    ["faq x mek #yoink x 3#" ,(make-func-expr "x" (string->expr "#yoink x 3#"))]                   
    ["faq m mek faq n mek #yasss m n#" ,(make-func-expr "m" (make-func-expr "n" (string->expr "#yasss m n#")))]                   

    ["let k17 := faq x mek 17 {: mod 5 wtf k17 :}" 17 ,(make-let-expr "k17" (make-func-expr "x" 17) (make-func-apply-expr "k17" 5))]                   

    ["let x := 3 {: (:let y := 4 {: #yasss x (:let x := 5 {: #yasss x y# :}:)# :} :):}" ,(+ 3 (+ 5 4))] ;>>>V4 from hw-assign #1.<>                   
    ["let y := 3 {: let x := 5 {: #yasss x y# :} :}" 8] ;>>>V4 from hw-assign #1.a                   
    ["let y := 3 {: let x := y {: #yasss x y#:} :} " 6] ;>>>V4 from hw-assign #1.b                   
    ["let x := 5 {: let y := 3 {: #yasss let x := y {: #yasss x y# :} x #:}:}" 11] ;>>>V4 from hw-assign #1.c                   
    ["let x := 5 {: let x := #yasss x 1# {: #yasss x 2# :}:}" 8] ;>>>V4 from hw-assign #1.d                   
    })
;
; For info on backquote, see documentation and/or:
;   http://www.radford.edu/itec380/2019fall-ibarland/Lectures/backquote.html



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following functions should really be in a separate file, and exported from there.
;; However, putting this in to a file 'Ui-test-harness.rkt' would become mildly problematic:
;; since it calls 'eval', 'string->expr' as provided in V0, it needs to require V0.rkt.
;; As we update our implementation V0.rkt to V2.rkt,U4.rkt etc,
;; we'd then need to updated *this* file each time, changing *nothing* but the 'require'.  Yuck.
;; An actual solution would be using "units":
;; http://docs.racket-lang.org/reference/creatingunits.html?q=unit#%28form._%28%28lib._racket%2Funit..rkt%29._unit%29%29
;;
;; But rather than add that level of indirection for a student-assignment, we'll just repeat
;; this code inside each Ui-test.rkt.


; my-check-equal? : any, any, string -> boolean
; If the two values aren't equal, print an error message.
; If they are equal (and `print-on-success`), a "." gets printed, just to show progress.
;
(define my-check-equal?
  (local {(define test# 0)
          (define print-on-success #true)}
      (λ (actual expected err-msg)
        (begin (set! test# (add1 test#))
               (if (equal? actual expected)
                   (when print-on-success (printf ".~a" "" #;(if (zero? (modulo test# 5)) " " "")))
                   (printf "\ntest #~a failed:\n~a\n" test# err-msg))
               ;(check-equal? actual expected) ; Use `check-equal?` to additionally-but-redundantly manage test cases.
               (equal? actual expected) ; return this boolean
               ))))


; test-internal-representation : S-example -> void?
; Test that t parses to the correct internal tree-representation (if provided)
;
(define (test-internal-representation t)
  (when (>= (length t) 3)
    (my-check-equal? (string->expr (first t))
                     (third t)
                     (format "Parsing     ~v\nresulted in ~v\ninstead of  ~v\nas expected."
                             (first t) (string->expr (first t)) (third t)))))

; test-eval : S-example -> void?
; Test that the S-example `eval`s to what it should.
;
(define (test-eval t)
  (my-check-equal? (eval (string->expr (first t)))
                   (second t)
                   (format "Program    ~v\neval'd to  ~v\ninstead of ~v\nas expected."
                           (first t) (eval (string->expr (first t))) (second t))))


; test-parse-inverse-of-to-string : S-example -> void?
; Test that `parse` and `expr->string` are inverses of each other:
;    `parse` is a right-inverse: for a string `s`,  (expr->string (parse s)) = s, and
;    `parse` is a left- inverse: for a tree `expr`, (parse (expr->string expr)) = expr.
; Note that spaces between tokens in a string is ignored, so they're not *quite* exact inverses.
;
; Also, other tests are redundant with checking the left-inverse,
; but we still check it to be independent of other code.
;
(define (test-parse-inverse-of-to-string t)
  (begin (my-check-equal? (string->tokens (expr->string (string->expr (first t))))
                          (string->tokens (first t))
                          (format "Parsing ~v then converting back to string gave ~v."
                                  (first t) (expr->string (string->expr (first t)))))
         (when (>= (length t) 3)
           (my-check-equal? (string->expr (expr->string (third t)))
                            (third t)
                            (format "Converting ~v to string and re-parsing it gave ~v."
                                    (third t) (expr->string (third t)))))))


; test-all : S-example -> void?
; Make sure that t meets the following properties:
;   i. Parsing the string results in the expected internal representation (*)
;  ii. Check that parsing the string and then to-string'ing the result
;      gives back the initial string
; iii. Check that to-string'ing the internal representation and then parsing
;      that resulting string gives back the initial internal representation (*)
;  iv. check that eval'ing the (parsed) string gives the expected value.
;
; (*) steps i,iii can only be performed if the S-example contained all three values.
;     If it only contained a string and a value, then only *two* tests get performed.
;     This affects the test-number reported, should a later test fail.
;
(define test# 0)
(define (test-all t)
  (begin
    (set! test# (add1 test#))
    (printf ":")
    (test-internal-representation t)
    (test-parse-inverse-of-to-string t) ; N.B. counts as two tests
    (test-eval t)
    (when (zero? (remainder test# 5)) (printf " "))
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; run the tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(printf "Running V-test-harness (each item in `all-tests` preceded by a ':')\n")

(for-each test-all all-tests)  ; This line actually invokes the checker
(if (= (length all-tests) test#)
    (printf "\nV-test-harness complete; ~v expressions each tested two-or-four ways.\n" (length all-tests))
    (printf "\nV-test-harness ended during test# ~v of ~v tests." test# (length all-tests)))
; `for-each` is like `map` except that it discards the result from each function-call;
; it is suitable for functions which are called solely for a side-effect.
; (`test-all` is such a function.)


;; a line which just "re"prints out the tests,
;; except with the *actual* (not expected) results of eval, string->expr.
;;  [useful for ibarland, in converting one year's spec to another]
#;(pretty-print (map (λ(tst) (let* {[prog (string->expr (first tst))]} (list (expr->string prog) (eval prog))))
       all-tests))
