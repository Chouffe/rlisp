#lang racket

(require rackunit)

(define tests
  (test-suite
   "Sample tests for the MUPL"

   ;; check racketlist to mupllist with normal list
   (check-equal? (racketlist->mupllist (list (int 3) (int 4))) (apair (int 3) (apair (int 4) (aunit))) "racketlist->mupllist test")

   ;; check mupllist to racketlist with normal list
   (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (aunit)))) (list (int 3) (int 4)) "racketlist->mupllist test")

   ;; tests if ifgreater returns (int 2)
   (check-equal? (eval-exp (ifgreater (int 3) (int 4) (int 3) (int 2))) (int 2) "ifgreater test")
   (check-equal? (eval-exp-c (ifgreater (int 3) (int 4) (int 3) (int 2))) (int 2) "ifgreater test")

   ;; mlet test
   (check-equal? (eval-exp (mlet "x" (int 1) (add (int 5) (var "x")))) (int 6) "mlet test")
   (check-equal? (eval-exp-c (mlet "x" (int 1) (add (int 5) (var "x")))) (int 6) "mlet test")

   ;; call test
   (check-equal? (eval-exp (call (closure '() (fun #f "x" (add (var "x") (int 7)))) (int 1))) (int 8) "call test")
   (check-equal? (eval-exp-c (call (closure '() (fun #f "x" (add (var "x") (int 7)))) (int 1))) (int 8) "call test")

   ;;snd test
   (check-equal? (eval-exp (snd (apair (int 1) (int 2)))) (int 2) "snd test")
   (check-equal? (eval-exp-c (snd (apair (int 1) (int 2)))) (int 2) "snd test")

   ;; isaunit test
   (check-equal? (eval-exp (isaunit (closure '() (fun #f "x" (aunit))))) (int 0) "isaunit test")
   (check-equal? (eval-exp-c (isaunit (closure '() (fun #f "x" (aunit))))) (int 0) "isaunit test")

   ;; ifaunit test
   (check-equal? (eval-exp (ifaunit (int 1) (int 2) (int 3))) (int 3) "ifaunit test")
   (check-equal? (eval-exp-c (ifaunit (int 1) (int 2) (int 3))) (int 3) "ifaunit test")

   ;; mlet* test
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10))) (var "x"))) (int 10) "mlet* test")
   (check-equal? (eval-exp-c (mlet* (list (cons "x" (int 10))) (var "x"))) (int 10) "mlet* test")

   ;; ifeq test
   (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4))) (int 4) "ifeq test")
   (check-equal? (eval-exp-c (ifeq (int 1) (int 2) (int 3) (int 4))) (int 4) "ifeq test")

   ;; mupl-map test
   (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit))))
                 (apair (int 8) (aunit)) "mupl-map test")

   (check-equal? (eval-exp-c (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit))))
                 (apair (int 8) (aunit)) "mupl-map test")



   (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7))))(aunit)))
   (eval-exp-c (call (call mupl-map (fun #f "x" (add (var "x") (int 7))))(aunit)))

   (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit))))
   (eval-exp-c (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit))))

   (check-equal? (mupllist->racketlist
                   (eval-exp (call (call mupl-mapAddN (int 7))
                                   (racketlist->mupllist
                                     (list (int 3) (int 4) (int 9)))))) (list (int 10) (int 11) (int 16)) "combined test")

   (check-equal? (mupllist->racketlist
                   (eval-exp-c (call (call mupl-mapAddN (int 7))
                                   (racketlist->mupllist
                                     (list (int 3) (int 4) (int 9)))))) (list (int 10) (int 11) (int 16)) "combined test")

   (check-true (set=? (set "f" "n" "x" "xs" "map") (compute-vars mupl-mapAddN)))

   (check-true (set=? (set "x") (compute-free-vars-aux (fun #f "y" (add (var "x") (var "y"))))))

   (check-equal? (list (list 1 2) (list 2 3)) (keep-in-env (list (list 1 2) (list 2 3) (list 3 4)) (set 0 1 2)))

   ))

(require rackunit/text-ui)

;; runs the test
(run-tests tests)
