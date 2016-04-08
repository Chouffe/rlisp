#lang racket
(provide (all-defined-out))

;; definition of structures for MUPL programs
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0
(struct closure (env fun) #:transparent)

; Some helper functions for list transformations

(define (racketlist->mupllist xs)
  (foldr (lambda (x acc) (apair x acc)) (aunit) xs))

(define (mupllist->racketlist es)
  (if (aunit? es)
    '()
    (cons (apair-e1 es) (mupllist->racketlist (apair-e2 es)))))

;; lookup a variable in an environment
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

(define (extend-env env str v)
  (cons (cons str v) env))

(define (eval-under-env e env)
  (cond [(var? e)
         (envlookup env (var-string e))]
        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
             (int (+ (int-num v1)
                     (int-num v2)))
             (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(closure? e) e]
        [(fun? e) (closure env e)]
        [(aunit? e) e]
        [(mlet? e)
         (let ([v (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (extend-env env (mlet-var e) v)))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (or (not (int? v1)) (not (int? v2)))
             (error "MUPL ifgreater applied to non-number")
             (eval-under-env (if (> (int-num v1) (int-num v2))
                               (ifgreater-e3 e)
                               (ifgreater-e4 e))
                             env)))]
        [(call? e)
         (let ([clos (eval-under-env (call-funexp e) env)]
               [val (eval-under-env (call-actual e) env)])
           (if (not (closure? clos))
               (error "MUPL call first arg is not a closure")
               (let* ([f (closure-fun clos)]
                      [fun-body (fun-body f)]
                      [fun-var (fun-formal f)])
                 (let ([new-env (if (not (fun-nameopt f))
                                  (extend-env (closure-env clos) fun-var val)
                                  (extend-env (extend-env (closure-env clos) fun-var val)
                                              (fun-nameopt f)
                                              clos))])
                 (eval-under-env fun-body new-env)))))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (not (apair? v))
               (error "bad MUPL expression - fst is not applied on a pair")
               (apair-e1 v)))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (not (apair? v))
               (error "bad MUPL expression - snd is not applied on a pair")
               (apair-e2 v)))]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v) (int 1) (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

(define (eval-exp e)
  (eval-under-env e null))

;; Macros as racket functions -> they are applied before
;; compile time in the target language

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
       e2
       (let ([binding (car lstlst)])
         (mlet (car binding) (cdr binding)
               (mlet* (cdr lstlst) e2)))))

(define (ifeq e1 e2 e3 e4)
  (let ([_x (eval-exp e1)]
        [_y (eval-exp e2)])
    (ifgreater _x _y (ifgreater _y _x e3 e4) e4)))

;; Some functions defined in our new language

(define mupl-map
  (fun "map" "f"
       (fun #f "xs"
            (ifaunit (var "xs")
                     (aunit)
                     (apair (call (var "f") (fst (var "xs")))
                            (call (call (var "map") (var "f"))
                                  (snd (var "xs"))))))))

(define mupl-mapAddN
  (mlet "map" mupl-map
        (fun #f "n" (call (var "map") (fun #f "x" (add (var "n") (var "x")))))))


; Space efficient solution for storing environments un closures
; Only free vars are kept
(struct fun2 (nameopt formal body freevars) #:transparent)

; Returns the set of free vars for the given expression
(define (compute-free-vars-aux e)
  (cond
    [(var? e) (set (var-string e))]
    [(int? e) (set)]
    [(add? e) (set-union (compute-free-vars-aux (add-e1 e))
                         (compute-free-vars-aux (add-e2 e)))]
    [(ifgreater? e)
     (apply set-union
            (map compute-free-vars-aux
                 (list (ifgreater-e1 e)
                       (ifgreater-e2 e)
                       (ifgreater-e3 e)
                       (ifgreater-e4 e))))]
    [(fun? e)
     (set-subtract (compute-free-vars-aux (fun-body e))
                   (if (fun-nameopt e)
                     (set (fun-nameopt e) (fun-formal e))
                     (set (fun-formal e))))]
    [(call? e) (set-union (compute-free-vars-aux (call-funexp e))
                          (compute-free-vars-aux (call-actual e)))]
    [(mlet? e)
     (set-subtract (apply set (map compute-free-vars-aux
                                   (list (mlet-e e) (mlet-body e))))
                   (set (mlet-var e)))]
    [(apair? e) (set-union (compute-free-vars-aux (apair-e1 e))
                           (compute-free-vars-aux (apair-e2 e)))]
    [(fst? e) (compute-free-vars-aux (fst-e e))]
    [(snd? e) (compute-free-vars-aux (snd-e e))]
    [(aunit? e) (set)]
    [(isaunit? e) (set)]
    [(closure? e) (compute-free-vars-aux (closure-fun e))]))

; Rewrite fun with fun2 and keeps free vars in a set
(define (compute-free-vars e)
  (cond
    [(fun? e)
     (fun2 (fun-nameopt e)
                    (fun-formal e)
                    (compute-free-vars (fun-body e))
                    (compute-free-vars-aux e))]
    [(add? e) (add (compute-free-vars (add-e1 e) )
                   (compute-free-vars (add-e2 e)))]
    [(ifgreater? e) (ifgreater (compute-free-vars (ifgreater-e1 e))
                               (compute-free-vars (ifgreater-e2 e))
                               (compute-free-vars (ifgreater-e3 e))
                               (compute-free-vars (ifgreater-e4 e)))]
    [(call? e) (call (compute-free-vars (call-funexp e)) (compute-free-vars (call-actual e)))]
    [(mlet? e) (mlet (mlet-var e)
                     (compute-free-vars (mlet-e e))
                     (compute-free-vars (mlet-body e)))]
    [(apair? e) (apair (compute-free-vars (apair-e1 e))
                       (compute-free-vars (apair-e2 e)))]
    [(fst? e) (fst (compute-free-vars (fst-e e)))]
    [(snd? e) (snd (compute-free-vars (snd-e e)))]
    [(isaunit? e) (isaunit (compute-free-vars (isaunit-e e)))]
    [(closure? e) (closure (closure-env e) (compute-free-vars (closure-fun e)))]
    [#t e]))

(define (keep-in-env env ts)
  (filter (lambda (x) (set-member? ts (car x))) env))

(define (fun2->fun f)
  (fun (fun2-nameopt f) (fun2-formal f) (fun2-body f)))

(define (eval-under-env-c e env)
  (cond [(var? e)
         (envlookup env (var-string e))]
        [(add? e)
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
             (int (+ (int-num v1)
                     (int-num v2)))
             (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(closure? e)
         (closure (closure-env e) (fun2->fun (closure-fun e)))]
        [(fun2? e)
         (closure (keep-in-env env (fun2-freevars e)) (fun2->fun e))]
        [(aunit? e) e]
        [(mlet? e)
         (let ([v (eval-under-env-c (mlet-e e) env)])
              (eval-under-env-c (mlet-body e) (extend-env env (mlet-var e) v)))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
           (if (or (not (int? v1)) (not (int? v2)))
             (error "MUPL ifgreater applied to non-number")
             (eval-under-env-c (if (> (int-num v1) (int-num v2))
                               (ifgreater-e3 e)
                               (ifgreater-e4 e))
                             env)))]
        [(call? e)
         (let ([clos (eval-under-env-c (call-funexp e) env)]
               [val (eval-under-env-c (call-actual e) env)])
           (if (not (closure? clos))
             (error "MUPL call first arg is not a closure")
             (let* ([f (closure-fun clos)]
                    [fun-body (fun-body f)]
                    [fun-var (fun-formal f)])
               (let ([new-env (if (not (fun-nameopt f))
                                (extend-env (closure-env clos) fun-var val)
                                (extend-env (extend-env (closure-env clos) fun-var val)
                                            (fun-nameopt f)
                                            clos))])
                 (eval-under-env-c fun-body new-env)))))]
        [(apair? e)
         (let ([v1 (eval-under-env-c (apair-e1 e) env)]
               [v2 (eval-under-env-c (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env-c (fst-e e) env)])
           (if (not (apair? v))
             (error "bad MUPL expression - fst is not applied on a pair")
             (apair-e1 v)))]
        [(snd? e)
         (let ([v (eval-under-env-c (snd-e e) env)])
           (if (not (apair? v))
             (error "bad MUPL expression - snd is not applied on a pair")
             (apair-e2 v)))]
        [(isaunit? e)
         (let ([v (eval-under-env-c (isaunit-e e) env)])
           (if (aunit? v) (int 1) (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
