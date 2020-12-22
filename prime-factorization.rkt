;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname prime-factorization) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;*********************************
;; PRIME FACTORIZATION
;; Veronika Sustrova
;; December 2020
;; ******************************** 

;; (p-factorization n) produces a list containing the prime factorization of n
;; Examples:
(check-expect (p-factorization 10) (list 2 5))
(check-expect (p-factorization 49) (list 7 7))

;; p-factorization: Nat -> (listof Nat)
;; Requires: n >= 2
(define (p-factorization n)
  (local [;; (prime-list n counter) produces a list of primes less
          ;;      than n
          ;; prime-list: Nat Nat -> (listof Nat)
          ;; Requires: counter >= 2
          (define (prime-list n counter)
            (cond [(= n counter) empty]
                  [(prime? counter)
                   (cons counter (prime-list n (add1 counter)))]
                  [else (prime-list n (add1 counter))]))

          ;; (factorize n lprimes) produces a list containing only
          ;;     those elements from lprimes that divide n
          ;; factorize: Nat (listof Nat) -> (listof Nat)
          ;; Requires: all elements of lprimes must be prime numbers
          (define (factorize n lprimes)
            (cond [(= n 1) empty]
                  [(integer? (/ n (first lprimes)))
                   (cons (first lprimes)
                         (factorize (/ n (first lprimes)) lprimes))]
                  [else (factorize n (rest lprimes))]))]
    
    (cond [(prime? n) (list n)]
          [else (factorize n (prime-list n 2))])))

;; Tests:
(check-expect (p-factorization 2) (list 2))
(check-expect (p-factorization 101) (list 101))
(check-expect (p-factorization 51) (list 3 17))
(check-expect (p-factorization 1080) (list 2 2 2 3 3 3 5))

;; Helper Functions ******************************************************

;; (prime? n) produces true if n is a prime number, and false otherwise
;; Examples:
(check-expect (prime? 7) true)
(check-expect (prime? 8) false)

;; prime?: Nat -> Bool
;; 
(define (prime? n)
  (local [;; (count-divisors n constn counter) produces the number of
          ;;    positive integers that divide n
          ;; count-divisors: Nat -> Nat
          ;; Requires: n > 1
          (define (count-divisors n constn counter)
            (cond [(= 1 n) (add1 counter)]
                  [(integer? (/ constn n))
                   (count-divisors (sub1 n) constn (add1 counter))]
                  [else
                   (count-divisors (sub1 n) constn counter)]))]

    (cond [(= 2 (count-divisors n n 0)) true]
          [else false])))


