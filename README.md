This repository contains an implementation for a small, typed Scheme-like language based on the presentation of the simply typed lambda calculus in the book Types and Programming Languages.

Its main features include single-parameter functions with automatic currying and product types (tuples). Some examples may be found below:

## Examples

**factorial**
```scm
(define sub1 : (Int -> Int)
  (lambda (x : Int)
    (- x 1)))

(define factorial : (Int -> Int)
  (lambda (n : Int)
    (if (= n 1)
      1
      (* n (factorial (sub1 n))))))

(factorial 5)
```

**tuples**
```scm
(define intPair : (Int, Int)
  (1, 2))

(define fst : ((Int, Int) -> Int)
  (lambda (pair : (Int, Int))
    (get pair 0)))

(define snd : ((Int, Int) -> Int)
  (lambda (pair : (Int, Int))
    (get pair 1)))

(define addPoints : ((Int, Int) -> ((Int, Int) -> (Int, Int)))
  (lambda (p1 : (Int, Int), p2 : (Int, Int))
    ((+ (fst p1) (fst p2)), (+ (snd p1) (snd p2)))))

(addPoints intPair intPair) // (2, 4)
```

**automatic currying**
```scm
(define inc : (Int -> Int)
  (+ 1))

(inc 1) // 2
```