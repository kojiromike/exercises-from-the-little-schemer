#lang scheme

(define %&
  (lambda (x y k)
    (k (modulo x y))))
(define =&
  (lambda (x y k)
    (k (= x y))))
(define car&
  (lambda (l k)
    (k (car l))))
(define cdr&
  (lambda (l k)
    (k (cdr l))))
(define null?&
  (lambda (l k)
    (k (null? l))))
(define string?&
  (lambda (x k)
    (k (string? x))))
(define number->string&
  (lambda (i k)
    (k (number->string i))))
(define fizzbuzz&
  (lambda (i modmap k)
    (string?& i (lambda (is_string)
                  (if is_string (k i)
                      (null?& modmap (lambda (done)
                                       (if done
                                           (number->string& i (lambda (s) (k s)))
                                           (car& modmap (lambda (cur_map)
                                                          (car& cur_map (lambda (key)
                                                                          (%& i key (lambda (mod)
                                                                                      (=& 0 mod (lambda (divisible)
                                                                                                  (if divisible (cdr& cur_map (lambda (m)
                                                                                                                                (car& m k)))
                                                                                                      (cdr& modmap (lambda (next)
                                                                                                                     (fizzbuzz& i next k))))))))))))))))))))
(define >=&
  (lambda (x y k)
    (k (>= x y))))
(define +&
  (lambda (i j k)
    (k (+ i j))))
(define string-append&
  (lambda (s1 s2 k)
    (k (string-append s1 s2))))
(define fbbuf&
  (lambda (buf start stop k)
    (fizzbuzz& start '((15 "fizzbuzz")
                       ( 5 "buzz")
                       ( 3 "fizz"))
               (lambda (fizzbuzz)
                 (>=& start stop
                      (lambda (done)
                        (if done (k buf)
                            (+& 1 start (lambda (next)
                                          (string-append& buf fizzbuzz (lambda (linebuf)
                                                                     (string-append& linebuf "\n" (lambda (newbuf)
                                                                                                        (fbbuf& newbuf next stop k))))))))))))))
(fbbuf& "" 1 20 display)