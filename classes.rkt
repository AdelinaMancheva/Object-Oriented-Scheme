#lang racket

(define (Object)
  (let ((super '())
        (self 'nil))

    (define (type-of) 'object)

   (define (dispatch message)
       (cond ((eqv? message 'type-of) type-of)))
 
   (set! self dispatch)
    
   self))

(define (Number)
  (let ((super (new-part Object))
        (self 'nill))
    
    (let ((Number-state 0))

      (define  (get-state) Number-state)
      (define (add) 'add)
      (define (subtract) 'subtract)
      (define (mult) 'mult)
      (define (div) 'div)
      (define (pow) 'pow)
      (define (type-of) 'Number)

       (define (set-self! object-part)
        (set! self object-part)
        (send 'set_self! super object-part))
      
      (define (self message)
        (cond ((eqv? message 'type-of) type-of) 
              ((eqv? message 'add) add)
              ((eqv? message 'subtract) subtract)
              ((eqv? message 'mult) mult)
              ((eqv? message 'div) div)
              ((eqv? message 'pow) pow)
              ((eqv? message 'get-state) get-state)
              ((eqv? message 'set-self!) set-self!)
              (else (method-lookup super message))))
        
    self)))

(define (Rational numer denom)
  (let ((super (new-part Number))
        (self 'nill))

    (let ((numer numer)
          (denom denom))

      (define (type-of) 'Rational)
      (define (get-numer) numer)
      (define (get-denom) denom)
     
      (define (pretty-print r)
        (list (send 'get-numer r) '/ (send 'get-denom r)))
      
      (define (add r2)
        (Rational
         (+ (* numer (send 'get-denom r2))
            (* denom (send 'get-numer r2)))
         (* denom (send 'get-denom r2))))

      (define (subtract r2)
        (Rational
         (- (* numer (send 'get-denom r2))
            (* denom (send 'get-numer r2)))
         (* denom (send 'get-denom r2))))

      (define (mult r2)
        (Rational
         (* numer (send 'get-numer r2))
         (* denom (send 'get-denom r2))))

      (define (div r2)
        (Rational
         (* numer (send 'get-denom r2))
         (* denom (send 'get-numer r2))))

      (define (pow n)
        (Rational
         (expt numer n)
         (expt denom n)))
              
      (define (set-self! object-part)
        (set! self object-part)
        (send 'set-self! super object-part))

      (define (self message)
        (cond ((eqv? message 'type-of) type-of)
              ((eqv? message 'get-numer) get-numer)
              ((eqv? message 'get-denom) get-denom)
              ((eqv? message 'subtract) subtract)
              ((eqv? message 'mult) mult)
              ((eqv? message 'div) div)
              ((eqv? message 'pow) pow)
              ((eqv? message 'pretty-print) pretty-print)
              ((eqv? message 'add) add)
              ((eqv? message 'set-self!) set-self!)
              (else (method-lookup super message))))

      self)))

(define (Complex real imag)
  (let ((super (new-part Number))
        (self 'nill))

    (let ((real real)
          (imag imag))

      (define (type-of) 'Complex)
      (define (get-real) real)
      (define (get-imag) imag)
      
      (define (add c)
        (Complex
         (+ real (send 'get-real c))
         (+ imag (send 'get-imag c))))

      (define (subtract c)
        (Complex
         (- real (send 'get-real c))
         (- imag (send 'get-imag c))))

      (define (mult c)
        (Complex
         (- (* real (send 'get-real c))
            (* imag (send 'get-imag c)))
         (+ (* real (send 'get-imag c))
            (* imag (send 'get-real c)))))

      (define (div c)
        (Complex
         (/ (+ (* real (send 'get-real c))
            (* imag (send 'get-imag c)))
         (+ (* (send 'get-real c) (send 'get-real c))
            (* (send 'get-imag c) (send 'get-imag c))))
         
         (/ (- (* imag (send 'get-real c))
            (* real (send 'get-imag c)))
         (- (* (send 'get-real c) (send 'get-real c))
            (* (send 'get-imag c) (send 'get-imag c)))) ))
                 

      (define (pretty-print c)
        (list (send 'get-real c) '+ (send 'get-imag c) 'i))

      (define (pretty-print-subtract c)
        (list (send 'get-real c) '- (send 'get-imag c) 'i))

      (define (pprint c)
        (list (send 'get-real c) (send 'get-imag c) 'i))

      (define (set-self! object-part)
        (set! self object-part)
        (send 'set-self! super object-part))

      (define (self message)
        (cond ((eqv? message 'type-of) type-of)
              ((eqv? message 'set-self!) set-self!)
              ((eqv? message 'get-real) get-real)
              ((eqv? message 'get-imag) get-imag)
              ((eqv? message 'add) add)
              ((eqv? message 'subtract) subtract)
              ((eqv? message 'mult) mult)
              ((eqv? message 'div) div)
              ((eqv? message 'pretty-print) pretty-print)
              ((eqv? message 'pretty-print-subtract) pretty-print-subtract)
              ((eqv? message 'pprint) pprint)
              (else (method-lookup super message))))

      self )))

(define (Integer int)
  (let ((super (new-part Number))
        (self 'nill))

    (let ((int int))

      (define (type-of) 'Integer)
      (define (get-int) int)
      
      (define (add integer)
        (Integer
         (+ int (send 'get-int integer))))

      (define (subtract integer)
        (Integer
         (- int (send 'get-int integer))))

      (define (mult integer)
        (Integer
         (* int (send 'get-int integer))))

      (define (div integer)
        (Integer
         (/ int (send 'get-int integer))))

      (define (pow integer)
        (Integer
         (expt int (send 'get-int integer))))

      (define (set-self! object-part)
        (set! self object-part)
        (send 'set-self! super object-part))

      (define (self message)
        (cond ((eqv? message 'type-of) type-of)
              ((eqv? message 'set-self) set-self!)
              ((eqv? message 'get-int) get-int)
              ((eqv? message 'add) add)
              ((eqv? message 'subtract) subtract)
              ((eqv? message 'mult) mult)
              ((eqv? message 'div) div)
              ((eqv? message 'pow) pow)
              (else (method-lookup super message))))

      self)))
        


(define (virtual-operations object)
  (send 'set-self! object object))

(define (new-instance class . parameters)
 (let ((instance (apply class parameters)))
   (virtual-operations instance)
   instance))

(define (new-part class . parameters)
  (apply class parameters))

(define (method-lookup object selector)
 (cond ((procedure? object) (object selector))
       ))

(define (send message object . args)
 (let ((method (method-lookup object message)))
  (cond ((procedure? method) (apply method args))
             )))
