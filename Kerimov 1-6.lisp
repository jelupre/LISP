;3 задача

(defun rep (list x y)
    (cond 
        ((null list) nil)
        ((equal (car list) y) (cons x (rep (cdr list) x y)))
        (t (cons (car list) (rep (cdr list) x y)))
    )
)

(print (rep '(2 2 4) 'a 2))
;(A A 4) 
(print (rep () 'a 2))
;NIL 

;6 задача

(defun trans (list)
    (cond 
        ((null list) nil)
        ((equal (car list) 0) (cons 'ноль (trans (cdr list))))
        ((equal (car list) 1) (cons 'один (trans (cdr list))))
        ((equal (car list) 2) (cons 'два (trans (cdr list))))
        ((equal (car list) 3) (cons 'три (trans (cdr list))))
        ((equal (car list) 4) (cons 'четыре (trans (cdr list))))
        ((equal (car list) 5) (cons 'пять (trans (cdr list))))
        ((equal (car list) 6) (cons 'шесть (trans (cdr list))))
        ((equal (car list) 7) (cons 'семь (trans (cdr list))))        
        ((equal (car list) 8) (cons 'восемь (trans (cdr list))))
        ((equal (car list) 9) (cons 'девять (trans (cdr list))))
        (t (cons (car list) (trans (cdr list))))
    )
)

(print (trans '(1 2 3 1 2 0)))
;(ОДИН ДВА ТРИ ОДИН ДВА НОЛЬ) 
(print (trans ()))
;NIL

;9 задача

(defun len (w n)
    (cond 
        ((null w) n)
        ((atom (car w)) (len (cdr w) (+ n 1)))
    )
)

(defun % (b a)
    (cond
        (t (- b (* a (floor b a))))
    )
)

(defun rev (lst lst1)
    (cond 
        ((Null lst)  lst1)
        (t (rev (cdr lst) (cons (car lst) lst1)))
    )
)

(defun split (w ls1 ls2)
    (cond
        ((null w) (list (rev ls1 ()) (rev ls2 ())))
        ((eql (% (len w 0) 2) 1) (split (cdr w) (cons (car w) ls1) ls2))
        (t (split (cdr w) ls1 (cons (car w) ls2)))
    )
)

(print (split '(1 2 3 4 5) () ()))
;((1 3 5) (2 4)) 
(print (split () () ()))
;(NIL NIL) 


;15 задача

(defun inner_product (l1 l2 sum)
    (cond
        ((null l1) sum)
        (t (+ (inner_product (cdr l1) (cdr l2) sum) (* (car l1) (car l2))))
    )
)

(print (inner_product '(1 2 3) '(4 5 6) 0))
;32
(print (inner_product () () 0))
;0 


;22 задача

(defun rever (lst)
    (cond 
        ((atom (cdr lst)) lst)
        (t (list (rever (cdr lst)) (car lst)))
    )
)

(print (rever '(1 2 3 4)))
;((((4) 3) 2) 1) 
(print (rever '(1)))
;(1) 
(print (rever ()))
;NIL


;31 задача

(defun is_similar (x lst) 
    (cond
        ((eql (car lst) x) x)
        ((null lst) NIL)
        (t (is_similar x (cdr lst)))
    )

)

(defun ПЕРВЫЙ-СОВПАДАЮЩИЙ (x y)
    (cond
        ((null x) NIL)
        ((eql (is_similar (car x) y) NIL) (ПЕРВЫЙ-СОВПАДАЮЩИЙ (cdr x) y))
        (t (car x))
    )
)    

(print (ПЕРВЫЙ-СОВПАДАЮЩИЙ '(10 11 12 13) '(7 8 4 3 7)))
;NIL 
(print (ПЕРВЫЙ-СОВПАДАЮЩИЙ '(9 5 7 2 3) '(2 8 4 3 7)))
;7 
(print (ПЕРВЫЙ-СОВПАДАЮЩИЙ '() '(2 8 4 3 7)))
;NIL 
(print (ПЕРВЫЙ-СОВПАДАЮЩИЙ '(9 5 7 2 3) '()))
;NIL 
