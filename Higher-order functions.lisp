;№1. Определите FUNCALL через функционал APPLY.

(defun MY_FUNCALL (func &rest args)
    (APPLY func args)
)

(print '(test cases for 1 problem))
(print (MY_FUNCALL '+ 1 2 3 4 5 6 7 8))
(print (MY_FUNCALL '* 1 2))
(print (MY_FUNCALL 'car '(a b 3 4)))
(print (MY_FUNCALL 'cdr '(a b 3 4)))


;№3. Определите функционал (APL-APPLY f x), который применяет каждую функцию fi
;списка (f1 f2 ... fn) к соответствующему элементу списка x = (x1 x2 ... xn) и
;возвращает список, сформированный из результатов.

(defun rev (lst &optional (tsl '()))
    (cond 
        ((null lst) tsl)
        (t (rev (cdr lst) (cons (car lst) tsl)))
    )
)

(defun APL-APPLY (f x &optional (lst '()))
    (cond 
        ((null f) (rev lst))
        (t (APL-APPLY (cdr f) (cdr x) (cons (APPLY (car f) (car x)) lst)))
    )
)

(print '(test case for 3 problem))
(print (APL-APPLY '(+ - * + car cdr) '((1 2) (3 4) (0 120) (1 2 3) ((1 2 3)) ((1 2 3)))))


;№5 Определите функциональный предикат (НЕКОТОРЫй пред список), который истинен, 
;когда, являющейся функциональным аргументом предикат пред истинен хотя бы 
;для одного элемента списка список.

(defun НЕКОТОРЫЙ (f lst)
    (cond
        ((null lst) NIL)
        ((APPLY f (car lst)) T)
        (t (НЕКОТОРЫЙ f (cdr lst)))
    )
)

(print '(test cases for 5 problem))
(print (НЕКОТОРЫЙ 'atom '(((1 2 3)) ((1 2)) ((2 4 3)))))
(print (НЕКОТОРЫЙ 'atom '((1) ((1 2 3)) ((1 2)) ((2 4 3)))))
(print (НЕКОТОРЫЙ 'equal '((1 2) (1 7) (3 2))))
(print (НЕКОТОРЫЙ 'equal '((1 2) (1 1) (3 2))))


;№7. Определите фильтр (УДАЛИТЬ-ЕСЛИ-НЕ пред список), удаляющий из списка список все элементы, 
;которые не обладают свойством, наличие которого проверяет предикат пред.

(defun my_delete (lst index &optional (ans '()) (cnt 1))
    ((lambda (f1 f2 f3)
    (cond    
        ((null lst) (reverse ans))
        ((equal cnt index) (my_delete f2 index ans f3))
        (t (my_delete f2 index (cons f1 ans) f3))
    ))
        (car lst)
        (cdr lst)
        (+ cnt 1)
    )
)


(defun УДАЛИТЬ_ЕСЛИ_НЕ (f lst &optional (current_lst '()) (cnt 0))
    (cond
        ((equal cnt 0) (УДАЛИТЬ_ЕСЛИ_НЕ f lst lst (+ cnt 1)))
        ((null current_lst) lst)
        ((APPLY f (car current_lst)) (УДАЛИТЬ_ЕСЛИ_НЕ f lst (cdr current_lst) (+ cnt 1)))
        (t (УДАЛИТЬ_ЕСЛИ_НЕ f (my_delete lst cnt) (cdr current_lst) cnt)) 
    )
)


(print '(test case for 7 problem))
(print (УДАЛИТЬ_ЕСЛИ_НЕ 'atom '(((1 2 3)) (1) (2) (3) ((7 8)))))
(print (УДАЛИТЬ_ЕСЛИ_НЕ 'equal '((1 1) (1 2) (2 2))))
