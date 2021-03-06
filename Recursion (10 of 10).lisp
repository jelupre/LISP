;№3. Определите функцию, заменяющую в исходном списке все вхождения 
;заданного значения другим.

(defun my_replace (lst val changeable)
    ((lambda (f1 f2)
        (cond 
            ((null lst) nil)
            ((equal f1 changeable) (cons val (my_replace f2 val changeable)))
            (t (cons f1 (my_replace f2 val changeable)))
        ))
        (car lst)
        (cdr lst)
     )
)

(print '(test cases for 3 problem))
(print (my_replace '(2 2 4) 'a 2))
(print (my_replace () 'a 2))


;№6 Определите функцию, переводящую список чисел в список соответствующих им названий.

(defun translate (val &optional (comparisons '(НОЛЬ ОДИН ДВА ТРИ ЧЕТЫРЕ ПЯТЬ 
                                                    ШЕСТЬ СЕМЬ ВОСЕМЬ ДЕВЯТЬ)))
    (cond
        ((equal val 0) (car comparisons))
        (t (translate (- val 1) (cdr comparisons)))
    )
)

(defun transmute (lst &optional (output ()))
    (cond
        ((null lst) output)
        (t (transmute (cdr lst) (cons (translate (car lst)) output)))
    )
)

(defun rev (lst lst1)
    (cond 
        ((null lst) lst1)
        (t (rev (cdr lst) (cons (car lst) lst1)))
    )
)

(print '(test cases for 6 problem))
(print (transmute '(1 2 3 1 2 0)))
(print (transmute '()))


;№9. Определите функцию, разделяющую исходный список на два подсписка. 
;Впервый из них должны попасть элементы с нечетными номерами, во второй 
;элементы с четными номерами.

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

(defun split (w &optional (ls1 ()) (ls2 ()))
    ((lambda (f1 f2)
        (cond
            ((null w) (list (rev ls1 ()) (rev ls2 ())))
            ((eql (% (len w 0) 2) 1) (split f2 (cons f1 ls1) ls2))
            (t (split f2 ls1 (cons f1 ls2)))
        ))
    (car w)
    (cdr w)
    )
)

(print '(test cases for 9 problem))
(print (split '(1 2 3 4 5)))
(print (split ()))


;№15. Определите функцию, вычисляющую скалярное произведение векторов, 
;заданных списками целых чисел.

(defun inner_product (lst1 lst2 &optional (sum 0))
    (cond
        ((null lst1) sum)
        (t (+ (inner_product (cdr lst1) (cdr lst2) sum) (* (car lst1) (car lst2))))
    )
)

(print '(test cases for 15 problem))
(print (inner_product '(1 2 3) '(4 5 6)))
(print (inner_product () ()))


;№22. Определите функцию,которая обращает список (а b с) и разбивает 
;его на уровни (((с) b) а).

(defun reverse_and_split (lst)
    ((lambda (f)
        (cond 
            ((atom f) lst)
            (t (list (reverse_and_split f) (car lst)))
        ))
        (cdr lst)
    )
)

(print '(test cases for 22 problem))
(print (reverse_and_split '(1 2 3 4)))
(print (reverse_and_split '(1)))
(print (reverse_and_split ()))


;№31. Определите функцию (ПЕРВЫЙ-СОВПАДАЮЩИЙ х у), которая возвращает первый 
;элемент, входящий в оба списка х и у, в противном случае NIL.

(defun is_similar (val lst) 
    (cond
        ((eql (car lst) val) val)
        ((null lst) NIL)
        (t (is_similar val (cdr lst)))
    )
)

(defun ПЕРВЫЙ-СОВПАДАЮЩИЙ (lst1 lst2)
    ((lambda (f)    
        (cond
            ((null lst1) NIL)
            ((eql (is_similar f lst2) NIL) (ПЕРВЫЙ-СОВПАДАЮЩИЙ (cdr lst1) lst2))
            (t f)
        ))
        (car lst1)
    )
)    

(print '(test cases for 31 problem))
(print (ПЕРВЫЙ-СОВПАДАЮЩИЙ '(10 11 12 13) '(7 8 4 3 7)))
(print (ПЕРВЫЙ-СОВПАДАЮЩИЙ '(9 5 7 2 3) '(2 8 4 3 7)))
(print (ПЕРВЫЙ-СОВПАДАЮЩИЙ '() '(2 8 4 3 7)))
(print (ПЕРВЫЙ-СОВПАДАЮЩИЙ '(9 5 7 2 3) '()))


;№32. Определите предикат МНОЖЕСТВО-Р, который проверяет, является ли список 
;множеством, т.е. входит ли каждый элемент в список лишь один раз.

(defun how_much (lst num cnt)
    (cond
        ((null lst) cnt)
        ((equal (car lst) num) (how_much (cdr lst) num (+ cnt 1)))
        (t (how_much (cdr lst) num cnt))
    )
)

(defun is_set (lst const_lst)
    (cond 
        ((null lst) t)
        ((> (how_much const_lst (car lst) 0) 1) NIL)
        (t (is_set (cdr lst) const_lst))
    )
)

(defun МНОЖЕСТВО-Р (lst)
    (is_set lst lst)
)

(print '(test cases for 32 problem))
(print (МНОЖЕСТВО-Р '(1 2 3 4)))
(print (МНОЖЕСТВО-Р '(1 1 3 4)))
(print (МНОЖЕСТВО-Р '()))


;№39. Определите функцию СИММЕТРИЧЕСКАЯ-РАЗНОСТЬ, формирующую множество из 
;элементов не входящих в оба множества.

(defun is_here (lst num)
    (cond
        ((null lst) NIL)
        ((equal (car lst) num) T)
        (t (is_here (cdr lst) num))
    
    )
)

(defun ПЕРЕСЕЧЕНИЕ (set1 set2 &optional (ans ()))
    ((lambda (f1 f2)
        (cond
            ((null set1) ans)
            ((is_here set2 f1) (cons f1 (ПЕРЕСЕЧЕНИЕ f2 set2 ans)))
            (t (ПЕРЕСЕЧЕНИЕ f2 set2 ans)) 
        ))
        (car set1)
        (cdr set1)
    )
)

(defun РАЗНОСТЬ (set1 set2 &optional (ans ()))
    ((lambda (f1 f2)
        (cond
            ((null set1) ans)
            ((is_here set2 f1) (РАЗНОСТЬ f2 set2 ans))
            (t (cons f1 (РАЗНОСТЬ f2 set2 ans)))
        ))
        (car set1)
        (cdr set1)
    )
)

(defun ОБЪЕДИНЕНИЕ_С_ПОВТОРАМИ (set1 set2 &optional (ans ()))
    (cond
        ((and (null set1) (null set2)) ans)
        ((null set1) (ОБЪЕДИНЕНИЕ_С_ПОВТОРАМИ set1 (cdr set2) (cons (car set2) ans)))
        (t (ОБЪЕДИНЕНИЕ_С_ПОВТОРАМИ (cdr set1) set2 (cons (car set1) ans)))
    )
)


(defun СИММЕТРИЧЕСКАЯ_РАЗНОСТЬ (set1 set2 &optional (ans ()))
    (РАЗНОСТЬ (ОБЪЕДИНЕНИЕ_С_ПОВТОРАМИ set1 set2) (ПЕРЕСЕЧЕНИЕ set1 set2)) 
)

(print '(test cases for 39 problem))
(print (СИММЕТРИЧЕСКАЯ_РАЗНОСТЬ '(1 2 3 4) '(5 2 3 6)))
(print (СИММЕТРИЧЕСКАЯ_РАЗНОСТЬ '(1 4) '(5 2 3 6)))
(print (СИММЕТРИЧЕСКАЯ_РАЗНОСТЬ '(1 4) '(1 4)))
(print (СИММЕТРИЧЕСКАЯ_РАЗНОСТЬ '() '(5 2 3 6)))
(print (СИММЕТРИЧЕСКАЯ_РАЗНОСТЬ '(1 2 3 4) '()))
(print (СИММЕТРИЧЕСКАЯ_РАЗНОСТЬ '() '()))


;№41. Реализовать генератор деревьев, чтобы выдаваемые им деревья имели 
;количество вершин, точно соответствующее числу, указанному в его первом аргументе.

(defun get_tree (n &optional (tree ()))
    (cond 
        ((equal n 0) tree)
        ((equal n 1) (get_tree (- n 1) (list n tree)))
        (t (get_tree (- n 1) (cons n tree)))
    )
)

(print '(test cases for 41 problem))
(print (get_tree 0))
(print (get_tree 1))
(print (get_tree 5))
(print (get_tree 10))


;№48. Функция GET возвращает в качестве результата NIL в том случае, если у 
;символа нет данного свойства, либо если значением этого свойства является NIL.
;Следовательно, функцией GET нельзя проверить, есть ли некоторое свойство в
;списке свойств. Напишите предикат (ИМЕЕТ-СВОЙСТВО символ свойство), который 
;проверяет, обладает ли символ данным свойством.

(defun ИМЕЕТ-СВОЙСТВО (symbol property &optional (prop_lst (symbol-plist symbol)))
    (cond
        ((null prop_lst) (list property 'does 'not 'exist))
        ((equal (car prop_lst) property) (list property (get symbol property)))
        (t (ИМЕЕТ-СВОЙСТВО symbol property (cddr prop_lst)))
    )
)

(setf (getf (symbol-plist 'car) 'color) 'blue)
(setf (getf (symbol-plist 'car) 'brand) 'BMW)
(setf (getf (symbol-plist 'car) 'type) NIL)

(print '(test cases for 48 problem))
(print (ИМЕЕТ-СВОЙСТВО 'car 'color))
(print (ИМЕЕТ-СВОЙСТВО 'car 'brand))
(print (ИМЕЕТ-СВОЙСТВО 'car 'type))
(print (ИМЕЕТ-СВОЙСТВО 'car 'door))
