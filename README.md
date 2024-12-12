<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 3</b><br/>
"Функціональний і імперативний підходи до роботи зі списками"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right">Студент: Сурмачевський Владислав Володимирович<p>
<p align="right">Рік: 2024<p>
  
# Завдання
### Реалізуйте алгоритм сортування чисел у списку двома способами: функціонально і імперативно.
## Пункт 1
### Функціональний варіант реалізації має базуватись на використанні рекурсії і конструюванні нових списків щоразу, коли необхідно виконати зміну вхідного списку. Не допускається використання: деструктивних операцій, циклів, функцій вищого порядку або функцій для роботи зі списками/послідовностями, що використовуються як функції вищого порядку. Також реалізована функція не має бути функціоналом (тобто приймати на вхід функції в якості аргументів).
## Варіант 2: Алгоритм сортування обміном No1 (без оптимізацій) за незменшенням.
  
#### Код (functional-exchange-sort):

     (defun functional-exchange-sort (lst)
        "Функціональна реалізація сортування обміном за незменшенням"
        (labels 
            ((find-min (current-list)
               "Знаходження мінімального елемента у списку"
               (cond 
                 ((null current-list) nil)
                 ((null (rest current-list)) (first current-list))
                 (t (let ((min-rest (find-min (rest current-list))))
                      (if (<= (first current-list) min-rest)
                          (first current-list)
                          min-rest)))))
             
             (remove-first-min (current-list min-val)
               "Видалення першого входження мінімального елемента"
               (cond
                 ((null current-list) nil)
                 ((= (first current-list) min-val) (rest current-list))
                 (t (cons (first current-list) 
                          (remove-first-min (rest current-list) min-val)))))
             
             (sort-list (current-list)
               "Рекурсивне сортування списку"
               (cond 
                 ((null current-list) nil)
                 (t (let ((min-val (find-min current-list)))
                      (cons min-val 
                            (sort-list (remove-first-min current-list min-val))))))))
          
          (sort-list lst))) 

#### Результат

     CL-USER> (functional-exchange-sort '(32 12 56 4 1 4 85))
    (1 4 4 12 32 56 85)

## Пункт 2
### Імперативний варіант реалізації має базуватись на використанні циклів і деструктивних функцій (псевдофункцій). Не допускається використання функцій вищого порядку або функцій для роботи зі списками/послідовностями, що використовуються як функції вищого порядку. Тим не менш, оригінальний список цей варіант реалізації також не має змінювати, тому перед виконанням деструктивних змін варто застосувати функцію copy-list (в разі необхідності).Також реалізована функція не має бути функціоналом (тобто приймати на вхід функції в якості аргументів).

#### Код (imperative-exchange-sort):

     (defun imperative-exchange-sort (lst)
      "Імперативна реалізація сортування обміном за незменшенням"
      (let* ((working-list (copy-list lst))
             (length (length working-list)))
        (dotimes (i (1- length))
          (dotimes (j (- length i 1))
            (let ((current-index j)
                  (next-index (1+ j)))
              (when (> (nth current-index working-list) 
                       (nth next-index working-list))
                (rotatef (nth current-index working-list)
                         (nth next-index working-list))))))
        working-list))

#### Результат

     CL-USER> (imperative-exchange-sort '(4 12 1 4 32 56 102 85))
    (1 4 4 12 32 56 85 102)

# Завдання 2
## Просте модульне тестування
### Для виконання тестування розроблених функцій можна написати функцію, яка виконує фактичний результат з очікуваним і вивести повідомлення про те, що пройшла перевірка однієї чи ні.

#### Функція для перевірки результатів
    (defun check-sorting (name input expected-output sorting-function)
      "Перевірка функції сортування з виведенням детальної інформації"
      (let ((result (funcall sorting-function input)))
        (format t "~%Test: ~a~%" name)
        (format t "Input list:  ~a~%" input)
        (format t "Result list: ~a~%" result)
        (format t "Expected:    ~a~%" expected-output)
        (format t "~:[FAILED~;PASSED~]~%" (equal result expected-output))))
#### Тестування функції (functional-exchange-sort)
    (defun test-functional-exchange-sort ()
      "Тести для функціонального варіанту сортування"
      (check-sorting "Functional Sort - Test 1" 
                     '(5 2 9 1 7 6 3) 
                     '(1 2 3 5 6 7 9) 
                     #'functional-exchange-sort)
      
      (check-sorting "Functional Sort - Test 2 (Empty List)" 
                     '() 
                     '() 
                     #'functional-exchange-sort)
      
      (check-sorting "Functional Sort - Test 3 (Already Sorted)" 
                     '(1 2 3 4 5) 
                     '(1 2 3 4 5) 
                     #'functional-exchange-sort)
      
      (check-sorting "Functional Sort - Test 4 (Reverse Sorted)" 
                     '(5 4 3 2 1) 
                     '(1 2 3 4 5) 
                     #'functional-exchange-sort)
      
      (check-sorting "Functional Sort - Test 5 (Duplicates)" 
                     '(3 1 4 1 5 9 2 6 5) 
                     '(1 1 2 3 4 5 5 6 9) 
                     #'functional-exchange-sort))

#### Тестування функції (imperative-exchange-sort)
    (defun test-imperative-exchange-sort ()
      "Тести для імперативного варіанту сортування"
      (check-sorting "Imperative Sort - Test 1" 
                     '(5 2 9 1 7 6 3) 
                     '(1 2 3 5 6 7 9) 
                     #'imperative-exchange-sort)
      
      (check-sorting "Imperative Sort - Test 2 (Empty List)" 
                     '() 
                     '() 
                     #'imperative-exchange-sort)
      
      (check-sorting "Imperative Sort - Test 3 (Already Sorted)" 
                     '(1 2 3 4 5) 
                     '(1 2 3 4 5) 
                     #'imperative-exchange-sort)
      
      (check-sorting "Imperative Sort - Test 4 (Reverse Sorted)" 
                     '(5 4 3 2 1) 
                     '(1 2 3 4 5) 
                     #'imperative-exchange-sort)
      
      (check-sorting "Imperative Sort - Test 5 (Duplicates)" 
                     '(3 1 4 1 5 9 2 6 5) 
                     '(1 1 2 3 4 5 5 6 9) 
                     #'imperative-exchange-sort))

#### Виконання тестів
    (defun run-all-sorting-tests ()
      "Виконання усіх тестів для обох варіантів сортування"
      (format t "~%=== Running Functional Exchange Sort Tests ===~%")
      (test-functional-exchange-sort)
      
      (format t "~%=== Running Imperative Exchange Sort Tests ===~%")
      (test-imperative-exchange-sort))

#### Результат
    === Running Functional Exchange Sort Tests ===
    
    Test: Functional Sort - Test 1
    Input list:  (5 2 9 1 7 6 3)
    Result list: (1 2 3 5 6 7 9)
    Expected:    (1 2 3 5 6 7 9)
    PASSED
    
    Test: Functional Sort - Test 2 (Empty List)
    Input list:  NIL
    Result list: NIL
    Expected:    NIL
    PASSED
    
    Test: Functional Sort - Test 3 (Already Sorted)
    Input list:  (1 2 3 4 5)
    Result list: (1 2 3 4 5)
    Expected:    (1 2 3 4 5)
    PASSED
    
    Test: Functional Sort - Test 4 (Reverse Sorted)
    Input list:  (5 4 3 2 1)
    Result list: (1 2 3 4 5)
    Expected:    (1 2 3 4 5)
    PASSED
    
    Test: Functional Sort - Test 5 (Duplicates)
    Input list:  (3 1 4 1 5 9 2 6 5)
    Result list: (1 1 2 3 4 5 5 6 9)
    Expected:    (1 1 2 3 4 5 5 6 9)
    PASSED
    
    === Running Imperative Exchange Sort Tests ===
    
    Test: Imperative Sort - Test 1
    Input list:  (5 2 9 1 7 6 3)
    Result list: (1 2 3 5 6 7 9)
    Expected:    (1 2 3 5 6 7 9)
    PASSED
    
    Test: Imperative Sort - Test 2 (Empty List)
    Input list:  NIL
    Result list: NIL
    Expected:    NIL
    PASSED
    
    Test: Imperative Sort - Test 3 (Already Sorted)
    Input list:  (1 2 3 4 5)
    Result list: (1 2 3 4 5)
    Expected:    (1 2 3 4 5)
    PASSED
    
    Test: Imperative Sort - Test 4 (Reverse Sorted)
    Input list:  (5 4 3 2 1)
    Result list: (1 2 3 4 5)
    Expected:    (1 2 3 4 5)
    PASSED
    
    Test: Imperative Sort - Test 5 (Duplicates)
    Input list:  (3 1 4 1 5 9 2 6 5)
    Result list: (1 1 2 3 4 5 5 6 9)
    Expected:    (1 1 2 3 4 5 5 6 9)
    PASSED
    NIL
