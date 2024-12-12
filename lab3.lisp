;;; Функціональний варіант сортування обміном за незменшенням
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

;;; Імперативний варіант сортування обміном за незменшенням
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

;;; Функція перевірки для тестування функцій сортування
(defun check-sorting (name input expected-output sorting-function)
  "Перевірка функції сортування з виведенням детальної інформації"
  (let ((result (funcall sorting-function input)))
    (format t "~%Test: ~a~%" name)
    (format t "Input list:  ~a~%" input)
    (format t "Result list: ~a~%" result)
    (format t "Expected:    ~a~%" expected-output)
    (format t "~:[FAILED~;PASSED~]~%" (equal result expected-output))))

;;; Набір тестів для функціонального сортування
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

;;; Набір тестів для імперативного сортування
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

;;; Функція для запуску всіх тестів
(defun run-all-sorting-tests ()
  "Виконання усіх тестів для обох варіантів сортування"
  (format t "~%=== Running Functional Exchange Sort Tests ===~%")
  (test-functional-exchange-sort)
  
  (format t "~%=== Running Imperative Exchange Sort Tests ===~%")
  (test-imperative-exchange-sort))