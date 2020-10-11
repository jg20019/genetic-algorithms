(in-package :genetic)

(defun take (n list)
  "Returns a list of the first n elements in list. 
   If n is less than 0, returns an empty list. 
   If n is greater than the length of the list, returns
   the entire list." 
  (if (or (<= n 0) (null list))
      '()
      (cons (car list) (take (- n 1) (cdr list)))))

(defun drop (n list)
  "Returns a list with the first n elements dropped. 
   If n is not positive, returns the list. 
   If n is greater than the length of the list, returns
   the empty list." 
  (if (or (<= n 0) (null list))
      list
      (drop (- n 1) (cdr list))))
