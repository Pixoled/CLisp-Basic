;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun all-length (alist) 
    (cond
        ((null alist) 0)
        ((atom alist) 1)
        ((listp alist) (+ (all-length (car alist)) (all-length (cdr alist))))
    )
)

(defun range (alist)
    (list (apply #'min alist) (apply #'max alist))
)

(defun before (x y alist)
    (if (eq x y) (return-from before (list x)))
    (before2 nil x y alist)
)

(defun before2 (flag x y alist)
    (setq f flag)
    (cond
        ((null alist) nil)
        ((equal (car alist) x) (progn   (setq f t)
                                        (cons (car alist) (before2 f x y (cdr alist)))
                                )
        )
        ((and (equal (car alist) y) f) (cons (car alist) nil))
        (f (cons (car alist) (before2 f x y (cdr alist))))
        (t (before2 f x y (cdr alist)))
    )
)

(defun split-if (fun alist)
    (setq left nil)
    (setq right nil)
    (split2 fun alist left right)
)

(defun split2 (fun alist left right)
    (cond
        ((null alist) (list left right))
        ((eq (funcall fun (car alist)) t) (progn (setq right (nconc right (list (car alist))))
                                                 (split2 fun (cdr alist) left right)
                                          ))
        (t (progn (setq left (nconc left (list (car alist)))) 
                  (split2 fun (cdr alist) left right)
           ))
    )
)

(defun group (alist num)
    (cond
        ((null alist) nil)
        (t (cons (before (car alist) (nth (- num 1) alist) alist) (group (nthcdr num alist) num)))
    )
)

(defun mostn (fun alist)
    (setq num (apply #'max (mapcar #'(lambda(l) (funcall fun l)) alist)))
    (cons (mlist fun alist num) (list num))
)

(defun mlist (fun alist num)
    (cond
        ((null alist) nil)
        ((eq (funcall fun (car alist)) num) (cons (car alist) (mlist fun (cdr alist) num)))
        (t (mlist fun (cdr alist) num))
    )
)

(defun match (pat alist)
    (if (null (member '! pat))
        (if (/= (length pat) (length alist)) (return-from match nil))
    )
    
    (cond
        ((and (null pat) (null alist)) t)
        ((equal (car pat) (car alist)) (match (cdr pat) (cdr alist)))
        ((equal (car pat) '?) (match (cdr pat) (cdr alist)))
        ((equal (car pat) '!) (wildcard pat alist))
        (t nil)
    )
)

(defun wildcard (pat alist)
    (cond
        ((null pat) t)
        ((null alist) nil)
        ((equal (car pat) '?) (match (cdr pat) (cdr alist)))
        ((equal (car pat) '!) (wildcard (cdr pat) (cdr alist)))
        (t (if (equal (car pat) (car alist)) (match (cdr pat) (cdr alist))
                                             (wildcard pat (cdr alist)))
        )
    )
)

(defun match-var (pat alist)
    (cond
        ((and (null pat) (null alist)) t)
        ((equal (car pat) (car alist)) (match-var (cdr pat) (cdr alist)))
        ((listp (car pat))  (cond
                                ((eq (caar pat) '?) (progn (set (car (cdar pat)) (car alist))
                                                           (match-var (cdr pat) (cdr alist))))
                                ((eq (caar pat) '!) (NOT (null (glist pat alist))))                    
                                (t (if (equal (car pat) (car alist)) 
                                    (match-var (cdr pat) (cdr alist))
                                    nil)
                                )
                            )
        )
        (t nil)
    )
)    

(defun glist (pat alist)
    (cond
        ((null (cdr pat)) (set (car (cdar pat)) alist))
        ((null (cdr alist)) nil)
        ((listp (cadr pat)) (progn
                                (set (cadar pat) (list (car alist)))
                                (match-var (cdr pat) (cdr alist)))
        )
        ((not (equal (cadr pat) (car alist)))  
            (if (setq num (position (cadr pat) alist))
                (if (match-var (cdr pat) (nthcdr num alist))
                    (progn
                        (set (cadar pat) (subseq alist 0 num))
                        t
                    )
                    (glist pat (nthcdr (+ num 1) alist))
                )
            )
        )
        (t  (if (equal (cadr pat) (cadr alist))
                (set (cadar pat) (cons (car alist) (glist pat (cdr alist))))
                (progn
                    (match-var (cddr pat) (cdr alist))
                    nil)
            )
        )
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 