(defun square (n)
  "returns the square of number <<n>>"
  (* n n))

(defun list-rotate (lis)
  "moves the first element to the end of the <<list>>"
  (append (cdr lis)
          (list (car lis))))

(defun cube (n)
  "returns the cube of number <<n>>"
  (* n n n))

(defun last1 (lis)
  "returns last element"
  (car (last lis)))

(defun right-rotate (lis)
  "moves the last element to the head of the <<list>>"
  (cons (last1 lis) (butlast lis)))

(defun parindrome-p (lis)
  "parindrome"
  (equal lis (reverse lis)))

(defun length=1 (lis)
  "whether a list length == 1"
  (and (consp lis) (equal 1 (length lis))))

(defun starts-with (lis ele)
  "start with"
  (and (consp lis) (equal ele (car lis))))

(defun our-member (item list)
  "This is almost the same functions as common lisp's member."
  (cond ((endp list) nil)
        ((equal item (car list)) t)
        (t (our-member item (cdr list)))))

(defun is-there-num (lis)
  "or $ map (\n -> n is number) lis"
  (cond ((endp lis) nil)
        ((numberp (car lis)) lis)
        (t (is-there-num (cdr lis)))))

(defun is-lat (lis)
  "and $ map (\x -> isAtom x) lis"
  (cond ((endp lis) t)
        ((atom (car lis)) (is-lat (cdr lis)))
        (t nil)))

(defun is-lnum (lis)
  "and $ map (\x -> isNumber x) lis"
  (cond ((endp lis) t)
        ((numberp (car lis)) (is-lnum (cdr lis)))
        (t nil)))

(defun count-elements (lis)
  (cond ((endp lis) 0)
        (t (1+ (count-elements (cdr lis))))))

(defun our-copy-list (lis)
  (cond ((endp lis) nil)
        (t (cons (car lis) (our-copy-list (cdr lis))))))

(defun our-reverse (lis)
  (cond ((endp lis) nil)
        (t (cons (car (last lis)) (our-reverse (butlast lis))))))

(defun square-list (lis)
  (cond ((not (is-lnum lis)) nil)
        ((endp lis) nil)
        ((numberp (car lis)) (cons (* (car lis) (car lis)) (square-list (cdr lis))))
        (t nil)))

(defun sqrt-list (lis)
  (cond ((null lis) '())
        ((< (car lis) 0)
         (sqrt-list (cdr lis)))
        ((= (car lis) 0)
         (cons 0 (sqrt-list (cdr lis))))
        (t (cons (sqrt (car lis))
                 (sqrt-list (cdr lis))))))

(defun rember (e lis)
  (cond ((endp lis) nil)
        ((equal e (car lis)) (cdr lis))
        (t (cons (car lis) (rember e (cdr lis))))))

; 3.6
(defun is-lats (lis)
  (cond ((endp lis) t)
        ((is-lat (car lis)) (is-lats (cdr lis)))
        (t nil)))

; ((a b) (c d) (e) (f g h) (i j))
; => (a c e f i)

(defun collect-firsts (liss)
  (cond ((not (is-lats liss)) nil)
        ((endp liss) nil)
        (t (cons (car (car liss)) (collect-firsts(cdr liss))))))

(defun insert-left (key item lat)
  (cond ((endp lat) nil)
        ((equal key (car lat))
         (cons item
               (cons (car lat)
                     (insert-left key item (cdr lat)))))
        (t (cons (car lat) (insert-left key item (cdr lat))))))

(defun insert-right (key item lat)
  (cond ((endp lat) nil)
        ((equal key (car lat))
         (cons (car lat)
               (cons item
                     (insert-right key item (cdr lat)))))
        (t (cons (car lat) (insert-right key item (cdr lat))))))

(defun our-copy-tree (list)
  (cond ((endp list) nil)
        ((atom (car list))
         (cons (car list)
               (our-copy-tree (cdr list))))
        (t (cons (our-copy-tree (car list))
                 (our-copy-tree (cdr list))))))

; '(a b (c d) (e (f g) h) i (j k))
(defun count-atoms (liss)
  (cond ((endp liss) 0)
        ((atom (car liss)) (+ 1 (count-atoms (cdr liss))))
        (t (+ (count-atoms (car liss)) (count-atoms (cdr liss))))))

; '(1 4 (5 9) (1 (3 5) 6) 7 (2 5))
; '(1 4 (5 9) (1 (3 g) 6) 7 (2 5))
(defun all-numberp (liss)
  (cond ((endp liss) t)
        ((atom (car liss))
         (and (numberp (car liss))
              (all-numberp (cdr liss))))
        (t (and (all-numberp (car liss)) (all-numberp (cdr liss))))))

; '(1 4 (5 9) (1 (3 g) 6) 7 (2 5))
; cons a '(b c) => (a b c)
; snoc '(b c) a => (b c a)
; append '(b c) '(a) => (b c a)
(defun reverse-tree (liss)
  (cond ((endp liss) nil)
        ((atom (car liss))
         (append (reverse-tree (cdr liss))
                 (list (car liss))))
        (t
         (append (reverse-tree (cdr liss))
                 (list (reverse-tree (car liss)))))))

; '(1 4 (5 not 9) (1 (3 not g) not 6) 7 (2 5))
(defun remove-not (liss)
  (cond ((endp liss) nil)
        ((and (atom (car liss))
              (equal 'not (car liss)))
         (remove-not (cdr liss)))
        ((atom (car liss)) (cons (car liss) (remove-not (cdr liss))))
        (t (cons (remove-not (car liss)) (remove-not (cdr liss))))))

; '(1 4 (5 9) (1 (3 5) 6) 7 (2 5))
(defun squash (liss)
  (cond ((endp liss) nil)
        ((atom (car liss)) (cons (car liss) (squash (cdr liss))))
        (t (append (squash (car liss)) (squash (cdr liss))))))

(defun snoc (xs x)
  (append xs (list x)))

; 聴きたいこと　S式をどう書いていくか（動的に）

; fboundpを挿入している意味
; 4.3から読む（4.2までは読んだ）

(defun %our-delete (item list)
  (cond ((null (cdr list)) nil)
        ((eq item (second list))
         (rplacd list (cddr list)))
        (t (%our-delete item (cdr list)))))

(defun our-delete (item list)
  (setq list (cons 'dummy list))
  (%our-delete item list)
  (cdr list))

; (setq key 'a)
; (setq item 'z)
; (setq lat '(a b c a b c a b c a b c))
(defun insert-rightD (key item lat)
  (cond ((endp lat) nil)
        ((equal key (car lat))
         (let ((x (list (car lat) item)))
           (nconc x (insert-rightD key item (cdr lat)))
           x))
        (t
         (let ((rest-inserted (insert-rightD key item (cdr lat))))
           (rplacd lat rest-inserted)
           lat))))

(defun insert-leftD (key item lat)
  (cond ((endp lat) nil)
        ((equal key (car lat))
         (let ((x (list item (car lat))))
           (nconc x (insert-leftD key item (cdr lat)))
           x))
        (t
         (let ((rest-inserted (insert-leftD key item (cdr lat))))
           (rplacd lat rest-inserted)
           lat))))

; '(a b c d e)
(defun nreverse-helper (lst prev)
  (let ((next (cdr lst)))
    (rplacd lst prev)
    (if (endp next)
        lst
        (nreverse-helper next lst))))

(defun our-nreverse (lst)
  (if (endp lst)
      nil
      (nreverse-helper lst nil)))

(defun make-boys-group-from (lst)
  (cond ((endp lst) nil)
        ((equal (get (car lst) 'sex) 'male) (cons (car lst) (make-boys-group-from (cdr lst))))
        (t (make-boys-group-from (cdr lst)))))

(defun make-girls-group-from (lst)
  (cond ((endp lst) nil)
        ((equal (get (car lst) 'sex) 'female) (cons (car lst) (make-girls-group-from (cdr lst))))
        (t (make-girls-group-from (cdr lst)))))

(defun meetsPairs (list0 list1 acc)
  (cond ((endp list0) acc)
        ((endp list1) acc)
        (t (meetsPairs (cdr list0) (cdr list1) (cons (list (car list0) (car list1)) acc)))))

; 破壊的関数がよく分かってないのが今回のキツさの原因

;(defun boys-meet-girls* (lst)
;    (meetsPairs((make-boys-group-from lst) (make-boys-group-from lst) nil)))

;(apply '(lambda (lis)
;         (append (rest lis)
;          (list (first lis))))
;       '(a b c d) nil)
; Evaluation aborted on #<TYPE-ERROR expected-type: (OR FUNCTION SYMBOL)
;              datum: (LAMBDA (LIS) (APPEND (REST LIS) (LIST (FIRST LIS))))>.

(defun firsts (xs &optional (n 1))
  (%firsts xs n))

(defun %firsts (xs n)
  (cond ((< n 1) nil)
        (t (cons (car xs) (%firsts (cdr xs) (- n 1))))))

(defun square-list2 (xs)
  (mapcar #'(lambda (x) (* x x)) xs))

(defun count-numbers (x)
  (cond ((null x) 0)
        ((and (atom x) (numberp x)) 1)
        ((atom x) 0)
        (t (apply #'+ (mapcar #'count-numbers x)))))

(defun mappend (fn list)
  "Append the results of calling 《fn》 on each element of 《list》.
   Like 《mapcon》, but uses 《append》 instead of 《nconc》."
  (apply #'append (mapcar fn list)))

(defun andl (lis)
  (cond ((endp lis) t)
        ((equal (car lis) t) (andl (cdr lis)))
        (t nil)))

(defun filter (f xs)
  (cond ((endp xs) nil)
        ((funcall f (car xs)) (cons (car xs) (filter f (cdr xs))))
        (t (filter f (cdr xs)))))

(defun remove-not (x)
  (cond ((null x) nil)
        ((and (atom x) (equal x 'not)) nil)
        ((atom x) x)
        (t (filter #'(lambda (x) (not (null x))) (mapcar #'remove-not x)))))

; (remove-not '(a b (not c) (d e) f))

(defun squash (x)
  (cond ((null x) nil)
        ((atom x) (list x))
        (t (mapcan #'squash x))))

; (squash '(a b (c) (d e) f (z (y w (x u)))))

(defun is-lnum2 (ns)
  (every #'numberp ns))

(defmacro when2 (test &body forms)
  `(if ,test (progn ,@forms) nil))

(when2 (equal *wheather-forecast* '(it will rain))
  (have 'umbrella)
  (go-out))

(defun not-null (x) (not (null x)))

(defmacro not-null2 (x)
  (list 'not (list 'null x)))

; (defun last1 (lis)
;   "returns last element"
;   (car (last lis)))

(defmacro last1m2 (lis)
  (list 'car (list 'last lis)))

(defmacro not-null (x) `(not (null ,x)))

(defmacro last1m (lis) `(car (last ,lis)))

(our-let ((arg1 val1) (arg2 val2) ... )
         form1 form2 ...)

((lambda (arg1 arg2 ... )
   form1 form2 ...)
 val1 val2 ...)


(defmacro our-let (parms &rest body)
  `((lambda ,(mapcar #'car parms) ,@body)
    ,@(mapcar #'cadr parms)))

(defmacro our-let* (parms &rest body)
  (if (null parms)
      `(progn ,@body)
      `(our-let (,(first parms))
                (our-let* ,(rest parms) ,@body))))


(defmacro put (subj attr value)
  `(setf (get ,subj ,attr) ,value))

(defmacro getqq (subj attr)
  `(get ',subj ',attr))

(defmacro putqq (subj attr value)
  `(setf (get ',subj ',attr) ,value))

(defmacro define (variable value)
  `(defvar ,variable ',value))

(defmacro define (xs &body procs)
  (if (listp xs)
      `(defun ,(car xs) ,(cdr xs) ,@procs)
      `(defvar ,xs ,(car procs))))

(macroexpand-1 '(define (pythagoras x y) (sqrt (+ (* x x) (* y y))))) 
                                        ; => (DEFUN PYTHAGORAS (X Y) (SQRT (+ (* X X) (* Y Y))))
(macroexpand-1 '(define my-name 'Seiji-Koide)) 
                                        ; => (DEFVAR MY-NAME 'Seiji-Koide)


(defun swap (x y)
  (let ((tmp x))
    (setq x y)
    (setq y tmp)))

(defmacro swap (x y)
  `(let ((tmp ,x))
     (setq ,x ,y)
     (setq ,y tmp)))

(defmacro our-repeat (times &body body)
  `(dotimes (x ,times) ,@body))

(our-repeat 3 (print 'Hi))

(defmacro repeat (times &body body)
  (let ((x (gensym)))
    `(dotimes (,x ,times) ,@body)))

(defmacro cube (n)
  `(* ,n ,n ,n))

(defmacro cube (n)
  (let ((x (gensym)))
    `(let ((,x ,n))
       (* ,x ,x ,x))))

(defmacro once-only (variables &rest body)
  "Returns the code built by BODY.  If any of VARIABLES
  might have side effects, they are evaluated once and stored
  in temporary variables that are then passed to BODY."
  (assert (every #'symbolp variables))
  (let ((temps nil))
    (dotimes (i (length variables)) (push (gensym) temps))
    `(if
      ; (every #'side-effect-free? (list .,variables))
      ; (every #'side-effect-free? ,variables)
      ; (every #'side-effect-free? (list ,variables))
      (every #'side-effect-free? (list ,@variables))
         (progn .,body)
         (list 'let
             ,`(list ,@(mapcar #'(lambda (tmp var)
                                   `(list ',tmp ,var))
                         temps variables))
             (let ,(mapcar #'(lambda (var tmp) `(,var ',tmp))
                     variables temps)
               .,body)))))


(defun side-effect-free? (exp)
  "Is exp a constant, variable, or function,
  or of the form (THE type x) where x is side-effect-free?"
  (or (atom exp) (constantp exp)
      (starts-with exp 'function)
      (and (starts-with exp 'the)
           (side-effect-free? (third exp)))))

(defun starts-with (list x)
  "Is x a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defmacro cube (x)
  (once-only (x) `(* ,x ,x ,x)))

(macroexpand-1 '(once-only (x y)
  (progn (print x) (print y))))

(IF (EVERY #'SIDE-EFFECT-FREE? (LIST X Y))
    (PROGN (PROGN (PRINT X) (PRINT Y)))
    (LIST 'LET (LIST (LIST '#:G580 X) (LIST '#:G579 Y))
          (LET ((X '#:G580) (Y '#:G579))
            (PROGN (PRINT X) (PRINT Y)))))

(IF (EVERY #'SIDE-EFFECT-FREE? (LIST X Y))
    (PROGN (PROGN (PRINT X) (PRINT Y)))
    (LIST 'LET (LIST (LIST '#:G586 X) (LIST '#:G585 Y))
          (LET ((X '#:G586) (Y '#:G585))
            (PROGN (PRINT X) (PRINT Y)))))


(IF (EVERY #'SIDE-EFFECT-FREE? (X Y))
    (PROGN (PROGN (PRINT X) (PRINT Y)))
    (LIST 'LET (LIST (LIST '#:G582 X) (LIST '#:G581 Y))
          (LET ((X '#:G582) (Y '#:G581))
            (PROGN (PRINT X) (PRINT Y)))))

(IF (EVERY #'SIDE-EFFECT-FREE? (LIST (X Y)))
    (PROGN (PROGN (PRINT X) (PRINT Y)))
    (LIST 'LET (LIST (LIST '#:G584 X) (LIST '#:G583 Y))
          (LET ((X '#:G584) (Y '#:G583))
            (PROGN (PRINT X) (PRINT Y)))))


(((?* ?x) dream about (?* ?y))
 (How do you feel about ?y in reality?))
(((?* ?x) dream (?* ?y))    
 (What does this dream suggest to you?) (Do you dream often?)
 (What persons appear in your dreams?)
 (Do not you believe that dream has to do with your problem?))

(defparameter *eliza-rules* nil)

(defrule rule1
    (?* ?x) dream about (?* ?y) ->
  (What does this dream suggest to you?)
  (Do you dream often?)
  (What persons appear in your dream?)
  (Don't you believe that dream has to do with your problem?))

(defmacro defrule (name &rest body)
  (let ((pattern (gensym))
        (responces (gensym))
        (pos (gensym)))
    `(let ((,pos (position '-> ',body)))
       (let ((,pattern
               (butlast ',body
                        (- (length ',body) ,pos)))
             (,responces
               (cdr (last ',body
                          (- (length ',body) ,pos)))))
         (setf (get ',name 'eliza-rule)
               (cons ,pattern ,responces))
         (setq *eliza-rules*
               (append *eliza-rules
                       (list (get ',name 'eliza-rule))))
         ',name))))

(defmacro defrule (name &rest body)
  (let ((pattern (gensym))
        (responces (gensym)))
    `(let ((,pattern 
             ',(butlast body
                        (- (length body)
                           (position '-> body))))
           (,responces
             ',(cdr (last body
                          (- (length body)
                             (position '-> body))))))
       (setf (get ',name 'eliza-rule)
             (cons ,pattern ,responces))
       (setq *eliza-rules*
             (append *eliza-rules*
                     (list (get ',name 'eliza-rule))))
       ',name)))

(for (x a (> x 10)) (print x))
; => 
(do ((x 1 (+ 1 x)))
    ((> x 10))
  (print x))

(defmacro for (x xinit xterm &body body)
  `(do ((,x ,xinit ,(+ 1 x)))
      (,(> x xterm))
    ,@body))

(defmacro define (variable value)
  `(defvar ,variable ',value))

(defun test-tail (n)
  (print n)
  (test-tail (1+ n)))

; 10.1
(defun count-atoms (list)
  (cond ((null list) 0)
        ((atom list) 1)
        (t (+ (count-atoms (car list))
              (count-atoms (cdr list))))))

; (defun count-atomsTRH (list n)
;   (cond ((null list) n)
;         ((atom list) (+ 1 n))
;         (t (+ n (+ (count-atomsTRH (car list) 0)
;               (count-atomsTRH (cdr list) 0))))))

(defun count-atomsTRH (list n)
  (cond ((null list) n)
        ((atom list) (+ 1 n))
        (t (count-atomsTRH (cdr list) (count-atomsTRH (car list) n)))))

(defun count-atomsTR (list)
  (count-atomsTRH list 0))

(trace count-atoms)
(trace count-atomsTRH)
(trace count-atomsTR)

; 10.2

(defun our-reverse (list)
  (cond ((null list) '())
        (t (append (our-reverse (cdr list))
                   (list (car list))))))

(defun our-reverseTRH (lis accl)
  (cond ((null lis) accl)
        (t (our-reverseTRH (cdr lis) (append accl (list (car lis)))))))

(defun our-reverseTR (lis)
  (our-reverseTRH lis '()))

(setq lis '(a b c d e))
(trace our-reverse)
(trace our-reverseTRH)
(trace our-reverseTR)

(defun sum-1-to-n (-c- n)
  (labels ((sum-loop (nn sum)
             (cond ((= nn 0) (funcall -c- sum))
                   (t (sum-loop (1- nn) (+ sum nn))))))
    (sum-loop n 0)))
(sum-1-to-n #'values 10)

(defun factorial (n)
  (if (zerop n) 1
      (* n (factorial (1- n)))))

(defun actorial (-c- n)
  (if (zerop n) (funcall -c- 1)
      (actorial #'(lambda (z) (funcall -c- (* n z)))
                (1- n))))

(defun reader (-corou-)
  (labels ((mainloop (-corou- obj)
             (funcall -corou-
                      #'(lambda (-c-)
                          (mainloop -c- (read)))
                      obj)))
    (funcall #'mainloop -corou- (read))))

(defun writer (-corou-)
  (labels ((writerloop (-corou- thing)
             (cond ((eql thing :done) :done)
                   (t (print thing)
                      (print '>)
                      (funcall -corou- #'writerloop)))))
    (funcall -corou- #'writerloop)))

(defun reader (-corou- in-stream)
  (labels ((mainloop (-corou- char)
             (funcall -corou-
                      #'(lambda (-c-)
                          (mainloop -c- (read-char in-stream nil nil)))
                      char)))
    (funcall #'mainloop -corou- (read-char in-stream))))

(defun writer (-corou- out-stream)
  (labels ((writerloop (-corou- char)
             (cond ((null char) :done)
                   (t (write-char char out-stream)
                      (funcall -corou- #'writerloop)))))
    (funcall -corou- #'writerloop)))


(defun reader (-corou- in-stream)
  (labels ((mainloop (-corou- char)
             (cond ((null char)
                    (funcall -corou- #'values nil))
                   ((alphanumericp char)
                    (funcall -corou-
                             #'(lambda (-c-)
                                 (mainloop -c- (read-char in-stream nil nil)))
                             char))
                   (t ; special character such that #\Newline or #\Space
                    (specloop -corou- char (read-char in-stream nil nil)))))
           (specloop (-corou- prev-char char)
             (cond ((null char)
                    (funcall -corou- #'values nil))
                   ((not (alphanumericp char))
                    (specloop -corou- char (read-char in-stream nil nil)))
                   (t ;; new token started
                    (funcall -corou-
                             #'(lambda (-c-) (mainloop -c- char))
                             prev-char)))))
    (funcall #'mainloop -corou- (read-char in-stream))))

(defun writer (-corou-)
  (let ((token nil)
        (token-list nil))
    (labels ((writerloop (-corou- char)
               (cond ((null char)
                      (unless (null token)
                        (push (coerce (nreverse token) 'string) token-list))
                      (setq token nil)
                      (funcall -corou- (nreverse token-list)))
                     ((alphanumericp char)
                      (push char token)
                      (funcall -corou- #'writerloop))
                     (t ; special char such that #\Newline or #\Space
                      (unless (null token)
                        (push (coerce (nreverse token) 'string) token-list))
                      (setq token nil)
                      (funcall -corou- #'writerloop))
                     )))
      (funcall -corou- #'writerloop))))

(with-open-file (in-stream "/home/polymony/claip/test.txt")
  (writer #'(lambda (-corou-) (reader -corou- in-stream))))

