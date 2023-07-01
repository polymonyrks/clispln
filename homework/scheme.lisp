(defpackage scheme
  (:shadow #:if #:lambda #:write #:dynamic #:dynamic-reference)
  (:use :common-lisp)
  (:export #:scheme #:macro #:macro-expand #:begin #:set! 
           #:quote #:null? #:eq? #:equal? #:if #:lambda
           #:write #:display #:newline #:define #:letrec #:dynamic #:dynamic-reference)
  (:documentation "Scheme implemented by Common Lisp."))

(in-package :scheme)

(defun length=1 (x) 
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))

(defun last1 (list)
  "Return the last element (not last cons cell) of list"
  (first (last list)))

(defun rest2 (x)
  "The rest of a list after the first TWO elements."
  (rest (rest x)))

(defun starts-with (list x)
  "Is x a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun maybe-add (op exps &optional if-nil)
  "For example, (maybe-add 'and exps t) returns
  t if exps is nil, exps if there is only one,
  and (and exp1 exp2...) if there are several exps."
  (cond ((null exps) if-nil)
        ((length=1 exps) (first exps))
        (t (cons op exps))))

(defun set-global-var! (var val)
  (setf (get var 'global-val) val))

(defun get-global-var (var)
  (let* ((default "unbound")
         (val (get var 'global-val default)))
    (cl:if (eq val default)
           (error "Unbound scheme variable: ~a" var)
           val)))

(defun set-var! (var val env)
  "Set a variable to a value, in the given or global environment."
  (cl:if (assoc var env)
         (setf (second (assoc var env)) val)
         (set-global-var! var val))
  val)

(defun get-var (var env)
  "Get the value of a variable, from the given or global environment."
  (cl:if (assoc var env)
         (second (assoc var env))
         (get-global-var var)))

(defun extend-env (vars vals env)
  "Add some variables and values to an environment."
  (nconc (mapcar #'list vars vals) env))

(defun scheme:macro (symbol)
  (and (symbolp symbol) (get symbol 'scheme:macro)))

(defmacro def-scheme-macro (name parmlist &body body)
  "Define a Scheme macro."
  `(setf (get ',name 'scheme:macro)
         #'(cl:lambda ,parmlist .,body)))

(defun scheme:macro-expand (x)
  "Macro-expand this Scheme expression."
  (cl:if (and (listp x) (scheme:macro (first x)))
         (scheme:macro-expand
          (apply (scheme:macro (first x)) (rest x)))
         x))
(def-scheme-macro let (bindings &rest body)
  `((scheme:lambda ,(mapcar #'first bindings) . ,body)
    .,(mapcar #'second bindings)))

(def-scheme-macro let* (bindings &rest body)
  (cl:if (null bindings)
      `(scheme:begin .,body)
    `(let (,(first bindings))
       (let* ,(rest bindings) . ,body))))

(def-scheme-macro and (&rest args)
  (cond ((null args) 't)
        ((length=1 args) (first args))
        (t `(scheme:if ,(first args)
              (and . ,(rest args))))))

(def-scheme-macro or (&rest args)
  (cond ((null args) 'nil)
        ((length=1 args) (first args))
        (t (let ((var (gensym)))
             `(let ((,var ,(first args)))
                (scheme:if ,var ,var (or . ,(rest args))))))))

(def-scheme-macro cond (&rest clauses)
  (cond ((null clauses) nil)
        ((length=1 (first clauses))
         `(or ,(first clauses) (cond .,(rest clauses))))
        ((starts-with (first clauses) 'else)
         `(scheme:begin .,(rest (first clauses))))
        (t `(scheme:if ,(first (first clauses))
              (scheme:begin .,(rest (first clauses)))
              (cond .,(rest clauses))))))

(def-scheme-macro case (key &rest clauses)
  (let ((key-val (gensym "KEY")))
    `(let ((,key-val ,key))
       (cond ,@(mapcar
                   #'(cl:lambda (clause)
                       (cl:if (starts-with clause 'else)
                           clause
                         `((member ,key-val ',(first clause))
                           .,(rest clause))))
                 clauses)))))

(def-scheme-macro define (name &rest body)
  (cl:if (atom name)
      `(scheme:begin (scheme:set! ,name . ,body) ',name)
    `(define ,(first name) 
       (scheme:lambda ,(rest name) . ,body))))

(def-scheme-macro delay (computation)
  `(scheme:lambda () ,computation))

(def-scheme-macro letrec (bindings &rest body)
  `(let ,(mapcar #'(cl:lambda (v) (list (first v) nil)) bindings)
     ,@(mapcar #'(cl:lambda (v) `(scheme:set! .,v)) bindings)
     .,body))

;; 静的束縛版
;; (defun print-proc (proc &optional (stream *standard-output*) depth)
;;   (declare (ignore depth proc stream)))

;; (defstruct (proc (:print-function print-proc))
;;   "Represent a Scheme procedure"
;;   code (env nil) (name nil) (parms nil))

;; (defun print-proc (proc &optional (stream *standard-output*) depth)
;;   (declare (ignore depth))
;;   (format stream "{~a}" (or (proc-name proc) '??)))

(defun print-proc (proc &optional (stream *standard-output*) depth)
  (declare (ignore depth))
  (format stream "#<proc:~s>" (proc-name proc)))

(defstruct (proc (:print-function print-proc))
  "Represent a Scheme procedure"
  code (env nil) (name nil) (parms nil))

(defun extend-env (parms args lenv denv)
  (if (null parms)
      (list lenv denv)
      (let ((p (first parms)))
        (cond ((symbolp p)
               (extend-env (rest parms) (rest args)
                           (cons (list p :lexical (first args)) lenv)
                           denv))
              ((and (consp p) (eq (first p) 'scheme:dynamic))
               (let ((var (second p)))
                 (extend-env (rest parms) (rest args)
                             (cons (list var :dynamic) lenv)
                             (cons (list var (first args)) denv))))
              (t
               (error "invalid parameter specifier" p))))))

(defun interp-proc (proc args denv)
  (let ((envs (extend-env (proc-parms proc) args
                          (proc-env proc) denv)))
    (interp (proc-code proc) (first envs) (second envs))))

(defun interp (x &optional lenv denv)
  "Interpret (evaluate) the expression <<x>> in the static environment <<lenv>> and the dynamic environment <<denv>>.
   This version handles macros."
  (cond
   ((symbolp x)
    (let ((p (assoc x lenv)))
      (cl:if (and p (eq (second p) :lexical))
          (third p)
        (get-var x denv))))
   ((atom x) x)
   ((scheme:macro (first x))
    (interp (scheme:macro-expand x) lenv denv))
   ((case (first x)
      (cl:quote  (second x))
      (scheme:begin  (last1 (mapcar #'(cl:lambda (y) (interp y lenv denv))
                              (rest x))))
      (scheme:set!
       (let ((var (second x))
             (val (interp (third x) lenv denv)))
         (let ((p (assoc var lenv)))
           (cl:if (and p (eq (second p) :lexical))
               (setf (third p) val)
             (set-var! var val denv)))))
      (scheme:if
          (cl:if (interp (second x) lenv denv)
              (interp (third x) lenv denv)
            (interp (fourth x) lenv denv)))
      (scheme:lambda (make-proc :env lenv :parms (second x)
                                :code (maybe-add 'scheme:begin (rest2 x))))
      (scheme:dynamic-reference (get-var (second x) denv))
      (otherwise ;; a procedure application
       (let ((proc (interp (first x) lenv denv))
             (args (mapcar #'(cl:lambda (v) (interp v lenv denv)) (rest x))))
         (cl:if (proc-p proc)
             (interp-proc proc args denv)
           (apply proc args))))))))

; 以下、動的束縛オンリー版
;; (defun interp (x &optional env)
;;   "Interpret (evaluate) the expression <<x>> in the environment 
;;    <<env>>. This version handles macros."
;;   (cond
;; 	((symbolp x) (get-var x env))
;; 	((atom x) x)
;; 	((scheme:macro (first x))
;; 	 (interp (scheme:macro-expand x) env))
;; 	((case (first x)
;; 	   (quote  (second x))
;; 	   (scheme:begin  (last1 (mapcar #'(cl:lambda (y) (interp y env))
;;                                    (rest x))))
;; 	   (scheme:set!   (set-var! (second x) (interp (third x) env) env))
;; 	   (scheme:if            (cl:if (interp (second x) env)
;;                               (interp (third x) env)
;;                             (interp (fourth x) env)))
;; 	   (scheme:lambda (let ((parms (second x))
;; 				(code (maybe-add 'scheme:begin (rest2 x))))
;;                             #'(cl:lambda (env &rest args) ; ++++
;;                                 (interp code (extend-env parms args env)))))
;; 	   (otherwise ;; a procedure application
;; 		(apply (interp (first x) env)
;; 			   env                         ; ++++
;; 			   (mapcar #'(cl:lambda (v) (interp v env))
;;                              (rest x))))))))


;; これが静的束縛版(11.1)
;; (defun interp (x &optional env)
;;   "Evaluate the expression x in the environment env.
;;    This version is properly tail-recursive."
;;   (prog ()
;;     :INTERP
;;     (return
;;       (cond
;;        ((symbolp x) (get-var x env))
;;        ((atom x) x)
;;        ((scheme:macro (first x))
;;         (setf x (scheme:macro-expand x)) (go :INTERP))
;;        ((case (first x)
;;           (quote (second x))
;;           (scheme:begin (pop x) ; pop off the BEGIN to get at the args
;;                         ;; Now interpret all but the last expression
;;                         (loop while (rest x) do (interp (pop x) env))
;;                         ;; Finally, rename the last expression as x
;;                         (setf x (first x))
;;                         (go :INTERP))
;;           (scheme:set!  (set-var! (second x) (interp (third x) env) env))
;;           (scheme:if    (setf x (cl:if (interp (second x) env)
;;                                     (third x)
;;                                   (fourth x)))
;;               ;; That is, rename the right expression as x
;;               (go :INTERP))
;;           (scheme:lambda (make-proc :env env :parms (second x)
;;                            :code (maybe-add 'scheme:begin (rest2 x))))
;;           (otherwise   ;; a procedure application
;;            (let ((proc (interp (first x) env))
;;                  (args (mapcar #'(cl:lambda (v) (interp v env)) (rest x))))
;;             (cl:if (proc-p proc)
;;                  ;; Execute procedure with rename+goto
;;                  (progn
;;                    (setf x (proc-code proc))
;;                    (setf env (extend-env (proc-parms proc) args
;;                                          (proc-env proc)))
;;                    (go :INTERP))
;;                ;; else apply primitive procedure
;;                (apply proc args))))))))))

;; (defun interp (x &optional env)
;;   "Interpret (evaluate) the expression <x> in the environment <env>.
;;   This version handles macros."
;;   (cond
;;    ((symbolp x) (get-var x env))
;;    ((atom x) x)
;;    ((scheme:macro (first x))              ;***
;;     (interp (scheme:macro-expand x) env)) ;***
;;    ((case (first x)
;;       (cl:quote      (second x))
;;       (scheme:begin  (last1 (mapcar #'(cl:lambda (y) (interp y env))
;;                               (rest x))))
;;       (scheme:set!   (set-var! (second x) (interp (third x) env) env))
;;       (scheme:if     (cl:if (interp (second x) env)
;;                          (interp (third x) env)
;;                        (interp (fourth x) env)))
;;       (scheme:lambda (let ((parms (second x))
;;                            (code (maybe-add 'scheme:begin (rest2 x))))
;;                        #'(cl:lambda (&rest args)
;;                            (interp code (extend-env parms args env)))))
;;       (otherwise  ;; a procedure application
;;        (apply (interp (first x) env)
;;               (mapcar #'(cl:lambda (v) (interp v env))
;;                 (rest x))))))))


;; (defun interp (x &optional env)
;;   "Interpret (evaluate) the expression <x> in the environment ‹env›."
;;   (cond
;;     ((symbolp x) (get-var x env))
;;     ((atom x) x)
;;     ((case (first x)
;;        (cl:quote      (second x))
;;        (scheme:begin  (last1 (mapcar #'(cl:lambda (y) (interp y env))
;;                                      (rest x))))
;;        (scheme:set!   (set-var! (second x) (interp (third x) env) env))
;;        (scheme:if     (cl:if (interp (second x) env)
;;                              (interp (third x) env)
;;                              (interp (fourth x) env)))
;;        (scheme:lambda (let ((parms (second x))
;;                             (code (maybe-add 'scheme:begin (rest2 x))))
;;                         #'(cl:lambda (&rest args)
;;                             (interp code (extend-env parms args env)))))
;;        (otherwise  ;; a procedure application
;;         (apply (interp (first x) env)
;;                (mapcar #'(cl:lambda (v) (interp v env))
;;                        (rest x))))))))

(defun init-scheme-proc (f)
  "Define a Scheme primitive procedure as a CL function."
  (cl:if (listp f)
      (set-global-var! (first f)
                       #'(cl:lambda (env &rest args)       ; ++++
                           (apply (second f) args)))    ; ++++
      (init-scheme-proc (list f f))))                   ; ++++

;; これが静的束縛版(11.1)
;; (defun init-scheme-proc (f)
;;   "Define a Scheme procedure as a corresponding CL function."
;;   (cl:if (listp f)
;;          (set-global-var! (first f) (symbol-function (second f)))
;;          (set-global-var! f (symbol-function f))))

(defparameter *scheme-procs*
  '(+ - * / = < > <= >= cons car cdr not append list read member
    (null? null) (eq? eq) (equal? equal) (eqv? eql)
    (write prin1) (display princ) (newline terpri)))


(defun init-scheme-interp ()
  "Initialize the scheme interpreter with some global variables."
  ;; Define Scheme procedures as CL functions:
  (mapc #'init-scheme-proc *scheme-procs*)
  ;; Define the boolean `constants'. Unfortunately, this won't 
  ;; stop someone from saying: (set! t nil)
  (set-global-var! t t)
  (set-global-var! nil nil))

(defun scheme:scheme (&optional x)
  "A Scheme read-eval-print loop (using interp)"
  ;; Modified by norvig Jun 11 96 to handle optional argument
  ;; instead of always going into a loop.
  (init-scheme-interp)
  (cl:if x
         (interp x nil)
         (loop (format t "~&==> ")
               (cl:print (interp (cl:read) nil)))))

; (scheme:scheme)

; (in-package :scheme)
; (scheme)
