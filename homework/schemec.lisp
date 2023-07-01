(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :interp1))

(defpackage scheme
  (:export #:call/cc #:call-with-current-continuation))

(in-package :scheme)

(defun interp (x env cc)
  "Evaluate the expression x in the environment env,
   and pass the result to the continuation cc."
  (cond
   ((symbolp x) (funcall cc (get-var x env)))
   ((atom x)    (funcall cc x))
   ((scheme:macro (first x))
    (interp (scheme:macro-expand x) env cc))
   ((case (first x)
      (cl:quote     (funcall cc (second x)))
      (scheme:begin (interp-begin (rest x) env cc))
      (scheme:set!  (interp (third x) env
                            #'(cl:lambda (val)
                                (funcall cc (set-var! (second x)
                                                      val env)))))
      (scheme:if    (interp (second x) env
                            #'(cl:lambda (pred)
                                (interp (cl:if pred (third x) (fourth x))
                                        env cc))))
      (scheme:lambda (let ((parms (second x))
                           (code (maybe-add 'scheme:begin (rest2 x))))
                       (funcall
                        cc
                        #'(cl:lambda (cont &rest args)
                            (interp code
                                    (extend-env parms args env)
                                    cont)))))
      (otherwise (interp-call x env cc))))))

(defun interp-call (call env cc)
  "Interpret the call (f x...) and pass the result to CC."
  (map-interp call env
              #'(cl:lambda (fn-and-args)
                  (apply (first fn-and-args)
                         cc
                         (rest fn-and-args)))))

(defun map-interp (list env cc)
  "Interpret each element of LIST, and pass the list to CC."
  (cl:if (null list)
         (funcall cc nil)
         (interp (first list) env
                 #'(cl:lambda (x)
                     (map-interp (rest list) env
                                 #'(cl:lambda (y)
                                     (funcall cc (cons x y))))))))

(defun interp-begin (body env cc)
  "Interpret each element of BODY, passing the last to CC."
  (interp (first body) env
          #'(cl:lambda (val)
              (cl:if (null (rest body))
                     (funcall cc val) ;; fix, hsuc 2/20/93; forgot to call cc
                     (interp-begin (rest body) env cc)))))

(defun scheme:scheme (&optional x)
  "A Scheme read-eval-print loop (using interp).
   Handles call/cc by explicitly passing continuations."
  ;; Modified by norvig Jun 11 96 to handle optional argument
  ;; instead of always going into a loop.
  (init-scheme-interp)
  (cl:if x
      (interp x nil #'cl:print)
    (loop (format t "~&==> ")
      (interp (cl:read) nil #'cl:print))))

(defun init-scheme-proc (f)
  "Define a Scheme primitive procedure as a CL function."
  (cl:if (listp f)
      (set-global-var! (first f)
                       #'(cl:lambda (cont &rest args)
                           (funcall cont (apply (second f) args))))
    (init-scheme-proc (list f f))))

(defun call/cc (cc computation)
  "Make the continuation accessible to a Scheme procedure."
  (funcall computation cc
           ;; Package up CC into a Scheme function:
           #'(cl:lambda (cont val)
               (declare (ignore cont))
               (funcall cc val))))

;; Now install call/cc in the global environment
(set-global-var! 'scheme:call/cc #'call/cc)
(set-global-var! 'scheme:call-with-current-continuation #'call/cc)

; 以上は継続版

