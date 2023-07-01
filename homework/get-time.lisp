(defun get-compiled-time ()
  (decode-universal-time #.(get-universal-time) -9))

(defun get-current-time ()
  (decode-universal-time (get-universal-time) -9))
