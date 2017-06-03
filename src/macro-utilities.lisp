(in-package #:net.goenninger.lib.cellar)

(defun spliceable (value)
  (if value (list value)))

(defmacro ppme (form &environment env)
  (progn
    (write (macroexpand-1 form env)
           :length nil
           :level nil
           :circle nil
           :pretty t
           :gensym nil
           :right-margin 83
           :case :downcase)
    nil))
