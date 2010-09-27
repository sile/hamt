(defpackage hamt
  (:use :common-lisp)
  (:shadow :common-lisp get remove mapc)
  (:export hamt
           make
           entry-count
           get
           remove
           map
           mapc))
(in-package :hamt)

(defvar *fastest* '(optimize (speed 3) (safety 0) (debug 0)))
(defvar *interface* '(optimize (speed 3) (safety 1) (debug 1)))

(defconstant +FIXNUM-LENGTH+ (integer-length most-positive-fixnum))
(defconstant +PER-ARC-BIT-LENGTH+ 5)
(defconstant +BITMAP-SIZE+ 32)

(deftype positive-fixnum () '(integer 0 #.most-positive-fixnum))
(deftype fixnum-length () `(integer 0 ,+FIXNUM-LENGTH+))
(deftype arc () `(mod ,+BITMAP-SIZE+))
(deftype arc-start () `(integer 0 ,(+ +FIXNUM-LENGTH+ +PER-ARC-BIT-LENGTH+)))
(deftype bitmap () `(unsigned-byte ,+BITMAP-SIZE+))
(deftype bitmap-length () `(integer 0 ,+BITMAP-SIZE+))

