;;;; posix-extras-base.scm
;;;
;;; Runtime support for the posix-extras egg.
;;;
;;; Copyright (c) 2009 Drew Hess.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.

(use extras files posix)

(declare
 (fixnum-arithmetic)
 (export create-temporary-directory
         delete-path*
         with-current-directory
         with-temporary-directory))

(define mkdtemp
  (foreign-lambda c-string mkdtemp c-string))

;;; Probably doesn't work on Windows. Sorry.

(define (create-temporary-directory #!optional parentdir)
  ;; mkdtemp() on GNU/Linux requires at least 6 X's at the end of the
  ;; template string. On Mac OS X it's less strict but uses the same
  ;; pattern.
  (let ((pd
         (if parentdir
             parentdir
             (let ((env-tmpdir (getenv "TMPDIR")))
               (if env-tmpdir
                   env-tmpdir
                   "/tmp")))))
   (let ((result (mkdtemp (canonical-path
                            (string-append pd "/peXXXXXX")))))
      (if (not result)
          (##sys#posix-error)
          result))))
        
(define (delete-path* pathname)
  (if (directory? pathname)
      (let ((full-paths (map (lambda (basename) (pathname-replace-directory
                                            basename
                                            pathname))
                             (directory pathname #t))))
        (map delete-path* full-paths)
        (delete-directory pathname))
      (delete-file pathname)))

(define (with-current-directory dirname thunk)
  (let ((cwd (current-directory)))
    (dynamic-wind
      (lambda () (change-directory dirname))
      thunk
      (lambda () (change-directory cwd)))))

(define (with-temporary-directory thunk)
  (let ((cwd (current-directory))
        (tmpdir (create-temporary-directory)))
    (dynamic-wind
      (lambda () (change-directory tmpdir))
      thunk
      (lambda ()
        (begin
          (change-directory cwd)
          (delete-path* tmpdir))))))
