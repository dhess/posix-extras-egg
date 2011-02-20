;;;; posix-extras-test.scm
;;;
;;; Tests for the posix-extras egg.
;;;
;;; Copyright (c) 2011 Drew Hess <dhess-src@bothan.net>.
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

(use files posix posix-extras shell srfi-12)
(use test)

;; In certain tmpdir schemes (e.g., Mac OS X), current-directory may
;; return a different path than the path used to change the working
;; directory. One way to be consistent is to change to the directory
;; and then call current-directory.
;;
;; Note: don't use with-current-directory or with-current-directory*
;; here because we can't be sure they actually work. These are tests,
;; after all!

(define (get-actual-dirname path)
  (let ((cd (current-directory)))
    (change-directory path)
    (let ((dirname (current-directory)))
      (change-directory cd)
      dirname)))

;; unsetenv is broken in Chicken 3.4.0 and earlier on Mac OS X.

(define (working-unsetenv?)
  (let ((envvar
         (string-append "PE_TEST_" (number->string (random 1000000)))))
    (setenv envvar "no")
    (unsetenv envvar)
    (equal? #f (getenv envvar))))

(if (= 0 (current-user-id))
    (print
     "Skipping tests, don't run these as the root user")
    (begin
      (test-group "create-temporary-directory"
        (let ((tmpdir (create-temporary-directory)))
          (test-assert "created?" (directory? tmpdir))
          (test "has secure permissions?"
                ;; ugh, file-permissions returns non-perm bits, too
                (let ((s_ifdir 16384))
                  (bitwise-ior perm/irwxu s_ifdir))
                (file-permissions tmpdir))
          (let ((tmpdir2 (create-temporary-directory tmpdir)))
            (test "works with optional parentdir parameter?"
                  tmpdir
                  (pathname-directory tmpdir2))
            (if (directory? tmpdir2)
                (delete-directory tmpdir2)))
          (if (directory? tmpdir)
              (delete-directory tmpdir))))

      (test-group "create-temporary-directory, TMPDIR and /tmp"
        (define (env-tmpdir-test env-tmpdir)
          (let ((tmpdir (create-temporary-directory)))
            (test "uses TMPDIR environment variable?"
                  (get-actual-dirname env-tmpdir)
                  (get-actual-dirname (pathname-directory tmpdir)))
            (if (directory? tmpdir)
                (delete-directory tmpdir))))
        (define (tmp-test)
          (let ((tmpdir (create-temporary-directory)))
            (test "uses /tmp when TMPDIR environment variable not set?"
                  (get-actual-dirname "/tmp")
                  (get-actual-dirname (pathname-directory tmpdir)))
            (if (directory? tmpdir)
                (delete-directory tmpdir))))
        (let ((env-tmpdir (getenv "TMPDIR")))
          (if env-tmpdir
              (begin
                (env-tmpdir-test env-tmpdir)
                (if (working-unsetenv?)
                    (begin
                      (unsetenv "TMPDIR")
                      (tmp-test)
                      (setenv "TMPDIR" env-tmpdir))
                    (print
                     "skipping /tmp test, unsetenv is broken on this platform")))
              (begin
                (tmp-test)
                (let ((tmp-env-tmpdir (create-temporary-directory)))
                  (setenv "TMPDIR" tmp-env-tmpdir)
                  (env-tmpdir-test tmp-env-tmpdir)
                  (if (directory? tmp-env-tmpdir)
                      (delete-directory tmp-env-tmpdir))
                  (unsetenv "TMPDIR"))))))

      (define (touch filename)
        (run (touch ,filename)))

      (test-group "create-temporary-directory, error-handling"
        (let* ((tmpdir (create-temporary-directory))
               (tmpfile (make-pathname tmpdir "foobar")))
          (touch tmpfile)
          (test-error "signals an error on invalid parentdir?"
                      (create-temporary-directory tmpfile))
          (let ((orig-mode (file-permissions tmpdir)))
            (change-file-mode tmpdir perm/irusr)
            (test-error "signals an error on permission denied?"
                        (create-temporary-directory tmpdir))
            (change-file-mode tmpdir orig-mode))
          (delete-file tmpfile)
          (delete-directory tmpdir)))

      (test-group "delete-path*"
        (begin
          (let ((tmpdir (create-temporary-directory)))
            (let* ((subdir (make-pathname tmpdir "subdir"))
                   (empty-subdir (make-pathname tmpdir "empty-subdir"))
                   (dotdir (make-pathname tmpdir ".dotdir")))
              (let ((dirs (list subdir empty-subdir dotdir))
                    (files (list (make-pathname tmpdir "file1")
                                 (make-pathname tmpdir "file2")
                                 (make-pathname tmpdir ".dotfile")
                                 (make-pathname subdir "file3")
                                 (make-pathname dotdir "file4")
                                 (make-pathname dotdir "file5")))
                    (symlinks (list (cons (make-pathname tmpdir "file1")
                                          (make-pathname tmpdir "symlink")))))
                ;; create subdirs, files and symlinks
                (map create-directory dirs)
                (map touch files)
                (map (lambda (pair)
                       (create-symbolic-link (car pair) (cdr pair)))
                     symlinks)
                ;; now delete them all!
                (delete-path* tmpdir)
                (test-assert "recursively delete directory?"
                             (not (file-exists? tmpdir))))))
          (let ((tmpdir (create-temporary-directory)))
            (delete-path* tmpdir)
            (test-assert "delete empty directory?"
                         (not (file-exists? tmpdir))))
          (let ((tmpfile (create-temporary-file)))
            (delete-path* tmpfile)
            (test-assert "delete file?"
                         (not (file-exists? tmpfile))))))

      (test-group "with-curent-directory"
        (let ((startdir (current-directory))
              (tmpdir (create-temporary-directory)))
          (let ((actual-dirname (get-actual-dirname tmpdir)))
            (let ((cd
                   (with-current-directory tmpdir
                     (lambda () (current-directory)))))
              (test "changes working directory?" actual-dirname cd))
            (test "restores previous working directory?"
                  startdir
                  (current-directory))
            (let-values (((u v)
                          (with-current-directory tmpdir
                            (lambda () (values 'one 'two)))))
              (test-assert "returns multiple values from thunk?"
                           (and (eq? u 'one)
                                (eq? v 'two))))
            (delete-directory tmpdir))))

      (test-group "with-current-directory, continuations and exceptions"
        (let ((startdir (current-directory))
              (tmpdir (create-temporary-directory)))
          (call/cc
           (lambda (break)
             (with-current-directory tmpdir
               (lambda () (break 0)))))
          (test
           "continuation out of thunk restores previous working directory?"
           startdir
           (current-directory))
          (test-error "this test will signal an error"
                      (with-current-directory tmpdir
                        (lambda () (error "foo"))))
          (test "exception inside thunk restores previous working directory?"
                startdir (current-directory))
          (delete-directory tmpdir)))

      (test-group "with-curent-directory*"
        (let ((startdir (current-directory))
              (tmpdir (create-temporary-directory)))
          (let ((actual-dirname (get-actual-dirname tmpdir)))
            (let ((cd
                   (with-current-directory* tmpdir (current-directory))))
              (test "changes working directory?" actual-dirname cd))
            (test "restores previous working directory?"
                  startdir
                  (current-directory))
            (let-values (((u v)
                          (with-current-directory* tmpdir
                            1 ;dummy values
                            2
                            (values 'one 'two))))
              (test-assert "returns multiple values from body?"
                           (and (eq? u 'one)
                                (eq? v 'two))))
            (delete-directory tmpdir))))

      (test-group "with-current-directory*, continuations and exceptions"
        (let ((startdir (current-directory))
              (tmpdir (create-temporary-directory)))
          (call/cc
           (lambda (break)
             (with-current-directory* tmpdir (break 0))))
          (test "continuation out of body restores previous working directory?"
                startdir (current-directory))
          (test-error "this test will signal an error"
                      (with-current-directory* tmpdir (error "foo")))
          (test "exception inside body restores previous working directory?"
                startdir (current-directory))
          (delete-directory tmpdir)))

      (test-group "with-temporary-directory"
        (let ((startdir (current-directory)))
          (let ((tmpdir
                 (with-temporary-directory
                   (lambda ()
                     (begin
                       ;; create a file and a dir in the temporary dir
                       (touch "testfile")
                       (create-directory "foobar")
                       (touch ".this-is-a-dotfile")
                       (current-directory))))))
            (test-assert "changes working directory?"
                         (not (equal? startdir tmpdir)))
            (test "restores previous working directory?"
                  startdir (current-directory))
            (test-assert "deletes temporary directory and its contents?"
                         (not (file-exists? tmpdir))))
          (let-values (((u v)
                        (with-temporary-directory
                          (lambda () (values 'u 'v)))))
            (test-assert "returns multiple values from thunk?"
                         (and (eq? u 'u)
                              (eq? v 'v))))))

      (test-group "with-temporary-directory, continuations and exceptions"
        (let* ((startdir (current-directory))
               (tmpdir (call/cc
                        (lambda (break)
                          (begin 
                            (with-temporary-directory
                              (lambda ()
                                (begin
                                  (touch "testfile")              
                                  (break (current-directory))))))))))
          (test "continuation out of thunk restores previous working directory?"
                startdir (current-directory))
          (test-assert
           "continuation out of thunk deletes temp directory and its contents?"
           (not (file-exists? tmpdir)))
          (let ((tmpdir #f))
            (test-error "this test will signal an error"
                        (with-temporary-directory
                          (lambda ()
                            (begin
                              (set! tmpdir (current-directory))
                              (create-directory "blah")
                              (error "foo")))))
            (test "exception inside thunk restores previous working directory?"
                  startdir (current-directory))
            (test-assert
             "exception inside thunk deletes temp directory and its contents?"
             (not (file-exists? tmpdir))))))

      (test-group "with-temporary-directory*"
        (let ((startdir (current-directory)))
          (let ((tmpdir
                 (with-temporary-directory*
                   (let ((cwd (current-directory)))
                     ;; create a file and a dir in the temporary dir
                     (touch "testfile")
                     (create-directory "foobar")
                     (touch ".this-is-a-dotfile")
                     (current-directory)))))
            (test-assert "changes working directory?"
                         (not (equal? startdir tmpdir)))
            (test "restores previous working directory?"
                  startdir (current-directory))
            (test-assert "deletes temporary directory and its contents?"
                         (not (file-exists? tmpdir))))
          (let-values (((u v)
                        (with-temporary-directory*
                          (values 'u 'v))))
            (test-assert "returns multiple values from body?"
                         (and (eq? u 'u)
                              (eq? v 'v))))))

      (test-group "with-temporary-directory*, continuations and exceptions"
        (let ((startdir (current-directory))
              (tmpdir (call/cc
                       (lambda (break)
                         (with-temporary-directory*
                           (touch "testfile")              
                           (break (current-directory)))))))
          (test "continuation out of body restores previous working directory?"
                startdir (current-directory))
          (test-assert
           "continuation out of body deletes temp directory and its contents?"
           (not (file-exists? tmpdir)))
          (let ((tmpdir #f))
            (test-error "this test will signal an error"
                        (with-temporary-directory*
                          (set! tmpdir (current-directory))
                          (create-directory "blah")
                          (error "foo")))
            (test "exception inside body restores previous working directory?"
                  startdir (current-directory))
            (test-assert
             "exception inside body deletes temp directory and its contents?"
             (not (file-exists? tmpdir))))))))

(test-exit)
