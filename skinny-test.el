;;; skinny-test.el --- Skinny tests -*- lexical-binding: t -*-

;; Copyright (C) 2013  Nic Ferrier, Aidan Gauland

;; Author: Nic Ferrier <nferrier@ferrier.me.uk> and Aidan Gauland <aidalgol@amuri.net>
;; Keywords: hypermedia, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ERT tests for Skinny.

;;; Code:

(require 'ert)
(require 'skinny)

(defmacro skinny-test/with-test-site (&rest body)
  "Evaluate BODY with a test Skinny site in /tmp/.

`skinny-root' is rebound to a path in /tmp/.  `post-basenames' is
bound to a list of strings foo, bar, baz, and goo, from which
corresponding .creole files are created, in that order, in
`skinny-blog-dir'."
  `(let* ((skinny-root "/tmp/skinny-test-root/")
          (post-basenames '("foo" "bar" "baz" "goo")))
     (unwind-protect
         (progn
           (make-directory (concat skinny-root skinny-blog-dir) t)
           (dolist (post-name post-basenames)
             (with-temp-file (concat skinny-root skinny-blog-dir post-name ".creole")
               (insert post-name)))
           (cl-map nil
                   (lambda (post-name timestamp)
                     (with-temp-file (concat skinny-root skinny-blog-dir post-name ".sexp")
                       (prin1 `((title . ,post-name)
                                (summary . ,(concat "Dummy post " post-name))
                                (timestamp . ,timestamp))
                              (current-buffer))))
                   post-basenames
                   '("2013-05-13T09:38:10Z" "2013-05-13T09:38:18Z" "2013-05-13T09:38:21Z" "2013-05-13T09:38:27Z"))
           ,@body)
       ;; Path hard-coded here in case caller changes `skinny-root' in BODY.
       (delete-directory "/tmp/skinny-test-root/" t))))

(ert-deftest skinny/post-meta-data ()
  (let* ((metadata-string "((title . \"Lorem ipsum\")(summary . \"Lorem ipsum dolor sit amet, consectetuer adipiscing elit.\")(timestamp . \"2013-05-12T02:54:39Z\")(uuid . \"deadbeef-dead-beef-dead-beefdeadbeef\"))")
         (metadata (read metadata-string)))
   (fakir-mock-file
    (fakir-file
     :filename "post.sexp"
     :directory "/path/to/blog/"
     :content metadata-string)
    (should (equal metadata
                   (skinny/post-meta-data "post.html")))
    (should (equal metadata
                   (skinny/post-meta-data "post.creole"))))))

(ert-deftest skinny/list-posts ()
  (skinny-test/with-test-site
   (should (equal
            (reverse
             (mapcar
              (lambda (post)
                (concat skinny-root skinny-blog-dir
                        post "." (symbol-name skinny-post-format)))
              post-basenames))
            (skinny/list-posts)))))

(ert-deftest skinny/posts-html-list ()
  (skinny-test/with-test-site
   (should (equal
            `(ul ()
              ,@(mapcar
                 (lambda (post)
                   `(li ()
                      (a ((href . ,(concat skinny-blog-dir post)))
                         ,post)))
                 (reverse post-basenames)))
            (skinny/posts-html-list)))))

;;; skinny-test.el ends here
