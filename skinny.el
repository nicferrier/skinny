;;; skinny.el --- a blog engine with elnode -*- lexical-binding: t -*-

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: hypermedia
;; Version: 0.0.6
;; Package-Requires: ((elnode "0.9.9.6.1")(creole "0.8.17"))

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

;; Skinny is good for hipsters. You can write blog posts in creole and
;; serve them as HTML.

;;; Code:

(elnode-app skinny-dir noflet creole)

(defgroup skinny nil
  "A blog engine written with Elnode. Good for hipsters."
  :group 'applications)

(defcustom skinny-root ""
  "The directory used to store the skinny docroot."
  :group 'skinny
  :type 'directory)

(defcustom skinny-paste-root ""
  "The directory used to store the skinny pastes."
  :group 'skinny
  :type 'directory)

(defcustom skinny-port 8090
  "The TCP port to start talking hipster shite on."
  :group 'skinny
  :type 'integer)

(defcustom skinny-host "*"
  "The interface to start talking hipster shite on."
  :group 'skinny
  :type 'string)

(defcustom skinny-sort-posts-by-filename nil
  "Sort blog posts by filename instead of by mtime.
If you enable this option, files that match the following
pattern:

  <YEAR>_<MONTH>/<DAY>-<rest-of-file-name>

will be sorted by date by skinny/list-published.  Any files
that don't match that pattern are sorted below files with a date,
and their order is undefined.

For example, if you have the following files:

  2015_01/22-stuff.creole
  2015_02/05-more-stuff.creole
  2015_02/07-newest.creolen
  project/random-post.creole

skinny/list-published will return:

  (random-post.creole 22-stuff.creole 05-more-stuff.creole 07-newest.creole)

Regardless of mtimes of the files."
  :group 'skinny
  :type 'boolean)

(defun skinny-page (httpcon)
  (let ((css (concat skinny-root "/stuff/css/site.css"))
        (body-header (concat skinny-root "/template/headerhtml"))
        (body-footer (concat skinny-root "/template/footerhtml"))
        (skinny-docroot (concat skinny-root "/blog"))
        (creole-image-class "creole"))
    (elnode-docroot-for skinny-docroot
        with page
        on httpcon
        do
        (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
        (with-stdout-to-elnode httpcon
            (creole-wiki page
             :destination t
             :docroot skinny-root
             :css (list css)
             :body-header body-header
             :body-footer body-footer)))))

(defun skinny-paste (httpcon)
  "Serve a paste."
  (let ((css (concat skinny-root "/stuff/css/site.css"))
        (body-header (concat skinny-root "/template/headerhtml"))
        (body-footer (concat skinny-root "/template/footerhtml"))
        (skinny-pasteroot (concat skinny-paste-root "/pastes"))
        (targetfile (elnode-http-mapping httpcon 1))
        (creole-image-class "creole"))
    (noflet ((elnode-http-mapping (httpcon which)
               (concat targetfile ".creole")))
      (elnode-docroot-for skinny-pasteroot
          with page
          on httpcon
          do
          (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
          (with-stdout-to-elnode httpcon
              (creole-wiki page
               :destination t
               :docroot skinny-root
               :css (list css)
               :body-header "<div id=\"header\">"
               :body-footer "</div>"))))))

(defun skinny-redirector (httpcon)
  "Redirect non creole pages to creole."
  (let ((targetfile (elnode-http-mapping httpcon 1)))
    (noflet ((elnode-http-mapping (httpcon which)
               (concat targetfile ".creole")))
      (skinny-page httpcon))))

(defun skinny/directory-files (directory &rest excludes)
  "List DIRECTORY entries that do not match one of EXCLUDES."
  (loop for e in (directory-files directory t)
     unless (loop for re in excludes
                 if (string-match re e)
                 return t)
     collect e))

(defun skinny/directory-p (name)
  "Is NAME a directory?"
  (eq t (elt (file-attributes name) 0)))

(defun skinny/list-published ()
  "Produce the list of published files.

Published files are those not in the `drafts' folder."
  (let* ((excludes (list ".*/\\.*#.*"
                         ".*~"
                         ".*/drafts\\(/.*\\)*"
                         ".*/\\.\\{1,2\\}\\'"))
         (files (loop for entry in
                     (apply 'skinny/directory-files
                            (concat skinny-root "/blog") excludes)
                   if (skinny/directory-p entry)
                   append (apply 'skinny/directory-files
                                 entry excludes))))
    (sort files
          (if skinny-sort-posts-by-filename
              #'skinny/date-earlier-p
            (lambda (a b)
              (time-less-p
               (elt (file-attributes a) 5)
               (elt (file-attributes b) 5)))))))

(defun skinny/date-earlier-p (file-a file-b)
  "Return non-nil if FILE-A's date is earlier than FILE-B's date.
Files with dates must match the following pattern:

  <YEAR>_<MONTH>/<DAY>-<rest-of-file-name>

Any filename that doesn't match the pattern is considered to not
have a date. A file without a date is considered earlier than a
file with a date."
  (let* ((to-date
          (lambda (file)
            (let (our-name our-match)
              (setq our-name (replace-regexp-in-string
                              (concat "^" (expand-file-name skinny-root) "/blog/")
                              ""
                              file))
              (setq our-match (string-match
                               "^\\([[:digit:]]\\{4\\}\\)_\\([[:digit:]]\\{2\\}\\)/\\([[:digit:]]\\{2\\}\\)-"
                               our-name))
              (if our-match
                  (+ (* 10000 (string-to-number (match-string 1 our-name)))
                     (* 100 (string-to-number (match-string 2 our-name)))
                     (string-to-number (match-string 3 our-name)))
                0))))
         (sum-a (funcall to-date file-a)))
    (or (= sum-a 0) (< sum-a (funcall to-date file-b)))))

(defun skinny-homepage (httpcon)
  "The homepage.

Finds the latest published post and makes that the page."
  (let* ((top (car (last (skinny/list-published))))
         (root-re (concat
                   (file-name-as-directory (expand-file-name skinny-root))
                   "blog/\\(.*\\)"))
         (top-path (progn
                     (string-match root-re top)
                     (concat "/" (match-string 1 top))))
         (targetfile (elnode-http-mapping httpcon 1)))
    (noflet ((elnode-http-mapping (httpcon which)
               top-path))
      (skinny-page httpcon))))

(defun skinny-feed (httpcon)
  "For now a dummy feed."
  (elnode-http-start httpcon 200 '("Content-type" . "application/xml"))
  (elnode-http-return httpcon "<?xml version='1.0'?>"))

(defun skinny-index (httpcon)
  (let ((css (concat skinny-root "/stuff/css/site.css")))
    (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
    (with-stdout-to-elnode httpcon
        (creole-wiki (concat skinny-root "/indexes/index.creole")
         :destination t
         :docroot skinny-root
         :css (list css)))))

(defun skinny-router (httpcon)
  "Skinny the blog engine's url router."
  (let ((webserver
         (elnode-webserver-handler-maker
          (concat skinny-root "/stuff")))
        (favicon-sender
         (elnode-make-send-file
          (concat
           (expand-file-name skinny-root)
           "/stuff/ico/favicon.ico"))))
    (elnode-hostpath-dispatcher
     httpcon
     `(("^[^/]+//blog/feed.xml$" . skinny-feed)
       ("^[^/]+//blog/\\(.*\\.creole\\)" . skinny-page)
       ("^[^/]+//blog/\\(.*\\)" . skinny-redirector)
       ("^[^/]+//stuff/\\(.*\\)" . ,webserver)
       ("^[^/]+//index$" . skinny-index)
       ;; Deal with the favicon
       ("^[^/]+//favicon.ico" . ,favicon-sender)
       ("^[^/]+//pastes/\\(.*\\)$" . skinny-paste)
       ("^[^/]+//$" . skinny-homepage)))))

;;;###autoload
(defun skinny-start ()
  (interactive)
  (elnode-start 'skinny-router :port skinny-port :host skinny-host))

;;;###autoload
(defun skinny-stop ()
  (interactive)
  (elnode-stop skinny-port))

(provide 'skinny)

;;; skinny.el ends here
