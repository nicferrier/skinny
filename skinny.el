;;; skinny.el --- a blog engine with elnode -*- lexical-binding: t -*-

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: hypermedia

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

(elnode-app skinny-dir creole)

(defgroup skinny nil
  "A blog engine written with Elnode. Good for hipsters."
  :group 'applications)

(defcustom skinny-root ""
  "The directory used tostore the skinny docroot."
  :group 'skinny
  :type 'directory)

(defcustom skinny-port 8090
  "The TCP port to start talking hipster shite on."
  :group 'skinny
  :type 'integer)

(defun skinny-page (httpcon)
  (let ((css (concat skinny-root "/stuff/css/site.css"))
        (body-header (concat skinny-root "/template/headerhtml"))
        (body-footer (concat skinny-root "/template/footerhtml"))
        (skinny-docroot (concat skinny-root "/blog")))
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

(defun skinny-router (httpcon)
  (let ((webserver
         (elnode-webserver-handler-maker
          (concat skinny-root "/stuff"))))
    (elnode-hostpath-dispatcher
     httpcon
     `(("^[^/]+//blog/\\(.*\\)" . skinny-page)
       ("^[^/]+//stuff/\\(.*\\)" . ,webserver)))))

(defun skinny-start ()
  (interactive)
  (elnode-start 'skinny-router :port skinny-port))

(provide 'bliki)

;;; bliki.el ends here
