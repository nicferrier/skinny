;;; skinny.el --- a blog engine with elnode -*- lexical-binding: t -*-

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: hypermedia
;; Version: 0.0.4
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

(elnode-app skinny-dir
  creole esxml esxml-html)

(defgroup skinny nil
  "A blog engine written with Elnode. Good for hipsters."
  :group 'applications)

(defcustom skinny-port 8090
  "The TCP port to start talking hipster shite on."
  :type '(integer)
  :group 'skinny)

(defcustom skinny-host "localhost"
  "The interface to start talking hipster shite on."
  :type '(string)
  :group 'skinny)

(defcustom skinny-blog-name "skinny"
  "The name of the blog."
  :type '(string)
  :group 'skinny)

(defcustom skinny-blog-author nil
  "The blog author."
  :type '(string)
  :group 'skinny)

(defcustom skinny-post-format 'creole
  "Which markup language the posts are written in.

Skinny can render blog posts written in WikiCreole, or just
insert literal HTML into the post template.  The file extention
for blog posts must match the value of this variable.  For
Creole, set this to 'creole; for HTML, set this to 'html.")

(defcustom skinny-lang "en"
  "The language code for the blog.

The code for the language in which the blog is written.  This is
used as the value for the <html> lang attribute, and the <feed>
xml:lang attribute.  See the W3C article on language tags for
available language codes:
http://www.w3.org/International/articles/language-tags/"
  :type '(string)
  :group 'skinny)

(defcustom skinny-blog-css-file-names '("blog.css")
  "The names of the CSS files to use for blog posts."
  :type '(repeat file)
  :group 'skinny)

(defconst skinny-blog-includes-dir "includes/"
  "The directory for the blog include files.

The includes files are the top, header, footer, and
bottom (`skinny-blog-top-file-name',
`skinny-blog-header-file-name', `skinny-blog-footer-file-name',
and `skinny-blog-bottom-file-name').

Must be an immediate subdirectory of `skinny-root'.")

(defconst skinny-blog-top-file-name "top.html"
  "The name of the blog top file.

This file is inserted at the top of the blog post page, before
the <article> element.

This file is searched for in `skinny-blog-includes-dir'.")

(defconst skinny-blog-header-file-name "header.html"
  "The name of the blog header file.

This file is inserted at the end of the blog post page <header>
element.

This file is searched for in `skinny-blog-includes-dir'.")

(defconst skinny-blog-footer-file-name "footer.html"
  "The name of the blog footer file.

This file is inserted at the end of the blog post page <footer>
element.

This file is searched for in `skinny-blog-includes-dir'.")

(defconst skinny-blog-bottom-file-name "bottom.html"
  "The name of the blog bottom file.

This file is inserted at the end of the blog post page, after the
<article> element.

This file is searched for in `skinny-blog-includes-dir'.")

(defcustom skinny-blog-article-class nil
  "<article> class attribute.

If set, use as the value of the class attribute for the blog post
<article>."
  :type '(string)
  :group 'skinny)

(defgroup skinny-dirs nil
  "Various directories for the Skinny blog.

All paths are relative to `skinny-root'."
  :group 'skinny)

(defcustom skinny-root skinny-dir
  "The root directory of the Skinny site.

By default, this is the directory from which Skinny was loaded.
Blog posts are in a subdirectory, specified by `skinny-blog-dir'."
  :type '(directory)
  :group 'skinny-dirs)

(defcustom skinny-blog-dir "blog/"
  "The directory for blog posts.

Must be an immediate subdirectory of `skinny-root'."
  :type '(directory)
  :group 'skinny-dirs)

(defcustom skinny-css-dir "css/"
  "The directory for CSS files.

Must be an immediate subdirectory of `skinny-root'."
  :type '(directory)
  :group 'skinny-dirs)

(defcustom skinny-image-dir "images/"
  "The directory for images.

Must be an immediate subdirectory of `skinny-root'."
  :type '(directory)
  :group 'skinny-dirs)

(defun skinny/post-meta-data (post)
  "Return corresponding meta-data file for POST file.

Takes the file name of a blog post, and reads the corresponding
\".el\" file, which should contain only a single alist with the
following fields:

title
summary -- Used for the Atom <entry> <summary> element, and the
           \"title\" attribute for the HTML list items.
author -- Just the author name, not name then email.  If this is
          absent, then the value of `skinny-blog-author' is used
          instead.
timestamp -- RFC3339 format
uuid -- Used for the id of feed entries; see RFC4287."
  (with-temp-buffer
    (save-match-data
     (insert-file-contents
      (concat (file-name-sans-extension post) ".el")))
    (read (current-buffer))))

(defun skinny/list-posts ()
  "Produce the list of blog posts (file names), sorted by mtime.

Posts are all files in `skinny-blog-dir' with the extention
`skinny-post-format', excluding the include files."
  (sort
   (directory-files (concat skinny-root skinny-blog-dir)
                    t
                    (concat ".*\\." (symbol-name skinny-post-format) "\\'")
                    t)
   (lambda (a b)
     (time-less-p
      (elt (file-attributes a) 5)
      (elt (file-attributes b) 5)))))

(defun skinny/posts-html-list ()
  "Produce an HTML list of the posts.

Each post's title is listed, and links to the post itself.
HTML is returned as ESXML, rather than a string."
  (esxml-listify
   (mapcar
    (lambda (post)
      (let ((metadata (skinny/post-meta-data post)))
        (esxml-link
         (save-match-data
           (string-match (expand-file-name
                          (format "%s\\(%s.*\\.%s\\)"
                                  skinny-root skinny-blog-dir
                                  (symbol-name skinny-post-format)))
                         post)
           (file-name-sans-extension (match-string 1 post)))
         (cdr (assoc 'title metadata)))))
    (skinny/list-posts))))

(defun skinny-post (httpcon)
  "Return a blog post via HTTPCON.

If using creole, render it first."
  (let ((skinny-blog-dir (concat skinny-root skinny-blog-dir))
        (creole-image-class "creole")
        (targetfile (elnode-http-mapping httpcon 1)))
    (flet ((elnode-http-mapping (httpcon which)
            (concat targetfile "." (symbol-name skinny-post-format)))
           (insert-file-if-exists (file)
            (when (file-exists-p
                   (concat skinny-blog-dir skinny-blog-includes-dir
                           file))
              `(,(with-temp-buffer
                   (save-match-data
                     (insert-file-contents
                      (concat skinny-blog-dir skinny-blog-includes-dir
                              file))
                     (buffer-string)))))))
      (elnode-docroot-for skinny-blog-dir
        with post
        on httpcon
        do
        (elnode-error "Sending blog post: %s" post)
        (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
        (elnode-http-send-string httpcon
          (let ((metadata (skinny/post-meta-data post)))
            (pp-esxml-to-xml
             `(html ((lang . ,skinny-lang))
                ,(append
                  (esxml-head (cdr (assoc 'title metadata))
                    '(meta ((charset . "UTF-8")))
                    (meta 'author
                          (if (assoc 'author metadata)
                              (cdr (assoc 'author metadata))
                            skinny-blog-author))
                    (base "../")
                   (link 'alternate "application/atom+xml" "feed.xml"
                         '((title . "site feed"))))
                 (mapcar
                  (lambda (css)
                    (esxml-head-css-link (concat skinny-css-dir css)))
                  skinny-blog-css-file-names))
               (body ()
                 ,@(insert-file-if-exists skinny-blog-top-file-name)
                 (article ,(when skinny-blog-article-class
                             `((class . ,skinny-blog-article-class)))
                   (header ()
                     ,(cdr (assoc 'title metadata))
                     (br ())
                     ,(let ((timestamp (cdr (assoc 'timestamp metadata))))
                       `(time ((datetime . ,timestamp))
                          ,timestamp))
                     ,@(when (file-exists-p
                              (concat skinny-blog-dir skinny-blog-includes-dir
                                      skinny-blog-header-file-name))
                         `((br ())
                           ,(with-temp-buffer
                              (save-match-data
                                (insert-file-contents
                                 (concat skinny-blog-dir skinny-blog-includes-dir
                                         skinny-blog-header-file-name))
                                (buffer-string))))))
                   ,(with-temp-buffer
                      (save-match-data
                        (insert-file-contents post))
                      (if (eq 'creole skinny-post-format)
                          (with-current-buffer
                              (creole-html (current-buffer) nil
                                           :do-font-lock t)
                            (buffer-string))
                        (buffer-string)))
                   ,@(when (file-exists-p
                            (concat skinny-blog-dir skinny-blog-includes-dir
                                    skinny-blog-footer-file-name))
                       `((footer ()
                           ,(with-temp-buffer
                              (save-match-data
                                (insert-file-contents
                                 (concat skinny-blog-dir skinny-blog-includes-dir
                                         skinny-blog-footer-file-name))
                                (buffer-string)))))))
                 ,@(insert-file-if-exists skinny-blog-bottom-file-name))))))
       (elnode-http-return httpcon)))))

(defun skinny-index-page (httpcon)
  "Return the index page via HTTPCON."
  (let ((page (concat skinny-root "index.html")))
    (elnode-error "Sending index page.")
    (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
    (elnode-http-send-string httpcon
     (with-temp-buffer
       (save-match-data
         (insert-file-contents page)
        (while (search-forward "<!--{{{posts}}}-->" nil t)
          (replace-match (esxml-to-xml (skinny/posts-html-list)) nil t)))
       (buffer-string)))
    (elnode-http-return httpcon)))

(defun skinny/feed ()
  "Generate an Atom feed from the most recent posts."
  (let* ((posts (skinny/list-posts))
         (last-post-metadata (skinny/post-meta-data
                              (car posts))))
    (concat "<?xml version=\"1.0\"?>"
      (pp-esxml-to-xml
        `(feed ((xmlns . "http://www.w3.org/2005/Atom")
                (xml:lang . ,skinny-lang))
           ;; Feed metadata.
           (title () ,skinny-blog-name)
           (link ((href . "FIXME: absolute feed URL")
                  (rel . "self")))
           (link ((href . "./")))
           (id () ,(concat "urn:uuid:"
                     (cdr (assoc 'uuid
                                 last-post-metadata))))
           (updated () ,(cdr (assoc 'timestamp
                                    last-post-metadata)))
           ,@(when skinny-blog-author
               `((author ()
                   (name () ,skinny-blog-author))))
           ;; Now for the entries.
           ,@(mapcar
              (lambda (post)
                (let ((metadata (skinny/post-meta-data post)))
                  `(entry ()
                     (title () ,(cdr (assoc 'title metadata)))
                     ,(if (assoc 'author metadata)
                          `(author () ,(cdr (assoc 'author metadata)))
                        `(author ()
                           (name () ,skinny-blog-author)))
                     (link ((href . ,(file-name-sans-extension
                                      (file-name-nondirectory post)))))
                     (id () ,(concat "urn:uuid:"
                                     (cdr (assoc 'uuid metadata))))
                     (updated () ,(cdr (assoc 'timestamp metadata)))
                     (summary ((type . "xhtml"))
                       (div ((xmlns . "http://www.w3.org/1999/xhtml"))
                            ,(cdr (assoc 'summary metadata)))))))
              posts))))))

(defun skinny-feed (httpcon)
  "Return a blog feed via HTTPCON.

Calls `skinny/feed' to generate the feed."
  (elnode-http-start httpcon 200 '("Content-type" . "application/xml"))
  (elnode-http-return httpcon (skinny/feed)))

(defun skinny-router (httpcon)
  "Skinny the blog engine's URL router."
  (let ((webserver
         (elnode-webserver-handler-maker
          skinny-root)))
    (elnode-hostpath-dispatcher
     httpcon
     `((,(format "^[^/]+//%sfeed.xml$" skinny-blog-dir) . skinny-feed)
       (,(format "^[^/]+//%s\\(.*\\)" skinny-blog-dir) . skinny-post)
       ("^[^/]+//$" . skinny-index-page)
       ("^[^/]+//\\(.*\\)" . ,webserver)))))

;;;###autoload
(defun skinny-start ()
  (interactive)
  (elnode-start 'skinny-router :port skinny-port :host skinny-host))

(provide 'skinny)

;;; skinny.el ends here
