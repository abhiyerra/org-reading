;; org-reading.el --- Manage your reading with org-mode

;; Copyright (C) 2012  Abhi Yerra
;;
;; Author: Abhi Yerra <abhi at berkeley dot edu>
;; Keywords: org, reading
;; Homepage: https://github.com/abhiyerra/org-reading
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; This file is not part of GNU Emacs.
(eval-when-compile
  (require 'cl))

(require 'url)
(require 'json)

;; Interactive Functions

(defun org-reading-new-book-from-title (title)
  "Ask for a title and create a new book heading."
  (interactive "sTitle: ")
  (org-reading-create-heading
   (org-reading-info-request "title" title)))

(defun org-reading-new-book-from-isbn (isbn)
  "Ask for a ISBN and create a new book heading."
  (interactive "sISBN: ")
  (org-reading-create-heading
   (org-reading-info-request "isbn" isbn)))

(defun org-reading-new-books-from-region ()
  "Given a region which are a list of title separated by new
lines create new headers with the book titles."
  (interactive)
  (let (line)
    (dolist
        (element
         (split-string (buffer-substring-no-properties
                        (region-beginning) (region-end))
                       "\n")
         line)
      (org-reading-new-book-from-title element))))

(defun org-reading-start-book ()
  "Start reading a book."
  (interactive)
  (org-set-property
   "DATE_STARTED"
   (format-time-string (org-time-stamp-format)))
  (org-todo "STARTED"))

(defun org-reading-finish-book (rating)
  "Finish reading a book."
  (interactive "sRating: ")
  (org-set-property "DATE_FINISHED" (format-time-string (org-time-stamp-format)))
  (org-set-property "MY_RATING" rating)
  (org-todo "DONE"))


;; Backend Functions

(defun org-reading-info-request (type content)
  "Based on the type get the content and returns a plist."
  (let* ((query (cond ((string= type "title") (format "intitle:%s" content))
                      ((string= type "isbn") (format "isbn:%s" content))
                      (t content)))
         (results (org-reading-google-books-search-request query)))
    ;; TODO: Should ask for the correct book if we don't know which one is the real one.
    `(:googleId ,(plist-get (aref (plist-get results :items) 0) :id)
                :pageCount ,(plist-get (plist-get (aref (plist-get results :items) 0) :volumeInfo) :pageCount)
                :categories ,(plist-get (plist-get (aref (plist-get results :items) 0) :volumeInfo) :categories)
                :googleRating ,(plist-get (plist-get (aref (plist-get results :items) 0) :volumeInfo) :averageRating)
                :title ,(plist-get (plist-get (aref (plist-get results :items) 0) :volumeInfo) :title)
                :authors ,(plist-get (plist-get (aref (plist-get results :items) 0) :volumeInfo) :authors)
                :publishedDate ,(plist-get (plist-get (aref (plist-get results :items) 0) :volumeInfo) :publishedDate)
                :publisher ,(plist-get (plist-get (aref (plist-get results :items) 0) :volumeInfo) :publisher)
                :identifiers ,(plist-get (plist-get (aref (plist-get results :items) 0) :volumeInfo) :industryIdentifiers))))

(defun org-reading-google-books-search-request (query)
  "Perform a search query on the Google Books API and get info
based on the search query."
  (let ((url-request-method "GET")
        (header nil))
    (with-current-buffer
        (url-retrieve-synchronously
         (concat "https://www.googleapis.com/books/v1/volumes?q="
                 (url-hexify-string query)))
      (goto-char (point-min))
      (buffer-string)
      (search-forward "{")
      (forward-char -2)
      (let ((json-object-type 'plist))
        (json-read)))))

(defun org-reading-goodreads-search-request (query)
  "Perform a search query on the Google Books API and get info
based on the search query.")
;; http://www.goodreads.com/book/title.xml?key=9yPaSQZY080FQsOzqJT76A&title=peopleware

(defun org-reading-goodreads-isbn-to-book-id (isbn)
  1)

(defun org-reading-goodreads-add-review (isbn review)
  (let ((book-id (org-reading-goodreads-isbn-to-book-id isbn)))
    2))






(defun org-reading-create-heading (props)
  "Create a heading with information about a book set."
  (org-insert-subheading nil)
  (insert (format "TODO %s\n" (plist-get props :title)))
  (org-set-property "STYLE" "reading")
  (org-set-property "AUTHOR" (pp (plist-get props :authors)))
  (if (plist-get props :identifiers)
      (let (value)
        (dotimes (number (length (plist-get props :identifiers)) value)
          (let ((row (aref (plist-get props :identifiers) number)))
            (org-set-property (plist-get row :type) (plist-get row :identifier))))))
  (org-set-property "GENRE" (or (pp (plist-get props :genre)) ""))
  (org-set-property "PAGES" (number-to-string (plist-get props :pageCount)))
  (org-set-property "GOOGLE_RATING" (number-to-string (or (plist-get props :googleRating) 0)))
  (org-set-property "DATE_ADDED" (format-time-string (org-time-stamp-format)))
  (org-set-property "DATE_STARTED" "")
  (org-set-property "DATE_FINISHED" "")
  (org-set-property "MY_RATING" ""))


(provide 'org-reading)
