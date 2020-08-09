;;; crate.el --- A selection of specialized collections for Emacs Lisp -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2020 Julian Betz

;; Author: Julian Betz
;; Package-Version: 0.2.alpha1
;; Created: 2020-08-07
;; Package-Requires: ((emacs "25.1"))
;; Keywords: extensions
;; URL: https://github.com/julianbetz/elisp-crate

;; This file is NOT part of GNU Emacs.

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;; 
;;      http://www.apache.org/licenses/LICENSE-2.0
;; 
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.


;;; Code:

(require 'cl)
(require 'eieio)


(cl-defmethod crate-to-str (this)
  (object-print this))


(defclass crate-list-element ()
  ((prev :initarg :prev
         :initform nil
         :type (or crate-list-element null)
         :documentation "The previous linked list element.")
   (data :initarg :data
         :initform nil
         :type t
         :documentation "Satellite data.")
   (next :initarg :next
         :initform nil
         :type (or crate-list-element null)
         :documentation "The next linked list element."))
  "An element of a doubly linked list.")


(cl-defmethod crate--prepend ((this crate-list-element) &optional data)
  (let ((element (crate-list-element
                  :prev (oref this prev)
                  :data data
                  :next this)))
    (when (oref this prev)
      (oset (oref this prev) next element))
    (oset this prev element)))


(cl-defmethod crate--append ((this crate-list-element) &optional data)
  (let ((element (crate-list-element
                  :prev this
                  :data data
                  :next (oref this next))))
    (when (oref this next)
      (oset (oref this next) prev element))
    (oset this next element)))


(cl-defmethod crate--remove ((this crate-list-element))
  (when (oref this prev)
    (oset (oref this prev) next (oref this next)))
  (when (oref this next)
    (oset (oref this next) prev (oref this prev)))
  (oset this prev nil)
  (oset this next nil)
  this)


;; Doubly linked list

(defclass crate-linked-list (crate-list-element) () "A doubly linked list.")


(cl-defmethod initialize-instance ((this crate-linked-list) &optional slots)
  ;; XXX Maybe discouraging of constructor elements can be done more naturally
  (loop for slot in slots
        for i from 0
        do (when (and (evenp i) (member slot '(:prev :data :next)))
             (error "Linked list cannot be initialized with keyword %S" slot)))
  (oset this prev this)
  (oset this next this))


(cl-defmethod crate-empty-p ((this crate-linked-list))
  (eq (oref this next) this))


(cl-defmethod crate-enqueue ((this crate-linked-list) &optional data)
  "Add an element to the tail of the list."
  (crate--prepend this data)
  nil)


(cl-defmethod crate-push ((this crate-linked-list) &optional data)
  "Add an element to the head of the list."
  (crate--append this data)
  nil)


(cl-defmethod crate-prune ((this crate-linked-list))
  "Remove and return the last element of the list."
  (if (crate-empty-p this)
      (error "List empty")
    (oref (crate--remove (oref this prev)) data)))


(cl-defmethod crate-pop ((this crate-linked-list))
  "Remove and return the first element of the list."
  (if (crate-empty-p this)
      (error "List empty")
    (oref (crate--remove (oref this next)) data)))


(cl-defmethod crate-clear ((this crate-linked-list))
  "Remove all elements from this list.

Breaks the links between all elements."
  (while (not (crate-empty-p this))
    (crate-pop this))
  nil)


;; Length-restricted list

(defclass crate-limited-list (crate-linked-list)
  ((max-length :initarg :max-length
               :initform 0
               :type (integer 0 *)
               :documentation "The maximum length of the list.")
   (length :initform 0
           :type (integer 0 *)
           :documentation "The length of the list."))
  "A length-restricted list.

Drops elements once its maximum length is reached.")


(cl-defmethod initialize-instance ((this crate-limited-list) &optional slots)
  (cl-call-next-method)
  (when (plist-member slots :max-length)
    (oset this max-length (plist-get slots :max-length))))


(cl-defmethod crate-full-p ((this crate-limited-list))
  (>= (oref this length) (oref this max-length)))


(cl-defmethod crate-prune ((this crate-limited-list))
  (let ((data (cl-call-next-method)))
    (oset this length (1- (oref this length)))
    data))


(cl-defmethod crate-pop ((this crate-limited-list))
  (let ((data (cl-call-next-method)))
    (oset this length (1- (oref this length)))
    data))


(cl-defmethod crate-push ((this crate-limited-list) &optional data)
  (unless (< (oref this max-length) 1)
    (when (crate-full-p this)
      (crate-prune this))
    (oset this length (1+ (oref this length)))
    (cl-call-next-method)))


(cl-defmethod crate-enqueue ((this crate-limited-list) &optional data)
  (unless (< (oref this max-length) 1)
    (when (crate-full-p this)
      (crate-pop this))
    (oset this length (1+ (oref this length)))
    (cl-call-next-method)))


(provide 'crate)


;;; crate.el ends here
