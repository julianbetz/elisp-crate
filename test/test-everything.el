;;; test-everything.el --- Unit tests for crate -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2020 Julian Betz

;; Author: Julian Betz
;; Created: 2020-08-07
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

(require 'crate)


(defun crate-test--generate-basic-values ()
  "Generate a list of a symbol, an integer, a float and a string.

Values are chosen arbitrarily or pseudo-randomly."
  `(,(gensym) ,(random) ,(* float-pi (random 1000)) ,(format "%s" (gensym))))


(defun crate-test--generate-list ()
  "Generate an arbitrary non-empty list."
  `(,(gensym) ,`(,(gensym) ,(gensym))))


;; List element
;; -----------------------------------------------------------------------------

(ert-deftest crate-test-create-list-element ()
  "Test creating a list element."
  (let ((e (crate-list-element)))
    (should (crate-list-element-p e))
    (should (null (oref e prev)))
    (should (null (oref e data)))
    (should (null (oref e next))))
  (crate-list-element :data nil)
  (let* ((a (gensym)) (e (crate-list-element :data a)))
    (should (crate-list-element-p e))
    (should (null (oref e prev)))
    (should (eq (oref e data) a))
    (should (null (oref e next))))
  (crate-list-element :prev nil)
  (crate-list-element :next nil)
  (let* ((e (crate-list-element)) (f (crate-list-element :prev e)))
    (should (crate-list-element-p f))
    (should (eq (oref f prev) e))
    (should (null (oref f data)))
    (should (null (oref f next))))
  (let* ((e (crate-list-element)) (f (crate-list-element :next e)))
    (should (crate-list-element-p f))
    (should (null (oref f prev)))
    (should (null (oref f data)))
    (should (eq (oref f next) e)))
  (dolist (data (cons (crate-test--generate-list)
                      (crate-test--generate-basic-values)))
    (crate-list-element :data data)
    (should-error (crate-list-element :prev data))
    (should-error (crate-list-element :next data))))


;; List element: prepend

(ert-deftest crate-test-prepend-empty ()
  "Test prepending an empty element to another element."
  (let* ((a (gensym)) (e (crate-list-element :data a)) (f (crate--prepend e)))
    (should (eq f (oref e prev)))
    (should (crate-list-element-p (oref e prev)))
    (should-not (eq (oref e prev) e))
    (should (eq (oref e data) a))
    (should (null (oref e next)))
    (should (null (oref (oref e prev) prev)))
    (should (null (oref (oref e prev) data)))
    (should (eq (oref (oref e prev) next) e))))


(ert-deftest crate-test-prepend-data ()
  "Test prepending an element with satellite data to another element."
  (let* ((a (gensym)) (b (gensym))
         (e (crate-list-element :data a)) (f (crate--prepend e b)))
    (should (eq f (oref e prev)))
    (should (crate-list-element-p (oref e prev)))
    (should-not (eq (oref e prev) e))
    (should (eq (oref e data) a))
    (should (null (oref e next)))
    (should (null (oref (oref e prev) prev)))
    (should (eq (oref (oref e prev) data) b))
    (should (eq (oref (oref e prev) next) e))))


;; List element: append

(ert-deftest crate-test-append-empty ()
  "Test appending an empty element to another element."
  (let* ((a (gensym)) (e (crate-list-element :data a)) (f (crate--append e)))
    (should (eq f (oref e next)))
    (should (null (oref e prev)))
    (should (eq (oref e data) a))
    (should (crate-list-element-p (oref e next)))
    (should-not (eq (oref e next) e))
    (should (eq (oref (oref e next) prev) e))
    (should (null (oref (oref e next) data)))
    (should (null (oref (oref e next) next)))))


(ert-deftest crate-test-append-data ()
  "Test appending an element with satellite data to another element."
  (let* ((a (gensym)) (b (gensym))
         (e (crate-list-element :data a)) (f (crate--append e b)))
    (should (eq f (oref e next)))
    (should (null (oref e prev)))
    (should (eq (oref e data) a))
    (should (crate-list-element-p (oref e next)))
    (should-not (eq (oref e next) e))
    (should (eq (oref (oref e next) prev) e))
    (should (eq (oref (oref e next) data) b))
    (should (null (oref (oref e next) next)))))


;; List element: remove

(ert-deftest crate-test-remove-free ()
  "Test removing a list element without any connected elements."
  (let* ((a (gensym)) (e (crate-list-element :data a)))
    (should (eq (crate--remove e) e))
    (should (crate-list-element-p e))
    (should (null (oref e prev)))
    (should (eq (oref e data) a))
    (should (null (oref e next)))))


(ert-deftest crate-test-remove-singleton-in-ring ()
  "Test removing a list element connected only to itself."
  (let* ((a (gensym)) (e (crate-list-element :data a)))
    (oset e prev e)
    (oset e next e)
    (should (eq (crate--remove e) e))
    (should (crate-list-element-p e))
    (should (null (oref e prev)))
    (should (eq (oref e data) a))
    (should (null (oref e next)))))


(ert-deftest crate-test-remove-among ()
  "Test removing a list element surrounded by others."
  (let* ((a (gensym)) (b (gensym)) (c (gensym))
         f g (h (crate-list-element :data b)) i j)
    (crate--prepend h a)
    (crate--append h c)
    (setq g (oref h prev) i (oref h next))
    (crate--prepend g)
    (crate--append i)
    (setq f (oref g prev) j (oref i next))
    (should (eq (crate--remove h) h))
    (dolist (e `(,f ,g ,h ,i ,j))
      (should (crate-list-element-p e)))
    (should (eq (oref g prev) f))
    (should (eq (oref g data) a))
    (should (eq (oref g next) i))
    (should (null (oref h prev)))
    (should (eq (oref h data) b))
    (should (null (oref h next)))
    (should (eq (oref i prev) g))
    (should (eq (oref i data) c))
    (should (eq (oref i next) j))))


(ert-deftest crate-test-remove-head ()
  "Test removing the first element."
  (let* ((a (gensym)) (b (gensym))
         g (h (crate-list-element :data b)) i)
    (crate--prepend h a)
    (crate--append h)
    (setq g (oref h prev) i (oref h next))
    (should (eq (crate--remove g) g))
    (dolist (e `(,g ,h ,i))
      (should (crate-list-element-p e)))
    (should (null (oref g prev)))
    (should (eq (oref g data) a))
    (should (null (oref g next)))
    (should (null (oref h prev)))
    (should (eq (oref h data) b))
    (should (eq (oref h next) i))))


(ert-deftest crate-test-remove-tail ()
  "Test removing the last element."
  (let* ((a (gensym)) (b (gensym))
         g (h (crate-list-element :data a)) i)
    (crate--prepend h)
    (crate--append h b)
    (setq g (oref h prev) i (oref h next))
    (should (eq (crate--remove i) i))
    (dolist (e `(,g ,h ,i))
      (should (crate-list-element-p e)))
    (should (eq (oref h prev) g))
    (should (eq (oref h data) a))
    (should (null (oref h next)))
    (should (null (oref i prev)))
    (should (eq (oref i data) b))
    (should (null (oref i next)))))


;; Doubly linked list
;; -----------------------------------------------------------------------------

;; All cases for empty-p are covered in the tests for instance initialization,
;; enqueue, push, prune, pop and clear.

(ert-deftest crate-test-create-linked-list ()
  "Test creating a doubly linked list."
  (let ((l (crate-linked-list)))
    (should (crate-linked-list-p l))
    (should (crate-empty-p l))
    (should (eq (oref l prev) l))
    (should (null (oref l data)))
    (should (eq (oref l next) l)))
  (should-error (crate-linked-list :prev (gensym)))
  (should-error (crate-linked-list :data (gensym)))
  (should-error (crate-linked-list :next (gensym))))


(defun crate-test--list-singleton-p (l)
  "Test whether the given list consists of two distinct list elements.

This also takes the empty base component into account.  The list
thus has a length of 1."
  (should-not (crate-empty-p l))
  (should (eq (oref l prev) (oref l next)))
  (should-not (eq (oref l prev) l))
  (should (eq (oref (oref l prev) prev) l)))


(defun crate-test--list-pair-p (l)
  "Test whether the given list consists of three distinct list elements.

This also takes the empty base component into account.  The list
thus has a length of 2."
  (should-not (crate-empty-p l))
  (should (eq (oref l prev) (oref (oref l next) next)))
  (should-not (eq (oref l prev) l))
  (should (eq (oref l next) (oref (oref l prev) prev)))
  (should-not (eq (oref l next) l))
  (should (eq (oref (oref l prev) next) l))
  (should (eq (oref (oref l next) prev) l))
  (should-not (eq (oref l prev) (oref l next))))


;; Doubly linked list: enqueue

(ert-deftest crate-test-linked-list-enqueue-0-empty ()
  "Test adding an empty element to the back of a list of length 0."
  (let ((l (crate-linked-list)))
    (should (null (crate-enqueue l)))
    (crate-test--list-singleton-p l)
    (should (null (oref l data)))
    (should (null (oref (oref l next) data)))))


(ert-deftest crate-test-linked-list-enqueue-0-data ()
  "Test adding a data element to the back of a list of length 0."
  (let ((a (gensym)) (l (crate-linked-list)))
    (should (null (crate-enqueue l a)))
    (crate-test--list-singleton-p l)
    (should (null (oref l data)))
    (should (eq (oref (oref l next) data) a))))


(ert-deftest crate-test-linked-list-enqueue-1-empty ()
  "Test adding an empty element to the back of a list of length 1."
  (let ((a (gensym)) (l (crate-linked-list)))
    (crate-enqueue l a)
    (should (null (crate-enqueue l)))
    (crate-test--list-pair-p l)
    (should (null (oref l data)))
    (should (eq (oref (oref l next) data) a))
    (should (null (oref (oref l prev) data)))))


(ert-deftest crate-test-linked-list-enqueue-1-data ()
  "Test adding a data element to the back of a list of length 1."
  (let ((a (gensym)) (b (gensym)) (l (crate-linked-list)))
    (crate-enqueue l a)
    (should (null (crate-enqueue l b)))
    (crate-test--list-pair-p l)
    (should (null (oref l data)))
    (should (eq (oref (oref l next) data) a))
    (should (eq (oref (oref l prev) data) b))))


;; Doubly linked list: push

(ert-deftest crate-test-linked-list-push-0-empty ()
  "Test adding an empty element to the front of a list of length 0."
  (let ((l (crate-linked-list)))
    (should (null (crate-push l)))
    (crate-test--list-singleton-p l)
    (should (null (oref l data)))
    (should (null (oref (oref l next) data)))))


(ert-deftest crate-test-linked-list-push-0-data ()
  "Test adding a data element to the front of a list of length 0."
  (let ((a (gensym)) (l (crate-linked-list)))
    (should (null (crate-push l a)))
    (crate-test--list-singleton-p l)
    (should (null (oref l data)))
    (should (eq (oref (oref l next) data) a))))


(ert-deftest crate-test-linked-list-push-1-empty ()
  "Test adding an empty element to the front of a list of length 1."
  (let ((a (gensym)) (l (crate-linked-list)))
    (crate-push l a)
    (should (null (crate-push l)))
    (crate-test--list-pair-p l)
    (should (null (oref l data)))
    (should (eq (oref (oref l prev) data) a))
    (should (null (oref (oref l next) data)))))


(ert-deftest crate-test-linked-list-push-1-data ()
  "Test adding a data element to the front of a list of length 1."
  (let ((a (gensym)) (b (gensym)) (l (crate-linked-list)))
    (crate-push l a)
    (should (null (crate-push l b)))
    (crate-test--list-pair-p l)
    (should (null (oref l data)))
    (should (eq (oref (oref l prev) data) a))
    (should (eq (oref (oref l next) data) b))))


;; Doubly linked list: prune

(ert-deftest crate-test-linked-list-prune-1 ()
  "Test removing the last element of a list of length 1."
  (let ((a (gensym)) (l (crate-linked-list)))
    (crate-enqueue l a)
    (should (eq (crate-prune l) a))
    (should (crate-empty-p l))
    (should (eq (oref l prev) l))
    (should (null (oref l data)))
    (should (eq (oref l next) l))))


(ert-deftest crate-test-linked-list-prune-2 ()
  "Test removing the last element of a list of length 2."
  (let ((a (gensym)) (b (gensym)) (l (crate-linked-list)))
    (dolist (i `(,a ,b))
      (crate-enqueue l i))
    (should (eq (crate-prune l) b))
    (crate-test--list-singleton-p l)
    (should (null (oref l data)))
    (should (eq (oref (oref l next) data) a))))


(ert-deftest crate-test-linked-list-prune-3 ()
  "Test removing the last element of a list of length 3."
  (let ((a (gensym)) (b (gensym)) (c (gensym)) (l (crate-linked-list)))
    (dolist (i `(,a ,b ,c))
      (crate-enqueue l i))
    (should (eq (crate-prune l) c))
    (crate-test--list-pair-p l)
    (should (eq (oref (oref l prev) data) b))
    (should (null (oref l data)))
    (should (eq (oref (oref l next) data) a))))


;; Doubly linked list: pop

(ert-deftest crate-test-linked-list-pop-1 ()
  "Test removing the first element of a list of length 1."
  (let ((a (gensym)) (l (crate-linked-list)))
    (crate-enqueue l a)
    (should (eq (crate-pop l) a))
    (should (crate-empty-p l))
    (should (eq (oref l prev) l))
    (should (null (oref l data)))
    (should (eq (oref l next) l))))


(ert-deftest crate-test-linked-list-pop-2 ()
  "Test removing the first element of a list of length 2."
  (let ((a (gensym)) (b (gensym)) (l (crate-linked-list)))
    (dolist (i `(,a ,b))
      (crate-enqueue l i))
    (should (eq (crate-pop l) a))
    (crate-test--list-singleton-p l)
    (should (null (oref l data)))
    (should (eq (oref (oref l next) data) b))))


(ert-deftest crate-test-linked-list-pop-3 ()
  "Test removing the first element of a list of length 3."
  (let ((a (gensym)) (b (gensym)) (c (gensym)) (l (crate-linked-list)))
    (dolist (i `(,a ,b ,c))
      (crate-enqueue l i))
    (should (eq (crate-pop l) a))
    (crate-test--list-pair-p l)
    (should (eq (oref (oref l prev) data) c))
    (should (null (oref l data)))
    (should (eq (oref (oref l next) data) b))))


;; Doubly linked list: clear

(ert-deftest crate-test-linked-list-clear-0 ()
  "Test clearing a list of length 0."
  (let ((l (crate-linked-list)))
    (should (null (crate-clear l)))
    (should (crate-empty-p l))
    (should (eq (oref l prev) l))
    (should (null (oref l data)))
    (should (eq (oref l next) l))))


(ert-deftest crate-test-linked-list-clear-1 ()
  "Test clearing a list of length 1."
  (let ((a (gensym)) (l (crate-linked-list)) m)
    (crate-enqueue l a)
    (setq m (oref l next))
    (should (null (crate-clear l)))
    (should (crate-empty-p l))
    (should (eq (oref l prev) l))
    (should (null (oref l data)))
    (should (eq (oref l next) l))
    (should (null (oref m prev)))
    (should (eq (oref m data) a))
    (should (null (oref m next)))))


(ert-deftest crate-test-linked-list-clear-2 ()
  "Test clearing a list of length 2."
  (let ((a (gensym)) (b (gensym)) (l (crate-linked-list)) m n)
    (dolist (i `(,a ,b))
      (crate-enqueue l i))
    (setq m (oref l next) n (oref m next))
    (should (null (crate-clear l)))
    (should (crate-empty-p l))
    (should (eq (oref l prev) l))
    (should (null (oref l data)))
    (should (eq (oref l next) l))
    (loop for i in `(,a ,b)
          for j in `(,m ,n)
          do (progn
               (should (null (oref j prev)))
               (should (eq (oref j data) i))
               (should (null (oref j next)))))))


(ert-deftest crate-test-linked-list-clear-3 ()
  "Test clearing a list of length 3."
  (let ((a (gensym)) (b (gensym)) (c (gensym)) (l (crate-linked-list)) m n o)
    (dolist (i `(,a ,b ,c))
      (crate-enqueue l i))
    (setq m (oref l next) n (oref m next) o (oref n next))
    (should (null (crate-clear l)))
    (should (crate-empty-p l))
    (should (eq (oref l prev) l))
    (should (null (oref l data)))
    (should (eq (oref l next) l))
    (loop for i in `(,a ,b ,c)
          for j in `(,m ,n ,o)
          do (progn
               (should (null (oref j prev)))
               (should (eq (oref j data) i))
               (should (null (oref j next)))))))


;;; test-everything.el ends here
