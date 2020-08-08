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


(ert-deftest crate-test-create ()
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


(ert-deftest crate-test-prepend-empty ()
  "Test prepending an empty element to another element."
  (let* ((a (gensym)) (e (crate-list-element :data a)))
    (crate--prepend e)
    (should (crate-list-element-p (oref e prev)))
    (should-not (eq (oref e prev) e))
    (should (eq (oref e data) a))
    (should (null (oref e next)))
    (should (null (oref (oref e prev) prev)))
    (should (null (oref (oref e prev) data)))
    (should (eq (oref (oref e prev) next) e))))


(ert-deftest crate-test-prepend-data ()
  "Test prepending an element with satellite data to another element."
  (let* ((a (gensym)) (b (gensym)) (e (crate-list-element :data a)))
    (crate--prepend e b)
    (should (crate-list-element-p (oref e prev)))
    (should-not (eq (oref e prev) e))
    (should (eq (oref e data) a))
    (should (null (oref e next)))
    (should (null (oref (oref e prev) prev)))
    (should (eq (oref (oref e prev) data) b))
    (should (eq (oref (oref e prev) next) e))))


(ert-deftest crate-test-append-empty ()
  "Test appending an empty element to another element."
  (let* ((a (gensym)) (e (crate-list-element :data a)))
    (crate--append e)
    (should (null (oref e prev)))
    (should (eq (oref e data) a))
    (should (crate-list-element-p (oref e next)))
    (should-not (eq (oref e next) e))
    (should (eq (oref (oref e next) prev) e))
    (should (null (oref (oref e next) data)))
    (should (null (oref (oref e next) next)))))


(ert-deftest crate-test-append-data ()
  "Test appending an element with satellite data to another element."
  (let* ((a (gensym)) (b (gensym)) (e (crate-list-element :data a)))
    (crate--append e b)
    (should (null (oref e prev)))
    (should (eq (oref e data) a))
    (should (crate-list-element-p (oref e next)))
    (should-not (eq (oref e next) e))
    (should (eq (oref (oref e next) prev) e))
    (should (eq (oref (oref e next) data) b))
    (should (null (oref (oref e next) next)))))


(ert-deftest crate-test-remove-free ()
  "Test removing a list element without any connected elements."
  (let* ((a (gensym)) (e (crate-list-element :data a)))
    (crate--remove e)
    (should (crate-list-element-p e))
    (should (null (oref e prev)))
    (should (eq (oref e data) a))
    (should (null (oref e next)))))


(ert-deftest crate-test-remove-singleton-in-ring ()
  "Test removing a list element connected only to itself."
  (let* ((a (gensym)) (e (crate-list-element :data a)))
    (oset e prev e)
    (oset e next e)
    (crate--remove e)
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
    (crate--remove h)
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
    (crate--remove g)
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
    (crate--remove i)
    (dolist (e `(,g ,h ,i))
      (should (crate-list-element-p e)))
    (should (eq (oref h prev) g))
    (should (eq (oref h data) a))
    (should (null (oref h next)))
    (should (null (oref i prev)))
    (should (eq (oref i data) b))
    (should (null (oref i next)))))


;;; test-everything.el ends here
