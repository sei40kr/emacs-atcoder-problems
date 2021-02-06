;;; atcoder-problems.el --- AtCoder Problems client

;; Copyright (c) 2021 Seong Yong-ju All rights reserved.
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (dash "1.5") (request-deferred "0.2"))
;; Keywords: tools
;; URL: https://github.com/sei40kr/emacs-atcoder-problems

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; AtCoder Problems client for Emacs.

;;; Code:

(require 'dash)
(require 'request-deferred)

(defvar atcoder-problems--url-base "https://kenkoooo.com/atcoder")

(defun atcoder-problems--url (path)
  (concat atcoder-problems--url-base path))

(defun atcoder-problems--json-read ()
  (let* ((json-array-type 'list)
         (json-object-type 'hash-table))
    (json-read)))

(defun atcoder-problems--entries (contests problems model-by-problem-id)
  (let* ((contest-id-symbols (->> contests
                                  (-sort (-lambda ((&hash "start_epoch_second" a)
                                                   (&hash "start_epoch_second" b))
                                           (> a b)))
                                  (-map (-lambda ((&hash "id" id)) (intern id)))))
         (problems-by-contest-id (make-hash-table :size (length contest-id-symbols))))
    (dolist (contest-id-symbol contest-id-symbols)
      (puthash contest-id-symbol '() problems-by-contest-id))
    (dolist (problem problems)
      (-let* (((&hash "contest_id" contest-id) problem)
              (contest-id-symbol (intern contest-id)))
        (push problem (gethash contest-id-symbol problems-by-contest-id))))
    (mapcar
     #'(lambda (contest-id-symbol)
         (-let* (((&hash contest-id-symbol contest-problems) problems-by-contest-id)
                 (contest-id (format "%S" contest-id-symbol))
                 (problem-titles (->> contest-problems
                                      (-take 6)
                                      (-map (-lambda ((&hash "title" title))
                                              (substring title 3)))))
                 (pad (make-list (- 6 (length problem-titles)) "")))
           `(,contest-id-symbol ,(vconcat `(,contest-id)
                                          problem-titles pad))))
     contest-id-symbols)))

(defun atcoder-problems--deferred-revert ()
  (deferred:$
    (deferred:parallel
      (request-deferred (atcoder-problems--url "/resources/contests.json")
                        :parser #'atcoder-problems--json-read)
      (request-deferred (atcoder-problems--url "/resources/problems.json")
                        :parser #'atcoder-problems--json-read)
      (request-deferred (atcoder-problems--url "/resources/problem-models.json")
                        :parser #'atcoder-problems--json-read))
    (deferred:nextc it
      (lambda (responses)
        (let* ((contests (request-response-data (nth 0 responses)))
               (problems (request-response-data (nth 1 responses)))
               (model-by-problem-id (request-response-data (nth 2 responses))))
          (setq tabulated-list-entries
                (atcoder-problems--entries contests
                                           problems
                                           model-by-problem-id))
          (tablist-revert))))))

(define-derived-mode atcoder-problems-mode tabulated-list-mode
  "AtCoder Problems"
  "Major mode for browsing AtCoder Problems."
  (setq tabulated-list-format [("Contest" 20)
                               ("A" 20)
                               ("B" 20)
                               ("C" 20)
                               ("D" 20)
                               ("E" 20)
                               ("F" 20)]
        tabulated-list-padding 2)
  (tablist-minor-mode)
  (tabulated-list-init-header))

;;;###autoload
(defun atcoder-problems ()
  "Display AtCoder Problems."
  (interactive)
  (pop-to-buffer "*atcoder-problems*")
  (atcoder-problems-mode)
  (atcoder-problems--deferred-revert))

(provide 'atcoder-problems)

;;; atcoder-problems.el ends here
