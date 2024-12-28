;;; org-datetree.el --- Create date entries in a tree -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2024 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Keywords: outlines, hypermedia, calendar, text
;; URL: https://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains code to create entries in a tree where the top-level
;; nodes represent years, the level 2 nodes represent the months, and the
;; level 1 entries days. TODO Update this

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'cal-iso)
(require 'org)

(defcustom org-datetree-add-timestamp nil
  "When non-nil, add a time stamp matching date of entry.
Added time stamp is active unless value is `inactive'."
  :group 'org-capture
  :version "24.3"
  :type '(choice
	  (const :tag "Do not add a time stamp" nil)
	  (const :tag "Add an inactive time stamp" inactive)
	  (const :tag "Add an active time stamp" active)))

;;;###autoload
(defun org-datetree-find-date-create (d &optional keep-restriction)
  "Find or create a day entry for date D.
If KEEP-RESTRICTION is non-nil, do not widen the buffer.
When it is nil, the buffer will be widened to make sure an existing date
tree can be found.  If it is the symbol `subtree-at-point', then the tree
will be built under the headline at point."
  (org-datetree-find-create-entry d '(year month day) keep-restriction))

;;;###autoload
(defun org-datetree-find-month-create (d &optional keep-restriction)
  "Find or create a month entry for date D.
Compared to `org-datetree-find-date-create' this function creates
entries grouped by year-month instead of year-month-day.
If KEEP-RESTRICTION is non-nil, do not widen the buffer.
When it is nil, the buffer will be widened to make sure an existing date
tree can be found.  If it is the symbol `subtree-at-point', then the tree
will be built under the headline at point."
  (org-datetree-find-create-entry d '(year month) keep-restriction))

;;;###autoload
(defun org-datetree-find-iso-week-create (d &optional keep-restriction)
  "Find or create an ISO week entry for date D.
Compared to `org-datetree-find-date-create' this function creates
entries grouped by year-week-day instead of year-month-day.  If
KEEP-RESTRICTION is non-nil, do not widen the buffer.  When it is
nil, the buffer will be widened to make sure an existing date
tree can be found.  If it is the symbol `subtree-at-point', then
the tree will be built under the headline at point."
  (org-datetree-find-create-entry d '(year week day) keep-restriction))

(defun org-datetree-find-create-entry
    (d time-grouping &optional keep-restriction)
  "Find or create an entry for date D.
TIME-GROUPING specifies the grouping levels of the datetree, and
should be a subset of `(year quarter month week day)'.  TODO
document quarter behavior, KEEP-RESTRICTION"
  (let* ((level 1)
         (year (calendar-extract-year d))
	 (month (calendar-extract-month d))
	 (day (calendar-extract-day d))
         (time (org-encode-time 0 0 0 day month year))
         (iso-date (calendar-iso-from-absolute
		    (calendar-absolute-from-gregorian d)))
         (week (nth 0 iso-date))
         (nominal-year
          (if (memq 'week time-grouping)
              (nth 2 iso-date)
            year))
         (nominal-month
          (if (memq 'week time-grouping)
              (calendar-extract-month
               ;; anchor on Thurs, to be consistent with weekyear
               (calendar-gregorian-from-absolute
                (calendar-iso-to-absolute
                 `(,week 4 ,nominal-year))))
            month))
         (quarter (if (and (memq 'week time-grouping)
                           (not (memq 'month time-grouping)))
                      (min 4 (1+ (/ (1- week) 13)))
                    (1+ (/ (1- nominal-month) 3))))
         ;; Support the old way of tree placement, using a property
         (legacy-prop (cond
                       ((seq-set-equal-p time-grouping '(year month day))
                        "DATE_TREE")
                       ((seq-set-equal-p time-grouping '(year month))
                        "DATE_TREE")
                       ((seq-set-equal-p time-grouping '(year week day))
                        "WEEK_TREE"))))
    (save-restriction
      ;; find the base level of the datetree, narrow, and goto top
      (if (eq keep-restriction 'subtree-at-point)
          (progn
	    (unless (org-at-heading-p) (error "Not at heading"))
	    (widen)
	    (org-narrow-to-subtree)
	    (setq level (org-get-valid-level (org-current-level) 1)))
        (unless keep-restriction (widen))
        (when legacy-prop
          (let ((prop (org-find-property legacy-prop)))
            (when prop
	      (goto-char prop)
	      (setq level (org-get-valid-level (org-current-level) 1))
	      (org-narrow-to-subtree)))))
      (goto-char (point-min))
      ;; find/create entries for each datetree level
      (when (memq 'year time-grouping)
        (org-datetree--find-create-subheading
         "\\([12][0-9]\\{3\\}\\)"
         nominal-year (number-to-string nominal-year) level)
        (org-narrow-to-subtree)
        (setq level (1+ level)))
      (when (memq 'quarter time-grouping)
        (org-datetree--find-create-subheading
         (format "%d-Q\\([1-4]\\)" nominal-year)
         quarter
         (format "%d-Q%d" nominal-year quarter) level)
        (org-narrow-to-subtree)
        (setq level (1+ level)))
      (when (memq 'month time-grouping)
        (org-datetree--find-create-subheading
         (format "%d-\\([01][0-9]\\) \\w+" nominal-year)
         nominal-month
         (format-time-string "%Y-%m %B" (org-encode-time 0 0 0 1 nominal-month
                                                         nominal-year))
         level)
        (org-narrow-to-subtree)
        (setq level (1+ level)))
      (when (memq 'week time-grouping)
        (org-datetree--find-create-subheading
         (format "%d-W\\([0-5][0-9]\\)" nominal-year)
         week
         (format-time-string "%G-W%V" time) level)
        (org-narrow-to-subtree)
        (setq level (1+ level)))
      (when (memq 'day time-grouping)
        ;; Use regular date instead of ISO-week year/month
	(org-datetree--find-create-subheading
         (format "%d-%02d-\\([0123][0-9]\\) \\w+" year month)
	 day
         (format-time-string "%Y-%m-%d %A" (org-encode-time 0 0 0 day month year))
         level)
        (when org-datetree-add-timestamp
          (save-excursion
            (end-of-line)
            (insert "\n")
            (org-indent-line)
            (org-insert-timestamp
             (org-encode-time 0 0 0 day month year)
             nil
             (eq org-datetree-add-timestamp 'inactive))))))))

(defun org-datetree--find-create-subheading
    (sibling-regex match-num new-title level)
  "Find datetree subheading, or create it if it doesn't exist.
SIBLING-REGEX should be a regex that matches the headline and its
siblings, with 1 match group that captures the order of the
headline among its siblings, specified as MATCH-NUM.  If a
sibling is found that is subsequent to MATCH-NUM, a new headline
is inserted before it.  Otherwise, if a headline matching
MATCH-NUM is found, point is moved there.  Otherwise, if neither
the headline nor a subsequent sibling is found, the headline is
inserted at the bottom of the narrowed buffer.  If a new headline
is inserted, it is created with the text NEW-TITLE.

For example, if we want to find or create the headline for
\"2024-12-27 Friday\", then we could call this as:

  (org-datetree--find-create-subheading
    \"2024-12-\\([0123][0-9]\\) \\w+\" 27
    \"2024-12-27 Friday\")"
  ;; ensure that the first match group in SIBLING-REGEX
  ;; is the first inside `org-complex-heading-regexp-format'
  (when (and (not (string-match-p "\\\\(\\?1:" sibling-regex))
             (string-match "\\\\(" sibling-regex))
    (setq sibling-regex (replace-match "\\(?1:" nil t sibling-regex)))
  (let ((re (format org-complex-heading-regexp-format
                    sibling-regex))
	match)
    (goto-char (point-min))
    (while (and (setq match (re-search-forward re nil t))
                (goto-char (match-beginning 1))
		(< (string-to-number (match-string 1)) match-num)))
    (if match
        (beginning-of-line)
      (goto-char (point-max))
      (unless (bolp) (insert "\n")))
    (unless (and match (= (string-to-number (match-string 1)) match-num))
      (delete-region (save-excursion (skip-chars-backward " \t\n") (point)) (point))
      (when (org--blank-before-heading-p) (insert "\n"))
      (insert
       (format "\n%s \n" (make-string (if org-odd-levels-only
                                          (1- (* 2 level))
                                        level)
                                      ?*)))
      (backward-char)
      (insert new-title)
      (beginning-of-line))))

(defun org-datetree-file-entry-under (txt d)
  "Insert a node TXT into the date tree under date D."
  (org-datetree-find-date-create d)
  (let ((level (org-get-valid-level (funcall outline-level) 1)))
    (org-end-of-subtree t t)
    (org-back-over-empty-lines)
    (org-paste-subtree level txt)))

(defun org-datetree-cleanup ()
  "Make sure all entries in the current tree are under the correct date.
It may be useful to restrict the buffer to the applicable portion
before running this command, even though the command tries to be smart."
  (interactive)
  (goto-char (point-min))
  (let ((dre (concat "\\<" org-deadline-string "\\>[ \t]*\\'"))
	(sre (concat "\\<" org-scheduled-string "\\>[ \t]*\\'")))
    (while (re-search-forward org-ts-regexp nil t)
      (catch 'next
	(let ((tmp (buffer-substring
		    (max (line-beginning-position)
			 (- (match-beginning 0) org-ds-keyword-length))
		    (match-beginning 0))))
	  (when (or (string-suffix-p "-" tmp)
		    (string-match dre tmp)
		    (string-match sre tmp))
	    (throw 'next nil))
	  (let* ((dct (decode-time (org-time-string-to-time (match-string 0))))
		 (date (list (nth 4 dct) (nth 3 dct) (nth 5 dct)))
		 (year (nth 2 date))
		 (month (car date))
		 (day (nth 1 date))
		 (pos (point))
		 (hdl-pos (progn (org-back-to-heading t) (point))))
	    (unless (org-up-heading-safe)
	      ;; No parent, we are not in a date tree.
	      (goto-char pos)
	      (throw 'next nil))
	    (unless (looking-at "\\*+[ \t]+[0-9]+-[0-1][0-9]-[0-3][0-9]")
	      ;; Parent looks wrong, we are not in a date tree.
	      (goto-char pos)
	      (throw 'next nil))
	    (when (looking-at (format "\\*+[ \t]+%d-%02d-%02d" year month day))
	      ;; At correct date already, do nothing.
	      (goto-char pos)
	      (throw 'next nil))
	    ;; OK, we need to refile this entry.
	    (goto-char hdl-pos)
	    (org-cut-subtree)
	    (save-excursion
	      (save-restriction
		(org-datetree-file-entry-under (current-kill 0) date)))))))))

(provide 'org-datetree)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-datetree.el ends here
