;;; helm-wordnut.el --- Helm interface for WordNet -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Manuel Uberti <manuel.uberti@inventati.org>

;; Author: Manuel Uberti <manuel.uberti@inventati.org>
;; URL: https://github.com/manuel-uberti/helm-wordnut
;; Version: 0.1

;;; Commentary:

;; This package is a combination of two packages already available:
;;
;; - https://github.com/raghavgautam/helm-wordnet
;; - https://github.com/gromnitsky/wordnut
;;
;; See the README for more information.

;;; Code:

(require 'dash)
(require 'helm)
(require 'outline)

(defgroup helm-wordnut nil
  "Helm interface for WordNet."
  :group 'convenience)

(defcustom helm-wordnut-wordnet-location
  (car (file-expand-wildcards "/usr/share/wordnet*"))
  "Location of WordNet index files."
  :type 'string
  :group 'helm-wordnut)

(defcustom helm-wordnut-prog "wn"
  "Name of the WordNet program."
  :type 'string
  :group 'helm-wordnut)

(defconst helm-wordnut-cmd-options
  '("-over"
    "-synsn" "-synsv" "-synsa" "-synsr"
    "-simsv"
    "-antsn" "-antsv" "-antsa" "-antsr"
    "-famln" "-famlv" "-famla" "-famlr"
    "-hypen" "-hypev"
    "-hypon" "-hypov"
    "-treen" "-treev"
    "-coorn" "-coorv"
    "-derin" "-deriv"
    "-domnn" "-domnv" "-domna" "-domnr"
    "-domtn" "-domtv" "-domta" "-domtr"
    "-subsn"
    "-partn"
    "-membn"
    "-meron"
    "-hmern"
    "-sprtn"
    "-smemn"
    "-ssubn"
    "-holon"
    "-hholn"
    "-entav"
    "-framv"
    "-causv"
    "-perta" "-pertr"
    "-attrn" "-attra")
  "Optional arguments for WordNet command.")

(defconst helm-wordnut-section-headings
  '("Antonyms" "Synonyms" "Hyponyms" "Troponyms"
    "Meronyms" "Holonyms" "Pertainyms"
    "Member" "Substance" "Part"
    "Attributes" "Derived" "Domain" "Familiarity"
    "Coordinate" "Grep" "Similarity"
    "Entailment" "'Cause To'" "Sample" "Overview of"))

(defun helm-wordnut--get-wordlist ()
  "Fetch WordNet suggestions and return them as a list."
  (let* ((all-indexes (directory-files
                       helm-wordnut-wordnet-location t "index\\..*" ))
         (word-indexes (cl-remove-if
                        (lambda (x) (string-match-p "index\\.sense$" x))
                        all-indexes)))
    (cl-mapcan
     (lambda (x)
       (with-temp-buffer
         (insert-file-contents x)
         (goto-char (point-min))
         (while (re-search-forward "^  .*\n\\| .*" nil t)
           (replace-match ""))
         (split-string (buffer-string) "\n" t)))
     word-indexes)))

(defvar helm-wordnut-all-words nil
  "List of all the words available via WordNet.")

(defun helm-wordnut--get-candidates ()
  "Initialize `helm-wordnut-all-words' and return it."
  (unless (bound-and-true-p helm-wordnut-all-words)
    (setq helm-wordnut-all-words (helm-wordnut--get-wordlist)))
  helm-wordnut-all-words)

(defconst helm-wordnut-fl-link-cat-re "->\\((.+?)\\)?")
(defconst helm-wordnut-fl-link-word-sense-re "\\([^,;)>]+#[0-9]+\\)")
(defconst helm-wordnut-fl-link-re (concat helm-wordnut-fl-link-cat-re " "
                                          helm-wordnut-fl-link-word-sense-re))
(defconst helm-wordnut-font-lock-keywords
  `(("^\\* .+$" . 'outline-1)
    ("^\\*\\* .+$" . 'outline-2)
    (,helm-wordnut-fl-link-cat-re ;; anchor
     ,(concat " " helm-wordnut-fl-link-word-sense-re) nil nil (1 'link))))

(define-derived-mode helm-wordnut-mode special-mode "Helm-Wordnut"
  "Major mode interface to WordNet lexical database."
  (visual-line-mode +1)
  (setq font-lock-defaults '(helm-wordnut-font-lock-keywords)))

(defun helm-wordnut--format-buffer ()
  "Format the entry buffer."
  (let ((inhibit-read-only t)
        (case-fold-search nil))
    ;; Delete the first empty line
    (goto-char (point-min))
    (delete-blank-lines)

    ;; Make headings
    (delete-matching-lines "^ +$" (point-min) (point-max))
    (while (re-search-forward
            (concat "^" (regexp-opt helm-wordnut-section-headings t)) nil t)
      (replace-match "* \\1"))

    ;; Remove empty entries
    (goto-char (point-min))
    (while (re-search-forward "^\\* .+\n\n\\*" nil t)
      (replace-match "*" t t)
      ;; back over the '*' to remove next matching lines
      (backward-char))

    ;; Make sections
    (goto-char (point-min))
    (while (re-search-forward "^Sense [0-9]+" nil t)
      (replace-match "** \\&"))

    ;; Remove the last empty entry
    (goto-char (point-max))
    (if (re-search-backward "^\\* .+\n\\'" nil t)
        (replace-match "" t t))

    (goto-char (point-min))))

(defun helm-wordnut--persistent-action (word)
  "Display the meaning of WORD."
  (let ((buf (get-buffer-create "*WordNet*"))
        (options (apply #'concat
                        (-interpose " " helm-wordnut-cmd-options))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (shell-command-to-string
                 (format "%s %s %s" helm-wordnut-prog word options))))
      (helm-wordnut--format-buffer)
      (set-buffer-modified-p nil)
      (unless (eq major-mode 'helm-wordnut-mode) (helm-wordnut-mode))
      (display-buffer buf)
      (other-window 1))))

(defvar helm-wordnut-source
  (helm-build-sync-source "WordNet"
    :candidates #'helm-wordnut--get-candidates
    :action '(("Dictionary" . helm-wordnut--persistent-action))
    :persistent-action #'helm-wordnut--persistent-action
    :pattern-transformer #'downcase
    :requires-pattern 1))

;;;###autoload
(defun helm-wordnut ()
  "Lookup WordNet definitions with Helm."
  (interactive)
  (helm :sources 'helm-wordnut-source
        :buffer "*helm wordnut*"
        :default (thing-at-point 'word)))


(provide 'helm-wordnut)

;;; helm-wordnut.el ends here
