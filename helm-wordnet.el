;;; helm-wordnet.el --- Helm interface for WordNet -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Manuel Uberti <manuel.uberti@inventati.org>

;; Author: Manuel Uberti <manuel.uberti@inventati.org>
;; URL: https://github.com/manuel-uberti/helm-wordnet
;; Version: 0.1

;;; Commentary:

;; This package is merely a combination of two packages already available:
;;
;; - https://github.com/raghavgautam/helm-wordnet
;; - https://github.com/gromnitsky/wordnut/blob/master/wordnut.el

;;; Code:

(require 'helm)
(require 'dash)

(defconst helm-wordnet-wordnet-location
  (car (file-expand-wildcards "/usr/share/wordnet*"))
  "Location of WordNet index files.")

(defconst helm-wordnet-cmd-options
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
  "Optional arguments for `wn'.")

(defconst helm-wordnet-section-headings
  '("Antonyms" "Synonyms" "Hyponyms" "Troponyms"
    "Meronyms" "Holonyms" "Pertainyms"
    "Member" "Substance" "Part"
    "Attributes" "Derived" "Domain" "Familiarity"
    "Coordinate" "Grep" "Similarity"
    "Entailment" "'Cause To'" "Sample" "Overview of"))

(defun helm-wordnet--get-wordlist ()
  "Fetch WordNet suggestions and return them as a list."
  (let* ((all-indexes (directory-files
                       helm-wordnet-wordnet-location t "index\\..*" ))
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

(defvar helm-wordnet-all-words nil
  "List of all the words available via WordNet.")

(defun helm-wordnet--get-candidates ()
  "Fetch WordNet suggestions and return them as a list."
  (unless (bound-and-true-p helm-wordnet-all-words)
    (setq helm-wordnet-all-words (helm-wordnet--get-wordlist)))
  helm-wordnet-all-words)

(defconst helm-wordnet-fl-link-cat-re "->\\((.+?)\\)?")
(defconst helm-wordnet-fl-link-word-sense-re "\\([^,;)>]+#[0-9]+\\)")
(defconst helm-wordnet-fl-link-re (concat helm-wordnet-fl-link-cat-re " "
                                          helm-wordnet-fl-link-word-sense-re))
(defconst helm-wordnet-font-lock-keywords
  `(("^\\* .+$" . 'outline-1)
    ("^\\*\\* .+$" . 'outline-2)
    (,helm-wordnet-fl-link-cat-re ;; anchor
     ,(concat " " helm-wordnet-fl-link-word-sense-re) nil nil (1 'link))))

(define-derived-mode helm-wordnet-mode special-mode "Helm-WordNet"
  "Major mode interface to WordNet lexical database."
  (visual-line-mode +1)
  (setq font-lock-defaults '(helm-wordnet-font-lock-keywords)))

(defun helm-wordnet--format-buffer ()
  "Format the entry buffer."
  (let ((inhibit-read-only t)
        (case-fold-search nil))
    ;; Delete the first empty line
    (goto-char (point-min))
    (delete-blank-lines)

    ;; Make headings
    (delete-matching-lines "^ +$" (point-min) (point-max))
    (while (re-search-forward
            (concat "^" (regexp-opt helm-wordnet-section-headings t)) nil t)
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

(defun helm-wordnet--persistent-action (word)
  "Display the meaning of WORD."
  (let ((buf (get-buffer-create "*WordNet*"))
        (options (apply #'concat
                        (-interpose " " helm-wordnet-cmd-options))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (shell-command-to-string (format "wn %s %s" word options))))
      (helm-wordnet--format-buffer)
      (set-buffer-modified-p nil)
      (unless (eq major-mode 'helm-wordnet-mode) (helm-wordnet-mode))
      (display-buffer buf)
      (other-window 1))))

(defvar helm-wordnet-source
  (helm-build-sync-source "WordNet"
    :candidates #'helm-wordnet--get-candidates
    :action '(("Dictionary" . helm-wordnet--persistent-action))
    :persistent-action #'helm-wordnet--persistent-action
    :pattern-transformer #'downcase
    :requires-pattern 1))

(defun helm-wordnet ()
  "Lookup WordNet definitions with Helm."
  (interactive)
  (helm :sources 'helm-wordnet-source
        :buffer "*helm wordnet*"
        :default (thing-at-point 'word)))


(provide 'helm-wordnet)

;;; helm-wordnet.el ends here
