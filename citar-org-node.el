;;; citar-org-node.el --- Citar integration with org-node  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; URL: https://github.com/krisbalintona/citar-org-node
;; Keywords: tools
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))

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

;; Convenient integration between the citar UI and org-node.

;;; Code:
(require 'citar)

;;; Options
(defgroup citar-org-node nil
  "Integration between org-node and citar."
  :prefix "citar-org-node-"
  :group 'org)

(defcustom citar-org-node-new-node-title-template "${title}"
  "The citar formatting template for newly created nodes.")

;;; Variables
(defconst citar-org-node-notes-config
  (list :name "Org-node Notes"
        :category 'org-node-node
        :hasitems #'citar-org-node-has-notes
        :items #'citar-org-node--get-candidates
        :open #'citar-org-node-open-note
        :create #'citar-org-node--create-capture-note)
  "Org-node configuration for citar notes backend.
See `citar-notes-sources' for more details on configuration keys.")

;;; Functions
(defun citar-org-node--get-citekey-refs (&optional citekeys)
  "Return `org-node--ref-path<>ref-type' with only citekeys.
`org-node--ref-path<>ref-type' stores refs of any type (e.g., citekeys,
https).  This function removes (non-destructively) non-citekey pairs
from the hash table, returning the result.

The optional argument CITEKEYS should be a list of org-node
ref-paths (i.e. citekeys).  If non-nil, only the keys-value pairs whose
keys are in this list will be included in the final hash table."
  (when (and citekeys (not (listp citekeys)))
    (error "CITEKEYS should be a list"))
  (let ((filtered-hash-table (make-hash-table :test #'equal)))
    (if citekeys
        (maphash (lambda (ref-path ref-type)
                   (when (member ref-path citekeys)
                     (puthash ref-path ref-type filtered-hash-table)))
                 org-node--ref-path<>ref-type)
      (setq filtered-hash-table org-node--ref-path<>ref-type))
    (ht-select (lambda (ref-path ref-type)
                 (string-equal ref-type (concat "@" ref-path)))
               filtered-hash-table)))

(defun citar-org-node--get-candidates (&optional citekeys)
  "Return hash table mapping of CITEKEYS to completion candidates.
Return hash table whose CITEKEYS are elements of CITEKEYS and values are
the propertized candidate used for completion.

If CITEKEYS is nil, then return a hash table for all existent CITEKEYS
with their files.

See `citar-file--get-notes' for an example implementation.

See also `citar-org-node-notes-config'."
  (let ((node-info (ht-map (lambda (ref-path ref-type)
                             (let* ((id (gethash ref-type org-node--ref<>id))
                                    (node (gethash id org-node--id<>node))
                                    (title (org-node-get-title node)))
                               ;; Final list elements are:
                               (list id ref-path title)))
                           (citar-org-node--get-citekey-refs citekeys)))
        (cands (make-hash-table :test #'equal)))
    (pcase-dolist (`(,id ,citekey ,title) node-info)
      (push
       (concat
        (propertize id 'invisible t) " ["
        (propertize citekey 'face 'citar-highlight)
        (truncate-string-to-width "] " (- 60 (length citekey)) nil 32)
        (propertize title 'face 'citar))
       (gethash citekey cands)))
    cands))

(defun citar-org-node-has-notes ()
  "Return function to check for notes.
The returned function, when given a citekey, will return non-nil if
there's an associated note.

See also `citar-org-node-notes-config'."
  (let ((hasnotes (make-hash-table :test 'equal))
        (citekeys (hash-table-keys (citar-org-node--get-citekey-refs))))
    (dolist (citekey citekeys)
      (puthash citekey t hasnotes))
    (lambda (citekey)
      (gethash citekey hasnotes))))

(defun citar-org-node-open-note (candidate-string)
  "Open org-node node for CANDIDATE-STRING.
CANDIDATE-STRING is the completion candidate returned by
`citar-org-node--get-candidates'.

See also `citar-org-node-notes-config'."
  ;; We get the ID because, according to the return value of
  ;; `citar-org-node--get-candidates', it is the set of characters before the
  ;; first space
  (let ((id (substring-no-properties
             (car (split-string candidate-string)))))
    (org-node--goto (org-node-by-id id))))

(defun citar-org-node-add-ref (citekey)
  "Add CITEKEY to the nearest relevant property drawer.
CITEKEY will be the value of the \"ROAM_REFS\" property.

If called interactively, select CITEKEY using `citar-select-refs'."
  (interactive (list (car (citar-select-refs))) org-mode)
  (org-node--add-to-property-keep-space "ROAM_REFS" (concat "@" citekey)))

(defun citar-org-node--create-capture-note (citekey entry)
  "Open or create org-node node for CITEKEY and ENTRY.
This function calls `org-node-capture-target' specially by setting
`org-node-proposed-title' and `org-node-proposed-id'.  (Both are used
and required by `org-node-capture-target'.)

To configure the title of the new org-node node, set
`citar-org-node-new-node-title-template'."
  (let* ((org-node-proposed-title
          (citar-format--entry citar-org-node-new-node-title-template entry))
         (org-node-proposed-id (org-id-new)))
    (org-node-capture-target)
    (citar-org-node-add-ref citekey)))

;;; Minor mode
(defvar citar-org-node--orig-source citar-notes-source)

(defun citar-org-node--setup ()
  "Register and select the citar-org-node notes backend."
  (citar-register-notes-source 'citar-org-node citar-org-node-notes-config)
  (setq citar-notes-source 'citar-org-node))

(defun citar-org-node--teardown ()
  "Restore citar notes backend to what is what before."
  (setq citar-notes-source citar-org-node--orig-source)
  (citar-remove-notes-source 'citar-org-node))

;;;###autoload
(define-minor-mode citar-org-node-mode
  "Toggle org-node integration with citar."
  :global t
  :group 'org-node
  :lighter " citar-org-node"
  (if citar-org-node-mode
      (citar-org-node--setup)
    (citar-org-node--teardown)))

;;; Provide
(provide 'citar-org-node)
;;; citar-org-node.el ends here
