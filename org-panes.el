;;; org-panes.el --- show an org-file in three panes

;; Copyright (C) 2014 Florian Knupfer

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: Florian Knupfer

;;; Code:

(defgroup org-panes nil
  "show multiple panes of the document"
  :group 'org-mode)

(defcustom org-panes-main-size 50
  "Percentage of the frame width used for the show all buffer."
  :group 'org-panes
  :type 'integer)

(defcustom org-panes-contents-size 30
  "Percentage of the frame width used for the contents buffer."
  :group 'org-panes
  :type 'integer)

(defface org-panes-highlight-face
  '((t (:weight ultrabold)))
  "Face used for highlighted overview structures."
  :group 'org-panes)

(defface org-panes-non-highlight-face
  '((t (:weight ultralight)))
  "Face used for not used overview structures."
  :group 'org-panes)

(defvar org-panes-all nil)
(defvar org-panes-contents nil)
(defvar org-panes-overview nil)

(defun org-make-panes ()
  (interactive)
  (if (not (equal major-mode 'org-mode))
      (error "this is not an org file")
    (if (not org-panes-all)
        (progn
          (delete-other-windows)
          (let* ((size (window-body-width)))
            (setq org-panes-all (buffer-name)
                  org-panes-contents (concat org-panes-all ":CONTENTS")
                  org-panes-overview (concat org-panes-all ":OVERVIEW"))
            (setq org-panes-min (window-start)
                  org-panes-max (window-end))
            (save-excursion
              (goto-char org-panes-min)
              (beginning-of-line)
              (forward-line (window-body-height))
              (setq org-panes-max (1- (point))))
            (split-window-right (/ (* size org-panes-main-size) -100))
            (split-window-right (/ (* size org-panes-contents-size) -100))
            (other-window -1)
            (clone-indirect-buffer org-panes-contents t)
            (org-content)
            (org-panes--make-overlay)
            (clone-indirect-buffer org-panes-overview t)
            (org-overview)
            (org-panes--make-overlay)
            (other-window -1)
            (show-all))
          (add-hook 'post-command-hook 'org-panes-move-point)
          (redisplay)
          (message "org-panes created"))
      (org-panes-stop-panes))))

(defun org-panes-stop-panes ()
  (when (and org-panes-contents (get-buffer org-panes-contents))
    (kill-buffer org-panes-contents)
    (setq org-panes-contents nil))
  (when (and org-panes-overview (get-buffer org-panes-overview))
    (kill-buffer org-panes-overview)
    (setq org-panes-overview nil))
  (setq org-panes-all nil)
  (delete-other-windows)
  (remove-hook 'post-command-hook 'org-panes-move-point)
  (message "org-panes killed"))

(defun org-panes-move-point ()
  (unless (active-minibuffer-window)
    (if (or (equal (buffer-name) org-panes-all)
            (equal (buffer-name) org-panes-contents)
            (equal (buffer-name) org-panes-overview))
        (let ((pos (point))
              (org-panes-all (get-buffer-window org-panes-all))
              (org-panes-overview (get-buffer-window org-panes-overview))
              (org-panes-contents (get-buffer-window org-panes-contents)))
          (save-excursion (move-beginning-of-line nil)
                          (setq pos (point)))
          (when (and org-panes-all
                     (not (equal org-panes-all (get-buffer-window))))
            (progn
              (with-selected-window org-panes-all
                (goto-char pos)
                (move-beginning-of-line nil)
                (set-window-start nil (point)))))
          (when (and org-panes-overview
                     (not (equal org-panes-overview (get-buffer-window))))
            (with-selected-window org-panes-overview
              (goto-char pos)
              (move-beginning-of-line nil)
              (recenter)))
          (when (and org-panes-contents
                     (not (equal org-panes-contents (get-buffer-window))))
            (with-selected-window org-panes-contents
              (goto-char pos)
              (move-beginning-of-line nil)
              (recenter)))
          (redisplay)
          (when org-panes-all (with-selected-window org-panes-all
                                (setq org-panes-min (window-start))
                                (save-excursion
                                  (goto-char org-panes-min)
                                  (beginning-of-line)
                                  (forward-line (window-body-height))
                                  (setq org-panes-max (1- (point))))))
          (when org-panes-contents (with-selected-window org-panes-contents
                                     (org-panes--remove-overlay)
                                     (org-panes--make-overlay)))
          (when org-panes-overview (with-selected-window org-panes-overview
                                     (org-panes--remove-overlay)
                                     (org-panes--make-overlay))))
      (org-panes-stop-panes))))

(defun org-panes--make-overlay ()
  (save-excursion
    (goto-char org-panes-min)
    (move-beginning-of-line nil)
    (let (a b)
      (setq a (point))
      (setq b org-panes-max)
      (let ((ov (make-overlay (point-min) a)))
        (overlay-put ov 'category 'org-panes-highlight)
        (overlay-put ov 'face '(:weight ultralight)))
      (let ((ov (make-overlay a b)))
        (overlay-put ov 'category 'org-panes-highlight)
        (overlay-put ov 'face '(:weight ultrabold)))
      (let ((ov (make-overlay b (point-max))))
        (overlay-put ov 'category 'org-panes-highlight)
        (overlay-put ov 'face '(:weight ultralight))))))

(defun org-panes--remove-overlay ()
  (dolist (ov (org-panes--active-overlays))
    (delete-overlay ov)))

(defun org-panes--active-overlays ()
  (let ((del-from (point-min))
        (del-to (point-max)))
    (delq nil (mapcar (lambda (ov)
                        (and (eq (overlay-get ov 'category)
                                 'org-panes-highlight)
                             ov))
                      (overlays-in del-from del-to)))))

(provide 'org-panes)

;;; org-panes.el ends here
