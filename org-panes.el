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

(defvar all nil)
(defvar contents nil)
(defvar overview nil)

(defun org-make-panes ()
  (interactive)
  (if (not (equal major-mode 'org-mode))
      (error "this is not an org file")
    (if (not all)
        (progn
          (delete-other-windows)
          (let* ((size (window-body-width)))
            (setq all (buffer-name)
                  contents (concat all ":CONTENTS")
                  overview (concat all ":OVERVIEW"))
            (setq org-panes-min (window-start)
                  org-panes-max (window-end))
            (save-excursion
              (goto-char org-panes-min)
              (beginning-of-line)
              (forward-line (window-body-height))
              (setq org-panes-max (1- (point))))
            (split-window-right (/ size -2))
            (split-window-right (/ (* size 2) -7))
            (other-window -1)
            (clone-indirect-buffer contents t)
            (org-content)
            (org-panes--make-overlay)
            (clone-indirect-buffer overview t)
            (org-overview)
            (org-panes--make-overlay)
            (other-window -1)
            (show-all))
          (add-hook 'post-command-hook 'org-panes-move-point)
          (redisplay)
          (message "org-panes created"))
      (org-panes-stop-panes))))

(defun org-panes-stop-panes ()
  (when (and contents (get-buffer contents))
    (kill-buffer contents)
    (setq contents nil))
  (when (and overview (get-buffer overview))
    (kill-buffer overview)
    (setq overview nil))
  (setq all nil)
  (delete-other-windows)
  (remove-hook 'post-command-hook 'org-panes-move-point)
  (message "org-panes killed"))

(defun org-panes-move-point ()
  (unless (active-minibuffer-window)
    (if (or (equal (buffer-name) all)
            (equal (buffer-name) contents)
            (equal (buffer-name) overview))
        (let ((pos (point))
              (all (get-buffer-window all))
              (overview (get-buffer-window overview))
              (contents (get-buffer-window contents)))
          (save-excursion (move-beginning-of-line nil)
                          (setq pos (point)))
          (when (and all (not (equal all (get-buffer-window))))
            (progn
              (with-selected-window all
                (goto-char pos)
                (move-beginning-of-line nil)
                (set-window-start nil (point)))))
          (when (and overview (not (equal overview (get-buffer-window))))
            (with-selected-window overview
              (goto-char pos)
              (move-beginning-of-line nil)
              (recenter)))
          (when (and contents (not (equal contents (get-buffer-window))))
            (with-selected-window contents
              (goto-char pos)
              (move-beginning-of-line nil)
              (recenter)))
          (redisplay)
          (when all (with-selected-window all
                      (setq org-panes-min (window-start))
                      (save-excursion
                        (goto-char org-panes-min)
                        (beginning-of-line)
                        (forward-line (window-body-height))
                        (setq org-panes-max (1- (point))))))
          (when contents (with-selected-window contents
                           (org-panes--remove-overlay)
                           (org-panes--make-overlay)))
          (when overview (with-selected-window overview
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
      (let ((ov (make-overlay a b)))
        (overlay-put ov 'category 'org-pane-highlight)
        (overlay-put ov 'face '(:background "#500"))))))

(defun org-panes--remove-overlay ()
  (dolist (ov (org-panes--active-overlays))
    (delete-overlay ov)))

(defun org-panes--active-overlays ()
  (let ((del-from (point-min))
        (del-to (point-max)))
    (delq nil (mapcar (lambda (ov)
                        (and (eq (overlay-get ov 'category)
                                 'org-pane-highlight)
                             ov))
                      (overlays-in del-from del-to)))))

;;; org-panes.el ends here
