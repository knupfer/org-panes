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

;; This file provides a customizable `org-panes' function, which turns
;; an org-file into a three pane view.  Thereby your movement of point
;; is reflected in all panes and you can all three panes to navigate.
;; In the overview and contents pane, the visible part of the show all
;; pane is highlighted.  Visiting a non related buffer will
;; automatically kill the panes and clean up.  See following
;; screenshot: http://i.imgur.com/jwKpKzp.png


;;; Code:

(defgroup org-panes nil
  "show multiple panes of the document"
  :group 'org-mode)

(defcustom org-panes-overview-depth 1
  "Number of levels displayed in the overview buffer."
  :group 'org-panes
  :type 'integer)

(defcustom org-panes-contents-depth 10
  "Number of levels displayed in the contents buffer."
  :group 'org-panes
  :type 'integer)

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

(defface org-panes-highlight-point-face
  '((t (:weight ultrabold :foreground "red")))
  "Face used for highlighting star at point."
  :group 'org-panes)

(defvar org-panes-all nil)
(defvar org-panes-contents nil)
(defvar org-panes-overview nil)

(defun org-panes ()
  "Make different panes for an org-mode file.  Current point is
shared between the buffers and the visible part in the show all
buffer is highlighted in the contents and overview buffer."
  (interactive)
  (if (not (equal major-mode 'org-mode))
      (error "this is not an org file")
    (if (not org-panes-all)
        (progn
          (delete-other-windows)
          (let ((size (window-body-width)))
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
            (hide-sublevels org-panes-contents-depth)
            (setq-local cursor-in-non-selected-windows nil)
            (org-panes--make-overlay)
            (clone-indirect-buffer org-panes-overview t)
            (hide-sublevels org-panes-overview-depth)
            (setq-local cursor-in-non-selected-windows nil)
            (org-panes--make-overlay)
            (other-window -1)
            (show-all)
            (setq-local cursor-in-non-selected-windows nil))
          (add-hook 'post-command-hook 'org-panes-move-point)
          (redisplay)
          (message "org-panes created"))
      (org-panes-stop-panes))))

(defun org-panes-stop-panes ()
  "Kill all panes and clean up."
  (when (and org-panes-all (get-buffer org-panes-all))
    (let ((win (get-buffer-window org-panes-all)))
      (when win (with-selected-window win
                  (org-panes--remove-overlay)))))
  (when (and org-panes-contents (get-buffer org-panes-contents))
    (let ((win (get-buffer-window org-panes-contents)))
      (when win (with-selected-window win
                  (org-panes--remove-overlay))))
    (kill-buffer org-panes-contents))
  (when (and org-panes-overview (get-buffer org-panes-overview))
    (let ((win (get-buffer-window org-panes-overview)))
      (when win (with-selected-window win
                  (org-panes--remove-overlay))))
    (kill-buffer org-panes-overview))
  (setq org-panes-overview nil)
  (setq org-panes-contents nil)
  (setq org-panes-all nil)
  (delete-other-windows)
  (remove-hook 'post-command-hook 'org-panes-move-point)
  (message "org-panes killed"))

(defun org-panes-move-point ()
  "Share point and highlight."
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
          (when org-panes-all (with-selected-window org-panes-all
                                (setq org-panes-min (window-start))
                                (save-excursion
                                  (goto-char org-panes-min)
                                  (beginning-of-line)
                                  (forward-line (window-body-height))
                                  (setq org-panes-max (1- (point))))
                                (org-panes--remove-overlay)
                                (org-panes-center)))
          (when org-panes-contents (with-selected-window org-panes-contents
                                     (org-panes--remove-overlay)
                                     (org-panes-center)
                                     (org-panes--make-overlay)))
          (when org-panes-overview (with-selected-window org-panes-overview
                                     (org-panes--remove-overlay)
                                     (org-panes-center)
                                     (org-panes--make-overlay))))
      (org-panes-stop-panes))))

(defun org-panes-center ()
  (let ((len 0)
        (ov (make-overlay 0 0))
        (pos -1))
    (save-excursion (goto-char (point-min))
                    (while (> (point) pos)
                      (setq len (+ len 1)
                            pos (point))
                      (forward-visible-line 1)))
    (overlay-put ov 'category 'org-panes-highlight)
    (when (< len (window-body-height))
      (setq len (/ (- (window-body-height) len) 2))
      (overlay-put ov 'before-string (make-string len (string-to-char "\n"))))))

(defun org-panes--make-overlay ()
  "Put the different overlays for highlighting."
  (save-excursion
    (let (a b p)
      (move-beginning-of-line nil)
      (re-search-forward " " nil t)
      (setq p (point))
      (move-beginning-of-line nil)
      (setq a (min p org-panes-min))
      (setq b (max p org-panes-max))
      (let ((ov (make-overlay (- p 2) (- p 1))))
        (overlay-put ov 'category 'org-panes-highlight)
        (overlay-put ov 'face 'org-panes-highlight-point-face))
      (forward-char 5);;aea tia
      (when (re-search-backward "^* " nil t)
        (let ((pos (point)))
          (goto-char (point-min))
          (while (re-search-forward "^*+ " pos t)
            (let ((ov (make-overlay (point) (progn (end-of-line)
                                                   (point)))))
              (overlay-put ov 'category 'org-panes-highlight)
              (overlay-put ov 'face 'font-lock-comment-face)))))
      (let ((ov (make-overlay a b)))
        (overlay-put ov 'category 'org-panes-highlight)
        (overlay-put ov 'face 'org-panes-highlight-face))
      (when (and (re-search-forward "^* " nil t)
                 (re-search-forward "^* " nil t))
        (move-beginning-of-line nil)
        (let ((pos (point-max)))
          (while (re-search-forward "^*+ " pos t)
            (let ((ov (make-overlay (point) (progn (end-of-line)
                                                   (point)))))
              (overlay-put ov 'category 'org-panes-highlight)
              (overlay-put ov 'face 'font-lock-comment-face))))))))

(defun org-panes--remove-overlay ()
  "Delete all overlays in current buffer."
  (dolist (ov (org-panes--active-overlays))
    (delete-overlay ov)))

(defun org-panes--active-overlays ()
  "Collect all overlays in current buffer."
  (let ((del-from (point-min))
        (del-to (point-max)))
    (delq nil (mapcar (lambda (ov)
                        (and (eq (overlay-get ov 'category)
                                 'org-panes-highlight)
                             ov))
                      (overlays-in del-from del-to)))))

(provide 'org-panes)

;;; org-panes.el ends here
