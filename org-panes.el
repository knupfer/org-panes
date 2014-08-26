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
;; screenshots:
;; http://i.imgur.com/IHpX57b.png
;; http://i.imgur.com/Zrhc7lG.png


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

(defcustom org-panes-split-overview-horizontally nil
  "Change the split behaviour of the overview and the contents
buffer.  The show all buffer is always split vertically because
of the bigger space needs."
  :group 'org-panes
  :type 'boolean)

(defcustom org-panes-force-centering-text-vertically t
  "When non-nil adds padding to overview and contents buffer to
be able to center point even when there aren't enough headings."
  :group 'org-panes
  :type 'boolean)

(defcustom org-panes-persist-panes t
  "When non-nil create automatically new panes when revisiting
buffer unless org-panes is called another time."
  :group 'org-panes
  :type 'boolean)

(defcustom org-panes-main-size 50
  "Percentage of the frame width used for the show all buffer."
  :group 'org-panes
  :type 'integer)

(defcustom org-panes-timer-intervall 0.15
  "Seconds after which overlays are redrawn and buffers moved.
This value greatly influences responsiveness and ressource
consumption."
  :group 'org-panes
  :type 'integer)

(defcustom org-panes-contents-size 60
  "Percentage of the remaining frame width/height used for the
contents buffer."
  :group 'org-panes
  :type 'integer)

(defface org-panes-hide-tree-face
  '((t (:inherit font-lock-comment-face)))
  "Face used for not used overview structures."
  :group 'org-panes)

(defface org-panes-highlight-window-face
  '((t (:inherit error)))
  "Face used for highlighting stars to indicate visible region."
  :group 'org-panes)

(defvar org-panes-change-string nil)
(defvar org-panes-min)
(defvar org-panes-max)
(defvar org-panes-timer nil)

(defun org-panes ()
  "Make different panes for an org-mode file.  Current point is
shared between the buffers and the visible part in the show all
buffer is highlighted in the contents and overview buffer."
  (interactive)
  (if (not (equal major-mode 'org-mode))
      (error "this is not an org file")
    (if (not org-panes-list)
        (progn
          (delete-other-windows)
          (let ((size (window-body-width))
                (height (window-body-height)))
            (setq org-panes-list (let ((b (buffer-name)))
                                   (list (concat b ":OVERVIEW")
                                         (concat b ":CONTENTS")
                                         b)))
            (setq org-panes-min (window-start)
                  org-panes-max (window-end))
            (save-excursion
              (goto-char org-panes-min)
              (beginning-of-line)
              (forward-line (window-body-height))
              (setq org-panes-max (1- (point))))
            (split-window-right (/ (* size org-panes-main-size) -100))
            (if org-panes-split-overview-horizontally
                (split-window-below)
              (split-window-right (/ (* size
                                        org-panes-contents-size) -200)))
            (other-window -1)
            (clone-indirect-buffer (nth 1 org-panes-list) t)
            (hide-sublevels org-panes-contents-depth)
            (setq-local cursor-in-non-selected-windows nil)
            (org-panes--make-overlay)
            (clone-indirect-buffer (nth 0 org-panes-list) t)
            (hide-sublevels org-panes-overview-depth)
            (if org-panes-split-overview-horizontally
                (window-resize nil (+ (/ (* height org-panes-contents-size)
                                         -100) (/ height 2))))
            (setq-local cursor-in-non-selected-windows nil)
            (org-panes--make-overlay)
            (other-window -1)
            (show-all)
            (setq-local cursor-in-non-selected-windows nil)
            (when org-panes-persist-panes
              (add-hook 'post-command-hook 'org-panes-persist nil t)))
          (setq org-panes-timer
                (run-with-idle-timer org-panes-timer-intervall t 'org-panes-move-point))
          (message "org-panes created"))
      (remove-hook 'post-command-hook 'org-panes-persist t)
      (org-panes-stop-panes)
      (when org-panes-persist-panes
        (message "org-panes killed...")))))

(defun org-panes-persist ()
  (when (not org-panes-list)
    (remove-hook 'post-command-hook 'org-panes-persist t)
    (org-panes)))

(defun org-panes-stop-panes ()
  "Kill all panes and clean up."
  (while (let ((buf (pop org-panes-list)))
           (when (and buf (get-buffer buf))
             (let ((win (get-buffer-window buf)))
               (when win (with-selected-window win
                           (org-panes--remove-overlay)))))
           (when (car org-panes-list)
             (kill-buffer buf) t)))
  (delete-other-windows)
  (cancel-timer org-panes-timer)
  (unless org-panes-persist-panes
    (message "org-panes killed...")))

(defun org-panes-move-point ()
  "Share point and highlight."
  (unless (active-minibuffer-window)
    (if (member (buffer-name) org-panes-list)
        (let ((old-win (selected-window)))
          (when (org-panes-changed-p)
            (catch 'exit
              (let ((pos (point))
                    (win-list (map 'list 'get-buffer-window org-panes-list))
                    (win (get-buffer-window)))
                (save-excursion (move-beginning-of-line nil)
                                (setq pos (point)))
                (mapc (lambda (x) (when (and x (not (equal x win)))
                                    (with-selected-window x
                                      (goto-char pos)
                                      (move-beginning-of-line nil)
                                      (when (equal x (nth 2 win-list))
                                        (set-window-start nil (point))))))
                      win-list)
                (when (input-pending-p) (throw 'exit t))
                (when (nth 2 win-list) (with-selected-window (nth 2 win-list)
                                         (setq org-panes-min (window-start))
                                         (save-excursion
                                           (goto-char org-panes-min)
                                           (beginning-of-line)
                                           (forward-line (window-body-height))
                                           (setq org-panes-max (1- (point))))))
                (when (nth 1 win-list)
                  (with-selected-window (nth 1 win-list)
                    (org-panes--remove-overlay)
                    (org-panes-center org-panes-contents-depth)
                    (let  ((pos (org-panes--make-overlay)))
                      (recenter (round (- (min (* 0.5 (window-body-height)
                                                  (/ (float (car pos))
                                                     (cadr pos))) (car pos))
                                          (/ (+ (window-body-height)
                                                (min (* 0.5 (window-body-height))
                                                     (cadr pos))) 2)))))))
                (when (nth 0 win-list) (with-selected-window (nth 0 win-list)
                                         (recenter)
                                         (org-panes--remove-overlay)
                                         (org-panes-center org-panes-overview-depth)
                                         (org-panes--make-overlay))))))
          (select-window old-win))
      (when (not (equal "*Org Src"
                        (substring (buffer-name) 0
                                   (min (length (buffer-name)) 8))))
        (org-panes-stop-panes)))))

(defun org-panes-changed-p ()
  (let ((old-string org-panes-change-string))
    (if (equal (buffer-name) (nth 2 org-panes-list))
        (save-excursion
          (end-of-line)
          (let ((p (point)))
            (setq org-panes-change-string nil)
            (goto-char (window-start))
            (while (re-search-forward "^\\(*+\\) ..." (max (point)
                                                           (window-end)) t)
              (setq org-panes-change-string
                    (concat org-panes-change-string (match-string 0)
                            (when (and (= (length (match-string 1)) 1)
                                       (> (match-end 0) p)) "P")))))
          (when org-panes-change-string
            (setq org-panes-change-string
                  (substring-no-properties org-panes-change-string))))
      (setq org-panes-change-string (line-number-at-pos)))
    (unless (equal old-string org-panes-change-string) t)))

(defun org-panes-center (limit)
  (when org-panes-force-centering-text-vertically
    (let ((len 0)
          (ov (make-overlay 0 0)))
      (save-excursion (goto-char (point-min))
                      (while (re-search-forward "^*+ " nil t)
                        (when (< (- (match-end 0)
                                    (match-beginning 0) 2)
                                 limit)
                          (setq len (+ len 1)))))
      (overlay-put ov 'category 'org-panes-highlight)
      (when (< len (window-body-height))
        (setq len (/ (- (window-body-height) len) 2))
        (overlay-put ov 'before-string
                     (make-string len (string-to-char "\n")))))))

(defun org-panes--make-overlay ()
  "Put the different overlays for highlighting and return size of
overview tree."
  (save-excursion
    (let ((a (min (point) org-panes-min))
          (b (max (point) org-panes-max))
          (tree-start (point-min))
          (tree-end (point-max))
          (tree-size 0)
          (point-pos 0)
          (old-point (point)))
      (end-of-line)
      (re-search-backward "^* " nil t)
      (setq tree-start (point))
      (when (and (re-search-forward "^* " nil t)
                 (re-search-forward "^* " nil t))
        (setq tree-end (progn (beginning-of-line)
                              (point))))
      (goto-char (point-min))
      (while (re-search-forward "^*+ " nil t)
        (let ((p (point)))
          (when (and (> p a) (< p b))
            (let ((ov (make-overlay (- p 2) (- p 1))))
              (overlay-put ov 'category 'org-panes-highlight)
              (overlay-put ov 'face 'org-panes-highlight-window-face)))
          (if (or (< p tree-start)
                  (> p tree-end))
              (let ((ov (make-overlay p (progn (end-of-line)
                                               (point)))))
                (overlay-put ov 'category 'org-panes-highlight)
                (overlay-put ov 'face 'org-panes-hide-tree-face))
            (setq tree-size (+ 1 tree-size))
            (when (< p old-point)
              (setq point-pos (+ 1 point-pos))))))
      (list point-pos tree-size))))

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
