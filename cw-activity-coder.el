;;;###autoload
(defun cw-activity-coder-add-files ()
  "Prompt to select a file and add it to the processing queue."
  (interactive)
  (let ((file
         (read-file-name "Select a CSV or JSON file: "
                         nil nil t nil
                         (lambda (f)
                           (or (string-suffix-p ".csv" f)
                               (string-suffix-p ".json" f))))))
    (when (and (file-regular-p file)
               (or (string-suffix-p ".csv" file)
                   (string-suffix-p ".json" file)))
      (push file cw-activity-coder-files-to-process)
      (message "Added %s to queue. Current queue (%d): %s"
               file
               (length cw-activity-coder-files-to-process)
               (string-join cw-activity-coder-files-to-process ", "))
      (transient-setup 'cw-activity-coder-menu))))

;;;###autoload
(defun cw-activity-coder-edit-queue ()
  "Edit the processing queue in a temporary buffer."
  (interactive)
  (if (null cw-activity-coder-files-to-process)
      (message "Queue is empty—nothing to edit.")
    (let ((buffer (get-buffer-create "*CW Activity Queue*")))
      (with-current-buffer buffer
        (erase-buffer)
        (insert
         "# Edit the queue below. Delete lines to remove files.\n")
        (insert
         "# Save with C-c C-c when done, or C-c C-k to cancel.\n\n")
        (dolist (file cw-activity-coder-files-to-process)
          (insert (format "%s\n" file)))
        (text-mode)
        (goto-char (point-min))
        (set-buffer-modified-p nil)
        (local-set-key
         (kbd "C-c C-c") #'cw-activity-coder--save-queue)
        (local-set-key
         (kbd "C-c C-k") #'cw-activity-coder--cancel-queue-edit))
      (switch-to-buffer buffer)
      (message
       "Delete lines to remove files. Save with C-c C-c, cancel with C-c C-k"))))

;;;###autoload
(transient-define-prefix
 cw-activity-coder-menu () "Menu for CW Activity Coder."
 :refresh-suffixes t
 ["CW Activity Coder" [("a" "Add File" cw-activity-coder-add-files)
   ("m" "Modify Queue" cw-activity-coder-edit-queue)]
  [("p" "Process Queued Files" cw-activity-coder-process-queued-files)
   ("r" "Show Receipt" cw-activity-coder-display-receipt)
   ("e" "Edit Activity Codes" cw-activity-coder-edit-codes)]
  [("c" "Clear Queue" cw-activity-coder-clear-queue)
   ("q" "Quit" transient-quit-one)]
  ["Queue" (lambda ()
     (format "Current queue (%d): %s"
             (length cw-activity-coder-files-to-process)
             (if cw-activity-coder-files-to-process
                 (string-join cw-activity-coder-files-to-process ", ")
               "empty")))]]
 (interactive) (transient-setup 'cw-activity-coder-menu))
