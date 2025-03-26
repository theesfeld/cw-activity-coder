;;; cw-activity-coder.el --- Process files with xAI API to assign CW activity codes -*- lexical-binding: t; -*-

;; Author: William Theesfeld <william@theesfeld.net>
;; Version: 0.4.7
;; Package-Version: 0.4.7
;; Package-Requires:
;;   ((emacs "30.1")
;;    (json "1.4")
;;    (url "1.0")
;;    (transient "0.3")
;;    (org "9.6")
;;    (json-mode "1.8.3"))
;; Keywords: tools, api, data-processing
;; URL: https://github.com/theesfeld/cw-activity-coder

;;; Commentary:
;; This package processes CSV/JSON files with the xAI API to assign CW activity
;; codes. Features include a Transient menu with queue display, live Org-mode
;; output, and file selection. Requires an XAI_API_KEY environment variable.
;; Use `M-x cw-activity-coder` to start.

;;; Code:

(require 'json)
(require 'url)
(require 'org)
(require 'transient)
(require 'json-mode)

(defgroup cw-activity-coder nil
  "Customization group for CW Activity Coder."
  :group 'tools)

(defcustom cw-activity-coder-api-key (getenv "XAI_API_KEY")
  "API key for xAI API, fetched from XAI_API_KEY environment variable."
  :type 'string
  :risky t
  :group 'cw-activity-coder)

(defcustom cw-activity-coder-model "grok-2-latest"
  "Model to use with xAI API."
  :type 'string
  :group 'cw-activity-coder)

(defcustom cw-activity-coder-batch-size 100
  "Number of rows per batch."
  :type 'integer
  :group 'cw-activity-coder)

(defcustom cw-activity-coder-rate-limit 2.0
  "Maximum API calls per second."
  :type 'float
  :group 'cw-activity-coder)

(defcustom cw-activity-coder-max-retries 3
  "Maximum retries for failed API requests."
  :type 'integer
  :group 'cw-activity-coder)

(defcustom cw-activity-coder-api-timeout 300
  "Timeout in seconds for API requests."
  :type 'integer
  :group 'cw-activity-coder)

(defcustom cw-activity-coder-output-dir
  (expand-file-name "~/cw-activity-coder-output/")
  "Directory for output CSV files and activity codes."
  :type 'directory
  :group 'cw-activity-coder)

(defcustom cw-activity-coder-activity-codes-file
  (expand-file-name "activitycodes.json" cw-activity-coder-output-dir)
  "File for persistent activity codes."
  :type 'file
  :group 'cw-activity-coder)

(defvar cw-activity-coder-session-stats
  (list
   :files nil
   :total-duration 0
   :total-prompt-tokens 0
   :total-completion-tokens 0
   :fingerprints (make-hash-table :test 'equal)
   :response-times nil)
  "Session statistics for the current run.")

(defvar cw-activity-coder-files-to-process nil
  "List of files queued for processing.")

(defvar cw-activity-coder-output-buffer "*CW Activity Coder Output*"
  "Buffer for live processing output.")

(defvar cw-activity-coder-progress nil
  "Current processing progress: (current . total).")

(defvar cw-activity-coder-last-request-time 0
  "Timestamp of the last API request for rate limiting.")

(defvar cw-activity-coder-activity-codes nil
  "In-memory activity codes, loaded from file.")

(defun cw-activity-coder--ensure-api-key ()
  "Ensure XAI_API_KEY is set, or signal an error."
  (unless cw-activity-coder-api-key
    (error
     "XAI_API_KEY not set. Set it with 'export XAI_API_KEY=your-key' in your shell")))

(defun cw-activity-coder--generate-ref (filename index)
  "Generate a unique reference ID from FILENAME and INDEX."
  (let ((file-hash (substring (secure-hash 'md5 filename) 0 8)))
    (format "%s-%d" file-hash index)))

(defun cw-activity-coder--read-json-file (file)
  "Read and parse JSON from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (condition-case err
        (json-parse-buffer :object-type 'alist)
      (error (error "Failed to parse JSON in %s: %s" file err)))))

(defun cw-activity-coder--write-json-file (file data)
  "Write DATA to FILE as JSON."
  (with-temp-file file
    (insert (json-encode data))))

(defun cw-activity-coder--load-activity-codes ()
  "Load activity codes from file or initialize with base if missing."
  (unless cw-activity-coder-activity-codes
    (unless (file-directory-p cw-activity-coder-output-dir)
      (make-directory cw-activity-coder-output-dir t))
    (if (file-exists-p cw-activity-coder-activity-codes-file)
        (setq cw-activity-coder-activity-codes
              (cw-activity-coder--read-json-file
               cw-activity-coder-activity-codes-file))
      (let ((base-file
             (expand-file-name "activitycodes.json"
                               (file-name-directory
                                (or load-file-name
                                    buffer-file-name)))))
        (if (file-exists-p base-file)
            (progn
              (setq cw-activity-coder-activity-codes
                    (cw-activity-coder--read-json-file base-file))
              (cw-activity-coder--write-json-file
               cw-activity-coder-activity-codes-file
               cw-activity-coder-activity-codes))
          (error
           "Base activitycodes.json not found in package directory"))))))

(defun cw-activity-coder--read-data-file (file)
  "Read data from FILE (CSV/JSON) and return a list of records."
  (cond
   ((string-suffix-p ".json" file)
    (cw-activity-coder--read-json-file file))
   ((string-suffix-p ".csv" file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((lines (split-string (buffer-string) "\n" t))
            headers
            data)
        (setq headers (split-string (car lines) "," t))
        (dolist (line (cdr lines))
          (when (string-match "[^[:space:]]" line)
            (let ((values (split-string line "," t)))
              (push (cl-mapcar #'cons headers values) data))))
        (nreverse data))))
   (t
    (error "Unsupported file format: %s" file))))

(defun cw-activity-coder--build-system-prompt (activity-codes)
  "Build the system prompt with ACTIVITY-CODES for xAI API processing.
  Rules and definitions guide code assignment."
  (format
   (concat
    "Process the provided JSON array of objects. For each object, "
    "determine the appropriate activity code based on all its fields "
    "and the definitions below. Return the assigned 'cw_at' code along "
    "with the object's 'ref' field.\n\n"
    "Rules:\n- Use 'code', 'description', and 'type_of_work' from "
    "definitions to infer the code based on the full row data.\n"
    "- For 'CB' prefixed situations:\n  - Use 'CB-EMG' if person(s) "
    "trapped in elevator is indicated in any field.\n  - Otherwise, "
    "use specific CB-XXX (e.g., 'CB-EF', 'CB-MU'), defaulting to "
    "'CB-EF' if unclear.\n- Use 'NDE' if no code fits.\n\n"
    "Activity Code Definitions (JSON):\n%s")
   (json-encode activity-codes)))

(defun cw-activity-coder--rate-limit-wait ()
  "Wait to enforce rate limiting based on `cw-activity-coder-rate-limit'."
  (let* ((now (float-time))
         (interval (/ 1.0 cw-activity-coder-rate-limit))
         (elapsed (- now cw-activity-coder-last-request-time)))
    (when (< elapsed interval)
      (sleep-for (- interval elapsed)))
    (setq cw-activity-coder-last-request-time (float-time))))

(defun cw-activity-coder--api-request
    (payload _ batch-num total-batches callback)
  "Send PAYLOAD to xAI API asynchronously for batch BATCH-NUM of 
  TOTAL-BATCHES, calling CALLBACK."
  (cw-activity-coder--rate-limit-wait)
  (let* ((url "https://api.x.ai/v1/chat/completions")
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" .
             ,(concat "Bearer " cw-activity-coder-api-key))))
         (url-request-data (json-encode payload))
         (start-time (float-time)))
    (url-retrieve
     url
     (lambda (status &rest _args)
       (let ((duration (- (float-time) start-time)))
         (if (plist-get status :error)
             (if (< (or (plist-get status :retry-count) 0)
                    cw-activity-coder-max-retries)
                 (progn
                   (message
                    "API request failed for batch %d/%d, retrying (%d/%d)..."
                    batch-num
                    total-batches
                    (1+ (or (plist-get status :retry-count) 0))
                    cw-activity-coder-max-retries)
                   (sleep-for 1)
                   (cw-activity-coder--api-request
                    (plist-put
                     payload
                     :retry-count
                     (1+ (or (plist-get status :retry-count) 0)))
                    _ batch-num total-batches callback))
               (error
                "API request failed after %d retries: %s"
                cw-activity-coder-max-retries
                (plist-get status :error)))
           (goto-char (point-min))
           (re-search-forward "\n\n" nil t)
           (let* ((response (json-parse-buffer :object-type 'alist))
                  (choices (alist-get 'choices response)))
             (plist-put
              cw-activity-coder-session-stats
              :total-prompt-tokens
              (+ (plist-get
                  cw-activity-coder-session-stats
                  :total-prompt-tokens)
                 (or (alist-get
                      'prompt_tokens (alist-get 'usage response))
                     0)))
             (plist-put
              cw-activity-coder-session-stats
              :total-completion-tokens
              (+ (plist-get
                  cw-activity-coder-session-stats
                  :total-completion-tokens)
                 (or (alist-get
                      'completion_tokens (alist-get 'usage response))
                     0)))
             (puthash
              (or (alist-get 'system_fingerprint response)
                  "unknown")
              t
              (plist-get
               cw-activity-coder-session-stats
               :fingerprints))
             (push duration
                   (plist-get
                    cw-activity-coder-session-stats
                    :response-times))
             (funcall callback _file choices duration)))))
     nil t cw-activity-coder-api-timeout)))

(defun cw-activity-coder--update-output-buffer (file results)
  "Update the output buffer with RESULTS for FILE, highlighting NDE errors."
  (with-current-buffer (get-buffer-create
                        cw-activity-coder-output-buffer)
    (unless (eq major-mode 'org-mode)
      (org-mode)
      (insert
       "* CW Activity Coder Output\n** Results\n| Ref | Activity Code (cw_at) |\n|-----+---------------|\n"))
    (goto-char (point-max))
    (dolist (row results)
      (let ((cw-at (alist-get 'cw_at row)))
        (insert
         (format "| %s | %s%s%s |\n"
                 (alist-get 'ref row)
                 (if (string= cw-at "NDE")
                     "#+BEGIN_ERROR\n"
                   "")
                 cw-at
                 (if (string= cw-at "NDE")
                     "\n#+END_ERROR"
                   "")))))
    (org-table-align)
    (org-toggle-pretty-entities)
    (when (get-buffer-window)
      (with-selected-window (get-buffer-window)
        (goto-char (point-max))
        (recenter -1)))))

(defun cw-activity-coder--update-modeline ()
  "Update the modeline with current progress."
  (when cw-activity-coder-progress
    (let* ((current (car cw-activity-coder-progress))
           (total (cdr cw-activity-coder-progress))
           (percent
            (if (zerop total)
                0
              (/ (* 100.0 current) total))))
      (force-mode-line-update)
      (setq mode-line-format
            (list
             "CW Coding: %d/%d rows (%.1f%%)"
             current
             total
             percent)))))

(defun cw-activity-coder-process-file (file callback)
  "Process FILE with xAI API asynchronously and call CALLBACK when done."
  (cw-activity-coder--ensure-api-key)
  (cw-activity-coder--load-activity-codes)
  (unless (file-directory-p cw-activity-coder-output-dir)
    (make-directory cw-activity-coder-output-dir t))
  (let* ((start-time (float-time))
         (data (cw-activity-coder--read-data-file file))
         (system-prompt
          (cw-activity-coder--build-system-prompt
           cw-activity-coder-activity-codes))
         (total-rows (length data))
         (batches (seq-partition data cw-activity-coder-batch-size))
         (total-batches (length batches))
         (results nil)
         (file-stats (list :response-times nil))
         (pending-batches total-batches))
    (message "Processing %s (%d rows)..." file total-rows)
    (setq cw-activity-coder-progress (cons 0 total-rows))
    (cw-activity-coder--update-modeline)
    (dolist (batch batches)
      (let* ((batch-num (1+ (- total-batches pending-batches)))
             (batch-data
              (mapcar
               (lambda (row)
                 (append
                  row
                  (list
                   (cons
                    "ref"
                    (cw-activity-coder--generate-ref
                     file (car (last row)))))))
               batch))
             (payload
              `((model . ,cw-activity-coder-model)
                (messages
                 .
                 (((role . "system") (content . ,system-prompt))
                  ((role . "user")
                   (content
                    .
                    ,(format "Process this JSON data:\n%s"
                             (json-encode batch-data))))))
                (response_format
                 .
                 ((type . "json_schema")
                  (json_schema
                   .
                   ((name . "activity_code_response")
                    (strict . t)
                    (schema
                     .
                     ((type . "array")
                      (items
                       .
                       ((type . "object")
                        (properties
                         .
                         ((ref . ((type . "string")))
                          (cw_at . ((type . "string")))))
                        (required . ("ref" "cw_at")))))))))))))
        (cw-activity-coder--api-request
         payload file batch-num total-batches
         (lambda (_ choices duration)
           (dolist (choice choices)
             (let ((content
                    (json-parse-string (alist-get
                                        'content
                                        (alist-get 'message choice))
                                       :object-type 'alist)))
               (push content results)))
           (setq cw-activity-coder-progress
                 (cons
                  (+ (car cw-activity-coder-progress)
                     (length batch))
                  total-rows))
           (cw-activity-coder--update-modeline)
           (cw-activity-coder--update-output-buffer
            file (car results))
           (push duration (plist-get file-stats :response-times))
           (when (zerop (cl-decf pending-batches))
             (let* ((result-data (apply #'append results))
                    (output-file
                     (expand-file-name
                      (concat (file-name-base file) "-CODED.csv")
                      cw-activity-coder-output-dir))
                    (buffer (find-file-noselect output-file)))
               (with-current-buffer buffer
                 (erase-buffer)
                 (insert "ref,cw_at\n")
                 (dolist (row result-data)
                   (insert
                    (format "%s,%s\n"
                            (alist-get 'ref row)
                            (alist-get 'cw_at row))))
                 (save-buffer)
                 (kill-buffer))
               (message "Saved processed data to %s" output-file)
               (push
                (list
                 :name file
                 :rows total-rows
                 :prompt-tokens
                 (plist-get
                  cw-activity-coder-session-stats
                  :total-prompt-tokens)
                 :completion-tokens
                 (plist-get
                  cw-activity-coder-session-stats
                  :total-completion-tokens)
                 :cumulative-duration
                 (apply #'+ (plist-get file-stats :response-times))
                 :wall-clock-time (- (float-time) start-time)
                 :response-times (plist-get file-stats :response-times))
                (plist-get cw-activity-coder-session-stats :files))
               (plist-put
                cw-activity-coder-session-stats
                :total-duration (- (float-time) start-time))
               (setq cw-activity-coder-progress nil)
               (force-mode-line-update)
               (funcall callback)))))))
    (when (zerop total-batches)
      (funcall callback))))

;;;###autoload
(defun cw-activity-coder-display-receipt ()
  "Display session receipt in an Org-mode buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*CW Activity Coder Receipt*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "* CW Activity Coder Receipt\n")
      (insert "** Files Processed\n")
      (insert
       "| File Name | Rows | Prompt Tokens | Completion Tokens | Cumulative Duration (s) | Wall-Clock Time (s) |\n")
      (insert
       "|-----------+------+---------------+-------------------+-----------------------+---------------------|\n")
      (dolist (file
               (plist-get cw-activity-coder-session-stats :files))
        (insert
         (format "| %s | %d | %d | %d | %.2f | %.2f |\n"
                 (plist-get file :name)
                 (plist-get file :rows)
                 (plist-get file :prompt-tokens)
                 (plist-get file :completion-tokens)
                 (plist-get file :cumulative-duration)
                 (plist-get file :wall-clock-time))))
      (insert "\n** Session Totals\n")
      (let ((total-cost
             (+ (/ (* (plist-get
                       cw-activity-coder-session-stats
                       :total-prompt-tokens)
                      2)
                   1e6)
                (/ (* (plist-get
                       cw-activity-coder-session-stats
                       :total-completion-tokens)
                      10)
                   1e6))))
        (insert
         (format
          "| Metric | Value |\n|--------|-------|\n| Session Wall-Clock Time (s) | %.2f |\n| Total Prompt Tokens | %d |\n| Total Completion Tokens | %d |\n| Total Cost ($) | %.4f |\n"
          (plist-get cw-activity-coder-session-stats :total-duration)
          (plist-get
           cw-activity-coder-session-stats
           :total-prompt-tokens)
          (plist-get
           cw-activity-coder-session-stats
           :total-completion-tokens)
          total-cost)))
      (org-table-align)
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

;;;###autoload
(defun cw-activity-coder-edit-codes ()
  "Edit the activity codes in a pretty-printed JSON buffer."
  (interactive)
  (cw-activity-coder--ensure-api-key)
  (cw-activity-coder--load-activity-codes)
  (let ((buffer (get-buffer-create "*CW Activity Codes*")))
    (with-current-buffer buffer
      (erase-buffer)
      (json-mode)
      (insert (json-encode cw-activity-coder-activity-codes))
      (goto-char (point-min))
      (set-buffer-modified-p nil))
    (switch-to-buffer buffer)
    (message "Edit codes and save with C-c C-c to update")))

(defun cw-activity-coder--save-codes ()
  "Save edited activity codes to file and update in-memory."
  (interactive)
  (when (eq (current-buffer) (get-buffer "*CW Activity Codes*"))
    (condition-case err
        (let ((new-codes (json-parse-buffer :object-type 'alist)))
          (setq cw-activity-coder-activity-codes new-codes)
          (cw-activity-coder--write-json-file
           cw-activity-coder-activity-codes-file new-codes)
          (set-buffer-modified-p nil)
          (kill-buffer)
          (message "Activity codes saved to %s"
                   cw-activity-coder-activity-codes-file))
      (error (message "Failed to save activity codes: %s" err)))))

(define-key
 json-mode-map (kbd "C-c C-c") #'cw-activity-coder--save-codes)

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

(defun cw-activity-coder--save-queue ()
  "Save the edited queue and update the in-memory list."
  (interactive)
  (when (eq (current-buffer) (get-buffer "*CW Activity Queue*"))
    (let ((new-queue
           (split-string (buffer-string) "\n" t "[ \t\n#]+")))
      (setq cw-activity-coder-files-to-process
            (seq-filter
             (lambda (f)
               (and (file-exists-p f)
                    (file-regular-p f)
                    (or (string-suffix-p ".csv" f)
                        (string-suffix-p ".json" f))))
             new-queue))
      (kill-buffer)
      (message "Queue updated (%d files): %s"
               (length cw-activity-coder-files-to-process)
               (string-join cw-activity-coder-files-to-process ", "))
      (transient-setup 'cw-activity-coder-menu))))

(defun cw-activity-coder--cancel-queue-edit ()
  "Cancel queue editing without saving changes."
  (interactive)
  (when (eq (current-buffer) (get-buffer "*CW Activity Queue*"))
    (kill-buffer)
    (message "Queue edit canceled. Queue unchanged (%d files): %s"
             (length cw-activity-coder-files-to-process)
             (string-join cw-activity-coder-files-to-process ", "))))

;;;###autoload
(defun cw-activity-coder-clear-queue ()
  "Clear the processing queue."
  (interactive)
  (setq cw-activity-coder-files-to-process nil)
  (message "Processing queue cleared")
  (transient-setup 'cw-activity-coder-menu))

;;;###autoload
(defun cw-activity-coder-process-queued-files ()
  "Process all files in the queue asynchronously."
  (interactive)
  (cw-activity-coder--ensure-api-key)
  (if (not cw-activity-coder-files-to-process)
      (message "No files in queue to process")
    (let ((files (reverse cw-activity-coder-files-to-process))
          (pending (length cw-activity-coder-files-to-process)))
      (setq cw-activity-coder-files-to-process nil)
      (dolist (file files)
        (cw-activity-coder-process-file
         file
         (lambda ()
           (when (zerop (cl-decf pending))
             (cw-activity-coder-display-receipt)
             (message "Processed all queued files"))))))))

;;;###autoload
(defun cw-activity-coder ()
  "Start the CW Activity Coder interface."
  (interactive)
  (cw-activity-coder--ensure-api-key)
  (cw-activity-coder-menu))

;;(require 'transient) ; Ensure transient is loaded explicitly

;;;###autoload
(transient-define-suffix
 cw-activity-coder--queue-display
 ()
 "Display the current queue status."
 :transient t
 :description
 (lambda ()
   (format "Current queue (%d): %s"
           (length cw-activity-coder-files-to-process)
           (if cw-activity-coder-files-to-process
               (string-join cw-activity-coder-files-to-process ", ")
             "empty"))))

;;;###autoload
(transient-define-prefix
 cw-activity-coder-menu () "Menu for CW Activity Coder."
 :refresh-suffixes t
 [["CW Activity Coder"
   ("a" "Add File" cw-activity-coder-add-files)
   ("m" "Modify Queue" cw-activity-coder-edit-queue)]
  ["" ; Empty group for spacing
   ("p" "Process Queued Files" cw-activity-coder-process-queued-files)
   ("r" "Show Receipt" cw-activity-coder-display-receipt)
   ("e" "Edit Activity Codes" cw-activity-coder-edit-codes)]
  ["" ; Empty group for spacing
   ("c" "Clear Queue" cw-activity-coder-clear-queue)
   ("q" "Quit" transient-quit-one)]
  ["Queue" ("s" "Show Queue" cw-activity-coder--queue-display)]]
 (interactive) (transient-setup 'cw-activity-coder-menu))

(provide 'cw-activity-coder)

;;; cw-activity-coder.el ends here
