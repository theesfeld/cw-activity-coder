;;; cw-activity-coder.el --- Assign activity codes to CSV rows using xAI API -*- lexical-binding: t; -*-

;; Author: Your Name <your.email@example.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1") (request "0.3.3") (csv-mode "1.25"))
;; Keywords: tools, csv, xai, ai
;; URL: https://github.com/yourusername/cw-activity-coder

;;; Commentary:

;; This package processes CSV buffers in-place, adding a "cw_at" column with
;; activity codes determined by the xAI API. It validates CSV format, batches
;; requests, and handles retries and errors robustly. Requires an API key in
;; the environment variable specified by `cw-activity-coder-api-key-env-var'.
;; The `activitycodes.json' file must be in the package directory.

;;; Code:

(require 'cl-lib)
(require 'request)
(require 'csv-mode)
(require 'json)

(defgroup cw-activity-coder nil
  "Customization group for xAI Activity Coder."
  :group 'tools
  :prefix "cw-activity-coder-")

(defcustom cw-activity-coder-api-key-env-var "XAI_API_KEY"
  "Environment variable name containing the xAI API key."
  :type 'string
  :group 'cw-activity-coder)

(defcustom cw-activity-coder-rate-limit 8
  "Maximum concurrent API requests."
  :type 'integer
  :group 'cw-activity-coder)

(defcustom cw-activity-coder-max-batch-size 100
  "Maximum number of rows per API batch."
  :type 'integer
  :group 'cw-activity-coder)

(defcustom cw-activity-coder-max-retries 3
  "Maximum retries per failed batch."
  :type 'integer
  :group 'cw-activity-coder)

(defcustom cw-activity-coder-api-timeout 300
  "API request timeout in seconds."
  :type 'integer
  :group 'cw-activity-coder)

(defcustom cw-activity-coder-model "grok-2-latest"
  "xAI model to use for coding."
  :type 'string
  :group 'cw-activity-coder)

(defconst cw-activity-coder--package-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the package files.")

(defconst cw-activity-coder-activity-codes
  (let ((json-file
         (expand-file-name "activitycodes.json"
                           cw-activity-coder--package-dir)))
    (unless (file-exists-p json-file)
      (error
       "activitycodes.json not found in package directory: %s"
       cw-activity-coder--package-dir))
    (json-parse-string
     (with-temp-buffer
       (insert-file-contents-literally json-file)
       (buffer-string))))
  "Activity codes JSON object loaded from activitycodes.json in the package directory.")

(defvar cw-activity-coder--session-stats
  '((:prompt-tokens . 0)
    (:completion-tokens . 0)
    (:response-times . nil)
    (:fingerprints . nil))
  "Session statistics for API usage.")

(defun cw-activity-coder--validate-csv-buffer ()
  "Validate that the current buffer is a valid CSV file."
  (unless (eq major-mode 'csv-mode)
    (error "Buffer is not in csv-mode; please enable it first"))
  (save-excursion
    (goto-char (point-min))
    (unless (looking-at-p "^[^,\n]+,.*$")
      (error "Buffer does not start with a valid CSV header"))
    (let ((header (csv-mode--parse-line)))
      (unless header
        (error "Failed to parse CSV header"))
      (while (not (eobp))
        (forward-line 1)
        (when (and (not (eobp)) (not (csv-mode--parse-line)))
          (error "Invalid CSV line at %d" (line-number-at-pos))))))
  t)

(defun cw-activity-coder--generate-ref (row-index)
  "Generate a unique reference ID for a row."
  (let ((buffer-hash (md5 (buffer-name))))
    (format "%s-%d" (substring buffer-hash 0 8) row-index)))

(defun cw-activity-coder--system-prompt ()
  "Return the system prompt with activity codes."
  (format
   "Process the provided JSON array of objects. For each object, determine the appropriate activity code based on all its fields and the definitions below. Return the assigned 'cw_at' code along with the object's 'ref' field.\n\nRules:\n- Use 'code', 'description', and 'type_of_work' from definitions to infer the code based on the full row data.\n- For 'CB' prefixed situations:\n  - Use 'CB-EMG' if person(s) trapped in elevator is indicated in any field.\n  - Otherwise, use specific CB-XXX (e.g., 'CB-EF', 'CB-MU'), defaulting to 'CB-EF' if unclear.\n- Use 'NDE' if no code fits.\n\nActivity Code Definitions (JSON):\n%s"
   (json-encode cw-activity-coder-activity-codes)))

(defun cw-activity-coder--parse-buffer-to-json (start-line end-line)
  "Parse CSV lines from START-LINE to END-LINE into a JSON array."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- start-line))
    (let ((header (csv-mode--parse-line))
          (rows '()))
      (while (and (< (line-number-at-pos) end-line) (not (eobp)))
        (forward-line 1)
        (when (not (eobp))
          (let* ((fields (csv-mode--parse-line))
                 (row (cl-mapcar #'cons header fields))
                 (ref
                  (cw-activity-coder--generate-ref
                   (line-number-at-pos))))
            (push (append row (list (cons "ref" ref))) rows))))
      (nreverse rows))))

(defun cw-activity-coder--api-request (batch retry-count callback)
  "Send a batch to the xAI API with RETRY-COUNT retries, calling CALLBACK."
  (let* ((api-key
          (or (getenv cw-activity-coder-api-key-env-var)
              (error
               "API key not set in %s"
               cw-activity-coder-api-key-env-var)))
         (payload
          `((model . ,cw-activity-coder-model)
            (messages
             .
             [((role . "system")
               (content . ,(cw-activity-coder--system-prompt)))
              ((role . "user")
               (content
                .
                ,(format "Process this JSON data:\n%s"
                         (json-encode batch))))])
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
                    (required . ["ref" "cw_at"]))))))))))))
    (request
     "https://api.x.ai/v1/chat/completions"
     :type "POST"
     :headers
     `(("Authorization" . ,(concat "Bearer " api-key))
       ("Content-Type" . "application/json"))
     :data (json-encode payload)
     :timeout cw-activity-coder-api-timeout
     :parser 'json-read
     :success
     (cl-function
      (lambda (&key data &allow-other-keys)
        (let* ((content
                (json-encode
                 (aref (alist-get 'choices data) 0
                       'message
                       'content)))
               (result (json-parse-string content))
               (usage (alist-get 'usage data)))
          (push
           (alist-get 'system_fingerprint data "unknown")
           (alist-get :fingerprints cw-activity-coder--session-stats))
          (cl-incf
           (alist-get
            :prompt-tokens cw-activity-coder--session-stats)
           (alist-get 'prompt_tokens usage 0))
          (cl-incf
           (alist-get
            :completion-tokens cw-activity-coder--session-stats)
           (alist-get 'completion_tokens usage 0))
          (funcall callback result nil))))
     :error
     (cl-function
      (lambda (&key error-thrown &allow-other-keys)
        (if (< retry-count cw-activity-coder-max-retries)
            (progn
              (message "Retrying batch (attempt %d/%d): %s"
                       (1+ retry-count)
                       cw-activity-coder-max-retries
                       error-thrown)
              (sleep-for 1)
              (cw-activity-coder--api-request
               batch (1+ retry-count) callback))
          (funcall callback
                   nil
                   (format "Failed after %d retries: %s"
                           cw-activity-coder-max-retries
                           error-thrown))))))))

(defun cw-activity-coder--process-batch
    (start-line end-line semaphore callback)
  "Process a batch from START-LINE to END-LINE with SEMAPHORE, calling CALLBACK."
  (when (> (- end-line start-line) 0)
    (let ((batch
           (cw-activity-coder--parse-buffer-to-json
            start-line end-line)))
      (semaphore-acquire
       semaphore
       (lambda ()
         (let ((start-time (float-time)))
           (cw-activity-coder--api-request
            batch 0
            (lambda (batch-result error)
              (push
               (- (float-time) start-time)
               (alist-get
                :response-times cw-activity-coder--session-stats))
              (semaphore-release semaphore)
              (funcall callback batch-result error)))))))))

(defun cw-activity-coder--update-buffer (results)
  "Update the buffer with RESULTS, adding or updating the 'cw_at' column."
  (save-excursion
    (goto-char (point-min))
    (let* ((header-line (csv-mode--parse-line))
           (has-cw-at (member "cw_at" header-line))
           (new-header
            (if has-cw-at
                header-line
              (append header-line '("cw_at")))))
      ;; Rewrite header if needed
      (unless has-cw-at
        (delete-region (point) (line-end-position))
        (insert (mapconcat #'identity new-header ",")))
      ;; Process each line
      (while (not (eobp))
        (forward-line 1)
        (when (not (eobp))
          (let* ((fields (csv-mode--parse-line))
                 (ref
                  (cw-activity-coder--generate-ref
                   (line-number-at-pos)))
                 (result
                  (cl-find
                   ref
                   results
                   :key (lambda (r) (gethash "ref" r))
                   :test #'string=))
                 (cw-at
                  (if result
                      (gethash "cw_at" result)
                    "NDE")))
            (delete-region (point) (line-end-position))
            (insert
             (mapconcat #'identity (append fields (list cw-at))
                        ","))))))))

;;;###autoload
(defun cw-activity-coder-process-buffer ()
  "Process the current CSV buffer, adding activity codes in-place."
  (interactive)
  (condition-case err
      (progn
        (cw-activity-coder--validate-csv-buffer)
        (let* ((total-lines (count-lines (point-min) (point-max)))
               (batches
                (ceiling (- total-lines 1)
                         cw-activity-coder-max-batch-size))
               (semaphore
                (make-semaphore cw-activity-coder-rate-limit))
               (results '())
               (errors '()))
          (message "Processing %d rows in %d batches..."
                   (- total-lines 1)
                   batches)
          (dotimes (i batches)
            (let ((start-line
                   (+ 2 (* i cw-activity-coder-max-batch-size)))
                  (end-line
                   (min (+ start-line
                           cw-activity-coder-max-batch-size)
                        total-lines)))
              (cw-activity-coder--process-batch
               start-line end-line semaphore
               (lambda (batch-result error)
                 (if error
                     (push error errors)
                   (setq results (append results batch-result)))
                 (when (= (+ (length errors) (length results))
                          batches)
                   (if errors
                       (error
                        "Processing failed: %s"
                        (string-join errors "; "))
                     (cw-activity-coder--update-buffer results))))))))
        (message "Processing complete. Stats: %s"
                 (cw-activity-coder--stats-string))))
  (error
   (message "Error in cw-activity-coder: %s"
            (error-message-string err))))

(defun cw-activity-coder--stats-string ()
  "Return a string summarizing session stats."
  (let ((stats cw-activity-coder--session-stats))
    (format
     "Prompt tokens: %d, Completion tokens: %d, Avg response: %.2fs, Fingerprints: %d"
     (alist-get :prompt-tokens stats)
     (alist-get :completion-tokens stats)
     (if (alist-get :response-times stats)
         (/ (apply #'+ (alist-get :response-times stats))
            (length (alist-get :response-times stats)))
       0.0)
     (length (alist-get :fingerprints stats)))))

(provide 'cw-activity-coder)
;;; cw-activity-coder.el ends here
