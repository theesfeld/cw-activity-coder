;;; cw-activity-coder.el --- Assign activity codes to CSV rows using xAI API -*- lexical-binding: t; -*-

;; Author: William Theesfeld <william@theesfeld.net>
;; Version: 1.0.2
;; Package-Requires: ((emacs "27.1") (request "0.3.3") (csv-mode "1.25"))
;; Keywords: tools, csv, xai, ai
;; URL: https://github.com/theesfeld/cw-activity-coder

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
  :type 'string)

(defcustom cw-activity-coder-rate-limit 8
  "Maximum concurrent API requests."
  :type 'integer)

(defcustom cw-activity-coder-max-batch-size 100
  "Maximum number of rows per API batch."
  :type 'integer)

(defcustom cw-activity-coder-max-retries 3
  "Maximum retries per failed batch."
  :type 'integer)

(defcustom cw-activity-coder-api-timeout 300
  "API request timeout in seconds."
  :type 'integer)

(defcustom cw-activity-coder-model "grok-2-latest"
  "xAI model to use for coding."
  :type 'string)

(defun cw-activity-coder--get-package-dir ()
  "Determine the package directory reliably."
  (let ((lib-path (locate-library "cw-activity-coder")))
    (cond
     (lib-path (file-name-directory lib-path))
     (load-file-name (file-name-directory load-file-name))
     ((and (boundp 'package-user-dir) (file-exists-p package-user-dir))
      (expand-file-name "cw-activity-coder" package-user-dir))
     (t (error "Cannot find cw-activity-coder package directory; check load-path or package installation")))))

(defvar cw-activity-coder--package-dir nil
  "Cached package directory.")

(defconst cw-activity-coder-activity-codes
  (progn
    (setq cw-activity-coder--package-dir (cw-activity-coder--get-package-dir))
    (message "DEBUG: Package dir set to %s" cw-activity-coder--package-dir)
    (let ((json-file (expand-file-name "activitycodes.json" cw-activity-coder--package-dir)))
      (unless (file-exists-p json-file)
        (error "activitycodes.json not found in package directory: %s" cw-activity-coder--package-dir))
      (with-temp-buffer
        (insert-file-contents json-file)
        (json-parse-buffer))))
  "Activity codes JSON object loaded from activitycodes.json.")

(defvar cw-activity-coder--session-stats
  '((:prompt-tokens . 0)
    (:completion-tokens . 0)
    (:response-times . nil)
    (:fingerprints . nil))
  "Session statistics for API usage.")

(defvar cw-activity-coder--active-requests 0
  "Counter for active API requests.")

(defun cw-activity-coder--validate-csv-buffer ()
  "Validate that the current buffer is a valid CSV file."
  (unless (derived-mode-p 'csv-mode)
    (error "Buffer is not in csv-mode; please enable it first"))
  (save-excursion
    (goto-char (point-min))
    (unless (looking-at-p "^[^,\n]+,.*$")
      (error "Buffer does not start with a valid CSV header")))
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

(defun cw-activity-coder--parse-csv-line ()
  "Parse current line as CSV, returning list of fields."
  (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
        (fields '())
        (current "")
        (in-quotes nil))
    (dolist (char (append line nil))
      (cond
       ((and (eq char ?\") (not in-quotes))
        (setq in-quotes t))
       ((and (eq char ?\") in-quotes)
        (setq in-quotes nil))
       ((and (eq char ?,) (not in-quotes))
        (push current fields)
        (setq current ""))
       (t
        (setq current (concat current (string char))))))
    (push current fields)
    (nreverse fields)))

(defun cw-activity-coder--parse-buffer-to-json (start-line end-line)
  "Parse CSV lines from START-LINE to END-LINE into a JSON array."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- start-line))
    (let ((header (cw-activity-coder--parse-csv-line))
          (rows '()))
      (while (and (< (line-number-at-pos) end-line) (not (eobp)))
        (forward-line 1)
        (when (not (eobp))
          (let* ((fields (cw-activity-coder--parse-csv-line))
                 (row (cl-mapcar #'cons header fields))
                 (ref (cw-activity-coder--generate-ref (line-number-at-pos))))
            (push (append row (list (cons "ref" ref))) rows))))
      (nreverse rows))))

(defun cw-activity-coder--api-request (batch retry-count callback)
  "Send a batch to the xAI API with RETRY-COUNT retries, calling CALLBACK."
  (let* ((api-key (or (getenv cw-activity-coder-api-key-env-var)
                      (error "API key not set in %s" cw-activity-coder-api-key-env-var)))
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
    (while (>= cw-activity-coder--active-requests cw-activity-coder-rate-limit)
      (sleep-for 0.1))
    (cl-incf cw-activity-coder--active-requests)
    (request
     "https://api.x.ai/v1/chat/completions"
     :type "POST"
     :headers `(("Authorization" . ,(concat "Bearer " api-key))
                ("Content-Type" . "application/json"))
     :data (json-encode payload)
     :timeout cw-activity-coder-api-timeout
     :parser 'json-read
     :success
     (cl-function
      (lambda (&key data &allow-other-keys)
        (let* ((content (alist-get 'content (alist-get 'message (aref (alist-get 'choices data) 0))))
               (raw-result (json-parse-string content))
               (result (mapcar (lambda (item)
                                 (let ((ht (if (hash-table-p item) item
                                             (error "Unexpected item type: %s" item))))
                                   (list (cons "ref" (gethash "ref" ht))
                                         (cons "cw_at" (gethash "cw_at" ht)))))
                               (if (vectorp raw-result) (append raw-result nil) raw-result)))
               (usage (alist-get 'usage data)))
          (push (alist-get 'system_fingerprint data "unknown")
                (alist-get :fingerprints cw-activity-coder--session-stats))
          (cl-incf (alist-get :prompt-tokens cw-activity-coder--session-stats)
                  (alist-get 'prompt_tokens usage 0))
          (cl-incf (alist-get :completion-tokens cw-activity-coder--session-stats)
                  (alist-get 'completion_tokens usage 0))
          (cl-decf cw-activity-coder--active-requests)
          (message "DEBUG: Processed batch with %d items" (length result))
          (funcall callback result nil))))
     :error
     (cl-function
      (lambda (&key error-thrown &allow-other-keys)
        (cl-decf cw-activity-coder--active-requests)
        (if (< retry-count cw-activity-coder-max-retries)
            (progn
              (message "Retrying batch (attempt %d/%d): %s"
                       (1+ retry-count)
                       cw-activity-coder-max-retries
                       error-thrown)
              (sleep-for 1)
              (cw-activity-coder--api-request batch (1+ retry-count) callback))
          (funcall callback nil (format "Failed after %d retries: %s"
                                       cw-activity-coder-max-retries
                                       error-thrown))))))))

(defun cw-activity-coder--process-batch (start-line end-line callback)
  "Process a batch from START-LINE to END-LINE, calling CALLBACK."
  (when (> (- end-line start-line) 0)
    (let ((batch (cw-activity-coder--parse-buffer-to-json start-line end-line)))
      (let ((start-time (float-time)))
        (cw-activity-coder--api-request
         batch 0
         (lambda (batch-result error)
           (push (- (float-time) start-time)
                 (alist-get :response-times cw-activity-coder--session-stats))
           (message "DEBUG: Batch from %d to %d completed" start-line end-line)
           (funcall callback batch-result error)))))))

(defun cw-activity-coder--update-buffer (results)
  "Update the buffer with RESULTS, adding or updating the 'cw_at' column."
  (save-excursion
    (goto-char (point-min))
    (let* ((header-line (cw-activity-coder--parse-csv-line))
           (has-cw-at (member "cw_at" header-line))
           (new-header (if has-cw-at header-line (append header-line '("cw_at")))))
      (message "DEBUG: Updating buffer with %d results" (length results))
      (unless has-cw-at
        (delete-region (point) (line-end-position))
        (insert (mapconcat #'identity new-header ",")))
      (while (not (eobp))
        (forward-line 1)
        (when (not (eobp))
          (let* ((fields (cw-activity-coder--parse-csv-line))
                 (ref (cw-activity-coder--generate-ref (line-number-at-pos)))
                 (result (cl-find ref results :key (lambda (r) (alist-get "ref" r)) :test #'string=))
                 (cw-at (if result (alist-get "cw_at" result) "NDE")))
            (delete-region (point) (line-end-position))
            (insert (mapconcat #'identity (append fields (list cw-at)) ","))))))))

;;;###autoload
(defun cw-activity-coder-process-buffer ()
  "Process the current CSV buffer, adding activity codes in-place."
  (interactive)
  (condition-case err
      (progn
        (cw-activity-coder--validate-csv-buffer)
        (let* ((total-lines (count-lines (point-min) (point-max)))
               (batches (ceiling (- total-lines 1) cw-activity-coder-max-batch-size))
               (results '())
               (errors '()))
          (message "Processing %d rows in %d batches..." (- total-lines 1) batches)
          (dotimes (i batches)
            (let ((start-line (+ 2 (* i cw-activity-coder-max-batch-size)))
                  (end-line (min (+ 2 (* (1+ i) cw-activity-coder-max-batch-size)) total-lines)))
              (cw-activity-coder--process-batch
               start-line end-line
               (lambda (batch-result error)
                 (if error
                     (push error errors)
                   (setq results (append results batch-result)))
                 (message "DEBUG: Batch %d/%d done, results: %d, errors: %d"
                          (1+ i) batches (length results) (length errors))
                 (when (= (+ (length errors) (length results)) batches)
                   (if errors
                       (error "Processing failed: %s" (string-join errors "; "))
                     (cw-activity-coder--update-buffer results)))))))
          (message "Processing complete. Stats: %s" (cw-activity-coder--stats-string))))
    (error (message "Error in cw-activity-coder: %s" (error-message-string err)))))

(defun cw-activity-coder--stats-string ()
  "Return a string summarizing session stats."
  (let ((stats cw-activity-coder--session-stats))
    (format "Prompt tokens: %d, Completion tokens: %d, Avg response: %.2fs, Fingerprints: %d"
            (alist-get :prompt-tokens stats)
            (alist-get :completion-tokens stats)
            (if (alist-get :response-times stats)
                (/ (apply #'+ (alist-get :response-times stats))
                   (length (alist-get :response-times stats)))
              0.0)
            (length (alist-get :fingerprints stats)))))

(provide 'cw-activity-coder)
;;; cw-activity-coder.el ends here
