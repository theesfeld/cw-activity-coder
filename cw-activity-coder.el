;;; cw-activity-coder.el --- Assign activity codes to CSV rows using xAI API -*- lexical-binding: t; -*-

;; Author: William Theesfeld <william@theesfeld.net>
;; Version: 1.3.0
;; Package-Requires: ((emacs "27.1") (request "0.3.3") (csv-mode "1.25"))
;; Keywords: tools, csv, xai, ai
;; URL: https://github.com/theesfeld/cw-activity-coder

;;; Commentary:
;; This package processes CSV files and assigns activity codes to each row
;; using the xAI API. It adds a 'cw_at' column with the assigned code.
;;
;; Usage:
;;   1. Open a CSV file
;;   2. Run M-x cw-activity-coder-process-buffer
;;   3. The buffer will be updated with activity codes
;;
;; Configuration:
;;   (use-package cw-activity-coder
;;     :custom
;;     (cw-activity-coder-api-key-env-var "XAI_API_KEY")
;;     (cw-activity-coder-model "grok-2-latest")
;;     (cw-activity-coder-debug nil)
;;     (cw-activity-coder-show-results-buffer t))

;;; Code:

(require 'cl-lib)
(require 'request)
(require 'csv-mode)
(require 'json)

(defgroup cw-activity-coder nil
  "Assign activity codes to CSV rows using xAI API."
  :group 'tools
  :prefix "cw-activity-coder-")

(defcustom cw-activity-coder-api-key-env-var "XAI_API_KEY"
  "Environment variable for xAI API key."
  :type 'string
  :group 'cw-activity-coder)

(defcustom cw-activity-coder-max-batch-size 100
  "Max rows per API batch."
  :type 'integer
  :group 'cw-activity-coder)

(defcustom cw-activity-coder-api-timeout 300
  "API timeout in seconds."
  :type 'integer
  :group 'cw-activity-coder)

(defcustom cw-activity-coder-model "grok-2-latest"
  "xAI model to use for API calls."
  :type 'string
  :group 'cw-activity-coder)

(defcustom cw-activity-coder-debug nil
  "Enable debug messages."
  :type 'boolean
  :group 'cw-activity-coder)

(defcustom cw-activity-coder-show-results-buffer t
  "Show results buffer after processing."
  :type 'boolean
  :group 'cw-activity-coder)

(defcustom cw-activity-coder-results-buffer-name "*CW Activity Coder Results*"
  "Name of the results buffer."
  :type 'string
  :group 'cw-activity-coder)

(defvar cw-activity-coder--package-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Directory containing this package.")

(defvar cw-activity-coder--active-requests 0
  "Number of active API requests.")

(defvar cw-activity-coder--progress "CW: Starting..."
  "Mode-line progress string.")

(defvar cw-activity-coder--current-buffer nil
  "Current buffer being processed.")

(defvar cw-activity-coder--original-header nil
  "Original CSV header with 'ref' and 'cw_at' added.")

(defvar cw-activity-coder--stats nil
  "Statistics about API usage.")

(defvar cw-activity-coder--results-buffer nil
  "Buffer for displaying results.")

(defvar-local cw-activity-coder-results-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for `cw-activity-coder-results-mode'.")

(define-derived-mode cw-activity-coder-results-mode special-mode "CW Results"
  "Major mode for viewing CW Activity Coder results."
  :group 'cw-activity-coder
  (setq buffer-read-only t))

(defun cw-activity-coder--log (format-string &rest args)
  "Log message if debug is enabled.
FORMAT-STRING and ARGS are passed to `message'."
  (when cw-activity-coder-debug
    (apply #'message (concat "CW-DEBUG: " format-string) args)))

(defun cw-activity-coder--update-progress (sent completed total)
  "Update mode-line progress with SENT, COMPLETED, and TOTAL."
  (setq cw-activity-coder--progress
        (format "CW: %d/%d sent, %d/%d done, %d active"
                sent
                total
                completed
                total
                cw-activity-coder--active-requests))
  (force-mode-line-update))

(defun cw-activity-coder--load-activity-codes ()
  "Load activity codes from JSON file."
  (let ((json-file
         (expand-file-name "activitycodes.json"
                           cw-activity-coder--package-dir)))
    (unless (file-exists-p json-file)
      (error "activitycodes.json not found at %s" json-file))
    (with-temp-buffer
      (insert-file-contents json-file)
      (let ((json-object-type 'alist)
            (json-array-type 'list))
        (json-read-from-string (buffer-string))))))

(defun cw-activity-coder--parse-csv-line ()
  "Parse current CSV line into a list of fields."
  (let* ((line
          (buffer-substring-no-properties
           (line-beginning-position) (line-end-position)))
         ;; Use csv-mode's parsing to handle quoted fields properly
         (fields (if (fboundp 'csv-split-string)
                     (csv-split-string line)
                   (split-string line "," t))))
    (cw-activity-coder--log "Parsed line %d, fields: %d - %s"
                           (line-number-at-pos)
                           (length fields)
                           fields)
    (when cw-activity-coder--original-header
      (let ((expected (length cw-activity-coder--original-header))
            (actual (length fields)))
        (cond
         ((< actual expected)
          (cw-activity-coder--log "Line %d short, padding: %d -> %d"
                                 (line-number-at-pos)
                                 actual
                                 expected)
          (setq fields
                (append fields (make-list (- expected actual) ""))))
         ((> actual expected)
          (cw-activity-coder--log "Line %d long, truncating: %d -> %d"
                                 (line-number-at-pos)
                                 actual
                                 expected)
          (setq fields (butlast fields (- actual expected)))))))
    fields))

(defun cw-activity-coder--generate-ref (row)
  "Generate SHA256 reference hash from ROW."
  (secure-hash 'sha256 (mapconcat #'identity row ",")))

(defun cw-activity-coder--add-ref-column ()
  "Add 'ref' and 'cw_at' columns to the CSV buffer with SHA256 refs."
  (with-current-buffer cw-activity-coder--current-buffer
    (save-excursion
      (goto-char (point-min))
      (let ((inhibit-read-only t))
        (cw-activity-coder--log "Starting ref column addition, buffer: %s"
                               (buffer-name))
        (cw-activity-coder--log "Before: %s" (buffer-string))
        (let* ((header (cw-activity-coder--parse-csv-line))
               (new-header (append header (list "ref" "cw_at"))))
          (cw-activity-coder--log "Header: %s" header)
          (setq cw-activity-coder--original-header new-header)
          (delete-region (point) (line-end-position))
          (insert (mapconcat #'identity new-header ","))
          (forward-line 1)
          (while (not (eobp))
            (let* ((fields (cw-activity-coder--parse-csv-line))
                   (ref (cw-activity-coder--generate-ref fields)))
              (delete-region (point) (line-end-position))
              (insert
               (mapconcat #'identity (append fields (list ref ""))
                          ","))
              (cw-activity-coder--log "Added ref at line %d: %s"
                                     (line-number-at-pos)
                                     ref))
            (forward-line 1)))
        (cw-activity-coder--log "After ref addition: %s" (buffer-string))))))

(defun cw-activity-coder--system-prompt ()
  "Generate system prompt for xAI API."
  (format
   "Process the JSON array below. For each object, assign an activity code based on all fields except 'ref':\n- Return ONLY [{\"ref\": \"...\", \"cw_at\": \"...\"}], no extra text.\n- Use the EXACT 'ref' from input.\n- 'CB-EMG' for entrapment ('STK', 'stuck').\n- 'CB-EF' for equipment failure, 'CB-ROA' if running on arrival, else 'CB-EF'.\n- 'PM' for maintenance ('PREVENTIVE MAINTENANCE', 'REGULAR SCHEDULE').\n- 'NDE' if no fit.\n\nDefinitions:\n%s"
   (json-encode (cw-activity-coder--load-activity-codes))))

(defun cw-activity-coder--parse-buffer-to-json (start-line end-line)
  "Parse CSV lines from START-LINE to END-LINE into JSON batch."
  (with-current-buffer cw-activity-coder--current-buffer
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- start-line))
      (let ((header (butlast cw-activity-coder--original-header 2))
            (ref-index
             (- (length cw-activity-coder--original-header) 2))
            (rows '()))
        (cw-activity-coder--log "Ref index: %d, Header: %s" ref-index header)
        (while (and (< (line-number-at-pos) end-line) (not (eobp)))
          (forward-line 1)
          (when (not (eobp))
            (let* ((fields (cw-activity-coder--parse-csv-line))
                   (ref (nth ref-index fields))
                   (row-fields (butlast fields 2))
                   (row '()))
              (dotimes (i (min (length header) (length row-fields)))
                (push (cons (nth i header) (nth i row-fields)) row))
              (push (cons "ref" ref) row)
              (push row rows))))
        (cw-activity-coder--log "JSON batch to send: %s" (json-encode rows))
        rows))))

(defun cw-activity-coder--api-request (batch callback)
  "Send BATCH to xAI API and call CALLBACK with results."
  (let* ((api-key
          (or (getenv cw-activity-coder-api-key-env-var)
              (error "API key not set")))
         (payload
          `((model . ,cw-activity-coder-model)
            (messages
             .
             [((role . "system")
               (content . ,(cw-activity-coder--system-prompt)))
              ((role . "user") (content . ,(json-encode batch)))])))
         (start-time (current-time)))
    (cl-incf cw-activity-coder--active-requests)
    (cw-activity-coder--log "Sending payload: %s" (json-encode payload))
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
        (let* ((end-time (current-time))
               (elapsed (float-time (time-subtract end-time start-time)))
               (content
                (alist-get
                 'content
                 (alist-get
                  'message (aref (alist-get 'choices data) 0))))
               (prompt-tokens (alist-get 'prompt_tokens (alist-get 'usage data) 0))
               (completion-tokens (alist-get 'completion_tokens (alist-get 'usage data) 0))
               (total-tokens (alist-get 'total_tokens (alist-get 'usage data) 0))
               (result (condition-case err
                           (json-read-from-string content)
                         (error
                          (message "JSON parse error: %s" err)
                          nil))))
          ;; Record stats
          (push (list :time elapsed
                      :prompt-tokens prompt-tokens
                      :completion-tokens completion-tokens
                      :total-tokens total-tokens
                      :batch-size (length batch)
                      :success (if result t nil))
                cw-activity-coder--stats)
          
          (cw-activity-coder--log "Raw response: %s" content)
          (cl-decf cw-activity-coder--active-requests)
          (if result
              (funcall callback
                       (mapcar
                        (lambda (item)
                          (list
                           (cons "ref" (alist-get 'ref item))
                           (cons "cw_at" (alist-get 'cw_at item))))
                        result))
            (funcall callback nil)))))
     :error
     (cl-function
      (lambda (&key error-thrown &allow-other-keys)
        (let* ((end-time (current-time))
               (elapsed (float-time (time-subtract end-time start-time))))
          ;; Record error stats
          (push (list :time elapsed
                      :prompt-tokens 0
                      :completion-tokens 0
                      :total-tokens 0
                      :batch-size (length batch)
                      :success nil
                      :error error-thrown)
                cw-activity-coder--stats)
          
          (message "API error: %s" error-thrown)
          (cl-decf cw-activity-coder--active-requests)
          (funcall callback nil)))))))

(defun cw-activity-coder--update-buffer (results)
  "Update CSV buffer with RESULTS from API."
  (when (buffer-live-p cw-activity-coder--current-buffer)
    (with-current-buffer cw-activity-coder--current-buffer
      (save-excursion
        (goto-char (point-min))
        (let ((inhibit-read-only t)
              (ref-index
               (- (length cw-activity-coder--original-header) 2))
              (new-header
               (butlast cw-activity-coder--original-header 1))
              (line-num 0))
          (cw-activity-coder--log "Updating buffer, ref-index: %d, results count: %d"
                                 ref-index
                                 (length results))
          (when (> (length new-header) 0)
            (delete-region (point) (line-end-position))
            (insert (mapconcat #'identity new-header ",")))
          (forward-line 1)
          (while (not (eobp))
            (let* ((fields (cw-activity-coder--parse-csv-line))
                   (ref (when (and fields (>= (length fields) ref-index))
                          (nth ref-index fields)))
                   (result
                    (when ref
                      (cl-find
                       ref
                       results
                       :key (lambda (r) (alist-get "ref" r))
                       :test #'string=)))
                   (cw-at
                    (or (when result
                          (alist-get "cw_at" result))
                        "NDE")))
              (cw-activity-coder--log
               "Update line %d, ref: %s, cw_at: %s"
               (line-number-at-pos)
               (or ref "nil")
               cw-at)
              (when (and fields (>= (length fields) 2))
                (delete-region (point) (line-end-position))
                (insert
                 (mapconcat #'identity (butlast fields 2) ",") "," cw-at)))
            (cl-incf line-num)
            (forward-line 1)))))))

(defun cw-activity-coder--display-results ()
  "Display results in a dedicated buffer."
  (when cw-activity-coder-show-results-buffer
    (let ((buf (get-buffer-create cw-activity-coder-results-buffer-name)))
      (setq cw-activity-coder--results-buffer buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (cw-activity-coder-results-mode)
          
          ;; Calculate statistics
          (let* ((total-requests (length cw-activity-coder--stats))
                 (successful-requests (cl-count-if (lambda (s) (plist-get s :success)) 
                                                  cw-activity-coder--stats))
                 (total-time (cl-reduce #'+ cw-activity-coder--stats 
                                        :key (lambda (s) (plist-get s :time)) 
                                        :initial-value 0.0))
                 (avg-time (if (> total-requests 0)
                              (/ total-time total-requests)
                            0))
                 (total-tokens (cl-reduce #'+ cw-activity-coder--stats 
                                         :key (lambda (s) (plist-get s :total-tokens)) 
                                         :initial-value 0))
                 (total-prompt-tokens (cl-reduce #'+ cw-activity-coder--stats 
                                               :key (lambda (s) (plist-get s :prompt-tokens)) 
                                               :initial-value 0))
                 (total-completion-tokens (cl-reduce #'+ cw-activity-coder--stats 
                                                   :key (lambda (s) (plist-get s :completion-tokens)) 
                                                   :initial-value 0))
                 (total-rows-processed (cl-reduce #'+ cw-activity-coder--stats 
                                                :key (lambda (s) (plist-get s :batch-size)) 
                                                :initial-value 0)))
            
            ;; Insert summary
            (insert (propertize "CW Activity Coder Results\n" 'face 'bold))
            (insert (propertize "========================\n\n" 'face 'bold))
            
            (insert (format "Total API requests: %d\n" total-requests))
            (insert (format "Successful requests: %d (%.1f%%)\n" 
                           successful-requests 
                           (if (> total-requests 0)
                               (* 100.0 (/ successful-requests total-requests))
                             0.0)))
            (insert (format "Total processing time: %.2f seconds\n" total-time))
            (insert (format "Average request time: %.2f seconds\n\n" avg-time))
            
            (insert (format "Total rows processed: %d\n" total-rows-processed))
            (insert (format "Total tokens used: %d\n" total-tokens))
            (insert (format "  - Prompt tokens: %d\n" total-prompt-tokens))
            (insert (format "  - Completion tokens: %d\n\n" total-completion-tokens))
            
            (insert (propertize "Request Details\n" 'face 'bold))
            (insert (propertize "==============\n\n" 'face 'bold))
            
            ;; Insert details for each request
            (cl-loop for stat in (reverse cw-activity-coder--stats)
                    for i from 1
                    do (insert (format "Request #%d:\n" i))
                    (insert (format "  Time: %.2f seconds\n" (plist-get stat :time)))
                    (insert (format "  Batch size: %d rows\n" (plist-get stat :batch-size)))
                    (insert (format "  Status: %s\n" 
                                   (if (plist-get stat :success) "Success" "Failed")))
                    (when (plist-get stat :error)
                      (insert (format "  Error: %s\n" (plist-get stat :error))))
                    (when (plist-get stat :success)
                      (insert (format "  Tokens: %d (prompt: %d, completion: %d)\n" 
                                     (plist-get stat :total-tokens)
                                     (plist-get stat :prompt-tokens)
                                     (plist-get stat :completion-tokens))))
                    (insert "\n"))))
        
        (goto-char (point-min))
        (display-buffer buf '(display-buffer-pop-up-window . nil))))))

;;;###autoload
(defun cw-activity-coder-process-buffer ()
  "Process the current CSV buffer, adding 'cw_at' codes via xAI API."
  (interactive)
  (unless (derived-mode-p 'csv-mode)
    (when (y-or-n-p "Not in CSV mode. Enable it?")
      (csv-mode)))
  
  (setq cw-activity-coder--current-buffer (current-buffer))
  (setq cw-activity-coder--progress "CW: Starting...")
  (setq cw-activity-coder--stats nil)
  (force-mode-line-update)
  
  (cw-activity-coder--add-ref-column)
  (let* ((total-lines (count-lines (point-min) (point-max)))
         (num-batches
          (max 1 (ceiling (- total-lines 1)
                          cw-activity-coder-max-batch-size)))
         (batch-ranges
          (let (ranges)
            (dotimes (i num-batches)
              (let ((start
                     (+ 2 (* i cw-activity-coder-max-batch-size)))
                    (end
                     (min (+ 2
                             (* (1+ i)
                                cw-activity-coder-max-batch-size))
                          total-lines)))
                (push (cons start end) ranges)))
            (nreverse ranges)))
         (results '())
         (sent 0)
         (completed 0))
    (setq cw-activity-coder--active-requests 0)
    (if (null batch-ranges)
        (message "No data to process")
      (dolist (range batch-ranges)
        (let ((batch (cw-activity-coder--parse-buffer-to-json
                      (car range) (cdr range))))
          (if (null batch)
              (progn
                (setq completed (1+ completed))
                (cw-activity-coder--update-progress
                 sent completed num-batches))
            (cw-activity-coder--api-request
             batch
             (lambda (batch-results)
               (when batch-results
                 (setq results (append results batch-results)))
               (setq completed (1+ completed))
               (cw-activity-coder--update-progress
                sent completed num-batches)
               (when (= completed num-batches)
                 (cw-activity-coder--update-buffer results)
                 (setq cw-activity-coder--progress "CW: Done!")
                 (force-mode-line-update)
                 (cw-activity-coder--display-results)
                 (message "Processing complete!"))))))
        (setq sent (1+ sent))
        (cw-activity-coder--update-progress
         sent completed num-batches)))))

(defvar cw-activity-coder-mode-line-entry
  '(:eval cw-activity-coder--progress))
(unless (member cw-activity-coder-mode-line-entry mode-line-misc-info)
  (add-to-list 'mode-line-misc-info cw-activity-coder-mode-line-entry
               t))

;;;###autoload
(defun cw-activity-coder-show-results ()
  "Show the results of the last processing run."
  (interactive)
  (if cw-activity-coder--stats
      (cw-activity-coder--display-results)
    (message "No processing has been done yet.")))

(provide 'cw-activity-coder)
;;; cw-activity-coder.el ends here
