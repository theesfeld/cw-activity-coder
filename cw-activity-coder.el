;;; cw-activity-coder.el --- Assign activity codes to CSV rows using xAI API -*- lexical-binding: t; -*-

;; Author: William Theesfeld <william@theesfeld.net>
;; Version: 1.2.5
;; Package-Requires: ((emacs "27.1") (request "0.3.3") (csv-mode "1.25"))
;; Keywords: tools, csv, xai, ai
;; URL: https://github.com/theesfeld/cw-activity-coder

;;; Code:

(require 'cl-lib)
(require 'request)
(require 'csv-mode)
(require 'json)

(defcustom cw-activity-coder-api-key-env-var "XAI_API_KEY"
  "Environment variable for xAI API key."
  :type 'string)

(defcustom cw-activity-coder-max-batch-size 100
  "Max rows per API batch."
  :type 'integer)

(defcustom cw-activity-coder-api-timeout 300
  "API timeout in seconds."
  :type 'integer)

(defcustom cw-activity-coder-model "grok-2-latest"
  "xAI model to use for API calls."
  :type 'string)

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
    (message "DEBUG: Parsed line %d, fields: %d - %s"
             (line-number-at-pos)
             (length fields)
             fields)
    (when cw-activity-coder--original-header
      (let ((expected (length cw-activity-coder--original-header))
            (actual (length fields)))
        (cond
         ((< actual expected)
          (message "DEBUG: Line %d short, padding: %d -> %d"
                   (line-number-at-pos)
                   actual
                   expected)
          (setq fields
                (append fields (make-list (- expected actual) ""))))
         ((> actual expected)
          (message "DEBUG: Line %d long, truncating: %d -> %d"
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
        (message "DEBUG: Starting ref column addition, buffer: %s"
                 (buffer-name))
        (message "DEBUG: Before: %s" (buffer-string))
        (let* ((header (cw-activity-coder--parse-csv-line))
               (new-header (append header (list "ref" "cw_at"))))
          (message "DEBUG: Header: %s" header)
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
              (message "DEBUG: Added ref at line %d: %s"
                       (line-number-at-pos)
                       ref))
            (forward-line 1)))
        (message "DEBUG: After ref addition: %s" (buffer-string))))))

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
        (message "DEBUG: Ref index: %d, Header: %s" ref-index header)
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
        (message "DEBUG: JSON batch to send: %s" (json-encode rows))
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
              ((role . "user") (content . ,(json-encode batch)))]))))
    (cl-incf cw-activity-coder--active-requests)
    (message "DEBUG: Sending payload: %s" (json-encode payload))
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
                (alist-get
                 'content
                 (alist-get
                  'message (aref (alist-get 'choices data) 0))))
               (result (condition-case err
                           (json-read-from-string content)
                         (error
                          (message "JSON parse error: %s" err)
                          nil))))
          (message "DEBUG: Raw response: %s" content)
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
        (message "DEBUG: API error: %s" error-thrown)
        (cl-decf cw-activity-coder--active-requests)
        (funcall callback nil))))))

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
          (message "DEBUG: Updating buffer, ref-index: %d, results count: %d"
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
              (message
               "DEBUG: Update line %d, ref: %s, cw_at: %s"
               (line-number-at-pos)
               (or ref "nil")
               cw-at)
              (when (and fields (>= (length fields) 2))
                (delete-region (point) (line-end-position))
                (insert
                 (mapconcat #'identity (butlast fields 2) ",") "," cw-at)))
            (cl-incf line-num)
            (forward-line 1)))))))

;;;###autoload
(defun cw-activity-coder-process-buffer ()
  "Process the current CSV buffer, adding 'cw_at' codes via xAI API."
  (interactive)
  (unless (derived-mode-p 'csv-mode)
    (when (y-or-n-p "Not in CSV mode. Enable it?")
      (csv-mode)))
  
  (setq cw-activity-coder--current-buffer (current-buffer))
  (setq cw-activity-coder--progress "CW: Starting...")
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
                 (message "Processing complete!"))))))
        (setq sent (1+ sent))
        (cw-activity-coder--update-progress
         sent completed num-batches)))))

(defvar cw-activity-coder-mode-line-entry
  '(:eval cw-activity-coder--progress))
(unless (member cw-activity-coder-mode-line-entry mode-line-misc-info)
  (add-to-list 'mode-line-misc-info cw-activity-coder-mode-line-entry
               t))

(provide 'cw-activity-coder)
;;; cw-activity-coder.el ends here
