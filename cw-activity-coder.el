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
  "Assign activity codes to CSV rows using LLM APIs."
  :group 'tools
  :prefix "cw-activity-coder-")

(defcustom cw-activity-coder-llm-provider nil
  "LLM provider to use for API calls.
Must be one of: 'xai, 'openai, 'anthropic, 'gemini, 'copilot."
  :type '(choice
          (const :tag "xAI" xai)
          (const :tag "OpenAI" openai)
          (const :tag "Anthropic" anthropic)
          (const :tag "Google Gemini" gemini)
          (const :tag "GitHub Copilot" copilot)
          (const :tag "Not Set" nil))
  :group 'cw-activity-coder)

(defcustom cw-activity-coder-api-key nil
  "API key for the selected LLM provider.
If nil, will try to get from environment variable or auth file."
  :type '(choice (string :tag "API Key")
                 (const :tag "Use environment variable or auth file" nil))
  :group 'cw-activity-coder)

(defcustom cw-activity-coder-api-key-env-vars
  '((xai . "XAI_API_KEY")
    (openai . "OPENAI_API_KEY")
    (anthropic . "ANTHROPIC_API_KEY")
    (gemini . "GEMINI_API_KEY")
    (copilot . "GITHUB_COPILOT_TOKEN"))
  "Environment variables for LLM provider API keys."
  :type '(alist :key-type symbol :value-type string)
  :group 'cw-activity-coder)

(defcustom cw-activity-coder-api-key-files
  '((xai . "~/.xai_key")
    (openai . "~/.openai_key")
    (anthropic . "~/.anthropic_key")
    (gemini . "~/.gemini_key")
    (copilot . "~/.copilot_key"))
  "Files containing API keys for LLM providers."
  :type '(alist :key-type symbol :value-type string)
  :group 'cw-activity-coder)

(defcustom cw-activity-coder-max-batch-size 100
  "Max rows per API batch."
  :type 'integer
  :group 'cw-activity-coder)

(defcustom cw-activity-coder-api-timeout 300
  "API timeout in seconds."
  :type 'integer
  :group 'cw-activity-coder)

(defcustom cw-activity-coder-models
  '((xai . "grok-2-latest")
    (openai . "gpt-4o")
    (anthropic . "claude-3-opus-20240229")
    (gemini . "gemini-1.5-pro")
    (copilot . "copilot-4"))
  "Default models for each LLM provider."
  :type '(alist :key-type symbol :value-type string)
  :group 'cw-activity-coder)

(defcustom cw-activity-coder-model nil
  "Model to use for API calls. If nil, uses the default for the provider."
  :type '(choice (string :tag "Model name")
                 (const :tag "Use provider default" nil))
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

(defun cw-activity-coder--get-api-key ()
  "Get API key for the selected LLM provider."
  (unless cw-activity-coder-llm-provider
    (error "LLM provider not set. Please set `cw-activity-coder-llm-provider`"))
  
  (let ((key
         (or
          ;; Directly set API key
          cw-activity-coder-api-key
          
          ;; From environment variable
          (let ((env-var (alist-get cw-activity-coder-llm-provider 
                                   cw-activity-coder-api-key-env-vars)))
            (when env-var
              (getenv env-var)))
          
          ;; From key file
          (let ((key-file (alist-get cw-activity-coder-llm-provider 
                                    cw-activity-coder-api-key-files)))
            (when (and key-file (file-exists-p (expand-file-name key-file)))
              (with-temp-buffer
                (insert-file-contents (expand-file-name key-file))
                (string-trim (buffer-string))))))))
    
    (if key
        key
      ;; No key found - prompt user interactively
      (let ((key-input (read-passwd 
                        (format "Enter API key for %s: " 
                                cw-activity-coder-llm-provider))))
        (when (and key-input (not (string-empty-p key-input)))
          ;; Save for this session
          (setq cw-activity-coder-api-key key-input)
          ;; Ask if user wants to save permanently
          (when (yes-or-no-p "Save this API key for future sessions?")
            (let ((save-method (completing-read 
                               "Save to: " 
                               '("Environment variable" "Key file") nil t)))
              (pcase save-method
                ("Environment variable"
                 (let ((env-var (alist-get cw-activity-coder-llm-provider 
                                          cw-activity-coder-api-key-env-vars)))
                   (message "Add this to your shell config: export %s=\"%s\""
                            env-var key-input)))
                ("Key file"
                 (let ((key-file (alist-get cw-activity-coder-llm-provider 
                                           cw-activity-coder-api-key-files)))
                   (with-temp-file (expand-file-name key-file)
                     (insert key-input))
                   (chmod (expand-file-name key-file) #o600)
                   (message "API key saved to %s" key-file))))))
          key-input)))))

(defun cw-activity-coder--get-model ()
  "Get model for the selected LLM provider."
  (or cw-activity-coder-model
      (alist-get cw-activity-coder-llm-provider cw-activity-coder-models)
      (error "No model specified for %s" cw-activity-coder-llm-provider)))

(defun cw-activity-coder--get-api-endpoint ()
  "Get API endpoint for the selected LLM provider."
  (pcase cw-activity-coder-llm-provider
    ('xai "https://api.x.ai/v1/chat/completions")
    ('openai "https://api.openai.com/v1/chat/completions")
    ('anthropic "https://api.anthropic.com/v1/messages")
    ('gemini "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-pro:generateContent")
    ('copilot "https://api.githubcopilot.com/chat/completions")
    (_ (error "Unknown LLM provider: %s" cw-activity-coder-llm-provider))))

(defun cw-activity-coder--prepare-payload (batch)
  "Prepare API payload for BATCH based on selected LLM provider."
  (let ((model (cw-activity-coder--get-model))
        (system-prompt (cw-activity-coder--system-prompt))
        (user-content (json-encode batch)))
    (pcase cw-activity-coder-llm-provider
      ('xai
       `((model . ,model)
         (messages . [((role . "system") (content . ,system-prompt))
                      ((role . "user") (content . ,user-content))])))
      
      ('openai
       `((model . ,model)
         (messages . [((role . "system") (content . ,system-prompt))
                      ((role . "user") (content . ,user-content))])))
      
      ('anthropic
       `((model . ,model)
         (system . ,system-prompt)
         (messages . [((role . "user") (content . ,user-content))])))
      
      ('gemini
       `((model . ,model)
         (contents . [((role . "system") (parts . [((text . ,system-prompt))]))
                      ((role . "user") (parts . [((text . ,user-content))]))])))
      
      ('copilot
       `((model . ,model)
         (messages . [((role . "system") (content . ,system-prompt))
                      ((role . "user") (content . ,user-content))])))
      
      (_ (error "Unknown LLM provider: %s" cw-activity-coder-llm-provider)))))

(defun cw-activity-coder--prepare-headers ()
  "Prepare API headers based on selected LLM provider."
  (let ((api-key (cw-activity-coder--get-api-key)))
    (pcase cw-activity-coder-llm-provider
      ('xai
       `(("Authorization" . ,(concat "Bearer " api-key))
         ("Content-Type" . "application/json")))
      
      ('openai
       `(("Authorization" . ,(concat "Bearer " api-key))
         ("Content-Type" . "application/json")))
      
      ('anthropic
       `(("x-api-key" . ,api-key)
         ("anthropic-version" . "2023-06-01")
         ("Content-Type" . "application/json")))
      
      ('gemini
       `(("x-goog-api-key" . ,api-key)
         ("Content-Type" . "application/json")))
      
      ('copilot
       `(("Authorization" . ,(concat "Bearer " api-key))
         ("Content-Type" . "application/json")))
      
      (_ (error "Unknown LLM provider: %s" cw-activity-coder-llm-provider)))))

(defun cw-activity-coder--extract-content (data)
  "Extract content from API response DATA based on selected LLM provider."
  (pcase cw-activity-coder-llm-provider
    ('xai
     (alist-get 'content
                (alist-get 'message (aref (alist-get 'choices data) 0))))
    
    ('openai
     (alist-get 'content
                (alist-get 'message (aref (alist-get 'choices data) 0))))
    
    ('anthropic
     (alist-get 'text
                (alist-get 'content
                           (aref (alist-get 'messages data) 0))))
    
    ('gemini
     (alist-get 'text
                (aref (alist-get 'parts
                                 (alist-get 'content
                                            (aref (alist-get 'candidates data) 0))) 0)))
    
    ('copilot
     (alist-get 'content
                (alist-get 'message (aref (alist-get 'choices data) 0))))
    
    (_ (error "Unknown LLM provider: %s" cw-activity-coder-llm-provider))))

(defun cw-activity-coder--extract-usage (data)
  "Extract token usage from API response DATA based on selected LLM provider."
  (pcase cw-activity-coder-llm-provider
    ((or 'xai 'openai 'copilot)
     (let ((usage (alist-get 'usage data)))
       (list (alist-get 'prompt_tokens usage 0)
             (alist-get 'completion_tokens usage 0)
             (alist-get 'total_tokens usage 0))))
    
    ('anthropic
     (let ((usage (alist-get 'usage data)))
       (list (alist-get 'input_tokens usage 0)
             (alist-get 'output_tokens usage 0)
             (+ (alist-get 'input_tokens usage 0)
                (alist-get 'output_tokens usage 0)))))
    
    ('gemini
     (let ((usage (alist-get 'usageMetadata data)))
       (list (alist-get 'promptTokenCount usage 0)
             (alist-get 'candidatesTokenCount usage 0)
             (+ (alist-get 'promptTokenCount usage 0)
                (alist-get 'candidatesTokenCount usage 0)))))
    
    (_ (list 0 0 0))))

(defun cw-activity-coder--api-request (batch callback)
  "Send BATCH to selected LLM API and call CALLBACK with results."
  (let* ((api-endpoint (cw-activity-coder--get-api-endpoint))
         (headers (cw-activity-coder--prepare-headers))
         (payload (cw-activity-coder--prepare-payload batch))
         (start-time (current-time)))
    (cl-incf cw-activity-coder--active-requests)
    (cw-activity-coder--log "Sending payload to %s: %s" 
                           cw-activity-coder-llm-provider
                           (json-encode payload))
    (request
     api-endpoint
     :type "POST"
     :headers headers
     :data (json-encode payload)
     :timeout cw-activity-coder-api-timeout
     :parser 'json-read
     :success
     (cl-function
      (lambda (&key data &allow-other-keys)
        (let* ((end-time (current-time))
               (elapsed (float-time (time-subtract end-time start-time)))
               (content (cw-activity-coder--extract-content data))
               (usage (cw-activity-coder--extract-usage data))
               (prompt-tokens (nth 0 usage))
               (completion-tokens (nth 1 usage))
               (total-tokens (nth 2 usage))
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
                      :provider cw-activity-coder-llm-provider
                      :model (cw-activity-coder--get-model)
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
                      :provider cw-activity-coder-llm-provider
                      :model (cw-activity-coder--get-model)
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
                                                :initial-value 0))
                 (provider-stats (make-hash-table :test 'eq))
                 ;; Estimated costs per 1000 tokens (adjust as needed)
                 (cost-per-1k (make-hash-table :test 'eq)))
            
            ;; Set estimated costs per provider
            (puthash 'openai 0.01 cost-per-1k)  ;; GPT-4o input
            (puthash 'xai 0.01 cost-per-1k)     ;; Grok-2
            (puthash 'anthropic 0.015 cost-per-1k) ;; Claude-3 Opus
            (puthash 'gemini 0.0035 cost-per-1k)   ;; Gemini Pro
            (puthash 'copilot 0.01 cost-per-1k)    ;; Copilot
            
            ;; Collect provider stats
            (dolist (stat cw-activity-coder--stats)
              (let* ((provider (plist-get stat :provider))
                     (provider-data (gethash provider provider-stats (list 0 0 0 0.0 0)))
                     (requests (1+ (nth 0 provider-data)))
                     (successful (+ (nth 1 provider-data) 
                                    (if (plist-get stat :success) 1 0)))
                     (tokens (+ (nth 2 provider-data) 
                                (plist-get stat :total-tokens 0)))
                     (time (+ (nth 3 provider-data) 
                              (plist-get stat :time 0.0)))
                     (rows (+ (nth 4 provider-data) 
                              (plist-get stat :batch-size 0))))
                (puthash provider (list requests successful tokens time rows) provider-stats)))
            
            ;; Calculate total estimated cost
            (let ((total-cost 0.0))
              (maphash (lambda (provider data)
                         (let* ((tokens (nth 2 data))
                                (provider-cost-per-1k (gethash provider cost-per-1k 0.01))
                                (cost (* (/ tokens 1000.0) provider-cost-per-1k)))
                           (setq total-cost (+ total-cost cost))))
                       provider-stats)
              
              ;; Insert summary with timestamp
              (insert (propertize "CW Activity Coder Results\n" 'face 'bold))
              (insert (propertize "========================\n" 'face 'bold))
              (insert (format "Generated: %s\n\n" 
                             (format-time-string "%Y-%m-%d %H:%M:%S")))
              
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
              (insert (format "  - Completion tokens: %d\n" total-completion-tokens))
              (insert (format "Estimated cost: $%.4f\n\n" total-cost)))
            
            ;; Insert provider summary
            (insert (propertize "Provider Summary\n" 'face 'bold))
            (insert (propertize "================\n\n" 'face 'bold))
            
            (maphash (lambda (provider data)
                       (let* ((requests (nth 0 data))
                              (successful (nth 1 data))
                              (tokens (nth 2 data))
                              (time (nth 3 data))
                              (rows (nth 4 data))
                              (provider-cost-per-1k (gethash provider cost-per-1k 0.01))
                              (cost (* (/ tokens 1000.0) provider-cost-per-1k)))
                         (insert (propertize (format "%s:\n" provider) 'face 'bold))
                         (insert (format "  Requests: %d (%d successful, %.1f%%)\n" 
                                        requests successful 
                                        (if (> requests 0)
                                            (* 100.0 (/ successful requests))
                                          0.0)))
                         (insert (format "  Tokens: %d\n" tokens))
                         (insert (format "  Time: %.2f seconds\n" time))
                         (insert (format "  Rows: %d\n" rows))
                         (insert (format "  Estimated cost: $%.4f\n\n" cost))))
                     provider-stats)
            
            ;; Add export options
            (insert (propertize "Export Options\n" 'face 'bold))
            (insert (propertize "==============\n\n" 'face 'bold))
            
            (insert "Press 'e' to export results as CSV\n")
            (insert "Press 's' to save as a report file\n\n")
            
            ;; Insert request details
            (insert (propertize "Request Details\n" 'face 'bold))
            (insert (propertize "==============\n\n" 'face 'bold))
            
            ;; Insert details for each request
            (cl-loop for stat in (reverse cw-activity-coder--stats)
                    for i from 1
                    do (insert (propertize (format "Request #%d:\n" i) 'face 'bold))
                    (insert (format "  Provider: %s\n" (plist-get stat :provider)))
                    (insert (format "  Model: %s\n" (plist-get stat :model)))
                    (insert (format "  Time: %.2f seconds\n" (plist-get stat :time)))
                    (insert (format "  Batch size: %d rows\n" (plist-get stat :batch-size)))
                    (insert (format "  Status: %s\n" 
                                   (if (plist-get stat :success) 
                                       (propertize "Success" 'face 'success)
                                     (propertize "Failed" 'face 'error))))
                    (when (plist-get stat :error)
                      (insert (propertize 
                               (format "  Error: %s\n" (plist-get stat :error))
                               'face 'error)))
                    (when (plist-get stat :success)
                      (insert (format "  Tokens: %d (prompt: %d, completion: %d)\n" 
                                     (plist-get stat :total-tokens)
                                     (plist-get stat :prompt-tokens)
                                     (plist-get stat :completion-tokens))))
                    (insert "\n"))))
        
        ;; Add keybindings for export
        (local-set-key (kbd "e") 'cw-activity-coder-export-results-csv)
        (local-set-key (kbd "s") 'cw-activity-coder-save-report)
        
        (goto-char (point-min))
        (display-buffer buf '(display-buffer-pop-up-window . nil))))))

;;;###autoload
(defun cw-activity-coder-process-buffer ()
  "Process the current CSV buffer, adding 'cw_at' codes via LLM API."
  (interactive)
  (unless (derived-mode-p 'csv-mode)
    (when (y-or-n-p "Not in CSV mode. Enable it?")
      (csv-mode)))
  
  ;; Select LLM provider if not set
  (unless cw-activity-coder-llm-provider
    (let ((providers '("xai" "openai" "anthropic" "gemini" "copilot")))
      (let ((choice (completing-read "Select LLM provider: " providers nil t)))
        (setq cw-activity-coder-llm-provider (intern choice)))))
  
  ;; Confirm with user before proceeding
  (unless (y-or-n-p (format "Process with %s? This may use API credits." 
                           cw-activity-coder-llm-provider))
    (user-error "Operation cancelled"))
  
  ;; Setup processing
  (setq cw-activity-coder--current-buffer (current-buffer))
  (setq cw-activity-coder--progress "CW: Starting...")
  (setq cw-activity-coder--stats nil)
  (force-mode-line-update)
  
  ;; Add reference column for tracking
  (cw-activity-coder--add-ref-column)
  
  ;; Calculate batches
  (let* ((total-lines (count-lines (point-min) (point-max)))
         (header-lines 1) ;; Assuming 1 header line
         (data-lines (- total-lines header-lines))
         (num-batches
          (max 1 (ceiling data-lines
                          cw-activity-coder-max-batch-size)))
         (batch-ranges
          (let (ranges)
            (dotimes (i num-batches)
              (let ((start
                     (+ (1+ header-lines) (* i cw-activity-coder-max-batch-size)))
                    (end
                     (min (+ (1+ header-lines)
                             (* (1+ i)
                                cw-activity-coder-max-batch-size))
                          total-lines)))
                (push (cons start end) ranges)))
            (nreverse ranges)))
         (results '())
         (sent 0)
         (completed 0)
         (errors 0))
    
    ;; Initialize request counter
    (setq cw-activity-coder--active-requests 0)
    
    ;; Check if we have data to process
    (if (< data-lines 1)
        (message "No data to process")
      
      ;; Show estimated cost if available
      (when-let* ((cost-per-1k (pcase cw-activity-coder-llm-provider
                                ('openai 0.01) ;; Example costs - adjust as needed
                                ('anthropic 0.015)
                                ('xai 0.01)
                                ('gemini 0.0035)
                                ('copilot 0.01)
                                (_ nil)))
                  (est-tokens (* data-lines 200)) ;; Rough estimate of tokens
                  (est-cost (* (/ est-tokens 1000.0) cost-per-1k)))
        (message "Estimated cost: $%.4f (based on ~%d tokens)" est-cost est-tokens))
      
      ;; Process each batch
      (dolist (range batch-ranges)
        (let ((batch (cw-activity-coder--parse-buffer-to-json
                      (car range) (cdr range))))
          (if (null batch)
              (progn
                (cl-incf completed)
                (cl-incf errors)
                (cw-activity-coder--update-progress
                 sent completed num-batches))
            (cw-activity-coder--api-request
             batch
             (lambda (batch-results)
               (when batch-results
                 (setq results (append results batch-results)))
               (cl-incf completed)
               (unless batch-results
                 (cl-incf errors))
               (cw-activity-coder--update-progress
                sent completed num-batches)
               (when (= completed num-batches)
                 (cw-activity-coder--update-buffer results)
                 (setq cw-activity-coder--progress 
                       (format "CW: Done! (%d errors)" errors))
                 (force-mode-line-update)
                 (cw-activity-coder--display-results)
                 (if (> errors 0)
                     (message "Processing complete with %d errors" errors)
                   (message "Processing complete!")))))))
        (cl-incf sent)
        (cw-activity-coder--update-progress
         sent completed num-batches)))))

(defvar cw-activity-coder-mode-line-entry
  '(:eval cw-activity-coder--progress))
(unless (member cw-activity-coder-mode-line-entry mode-line-misc-info)
  (add-to-list 'mode-line-misc-info cw-activity-coder-mode-line-entry
               t))

(defun cw-activity-coder-export-results-csv ()
  "Export the results statistics to a CSV file."
  (interactive)
  (when cw-activity-coder--stats
    (let* ((default-filename (format "cw-activity-results-%s.csv" 
                                    (format-time-string "%Y%m%d-%H%M%S")))
           (filename (read-file-name "Save CSV to: " nil nil nil default-filename))
           (provider-stats (make-hash-table :test 'eq)))
      
      ;; Collect provider stats
      (dolist (stat cw-activity-coder--stats)
        (let* ((provider (plist-get stat :provider))
               (provider-data (gethash provider provider-stats (list 0 0 0 0.0 0)))
               (requests (1+ (nth 0 provider-data)))
               (successful (+ (nth 1 provider-data) 
                              (if (plist-get stat :success) 1 0)))
               (tokens (+ (nth 2 provider-data) 
                          (plist-get stat :total-tokens 0)))
               (time (+ (nth 3 provider-data) 
                        (plist-get stat :time 0.0)))
               (rows (+ (nth 4 provider-data) 
                        (plist-get stat :batch-size 0))))
          (puthash provider (list requests successful tokens time rows) provider-stats)))
      
      ;; Write CSV file
      (with-temp-file filename
        (insert "Provider,Requests,Successful,Success Rate,Tokens,Time,Rows\n")
        (maphash (lambda (provider data)
                   (let ((requests (nth 0 data))
                         (successful (nth 1 data))
                         (tokens (nth 2 data))
                         (time (nth 3 data))
                         (rows (nth 4 data))
                         (success-rate (if (> (nth 0 data) 0)
                                          (* 100.0 (/ (nth 1 data) (nth 0 data)))
                                        0.0)))
                     (insert (format "%s,%d,%d,%.1f%%,%d,%.2f,%d\n"
                                    provider requests successful 
                                    success-rate tokens time rows))))
                 provider-stats)
        
        ;; Add summary row
        (let ((total-requests (length cw-activity-coder--stats))
              (successful-requests (cl-count-if (lambda (s) (plist-get s :success)) 
                                              cw-activity-coder--stats))
              (total-tokens (cl-reduce #'+ cw-activity-coder--stats 
                                     :key (lambda (s) (plist-get s :total-tokens)) 
                                     :initial-value 0))
              (total-time (cl-reduce #'+ cw-activity-coder--stats 
                                    :key (lambda (s) (plist-get s :time)) 
                                    :initial-value 0.0))
              (total-rows (cl-reduce #'+ cw-activity-coder--stats 
                                   :key (lambda (s) (plist-get s :batch-size)) 
                                   :initial-value 0))
              (success-rate (if (> (length cw-activity-coder--stats) 0)
                               (* 100.0 (/ (cl-count-if (lambda (s) (plist-get s :success)) 
                                                      cw-activity-coder--stats)
                                          (length cw-activity-coder--stats)))
                             0.0)))
          (insert (format "TOTAL,%d,%d,%.1f%%,%d,%.2f,%d\n"
                         total-requests successful-requests 
                         success-rate total-tokens total-time total-rows))))
      
      (message "Results exported to %s" filename))))

(defun cw-activity-coder-save-report ()
  "Save the results as a formatted report."
  (interactive)
  (when (and cw-activity-coder--stats cw-activity-coder--results-buffer)
    (let* ((default-filename (format "cw-activity-report-%s.txt" 
                                    (format-time-string "%Y%m%d-%H%M%S")))
           (filename (read-file-name "Save report to: " nil nil nil default-filename)))
      (with-current-buffer cw-activity-coder--results-buffer
        (write-region (point-min) (point-max) filename))
      (message "Report saved to %s" filename))))

;;;###autoload
(defun cw-activity-coder-show-results ()
  "Show the results of the last processing run."
  (interactive)
  (if cw-activity-coder--stats
      (cw-activity-coder--display-results)
    (message "No processing has been done yet.")))

;;;###autoload
(defun cw-activity-coder-setup ()
  "Setup the cw-activity-coder package interactively."
  (interactive)
  (let ((provider (completing-read 
                  "Select default LLM provider: "
                  '("xai" "openai" "anthropic" "gemini" "copilot")
                  nil t)))
    (setq cw-activity-coder-llm-provider (intern provider))
    
    ;; Try to get API key
    (condition-case nil
        (cw-activity-coder--get-api-key)
      (error
       (let ((key (read-passwd (format "Enter API key for %s: " provider))))
         (when (and key (not (string-empty-p key)))
           (setq cw-activity-coder-api-key key)
           (when (yes-or-no-p "Save this API key for future sessions?")
             (let ((save-method (completing-read 
                                "Save to: " 
                                '("Environment variable" "Key file") nil t)))
               (pcase save-method
                 ("Environment variable"
                  (let ((env-var (alist-get (intern provider) 
                                           cw-activity-coder-api-key-env-vars)))
                    (message "Add this to your shell config: export %s=\"%s\""
                             env-var key)))
                 ("Key file"
                  (let ((key-file (alist-get (intern provider) 
                                            cw-activity-coder-api-key-files)))
                    (with-temp-file (expand-file-name key-file)
                      (insert key))
                    (chmod (expand-file-name key-file) #o600)
                    (message "API key saved to %s" key-file))))))))))
    
    ;; Select model
    (let* ((default-model (alist-get (intern provider) cw-activity-coder-models))
           (model-options (pcase (intern provider)
                           ('openai '("gpt-4o" "gpt-4-turbo" "gpt-3.5-turbo"))
                           ('anthropic '("claude-3-opus-20240229" "claude-3-sonnet-20240229" "claude-3-haiku-20240307"))
                           ('xai '("grok-2-latest" "grok-1"))
                           ('gemini '("gemini-1.5-pro" "gemini-1.5-flash"))
                           ('copilot '("copilot-4"))
                           (_ (list default-model))))
           (selected-model (completing-read 
                           (format "Select model (default: %s): " default-model)
                           model-options nil nil nil nil default-model)))
      (setq cw-activity-coder-model selected-model))
    
    ;; Configure batch size
    (let* ((default-batch-size cw-activity-coder-max-batch-size)
           (batch-size-input (read-string 
                             (format "Max batch size (default: %d): " default-batch-size)
                             nil nil (number-to-string default-batch-size))))
      (setq cw-activity-coder-max-batch-size (string-to-number batch-size-input)))
    
    ;; Configure debug mode
    (setq cw-activity-coder-debug (yes-or-no-p "Enable debug mode? "))
    
    (message "Setup complete. Use M-x cw-activity-coder-process-buffer to process CSV files.")))

(provide 'cw-activity-coder)
;;; cw-activity-coder.el ends here
