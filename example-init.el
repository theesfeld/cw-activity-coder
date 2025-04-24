;; Complete use-package configuration for cw-activity-coder
(use-package cw-activity-coder
  :vc (:fetcher github :repo "username/cw-activity-coder")
  :custom
  ;; LLM Provider Configuration
  (cw-activity-coder-llm-provider 'openai)  ;; Choose: 'xai, 'openai, 'anthropic, 'gemini, 'copilot
  (cw-activity-coder-api-key nil)  ;; Set to your API key or leave nil to use env var or key file
  
  ;; Optional: Override default models
  ;; (cw-activity-coder-model "gpt-4-turbo")  ;; Override default model for the selected provider
  
  ;; Optional: Custom API key locations
  ;; (cw-activity-coder-api-key-env-vars '((openai . "MY_OPENAI_KEY")))  ;; Custom env var names
  ;; (cw-activity-coder-api-key-files '((openai . "~/.config/openai/key")))  ;; Custom key file paths
  
  ;; API Request Configuration
  (cw-activity-coder-api-timeout 300)
  (cw-activity-coder-max-batch-size 100)
  
  ;; UI Configuration
  (cw-activity-coder-debug nil)
  (cw-activity-coder-show-results-buffer t)
  (cw-activity-coder-results-buffer-name "*CW Activity Results*")
  
  :bind
  (("C-c a c" . cw-activity-coder-process-buffer)
   ("C-c a r" . cw-activity-coder-show-results))
  
  :mode ("\\.csv\\'" . csv-mode)
  
  :hook
  (csv-mode . (lambda ()
                (local-set-key (kbd "C-c C-p") 'cw-activity-coder-process-buffer)))
  
  :config
  ;; Add to mode line if desired
  (unless (member cw-activity-coder-mode-line-entry mode-line-misc-info)
    (add-to-list 'mode-line-misc-info cw-activity-coder-mode-line-entry t))
  
  ;; Optional: Set up additional keybindings or configurations
  (with-eval-after-load 'csv-mode
    (define-key csv-mode-map (kbd "C-c C-a") 'cw-activity-coder-process-buffer))
  
  ;; Optional: Set up auto-processing for specific directories
  (when (boundp 'auto-mode-alist)
    (add-to-list 'auto-mode-alist '("/path/to/activity/logs/.*\\.csv\\'" . 
                                    (lambda ()
                                      (csv-mode)
                                      (when (y-or-n-p "Process with activity coder?")
                                        (cw-activity-coder-process-buffer))))))
  
  :init
  ;; Any initialization before loading the package
  (setq csv-separators '("," ";" "\t"))
  
  :defer t)
