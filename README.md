* CW Activity Coder

An Emacs package to process CSV/JSON files with the xAI API, assigning CW activity codes.

** Features
- Interactive Transient menu
- Live Org-mode output with error highlighting (NDE codes)
- Modeline progress display
- Dired integration for file selection
- Asynchronous API requests with rate limiting
- Persistent, editable activity codes in =activitycodes.json=
- CSV output to a customizable directory

** Requirements
- Emacs 30.1+
- Packages: =json=, =url=, =transient=, =org= (install =transient= via =M-x package-install=)
- =XAI_API_KEY= environment variable set

** Installation
1. Place =cw-activity-coder.el= and =activitycodes.json= in a directory (e.g., =~/.emacs.d/lisp/=):
   #+BEGIN_SRC shell
   mkdir -p ~/.emacs.d/lisp
   mv cw-activity-coder.el activitycodes.json ~/.emacs.d/lisp/
   #+END_SRC
2. Use =use-package= in your =init.el= (install =use-package= if needed):
   #+BEGIN_SRC emacs-lisp
   (use-package cw-activity-coder
     :load-path "~/.emacs.d/lisp/"
     :ensure nil  ; Since it's a local file
     :commands (cw-activity-coder)
     :custom
     (cw-activity-coder-output-dir "~/my-custom-output-dir/")  ; Optional customization
     (cw-activity-coder-rate-limit 1.5)                       ; Optional: 1.5 calls/second
     :init
     (setenv "XAI_API_KEY" "your-key-here")                  ; Optional: Set within Emacs
     :config
     (message "CW Activity Coder loaded"))
   #+END_SRC
3. Install =transient=:
   #+BEGIN_SRC shell
   M-x package-install RET transient RET
   #+END_SRC
4. If not using =:init= to set the key, set =XAI_API_KEY= in your shell:
   #+BEGIN_SRC shell
   export XAI_API_KEY="your-key-here"
   #+END_SRC

** Usage
1. Launch: =M-x cw-activity-coder=
2. Menu options:
   - =a=: Add files from Dired (mark with =m= in Dired first)
   - =p=: Process queued files
   - =r=: Show receipt
   - =e=: Edit activity codes (save with =C-c C-c=)
   - =c=: Clear queue
   - =q=: Quit
3. Monitor progress in the modeline and output in =*CW Activity Coder Output*=
4. Results saved as CSV in =cw-activity-coder-output-dir= (default =~/cw-activity-coder-output/=)
5. Edit codes via =e=; changes persist in =cw-activity-coder-activity-codes-file=

** Customization
- =M-x customize-group RET cw-activity-coder= to adjust:
  - =cw-activity-coder-batch-size=
  - =cw-activity-coder-rate-limit=
  - =cw-activity-coder-output-dir=
  - etc.
