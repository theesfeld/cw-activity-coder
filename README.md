
# CW Activity Coder

An Emacs package to process CSV/JSON files with the xAI API, assigning CW activity codes.

## Features

- Interactive Transient menu
- Live Org-mode output with error highlighting (NDE codes)
- Modeline progress display
- Dired integration for file selection
- Asynchronous API requests with rate limiting
- Persistent, editable activity codes in ```activitycodes.json```
- CSV output to a customizable directory

## Requirements

- Emacs 30.1+ (with built-in ```use-package```)
- Dependencies: ```json```, ```url```, ```transient```, ```org``` (listed in ```Package-Requires```)
- ```XAI_API_KEY``` environment variable set

## Installation

### From GitHub (Recommended)

Emacs 30.1’s ```use-package``` can install this package directly from GitHub. Add this to your ```init.el```:

```emacs-lisp
(use-package cw-activity-coder
  :vc (:vc-backend Git :url "https://github.com/theesfeld/cw-activity-coder.git" :branch "master")
  :commands (cw-activity-coder)
  :custom
  (cw-activity-coder-api-key (getenv "XAI_API_KEY"))
  (cw-activity-coder-model "grok-2-latest")
  (cw-activity-coder-batch-size 100)
  (cw-activity-coder-rate-limit 2.0)
  (cw-activity-coder-max-retries 3)
  (cw-activity-coder-api-timeout 300)
  (cw-activity-coder-output-dir (expand-file-name "~/cw-activity-coder-output/"))
  (cw-activity-coder-activity-codes-file (expand-file-name "activitycodes.json" cw-activity-coder-output-dir))
  :init
  (unless cw-activity-coder-api-key
    (setenv "XAI_API_KEY" "your-key-here"))
  :config
  (message "CW Activity Coder loaded"))
```

#### Steps:

1. **Ensure Package System is Initialized**:
   - Emacs 30.1 includes ```package.el``` by default. Ensure your package archives are up-to-date:
     ```emacs-lisp
     M-x package-refresh-contents RET
     ```

2. **Set ```XAI_API_KEY```**:
   - In your shell (e.g., ```.bashrc``` or ```.zshrc```):
     ```bash
     export XAI_API_KEY="your-key-here"
     ```
   - Or use the ```:init``` block to set it within Emacs.

3. **Install**:
   - Restart Emacs or evaluate the ```use-package``` block with ```C-x C-e```.
   - ```use-package``` will clone the repository from ```https://github.com/theesfeld/cw-activity-coder.git``` and install dependencies like ```transient``` from MELPA.

### Manual Installation

Alternatively, clone the repo and load manually:

```bash
git clone https://github.com/theesfeld/cw-activity-coder.git ~/.emacs.d/lisp/cw-activity-coder
```

Add to ```init.el```:

```emacs-lisp
(use-package cw-activity-coder
  :load-path "~/.emacs.d/lisp/cw-activity-coder"
  :ensure nil
  :commands (cw-activity-coder))
```

Install ```transient``` manually if not already present:

```emacs-lisp
M-x package-install RET transient RET
```

## Usage

1. Launch: ```M-x cw-activity-coder```
2. Menu options:
   - ```a```: Add files from Dired (mark with ```m``` in Dired first)
   - ```p```: Process queued files
   - ```r```: Show receipt
   - ```e```: Edit activity codes (save with ```C-c C-c```)
   - ```c```: Clear queue
   - ```q```: Quit
3. Monitor progress in the modeline and output in ```*CW Activity Coder Output*```
4. Results saved as CSV in ```cw-activity-coder-output-dir``` (default ```~/cw-activity-coder-output/```)
5. Edit codes via ```e```; changes persist in ```cw-activity-coder-activity-codes-file```

## Customization

- Run ```M-x customize-group RET cw-activity-coder``` to adjust:
  - ```cw-activity-coder-batch-size```
  - ```cw-activity-coder-rate-limit```
  - ```cw-activity-coder-output-dir```
  - etc.

## Notes

- The ```Package-Requires``` header ensures ```transient``` is installed as a dependency when fetched via ```:vc```. If it’s missing, Emacs will prompt to install it from MELPA.
- Ensure ```package-vc-refresh``` is run if you encounter issues with the initial fetch (```M-x package-vc-refresh RET cw-activity-coder RET```).
