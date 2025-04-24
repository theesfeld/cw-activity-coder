
# CW Activity Coder

![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)
![Emacs: 27.1+](https://img.shields.io/badge/Emacs-27.1+-blueviolet.svg)

An Emacs package to process CSV/JSON files with various LLM APIs, assigning CW activity codes.

## Features

- **Multi-Provider Support**: Use xAI, OpenAI, Anthropic, Google Gemini, or GitHub Copilot
- **Interactive Setup**: Easy configuration with `M-x cw-activity-coder-setup`
- **Secure API Key Management**: Store keys in environment variables or secure files
- **Cost Estimation**: See estimated API costs before processing
- **Comprehensive Dashboard**: Detailed statistics with token usage and cost breakdown
- **Export Options**: Save results as CSV or formatted reports
- **Progress Tracking**: Real-time updates in the mode line
- **Error Handling**: Robust error recovery and reporting

## Requirements

- Emacs 30.1+ (with built-in ```use-package```)
- Dependencies: ```json```, ```url```, ```transient```, ```org``` (listed in ```Package-Requires```)
- API key for at least one supported LLM provider

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

## Quick Start

1. **Setup**: Run `M-x cw-activity-coder-setup` to configure your preferred LLM provider and API key
2. **Process**: Open a CSV file and run `M-x cw-activity-coder-process-buffer`
3. **Review**: Check the results dashboard that appears after processing
4. **Export**: Press `e` in the results buffer to export as CSV or `s` to save as a report

## API Key Management

You can provide your API key in three ways:

1. **Direct setting**: `(setq cw-activity-coder-api-key "your-key-here")`
2. **Environment variable**:
   ```bash
   export OPENAI_API_KEY="your-key-here"
   # Or for other providers:
   export XAI_API_KEY="your-key-here"
   export ANTHROPIC_API_KEY="your-key-here"
   export GEMINI_API_KEY="your-key-here"
   export GITHUB_COPILOT_TOKEN="your-key-here"
   ```
3. **Key file**: Create a file at `~/.openai_key` (or `~/.xai_key`, etc.) containing just your API key

The interactive setup will guide you through this process and offer to save your key securely.

## Available Commands

- `M-x cw-activity-coder-setup` - Interactive setup wizard
- `M-x cw-activity-coder-process-buffer` - Process the current CSV buffer
- `M-x cw-activity-coder-show-results` - Show results from the last processing run
- In results buffer:
  - `e` - Export results as CSV
  - `s` - Save as formatted report
  - `q` - Close the results buffer

## Customization

Run `M-x customize-group RET cw-activity-coder` to adjust:

| Variable | Description | Default |
|----------|-------------|---------|
| `cw-activity-coder-llm-provider` | LLM provider to use | `nil` (prompt) |
| `cw-activity-coder-api-key` | API key | `nil` (use env/file) |
| `cw-activity-coder-model` | Model name | Provider default |
| `cw-activity-coder-max-batch-size` | Rows per API batch | `100` |
| `cw-activity-coder-api-timeout` | API timeout in seconds | `300` |
| `cw-activity-coder-debug` | Enable debug messages | `nil` |
| `cw-activity-coder-show-results-buffer` | Show results dashboard | `t` |

## Notes

- The ```Package-Requires``` header ensures ```transient``` is installed as a dependency when fetched via ```:vc```. If it’s missing, Emacs will prompt to install it from MELPA.
- Ensure ```package-vc-refresh``` is run if you encounter issues with the initial fetch (```M-x package-vc-refresh RET cw-activity-coder RET```).
