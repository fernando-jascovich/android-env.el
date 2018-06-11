;;; android-env --- M-x compile support
;;; Commentary:
;;; Helper functions for working in android

;;; Code:

(defgroup android-env nil
  "Configuration options for android-env.el"
  :group 'convenience)

(defcustom android-env/executable "./gradlew"
  "The android-env gradle exec."
  :type 'string
  :group 'android-env)

(defcustom android-env/command "installDev"
  "The android-env default gradle action."
  :type 'string
  :group 'android-env)

(defcustom android-env/test-command "testDev"
  "The android-env default gradle action."
  :type 'string
  :group 'android-env)

(defcustom android-env/emulator-command
  "~/.android/sdk/emulator/emulator"
  "Android emulator bin location."
  :type 'string
  :group 'android-env)

(require 'compile)
(require 'cl)

(defun* get-closest-pathname (&optional (file "Makefile"))
  (let ((root (expand-file-name "/")))
    (loop for d = default-directory
          then (expand-file-name ".." d)
          if (file-exists-p
              (expand-file-name file d)) return (replace-regexp-in-string " " "\\ " d t t)
              if (equal d root) return nil)))

(defun gradleMake ()
  (unless (file-exists-p "gradlew")
    (set (make-local-variable 'compile-command)
         (let ((mkfile (get-closest-pathname "gradlew")))
           (if mkfile
               (format "cd %s; %s %s" mkfile android-env/executable android-env/command)))))
  (add-to-list 'compilation-error-regexp-alist
               '(":compile.*?\\(/.*?\\):\\([0-9]+\\): " 1 2))
  (add-to-list 'compilation-error-regexp-alist
               '("^e: \\(.[^:]*\\): (\\([0-9]*\\), \\([0-9]*\\)" 1 2 3)))

(defun android-env ()
  "Set the classpath and invokes jde."
  (interactive)
  (add-hook 'java-mode-hook 'gradleMake)
  (add-hook 'nxml-mode-hook 'gradleMake)
  (add-hook 'web-mode-hook 'gradleMake)
  (add-hook 'groovy-mode-hook 'gradleMake)
  (add-hook 'kotlin-mode-hook 'gradleMake)
  (add-to-list 'compilation-error-regexp-alist
               '(":compile.*?\\(/.*?\\):\\([0-9]+\\): " 1 2))
  (gradleMake))

(defun android-gradle-daemon-start ()
  "Run gradlew in a shell buffer for daemon persistence."
  (interactive)
  (let ((mkfile (get-closest-pathname "gradlew")))
    (call-process-shell-command (format "%s/gradlew &" mkfile) nil "*Gradle daemon*")))

(defun android-crashlytics (module build)
  "Triggers the gradle commands for assemble and upload the MODULE and BUILD to crashlytics."
  (interactive "sModule: \nsBuild: ")
  (let ((mkfile (get-closest-pathname "gradlew")))
    (compile (format "cd %s; %s %s:assemble%s %s:crashlyticsUploadDistribution%s" mkfile android-env/executable module build module build)))
  (gradleMake))

(defun android-test ()
  "Execute test."
  (interactive)
  (add-to-list 'compilation-error-regexp-alist
               '(":compile.*?\\(/.*?\\):\\([0-9]+\\): " 1 2))
  (add-to-list 'compilation-error-regexp-alist
               '("^e: \\(.[^:]*\\): (\\([0-9]*\\), \\([0-9]*\\)" 1 2 3))
  (let ((mkfile (get-closest-pathname "gradlew")) cmd)
    (compile (format "cd %s; %s %s" mkfile android-env/executable android-env/test-command))))

(defun android-avd-list ()
  "Return shell command output as list."
  (let (out out-list)
    (setq out (shell-command-to-string "avdmanager list avd --compact -0"))
    (setq out-list (split-string out "\0"))
    (delete (pop out-list) out-list)
    (delete "" out-list)))

(defun android-avd ()
  "Prompts for avd launch based on current avds."
  (interactive)
  (message "Getting avd list..." )
  (let (selected)
    (setq selected (ido-completing-read "Select avd: " (android-avd-list)))
    (async-shell-command
     (format "%s @%s" android-env/emulator-command selected)
     (format "*android-emulator-%s" selected))))

(defun android-auto-dhu ()
  "Launches android auto desktop head unit."
  (interactive)
  (let ((cmd1 "adb forward tcp:5277 tcp:5277")
        (cmd2 "$ANDROID_SDK_ROOT/extras/google/auto/desktop-head-unit")
        (bname "*android-auto-dhu*"))
    (async-shell-command (format "%s && %s" cmd1 cmd2) bname)))

(provide 'android-env)
;;; android-env.el ends here