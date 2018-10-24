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

(defcustom android-env/unit-test-command "testDevDebug"
  "The android-env default unit test action."
  :type 'string
  :group 'android-env)


(defcustom android-env/adb-buffer-name "*android-adb*"
  "Name used for adb async output."
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

(defun android-gradle (gradle-cmd)
  "Execute GRADLE-CMD."
  (add-to-list 'compilation-error-regexp-alist
               '(":compile.*?\\(/.*?\\):\\([0-9]+\\): " 1 2))
  (add-to-list 'compilation-error-regexp-alist
               '("^e: \\(.[^:]*\\): (\\([0-9]*\\), \\([0-9]*\\)" 1 2 3))
  (let ((mkfile (get-closest-pathname "gradlew")) cmd)
    (compile (format "cd %s; %s %s" mkfile android-env/executable gradle-cmd))))

(defun android-test ()
  "Execute instrumented test."
  (interactive)
  (android-gradle android-env/test-command))

(defun android-unit-test ()
  "Execute unit test."
  (interactive)
  (android-gradle android-env/unit-test-command))

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

(defun android-logcat-clear ()
  "Clear android logcat."
  (interactive)
  (shell-command "adb logcat -c"))

(defun android-logcat-buffer (&optional logcat-args)
  "Handles buffer related tasks using LOGCAT-ARGS."
  (let ((bname "*Android Logcat*"))
    (with-current-buffer (get-buffer-create bname)
      (switch-to-buffer bname)
      (let ((inhibit-read-only t)
            (p (get-buffer-process (current-buffer))))
        (erase-buffer)
        (while p
          (delete-process p)
          (setq p (get-buffer-process (current-buffer)))))
      (apply 'start-process "Android Logcat" bname "adb" "logcat" logcat-args)
      (face-remap-add-relative 'default '(:height 105))
      (view-mode))))

(defun android-logcat (&optional tag)
  "Show logcat using TAG for filtering."
  (interactive "sTag: ")
  (let ((bname "*Android Logcat*")
        (args '())
        (args-format ""))
    (when (and tag (not (string= "" tag)))
      (add-to-list 'args "*:S")
      (add-to-list 'args tag t))
    (android-logcat-buffer args)))

(defun android-logcat-crash ()
  "Show logcat's crash buffer."
  (interactive)
  (android-logcat-buffer '("-b" "crash")))


(defun android-uninstall-app (package)
  "Uninstall application by PACKAGE name."
  (interactive "sPackage: ")
  (shell-command
   (format "adb shell pm uninstall '%s'" package)
   android-env/adb-buffer-name))


(defun android-deeplink (deeplink)
  "Send DEEPLINK to emulator."
  (interactive "sDeep link: ")
  (shell-command
   (format "adb shell am start -a android.intent.action.VIEW -d \"%s\"" deeplink)
   android-env/adb-buffer-name))

;;; Hydras
(when (require 'hydra nil 'noerror)
  (defhydra hydra-android (:color teal :hint nil)
    "

^Compiling^                ^Devices^            ^Logcat^                    ^Adb^
^^^^^-------------------------------------------------------------------------------------
_w_: Compile               _e_: Avd             _l_: Logcat                 _U_: Uninstall
_s_: Instrumented Test     _d_: Auto DHU        _c_: Logcat crash           _L_: Deep link
_u_: Unit Test             ^ ^                  _C_: Logcat clear
_x_: Crashlytics

"
    ("w" compile)
    ("s" android-test)
    ("u" android-unit-test)
    ("e" android-avd)
    ("d" android-auto-dhu)
    ("l" android-logcat)
    ("c" android-logcat-crash)
    ("C" android-logcat-clear)
    ("x" android-crashlytics)
    ("U" android-uninstall-app)
    ("L" android-deeplink)
    ("q" nil "quit")))

(provide 'android-env)
;;; android-env.el ends here
