;;; android-env.el --- Helper functions for working in android

;; Author: Fernando Jascovich
;; Keywords: android, gradle, java, tools, convenience
;; Version: 0.1
;; Url: https://github.com/fernando-jascovich/android-env.el
;; Package-Requires: ((emacs "24.1"))

;; This file is NOT part of GNU Emacs
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Helper functions for working in android, mainly helps with compiling,
;; launching emulators, deeplinks and common day-to-day tasks of an android
;; developer.

;;; Code:
(require 'compile)
(require 'hydra nil 'noerror)

(defgroup android-env nil
  "Configuration options for android-env.el"
  :group 'convenience)

(defcustom android-env-executable "./gradlew"
  "The android-env gradle exec."
  :type 'string
  :group 'android-env)

(defcustom android-env-command "installDev"
  "The android-env default gradle action."
  :type 'string
  :group 'android-env)

(defcustom android-env-test-command "testDev"
  "The android-env default gradle action."
  :type 'string
  :group 'android-env)

(defcustom android-env-emulator-command
  "~/.android/sdk/emulator/emulator"
  "Android emulator bin location."
  :type 'string
  :group 'android-env)

(defcustom android-env-unit-test-command "testDevDebug"
  "The android-env default unit test action."
  :type 'string
  :group 'android-env)


(defcustom android-env-adb-buffer-name "*android-adb*"
  "Name used for adb async output."
  :type 'string
  :group 'android-env)

(defun android-env-get-closest-pathname ()
  "Find closest ancestor directory containing gradlew file."
  (let ((file "gradlew")
        (root (expand-file-name "/"))
        (fname nil)
        (dir default-directory))
    (while (not fname)
      (if (file-exists-p (expand-file-name file dir))
          (setq fname (replace-regexp-in-string " " "\\ " dir t t))
        (setq dir (expand-file-name ".." dir))
        (if (equal dir root)
            (setq fname t))))
    (if (not (equal fname t))
        fname)))

(defun android-env-gradle-make ()
  "Look for nearest gradlew file and add javac and kotlin regex to compile."
  (unless (file-exists-p "gradlew")
    (set (make-local-variable 'compile-command)
         (let ((mkfile (android-env-get-closest-pathname))
               (fmt "cd %s; %s %s"))
           (if mkfile
               (format fmt
                       mkfile
                       android-env-executable
                       android-env-command)))))
  (add-to-list 'compilation-error-regexp-alist
               '(":compile.*?\\(/.*?\\):\\([0-9]+\\): " 1 2))
  (add-to-list 'compilation-error-regexp-alist
               '("^e: \\(.[^:]*\\): (\\([0-9]*\\), \\([0-9]*\\)" 1 2 3)))

(defun android-env ()
  "Set the classpath and invokes jde."
  (interactive)
  (add-hook 'java-mode-hook 'android-env-gradle-make)
  (add-hook 'nxml-mode-hook 'android-env-gradle-make)
  (add-hook 'web-mode-hook 'android-env-gradle-make)
  (add-hook 'groovy-mode-hook 'android-env-gradle-make)
  (add-hook 'kotlin-mode-hook 'android-env-gradle-make)
  (add-to-list 'compilation-error-regexp-alist
               '(":compile.*?\\(/.*?\\):\\([0-9]+\\): " 1 2))
  (android-env-gradle-make))

(defun android-env-gradle-daemon-start ()
  "Run gradlew in a shell buffer for daemon persistence."
  (interactive)
  (let ((mkfile (android-env-get-closest-pathname)))
    (call-process-shell-command
     (format "%s/gradlew &" mkfile) nil "*Gradle daemon*")))

(defun android-env-crashlytics (module build)
  "Assemble and upload the MODULE and BUILD to crashlytics."
  (interactive "sModule: \nsBuild: ")
  (let ((mkfile (android-env-get-closest-pathname))
        (fmt "cd %s; %s %s:assemble%s %s:crashlyticsUploadDistribution%s"))
    (compile (format fmt
                     mkfile
                     android-env-executable module build module build)))
  (android-env-gradle-make))

(defun android-env-gradle (gradle-cmd)
  "Execute GRADLE-CMD."
  (add-to-list 'compilation-error-regexp-alist
               '(":compile.*?\\(/.*?\\):\\([0-9]+\\): " 1 2))
  (add-to-list 'compilation-error-regexp-alist
               '("^e: \\(.[^:]*\\): (\\([0-9]*\\), \\([0-9]*\\)" 1 2 3))
  (let ((mkfile (android-env-get-closest-pathname)) cmd)
    (compile (format "cd %s; %s %s" mkfile android-env-executable gradle-cmd))))

(defun android-env-test ()
  "Execute instrumented test."
  (interactive)
  (android-env-gradle android-env-test-command))

(defun android-env-unit-test ()
  "Execute unit test."
  (interactive)
  (android-env-gradle android-env-unit-test-command))

(defun android-env-avd-list ()
  "Return shell command output as list."
  (let (out out-list avdmanager)
    (setq avdmanager (concat
                      (getenv "ANDROID_SDK_ROOT")
                      "/tools/bin/avdmanager"))
    (setq out (shell-command-to-string
               (concat avdmanager " list avd --compact -0")))
    (setq out-list (split-string out "\0"))
    (delete (pop out-list) out-list)
    (delete "" out-list)))

(defun android-env-avd ()
  "Prompts for avd launch based on current avds."
  (interactive)
  (message "Getting avd list..." )
  (let (selected)
    (setq selected (completing-read "Select avd: " (android-env-avd-list)))
    (async-shell-command
     (format "%s @%s" android-env-emulator-command selected)
     (format "*android-emulator-%s" selected))))

(defun android-env-adb ()
  "Return adb full path based on ANDROID_SDK_ROOT env var."
  (concat (getenv "ANDROID_SDK_ROOT") "/platform-tools/adb"))

(defun android-env-auto-dhu ()
  "Launches android auto desktop head unit."
  (interactive)
  (let ((cmd1 "$ANDROID_SDK_ROOT/platform-tools/adb forward tcp:5277 tcp:5277")
        (cmd2 "$ANDROID_SDK_ROOT/extras/google/auto/desktop-head-unit")
        (bname "*android-auto-dhu*"))
    (async-shell-command (format "%s && %s" cmd1 cmd2) bname)))

(defun android-env-logcat-clear ()
  "Clear android logcat."
  (interactive)
  (shell-command (concat (android-env-adb) " logcat -c")))

(defun android-env-logcat-buffer (&optional logcat-args)
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
      (apply 'start-process
             "Android Logcat"
             bname
             (android-env-adb) "logcat" logcat-args)
      (face-remap-add-relative 'default '(:height 105))
      (view-mode))))

(defun android-env-logcat (&optional tag)
  "Show logcat using TAG for filtering."
  (interactive "sTag: ")
  (let ((bname "*Android Logcat*")
        (args '())
        (args-format ""))
    (when (and tag (not (string= "" tag)))
      (add-to-list 'args "*:S")
      (add-to-list 'args tag t))
    (android-env-logcat-buffer args)))

(defun android-env-logcat-crash ()
  "Show logcat's crash buffer."
  (interactive)
  (android-env-logcat-buffer '("-b" "crash")))


(defun android-env-uninstall-app (package)
  "Uninstall application by PACKAGE name."
  (interactive "sPackage: ")
  (shell-command
   (format "%s shell pm uninstall '%s'" (android-env-adb) package)
   android-env-adb-buffer-name))


(defun android-env-deeplink (deeplink)
  "Send DEEPLINK to emulator."
  (interactive "sDeep link: ")
  (shell-command
   (format "%s shell am start -a android.intent.action.VIEW -d \"%s\""
           (android-env-adb)
           deeplink)
   android-env-adb-buffer-name))

;;; Hydras
(when (require 'hydra nil 'noerror)
  (defhydra hydra-android (:color teal :hint nil)
    "
^Compiling^            ^Devices^       ^Logcat^                 ^Adb^
^^^^^------------------------------------------------------------------------
_w_: Compile           _e_: Avd        _l_: Logcat              _U_: Uninstall
_s_: Instrumented Test _d_: Auto DHU   _c_: Logcat crash        _L_: Deep link
_u_: Unit Test         ^ ^             _C_: Logcat clear
_x_: Crashlytics
"
    ("w" compile)
    ("s" android-env-test)
    ("u" android-env-unit-test)
    ("e" android-env-avd)
    ("d" android-env-auto-dhu)
    ("l" android-env-logcat)
    ("c" android-env-logcat-crash)
    ("C" android-env-logcat-clear)
    ("x" android-env-crashlytics)
    ("U" android-env-uninstall-app)
    ("L" android-env-deeplink)
    ("q" nil "quit")))

(provide 'android-env)

;;; android-env.el ends here
