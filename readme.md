# Android helper functions for Emacs

This is simply a bunch of helpers for develop android using emacs.

If you are trying to use emacs for working in android code, you should take a look at:

* https://github.com/dakrone/emacs-java-imports
* https://github.com/Emacs-Kotlin-Mode-Maintainers/kotlin-mode

## Installing

Package is available at melpa, so you could simply:
```
(use-package android-env
  :after hydra
  :bind (("C-c a" . hydra-android/body))
  :config
  (setq android-env-executable "./gradlew")
  (setq android-env-test-command "connectedDevDebugAndroidTest")
  (setq android-env-unit-test-command "testDevDebug")
  (android-env))
```

Or use the package manager of your choice.

Notice that in case you are using [hydras](https://github.com/abo-abo/hydra) you'll want to load this package after `hydra` and make use of `hydra-android` defined in the execution of `(android-env)`.
