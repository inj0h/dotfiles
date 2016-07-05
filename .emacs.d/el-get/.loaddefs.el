;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "../../.dotfiles/.emacs.d/el-get/el-get/el-get"
;;;;;;  "el-get/el-get.el" "1a33697168806a0939831603f6d737a4")
;;; Generated autoloads from el-get/el-get.el

(autoload 'el-get-version "../../.dotfiles/.emacs.d/el-get/el-get/el-get" "\
Message the current el-get version

\(fn)" t nil)

(autoload 'el-get-install "../../.dotfiles/.emacs.d/el-get/el-get/el-get" "\
Cause the named PACKAGE to be installed after all of its
dependencies (if any).

PACKAGE may be either a string or the corresponding symbol.

\(fn PACKAGE)" t nil)

(autoload 'el-get-update "../../.dotfiles/.emacs.d/el-get/el-get/el-get" "\
Update PACKAGE.

\(fn PACKAGE)" t nil)

(autoload 'el-get-update-all "../../.dotfiles/.emacs.d/el-get/el-get/el-get" "\
Performs update of all installed packages.

\(fn &optional NO-PROMPT)" t nil)

(autoload 'el-get-update-packages-of-type "../../.dotfiles/.emacs.d/el-get/el-get/el-get" "\
Update all installed packages of type TYPE.

\(fn TYPE)" t nil)

(autoload 'el-get-self-update "../../.dotfiles/.emacs.d/el-get/el-get/el-get" "\
Update el-get itself.  The standard recipe takes care of reloading the code.

\(fn)" t nil)

(autoload 'el-get-remove "../../.dotfiles/.emacs.d/el-get/el-get/el-get" "\
Remove any PACKAGE that is know to be installed or required.

\(fn PACKAGE)" t nil)

(autoload 'el-get-reinstall "../../.dotfiles/.emacs.d/el-get/el-get/el-get" "\
Remove PACKAGE and then install it again.

\(fn PACKAGE)" t nil)

(autoload 'el-get-cd "../../.dotfiles/.emacs.d/el-get/el-get/el-get" "\
Open dired in the package directory.

\(fn PACKAGE)" t nil)

(autoload 'el-get-make-recipes "../../.dotfiles/.emacs.d/el-get/el-get/el-get" "\
Loop over `el-get-sources' and write a recipe file for each
entry which is not a symbol and is not already a known recipe.

\(fn &optional DIR)" t nil)

(autoload 'el-get-checksum "../../.dotfiles/.emacs.d/el-get/el-get/el-get" "\
Compute the checksum of the given package, and put it in the kill-ring

\(fn PACKAGE)" t nil)

(autoload 'el-get-self-checksum "../../.dotfiles/.emacs.d/el-get/el-get/el-get" "\
Compute the checksum of the running version of el-get itself.

Also put the checksum in the kill-ring.

\(fn)" t nil)

(autoload 'el-get "../../.dotfiles/.emacs.d/el-get/el-get/el-get" "\
Ensure that packages have been downloaded once and init them as needed.

This will not update the sources by using `apt-get install' or
`git pull', but it will ensure that:

* the packages have been installed
* load-path is set so their elisp files can be found
* Info-directory-list is set so their info files can be found
* Autoloads have been prepared and evaluated for each package
* Any post-installation setup (e.g. `(require 'feature)') happens

When SYNC is nil (the default), all installations run
concurrently, in the background.

When SYNC is 'sync, each package will be installed synchronously,
and any error will stop it all.

Please note that the `el-get-init' part of `el-get' is always
done synchronously. There's `byte-compile' support though, and
the packages you use are welcome to use `autoload' too.

PACKAGES is expected to be a list of packages you want to install
or init.  When PACKAGES is omited (the default), the list of
already installed packages is considered.

\(fn &optional SYNC &rest PACKAGES)" nil nil)

;;;***

;;;### (autoloads nil "../../.dotfiles/.emacs.d/el-get/el-get/el-get-bundle"
;;;;;;  "el-get/el-get-bundle.el" "9dad303bdadcbad1beef42e29fa63fb1")
;;; Generated autoloads from el-get/el-get-bundle.el

(autoload 'el-get-bundle-el-get "../../.dotfiles/.emacs.d/el-get/el-get/el-get-bundle" "\


\(fn SRC)" nil nil)

(autoload 'el-get-bundle "../../.dotfiles/.emacs.d/el-get/el-get/el-get-bundle" "\
Install PACKAGE and run initialization FORM.

PACKAGE can be either a simple package name or a package name
with a modifier before the name to specify local recipe source
information:

* `<owner>/' : specifies a Github owner name
* `gist:<id>' : specifies a Gist ID
* `<type>:' : specifies a type of the package source

If `FEATURE in PACKAGE' form is used instead of PACKAGE, then
that FEATURE is `require'd after installing PACKAGE.  You can
also use `el-get-bundle!' macro if FEATURE and PACKAGE are the
same.  If you wish to `require' more than one feature, then use
`:features' property in FORM.

The initialization FORM may start with a property list that
describes a local recipe.  The FORM after the property list is
treated as initialization code, which is actually an `:after'
property of the local recipe.

A copy of the initialization code is stored in a directory
specified by `el-get-bundle-init-directory' and its byte-compiled
version is used if `el-get-bundle-byte-compile' is non-nil.

\(fn PACKAGE &rest FORM)" nil t)

(put 'el-get-bundle 'lisp-indent-function 'defun)

(autoload 'el-get-bundle! "../../.dotfiles/.emacs.d/el-get/el-get/el-get-bundle" "\
Install PACKAGE and run initialization form.
It is the same as `el-get-bundle' except that PACKAGE is explicitly
required.

\(fn PACKAGE &rest ARGS)" nil t)

(put 'el-get-bundle! 'lisp-indent-function 'defun)

;;;***

;;;### (autoloads nil "../../.dotfiles/.emacs.d/el-get/el-get/el-get-check"
;;;;;;  "el-get/el-get-check.el" "6145ed8054e30cf9530ba7c381446a14")
;;; Generated autoloads from el-get/el-get-check.el

(autoload 'el-get-check-recipe "../../.dotfiles/.emacs.d/el-get/el-get/el-get-check" "\
Check the format of the recipe.
Please run this command before sending a pull request.
Usage: M-x el-get-check-recipe RET

You can run this function from checker script like this:
    test/check-recipe.el PATH/TO/RECIPE.rcp

When used as a lisp function, FILE-OR-BUFFER must be a buffer
object or a file path.

\(fn FILE-OR-BUFFER)" t nil)

;;;***

;;;### (autoloads nil "../../.dotfiles/.emacs.d/el-get/el-get/el-get-list-packages"
;;;;;;  "el-get/el-get-list-packages.el" "af145f8b49dd63632e6277b79d546813")
;;; Generated autoloads from el-get/el-get-list-packages.el

(autoload 'el-get-list-packages "../../.dotfiles/.emacs.d/el-get/el-get/el-get-list-packages" "\
Display a list of packages.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "el-get/el-get" "../../../.emacs.d/el-get/el-get/el-get.el"
;;;;;;  "1a33697168806a0939831603f6d737a4")
;;; Generated autoloads from ../../../.emacs.d/el-get/el-get/el-get.el

(autoload 'el-get-version "el-get/el-get" "\
Message the current el-get version

\(fn)" t nil)

(autoload 'el-get-install "el-get/el-get" "\
Cause the named PACKAGE to be installed after all of its
dependencies (if any).

PACKAGE may be either a string or the corresponding symbol.

\(fn PACKAGE)" t nil)

(autoload 'el-get-update "el-get/el-get" "\
Update PACKAGE.

\(fn PACKAGE)" t nil)

(autoload 'el-get-update-all "el-get/el-get" "\
Performs update of all installed packages.

\(fn &optional NO-PROMPT)" t nil)

(autoload 'el-get-update-packages-of-type "el-get/el-get" "\
Update all installed packages of type TYPE.

\(fn TYPE)" t nil)

(autoload 'el-get-self-update "el-get/el-get" "\
Update el-get itself.  The standard recipe takes care of reloading the code.

\(fn)" t nil)

(autoload 'el-get-remove "el-get/el-get" "\
Remove any PACKAGE that is know to be installed or required.

\(fn PACKAGE)" t nil)

(autoload 'el-get-reinstall "el-get/el-get" "\
Remove PACKAGE and then install it again.

\(fn PACKAGE)" t nil)

(autoload 'el-get-cd "el-get/el-get" "\
Open dired in the package directory.

\(fn PACKAGE)" t nil)

(autoload 'el-get-make-recipes "el-get/el-get" "\
Loop over `el-get-sources' and write a recipe file for each
entry which is not a symbol and is not already a known recipe.

\(fn &optional DIR)" t nil)

(autoload 'el-get-checksum "el-get/el-get" "\
Compute the checksum of the given package, and put it in the kill-ring

\(fn PACKAGE)" t nil)

(autoload 'el-get-self-checksum "el-get/el-get" "\
Compute the checksum of the running version of el-get itself.

Also put the checksum in the kill-ring.

\(fn)" t nil)

(autoload 'el-get "el-get/el-get" "\
Ensure that packages have been downloaded once and init them as needed.

This will not update the sources by using `apt-get install' or
`git pull', but it will ensure that:

* the packages have been installed
* load-path is set so their elisp files can be found
* Info-directory-list is set so their info files can be found
* Autoloads have been prepared and evaluated for each package
* Any post-installation setup (e.g. `(require 'feature)') happens

When SYNC is nil (the default), all installations run
concurrently, in the background.

When SYNC is 'sync, each package will be installed synchronously,
and any error will stop it all.

Please note that the `el-get-init' part of `el-get' is always
done synchronously. There's `byte-compile' support though, and
the packages you use are welcome to use `autoload' too.

PACKAGES is expected to be a list of packages you want to install
or init.  When PACKAGES is omited (the default), the list of
already installed packages is considered.

\(fn &optional SYNC &rest PACKAGES)" nil nil)

;;;***

;;;### (autoloads nil "el-get/el-get-bundle" "../../../.emacs.d/el-get/el-get/el-get-bundle.el"
;;;;;;  "9dad303bdadcbad1beef42e29fa63fb1")
;;; Generated autoloads from ../../../.emacs.d/el-get/el-get/el-get-bundle.el

(autoload 'el-get-bundle-el-get "el-get/el-get-bundle" "\


\(fn SRC)" nil nil)

(autoload 'el-get-bundle "el-get/el-get-bundle" "\
Install PACKAGE and run initialization FORM.

PACKAGE can be either a simple package name or a package name
with a modifier before the name to specify local recipe source
information:

* `<owner>/' : specifies a Github owner name
* `gist:<id>' : specifies a Gist ID
* `<type>:' : specifies a type of the package source

If `FEATURE in PACKAGE' form is used instead of PACKAGE, then
that FEATURE is `require'd after installing PACKAGE.  You can
also use `el-get-bundle!' macro if FEATURE and PACKAGE are the
same.  If you wish to `require' more than one feature, then use
`:features' property in FORM.

The initialization FORM may start with a property list that
describes a local recipe.  The FORM after the property list is
treated as initialization code, which is actually an `:after'
property of the local recipe.

A copy of the initialization code is stored in a directory
specified by `el-get-bundle-init-directory' and its byte-compiled
version is used if `el-get-bundle-byte-compile' is non-nil.

\(fn PACKAGE &rest FORM)" nil t)

(put 'el-get-bundle 'lisp-indent-function 'defun)

(autoload 'el-get-bundle! "el-get/el-get-bundle" "\
Install PACKAGE and run initialization form.
It is the same as `el-get-bundle' except that PACKAGE is explicitly
required.

\(fn PACKAGE &rest ARGS)" nil t)

(put 'el-get-bundle! 'lisp-indent-function 'defun)

;;;***

;;;### (autoloads nil "el-get/el-get-check" "../../../.emacs.d/el-get/el-get/el-get-check.el"
;;;;;;  "6145ed8054e30cf9530ba7c381446a14")
;;; Generated autoloads from ../../../.emacs.d/el-get/el-get/el-get-check.el

(autoload 'el-get-check-recipe "el-get/el-get-check" "\
Check the format of the recipe.
Please run this command before sending a pull request.
Usage: M-x el-get-check-recipe RET

You can run this function from checker script like this:
    test/check-recipe.el PATH/TO/RECIPE.rcp

When used as a lisp function, FILE-OR-BUFFER must be a buffer
object or a file path.

\(fn FILE-OR-BUFFER)" t nil)

;;;***

;;;### (autoloads nil "el-get/el-get-list-packages" "../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "af145f8b49dd63632e6277b79d546813")
;;; Generated autoloads from ../../../.emacs.d/el-get/el-get/el-get-list-packages.el

(autoload 'el-get-list-packages "el-get/el-get-list-packages" "\
Display a list of packages.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/el-get/el-get/el-get-autoloading.el"
;;;;;;  "../../../.emacs.d/el-get/el-get/el-get-build.el" "../../../.emacs.d/el-get/el-get/el-get-bundle.el"
;;;;;;  "../../../.emacs.d/el-get/el-get/el-get-byte-compile.el"
;;;;;;  "../../../.emacs.d/el-get/el-get/el-get-check.el" "../../../.emacs.d/el-get/el-get/el-get-core.el"
;;;;;;  "../../../.emacs.d/el-get/el-get/el-get-custom.el" "../../../.emacs.d/el-get/el-get/el-get-dependencies.el"
;;;;;;  "../../../.emacs.d/el-get/el-get/el-get-install.el" "../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../.emacs.d/el-get/el-get/el-get-methods.el" "../../../.emacs.d/el-get/el-get/el-get-notify.el"
;;;;;;  "../../../.emacs.d/el-get/el-get/el-get-recipes.el" "../../../.emacs.d/el-get/el-get/el-get-status.el"
;;;;;;  "../../../.emacs.d/el-get/el-get/el-get.el" "el-get/el-get-bundle.el"
;;;;;;  "el-get/el-get-check.el" "el-get/el-get-list-packages.el"
;;;;;;  "el-get/el-get.el") (22395 52847 560474 0))

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
