;;;
;;; Comment Assist Functions and Commands
;;;
(defun xyt/comment-char (Lang)
  "Return the comment char of the language _lang_."
  (let* ((langCommentChar '(("c" . ?/) ("c++" . ?/) ("cpp" . ?/) ("objc" . ?/) ("swift" . ?/)
                            ("elisp" . ?\;) ("emacs-lisp" . ?\;) ("lisp" . ?\;)
                            ("ruby" . ?#) ("python" . ?#)
                            ("matlab" . ?%) ("octave" . ?%)
                            ("bash" . ?#) ("awk" . ?#) ("shell" . ?#) ("shell-script" . ?#)
                            ("cmake" . ?#) ("conf" . ?#)
                            ("tex" . ?%) ("latex" . ?%)))
         (lang (downcase Lang))
         (commentChar (cdr (assoc lang langCommentChar))))
    (if (eq commentChar nil) ?# commentChar)))

(defun xyt/comment-group (str language)
  "Add a group comment for language: c, cpp, elisp, etc., like
////////////////////////////////////////////////////////////////////////////////
////////////////////////////       Group Comments      /////////////////////////
////////////////////////////////////////////////////////////////////////////////"
  (let* ((len 80)
		 (space 6)
		 (cc (xyt/comment-char language))
		 (_str (if (evenp (string-bytes str)) str (concat str " ")))
		 (prefixn (/ (- (- 80 (+ space space)) (string-bytes _str)) 2))
		 (postfixn prefixn)
         (indent_space (- (point) (point-at-bol))))
    (when (string-match "[^\s]+" (buffer-substring (point-at-bol) (point)))
      (progn
        (insert-char ?\n 1)
        (setq indent_space 0)))
	(insert-char cc len)
	(insert-char ?\n 1)
    (insert-char ?\  indent_space)
	(insert-char cc prefixn)
	(insert-char ?\  space)   
	(insert _str)
	(insert-char ?\  space)
	(insert-char cc postfixn)
	(insert-char ?\n 1)
    (insert-char ?\  indent_space)
	(insert-char cc len)
	(insert-char ?\n 1)))

(defun xyt/comment-class (str language)
  "Add a group comment for language: c, cpp, elisp, etc., like
////////////////////////////       Group Comments      /////////////////////////"
  (let* ((len 80)
		 (space 6)
		 (cc (xyt/comment-char language))
		 (_str (if (evenp (string-bytes str)) str (concat str " ")))
		 (prefixn (/ (- (- 80 (+ space space)) (string-bytes _str)) 2))
		 (postfixn prefixn))
    (when (string-match "[^\s]+" (buffer-substring (point-at-bol) (point)))
      (insert-char ?\n 1))
	(insert-char cc prefixn)
	(insert-char ?\  space)   
	(insert _str)
	(insert-char ?\  space)
	(insert-char cc postfixn)
	(insert-char ?\n 1)))

(defun xyt/comment-block (str language)
  "Add a group comment for language: c, cpp, elisp, etc., like
///
/// A block comment
///"
  (let* ((len 3)
		 (cc (xyt/comment-char language))
		 (_str (concat " " str))
         (indent_space (- (point) (point-at-bol))))
    (when (string-match "[^\s]+" (buffer-substring (point-at-bol) (point)))
      (progn
        (insert-char ?\n 1)
        (setq indent_space 0)))
	(insert-char cc len)
	(insert-char ?\n 1)
    (insert-char ?\  indent_space)
	(insert-char cc len)
	(insert _str)
	(insert-char ?\n 1)
    (insert-char ?\  indent_space)
	(insert-char cc len)
	(insert-char ?\n 1)))

(defun xyt/comment-file (language)
  "Add a group comment for language: elisp, ruby, python, awk, shell etc., like
##
# @file   release
# @author Yantao Xie <xieyt1029@thundersoft.com>
# @date   Tue Jun 17 16:59:20 2014
#
# @brief
#"
  (let* ((cc (xyt/comment-char language)))
    (when (string-match "[^\s]+" (buffer-substring (point-at-bol) (point)))
      (insert-char ?\n 1))
    ; ##
    (insert-string (string cc cc ?\n))
    ; # @file
    (insert-string (concat (string cc) " @file   " (file-name-nondirectory (buffer-file-name)) "\n"))
    ; # @author name <email>
    (insert-string (concat (string cc) " @author " (user-full-name) " <" user-mail-address ">\n"))
    ; # @date date
    (insert-string (concat (string cc) " @date   " (format-time-string "%a %b %d %H:%M:%S %Y\n")))
    (insert-string (concat (string cc) "\n"))
    (insert-string (concat (string cc) " @brief\n"))
    (insert-string (concat (string cc) "\n"))))

(defun xyt/comment-function (language)
  "Add a function comment for the LANGUAGE, like
##------------------------------------------------------------------------------
#
# * @Params@ :
#   - ++ ->
# * @Returns@ :
#   -
# * @Raises@ :
#"
  (let* ((cc (xyt/comment-char language))
         (indent_space (- (point) (point-at-bol))))
    (message "xxx %d" indent_space)
    (insert-char cc 2)
    (insert-char ?- 78)
    (insert-char ?\n 1)
    (insert-char ?\  indent_space)
    (insert-string (concat (string cc) "\n"))
    (insert-char ?\  indent_space)
    (insert-string (concat (string cc) " * @Params@ :\n"))
    (insert-char ?\  indent_space)
    (insert-string (concat (string cc) "   - ++ ->\n"))
    (insert-char ?\  indent_space)
    (insert-string (concat (string cc) " * @Returns@ :\n"))
    (insert-char ?\  indent_space)
    (insert-string (concat (string cc) "   - \n"))
    (insert-char ?\  indent_space)
    (insert-string (concat (string cc) " * @Raises@ :\n"))
    (insert-char ?\  indent_space)
    (insert-string (concat (string cc) "\n"))))

;;;;;;;;;;;;;      Create comment functions for each language      ;;;;;;;;;;;;;
(defun xyt/create-comment-function-for-language (func lang)
  (let ((funcSymbol (intern (concat (symbol-name func) "-" lang))))
    (if (or (string-suffix-p "group" (symbol-name func))
            (string-suffix-p "class" (symbol-name func))
            (string-suffix-p "block" (symbol-name func)))
        `(defun ,funcSymbol (str)
           (interactive "scomments: ")
           (,func str ,lang))
      `(defun ,funcSymbol ()
         (interactive)
         (,func ,lang)))))

(defmacro xyt/create-comment-functions-for-languages (functions languages)
  `(progn ,@(mapcar* 'xyt/create-comment-function-for-language functions languages)))

;;; Uncomment the following to generate the input lists for
;;; xyt/create-comment-functions-for-languages. 但是将来要添加支持, 一般也是一个语言一个语言的添加, 所以
;;; 最好直接手动添加, 不要使用下面的代码, 这样简单一些.
;; (let ((functions '(xyt/comment-group xyt/comment-class xyt/comment-block xyt/comment-file xyt/comment-function))
;;       (languages '("c" "c++" "objc" "swift" "ruby" "python" "matlab" "octave" "latex" "elisp" "shell-script" "cmake" "awk" "conf"))
;;       (funcLangs (list)))
;;   (progn (dolist (func functions)
;;            (dolist (lang languages)
;;              (add-to-list 'funcLangs (cons func lang))))
;;          (insert-string "\n")
;;          (dolist (pair funcLangs)
;;            (insert-string (concat (symbol-name (car pair)) " ")))
;;          (insert-string "\n")
;;          (dolist (pair funcLangs)
;;            (insert-string (concat "\"" (cdr pair) "\" ")))))
(xyt/create-comment-functions-for-languages
(xyt/comment-function xyt/comment-function xyt/comment-function xyt/comment-function
  xyt/comment-function xyt/comment-function xyt/comment-function xyt/comment-function 
  xyt/comment-function xyt/comment-function xyt/comment-function xyt/comment-function 
  xyt/comment-function xyt/comment-function xyt/comment-file xyt/comment-file xyt/comment-file 
  xyt/comment-file xyt/comment-file xyt/comment-file xyt/comment-file xyt/comment-file 
  xyt/comment-file xyt/comment-file xyt/comment-file xyt/comment-file xyt/comment-file 
  xyt/comment-file xyt/comment-block xyt/comment-block xyt/comment-block xyt/comment-block 
  xyt/comment-block xyt/comment-block xyt/comment-block xyt/comment-block xyt/comment-block 
  xyt/comment-block xyt/comment-block xyt/comment-block xyt/comment-block xyt/comment-block 
  xyt/comment-class xyt/comment-class xyt/comment-class xyt/comment-class xyt/comment-class 
  xyt/comment-class xyt/comment-class xyt/comment-class xyt/comment-class xyt/comment-class 
  xyt/comment-class xyt/comment-class xyt/comment-class xyt/comment-class xyt/comment-group 
  xyt/comment-group xyt/comment-group xyt/comment-group xyt/comment-group xyt/comment-group 
  xyt/comment-group xyt/comment-group xyt/comment-group xyt/comment-group xyt/comment-group 
  xyt/comment-group xyt/comment-group xyt/comment-group)
 ("conf" "awk" "cmake" "shell-script" "elisp" "latex" "octave" "matlab" "python" "ruby" "swift" 
  "objc" "c++" "c" "conf" "awk" "cmake" "shell-script" "elisp" "latex" "octave" "matlab" "python" 
  "ruby" "swift" "objc" "c++" "c" "conf" "awk" "cmake" "shell-script" "elisp" "latex" "octave" 
  "matlab" "python" "ruby" "swift" "objc" "c++" "c" "conf" "awk" "cmake" "shell-script" "elisp" 
  "latex" "octave" "matlab" "python" "ruby" "swift" "objc" "c++" "c" "conf" "awk" "cmake" 
  "shell-script" "elisp" "latex" "octave" "matlab" "python" "ruby" "swift" "objc" "c++" "c"))

;;;;;;;;;;;;;      Add hook functions for each language mode       ;;;;;;;;;;;;;
(defun xyt/create-comment-hotkey-hook-for-language (lang)
  `(defun ,(intern (concat "xyt/comment-hotkey-hook-" lang)) ()
     (local-set-key (kbd "C-c d g") (quote ,(intern-soft (concat "xyt/comment-group-" lang))))
     (local-set-key (kbd "C-c d b") (quote ,(intern-soft (concat "xyt/comment-block-" lang))))
     (local-set-key (kbd "C-c d c") (quote ,(intern-soft (concat "xyt/comment-class-" lang))))
     (local-set-key (kbd "C-c d i") (quote ,(intern-soft (concat "xyt/comment-file-" lang))))
     (local-set-key (kbd "C-c d f") (quote ,(intern-soft (concat "xyt/comment-function-" lang))))))
(defmacro xyt/create-comment-hotkey-hook-for-languages (languages)
  `(progn ,@(mapcar 'xyt/create-comment-hotkey-hook-for-language languages)))

;; Define a alias for emacs-lisp
(defvaralias 'elisp-mode-hook 'emacs-lisp-mode-hook)
;; Add hotkey for each language mode.
(xyt/create-comment-hotkey-hook-for-languages ("c++" "c" "objc" "swift" "python" "ruby" "elisp"
                                               "cmake" "shell-script" "awk" "latex" "matlab"
                                               "octave" "conf"))
(let ((languages '("objc" "swift" "python" "ruby" "elisp" "cmake" "shell-script" "awk" 
                   "latex" "matlab" "octave" "conf")))
  (dolist (lang languages)
    (add-hook (intern (concat lang "-mode-hook")) 
              (intern (concat "xyt/comment-hotkey-hook-" lang)))))
;; 我不知道为什么C和C++必须使用c-mode-common-hook而不是c-mode-hook和c++-mode-hook.
(add-hook 'c-mode-common-hook 'xyt/comment-hotkey-hook-c)
