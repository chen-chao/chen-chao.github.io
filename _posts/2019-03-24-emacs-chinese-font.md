---
layout: post
title: Emacs 中文等宽字体设置
categories: Emacs
---

emacs默认的中文字符和英文字符是等高的(因为字号一般是字符的高度, 默认的
字号都是相同的). 但这样在需要字符对齐的时候就比较难看, 因为除非精心挑
选的字体外, 中文字符和英文字符所占的宽度是不一样的, 无法用空格来补齐.
比如 `org mode` 里未对齐的表格:

![img](/pics/20190324_220143.jpg) 

同理还有 `gnus summary` 里面中英文在一起的时候article列表也无法对齐.

通常的解决方法是单独设定中文字号, 让一个中文字符的宽度恰好等于一个或两
个英文字符的宽度. 不过这样中文字符就会显得过大或者过小, 很影响心情. 所
以需要给这些要对齐的地方建一个fontset, 特别设定汉字的宽度.

同时, 为了避免手动调整字号, 我在
[How can I determine the width of characters on the screen?](https://emacs.stackexchange.com/questions/5495/how-can-i-determine-the-width-of-characters-on-the-screen)
这里找到了一个返回字符宽度的函数(改了一下名字):

``` elisp
(defun get-char-font-width-on-screen (s) 
  "Return the width in pixels of a character in the current
window's default font.  More precisely, this returns the
width of the letter ‘m’.  If the font is mono-spaced, this
will also be the width of all other printable characters."
  (let ((window (selected-window))
	(remapping face-remapping-alist))
    (with-temp-buffer
      (make-local-variable 'face-remapping-alist)
      (setq face-remapping-alist remapping)
      (set-window-buffer window (current-buffer))
      (insert s)
      (aref (aref (font-get-glyphs (font-at 1) 1 2) 0) 4))))
```

然后可以根据字符宽度不断调整中文字号, 来得到想要的字符宽度:

``` elisp
(defun get-char-font-size-of-width (char charset width &optional action)
  (let* ((font (split-string (face-font 'default nil char) "-"))
	 (fontname (nth 2 font))
	 (fontsize (string-to-number (nth 7 font)))
	 (tempsize fontsize)
	 (fset (frame-parameter nil 'font))
	 )

    (while (< (get-char-font-width-on-screen char) width)
      (setq tempsize (1+ tempsize))
      (set-fontset-font-size fset charset fontname tempsize)
      )
    (while (> (get-char-font-width-on-screen char) width)
      (setq tempsize (1- tempsize))
      (set-fontset-font-size fset charset fontname tempsize)
      )

    (unless action
      (set-fontset-font-size fset charset fontname fontsize)
      )
    tempsize
    )
  )
```

最后设置一个fontset, 用于 `org table` 和 `gnus summary` :

``` elisp
(defun cc/fontset-for-org-table ()
  (let* ((expected-width (* 2 (get-char-font-width-on-screen ?m)))
	 (cn-fontsize (get-char-font-size-of-width ?中 'han expected-width))
	 (cn-font (split-string (face-font 'default nil ?中) "-"))
	 (cn-fontname (nth 2 cn-font))
	 (fset (frame-parameter nil 'font))
	 (fset-string (replace-regexp-in-string "-iso.*$" "-fontset-orgtable" fset))
	 (fset-org-table (create-fontset-from-fontset-spec fset-string))
	 )
    (set-fontset-font-size fset-org-table 'han cn-fontname cn-fontsize)
    fset-org-table
    )
  )
```

设置 `org table` 的字符格式:

    (set-face-attribute 'org-table nil :fontset (cc/fontset-for-org-table))

可以看到表格对齐了.

![img](/pics/20190324_232339.jpg)

`gnus-summary` 要麻烦一些:

``` elisp
(dolist (face '(gnus-summary-normal-read
		gnus-summary-normal-ancient
		gnus-summary-normal-undownloaded
		gnus-summary-normal-ticked
		gnus-summary-normal-unread
		))
	(set-face-attribute face nil :fontset (cc/fontset-for-org-table))
	)
```

