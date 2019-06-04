---
layout: post
title: Emacs Gnus 订阅 RSS
categories: Emacs
---


最近尝试使用emacs gnus来订阅RSS, 在此分享一下心得. 参考了
[emacswiki:Gnus Tutorial](https://www.emacswiki.org/emacs/GnusTutorial), [emacswiki:GnusRss](https://www.emacswiki.org/emacs/GnusRss)以及陈斌大神的[Practical
guide to use Gnus with Gmail](https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/gnus-guide-en.org). 其实看得一头雾水, 最后还是得自己瞎琢磨.
不得不说, gnus的设定很反人类, `M-x gnus` 进入之后一上来就是错误信息, 然后
所有的信息都是不可见的, 需要手动切换到 `Group Buffer`:

![img](/pics/20190217_233116.jpg)

然后在 `Group Buffer` 中按 `G R` 可以添加RSS源, `G DEL` 删除RSS源.
阅读界面是这样的:

![img](/pics/20190217_233931.jpg)

不过使用中发现buffer中只显示了RSS XML中 `<description>` 标签的内容, 而
没有显示 `<content:encoded>` 中的内容, 所以就查了一下源代码 `nnrss.el`,
利用 `debug-on-entry` 一步步定位中间结果, 最后发现问题出在:

``` elisp
(defun nnrss-check-group (group server)
    ...
    (setq dc-ns (nnrss-get-namespace-prefix xml "http://purl.org/dc/elements/1.1/")
	  rdf-ns (nnrss-get-namespace-prefix xml "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
	  rss-ns (nnrss-get-namespace-prefix xml "http://purl.org/rss/1.0/")
	  content-ns (nnrss-get-namespace-prefix xml "http://purl.org/rss/1.0/modules/content/"))
    ...
    (setq subject (nnrss-node-text rss-ns 'title item))
    (setq url (nnrss-decode-entities-string
	       (nnrss-node-text rss-ns 'link (cddr item))))
    (setq extra (or (nnrss-node-text content-ns 'encoded item)
		    (nnrss-node-text rss-ns 'description item)))
    ...
    )
```

其中 `xml` 是解析后的xml列表:

``` elisp
((rss ((version . "2.0") (xmlns:atom . "http://www.w3.org/2005/Atom"))
      (channel ((xmlns:content . "http://purl.org/rss/1.0/modules/content/"))
	       (title nil "laike9m's blog")
	       (link nil "https://laike9m.com/blog/rss")
	       (description nil "Update on laike9m blog's articles.")
	       (atom:link ((href . "https://laike9m.com/blog/rss/") (rel . "self")))
	       (language nil "en-us")
	       (lastBuildDate nil "Sun, 17 Feb 2019 08:16:59 +0000")
	       (item nil (title nil "所以，到底要不要读研？")
		     (link nil "https://laike9m.com/blog/suo-yi-dao-di-yao-bu-yao-du-yan,119/")
		     (description nil "关于究竟应不应该读研的一些讨论。")
		     (pubDate nil "Sun, 17 Feb 2019 08:16:59 +0000")
		     (guid nil "https://laike9m.com/blog/suo-yi-dao-di-yao-bu-yao-du-yan,119/")
		     (content:encoded nil "内容")))))
```

`item` 是上面xml中item部分:

``` elisp
(item nil (title nil "所以，到底要不要读研？")
      (link nil "https://laike9m.com/blog/suo-yi-dao-di-yao-bu-yao-du-yan,119/")
      (description nil "关于究竟应不应该读研的一些讨论。")
      (pubDate nil "Sun, 17 Feb 2019 08:16:59 +0000")
      (guid nil "https://laike9m.com/blog/suo-yi-dao-di-yao-bu-yao-du-yan,119/")
      (content:encoded nil "内容"))
```

而 `nnrss-node-text` 就是将相应标签的内容从列表中取出:

``` elisp
(defun nnrss-node-text (namespace local-name element)
  (let* ((node (assq (intern (concat namespace (symbol-name local-name)))
		     element))
	 (text (if (and node (listp node))
		   (nnrss-node-just-text node)
		 node))
	 (cleaned-text (if text
			   (replace-regexp-in-string
			    "\r\n" "\n"
			    (replace-regexp-in-string
			     "^[\000-\037\177]+\\|^ +\\| +$" ""
			     text)))))
    (if (string-equal "" cleaned-text)
	nil
      cleaned-text)))
```

可以看到, `nnrss-node-text` 提取的标签名字其实是由第一个和第二个参数合并而成的. 很不幸的, 在
`nnrss-check-group` 中 `content-ns` 的值为 `nil`, 所以提取 `nnrss-node-text` 相当于只查找了
名为 `<encoded>` 的标签, 自然就找不到相应内容了. 这里将 `content-ns` 更改为 `"content:"`
就可以得到正确的内容.

而这个 `content-ns` 又是由 `nnrss-get-namespace-prefix` 生成的:

``` elisp
(defun nnrss-get-namespace-prefix (el uri)
  "Given EL (containing a parsed element) and URI (containing a string
that gives the URI for which you want to retrieve the namespace
prefix), return the prefix."
  (let* ((prefix (car (rassoc uri (cadar el))))
	 (nslist (if prefix
		     (split-string (symbol-name prefix) ":")))
	 (ns (cond ((eq (length nslist) 1) ; no prefix given
		    "")
		   ((eq (length nslist) 2) ; extract prefix
		    (cadr nslist)))))
    (if (and ns (not (string= ns "")))
	(concat ns ":")
      ns)))
```

将前面的xml列表带入之后发现对任何输入都输出 `nil`, 很明显有问题. 简单测试一下就可以发现这里
`(cadar el)` 部分有误, 修正后就好:

``` elisp
(defun my-nnrss-get-namespace-prefix (el uri)
  "Given EL (containing a parsed element) and URI (containing a string
that gives the URI for which you want to retrieve the namespace
prefix), return the prefix."
  (let* ((prefix (car (rassoc uri (cadar (nthcdr 2 (car el))))))
	 (nslist (if prefix
		     (split-string (symbol-name prefix) ":")))
	 (ns (cond ((eq (length nslist) 1) ; no prefix given
		    "")
		   ((eq (length nslist) 2) ; extract prefix
		    (cadr nslist)))))
    (if (and ns (not (string= ns "")))
	(concat ns ":")
      ns)))
```

然后替换掉原来的 `nnrss-get-namespace-prefix`:

    (add-function :override (symbol-function 'nnrss-get-namespace-prefix) #'my-nnrss-get-namespace-prefix)

这样终于可以正确显示RSS文章了!

![img](/pics/20190218_002134.jpg)

UPDATE on 03/17/2019:

原来的 `nnrss-get-namespace-prefix` 大部分情况下是对的, 实际上是 [laike9m's blog](https://laike9m.com/blog/rss) 这
个RSS XML(出于未知的原因)把 `<xmlns:content>` 标签放到了 `<channel>`
标签里导致了解析错误. 通常这个标签应该放在 `<channel>` 标签前面的头里面去.

和 nnrss 的维护者联系后, 他表示这个标签其实可以放在XML中的任意位置, 不过如果影响不大的话, 暂时不用着急&#x2026;

目前的解决方案是如果 `nnrss-get-namespace-prefix` 没发现 `<xmlns:content>` 
就返回 `content:`.

