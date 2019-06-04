---
layout: post
title: Emacs Gnus 收发邮件
categories: Emacs
---

为emacs开发的收发邮件的package很多, 比如mew, wanderlust. 不过最终还是
选择了自带的gnus, 主要参考了[manateelazycat的配置](https://github.com/manateelazycat/lazycat-emacs/blob/master/site-lisp/config/init-gnus.el) 和 [emacswiki:gnus speed](https://www.emacswiki.org/emacs/GnusSpeed).

因为emacs gnus是单线程程序, 所以如果网速较慢或者下载的内容较多, 打开
gnus的时候不得不等待不短的时间. 所以参考manateelazycat的做法和
emacswiki中的建议, 所有邮件和RSS订阅都利用外部程序抓好, 然后让gnus从本
地文件读取. 邮件收取和分类使用 `getmail` 和 `maildrop`, 发送邮件利用msmtp. 抓
取RSS XML则使用 `wget`.


# 收取邮件


## getmail 和 maildrop 配置

`getmail` 和 `maildrop` 的配置都很简单. `getmailrc` 大概这样

    [retriever]
    type = SimplePOP3SSLRetriever
    server = pop.gmail.com
    username = username@gmail.com
    port = 995
    password = password
    
    [destination]
    type = MDA_external
    path = /usr/bin/maildrop

maildrop 的默认配置文件是 `~/.mailfilter`, 可以参考 [courier-mta上的示例](https://www.courier-mta.org/maildropex.html).

如果由多个邮箱可以放在不同的rcfile里, 然后使用

\#+BEGIN\_SRC shell
getmail &#x2013;rcfile rcfile1 &#x2013;rcfile rcfile2
\#+BEGIN\_SRC

来收取邮件. 如果需要使用代理可以考虑 `proxychains`. 然后利用 `crontab`
创建定时任务即可.


## gnus 收取邮件

我这里的配置基本来自 manateelazycat, 做了一些简化. 如果本地的Maildir
是 "~/Mail", 那么通过

    (setq gnus-secondary-select-methods '((nnmaildir "Mail" (directory "~/Mail/"))))

把邮箱添加到gnus中. 可以按 "L" 在buffer `*Group*` 中显示Maildir中的所有邮
箱.


# 发送邮件

`msmtp` 的配置也很简单, [archwiki:msmtp](https://wiki.archlinux.org/index.php/msmtp) 上就有现成的示例. 唯一的问题在于似
乎无法通过 `proxychains` 走ss代理. 

gnus里配置如下

    (setq send-mail-function 'sendmail-send-it
          message-send-mail-function 'sendmail-send-it
          sendmail-program (executable-find "msmtp")
          message-sendmail-extra-arguments '("--read-envelope-from")
          mail-specify-envelope-from t
          mail-envelope-from 'header
          ;; mail-envelope-from will be automatically loaded if
          ;; sendmail.el is loaded, this is in case
          message-sendmail-envelope-from 'header
          )

其中 `--read-envelope-from` 是让 `msmtp` 自己根据header来选择发送的
邮箱, 在使用多个邮箱的时候很有帮助.


# RSS 订阅

说起gnus的RSS 订阅, [上一篇](https://cchao.me/blog/4/) 博文里还挺惭愧的, 给 `nnrss` 找了个不存在的
bug. 这里介绍一下离线抓取 RSS XML的一些设置. 基本想法就是通过
`nnrss-generate-download-script` 自动生成抓取脚本, 然后每次添加RSS源的
时候自动向脚本里添加该源抓取链接.

设置从本地读取RSS

    (setq nnrss-use-local t)

默认的 `nnrss-generate-download-script` 会忽略掉没有新文件的源, 所以做
了一点修改

    (setq my-nnrss-download-script-file "~/bin/nnrsscrawler")
    
    (defun my-nnrss-generate-download-script ()
      "same as nnrss-generate-download-script, but read urls from `nnrss-group-alist'
    instead of `nnrss-group-data'"
      (interactive)
      (with-temp-buffer
        (progn
          (insert "#!/bin/sh\n")
          (insert "WGET=wget\n")
          (insert "RSSDIR='" (expand-file-name nnrss-directory) "'\n")
          (dolist (elem nnrss-group-alist)
    	(let ((xmlname (nnrss-translate-file-chars (concat (car elem) ".xml")))
    	      (url (nth 1 elem)))
    	  (insert "$WGET -q -O \"$RSSDIR\"/'" xmlname "' '" url "'\n")))
          (write-file my-nnrss-download-script-file)
          (shell-command (concat "chmod u+x " my-nnrss-download-script-file)))
        )
      )

最后是在订阅RSS同时把源添加到抓取脚本

    (defun my-save-crawler-after-make-rss-group (original-fun &rest args)
      "save newly added group to `my-nnrss-download-script-file'"
      (let ((old-length (length nnrss-group-alist)))
        (apply original-fun args)
        (if (file-exists-p my-nnrss-download-script-file)
    	;; compare old and new nnrss-group-alist to get new added rss groups
    	(unless (= old-length (length nnrss-group-alist))
    	  (let* ((elem (car nnrss-group-alist))
    		 (xmlname (nnrss-translate-file-chars (concat (car elem) ".xml")))
    		 (url (nth 1 elem))
    		 (cmd-string (concat "$WGET -q -O \"$RSSDIR\"/'" xmlname "' '" url "'\n")))
    	    (write-region cmd-string nil my-nnrss-download-script-file 'append)
    	    )
    	  )
          (my-nnrss-generate-download-script my-nnrss-download-script-file)
          )
        )
      )
    
    (advice-add 'gnus-group-make-rss-group :around #'my-save-crawler-after-make-rss-group)


# 小结

折腾了emacs gnus来收发邮件和订阅RSS. 虽然还没有实现 `msmtp` 通过代理来
发送gmail邮件, 不过暂时告一段落吧.

