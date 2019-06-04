---
layout: post
title: Python Django 搭建个人博客
categories: Python
---


搭建个人博客是一直就有的想法. 最近一边学习 [Django Tutorial](https://docs.djangoproject.com/en/2.0/intro/tutorial01/), 一边照猫画虎, 折腾了一个. 本来非常头痛的样式问题, 恰好看到了 [luoshao23/blogproj](https://github.com/luoshao23/blogproj), 果断fork过来, 写了一点后端代码, 过程中参考了 [laike9m/My\_Blog](https://github.com/laike9m/My_Blog), 在此非常感谢两位.


# 搭建过程

搭建的思路其实比较简单(毕竟是站在前人的肩膀上@@). Django app 遵循Model-Template-View设计原则, Model是存储和操作的数据模型, Template是显示模板, View用来接受并回应请求, 对于博客来说就是把文章和相关内容填写到模板里去. 

因为我一般都使用Emacs org mode记录一些想法, 所以希望博客能够除了markdown格式外支持org mode. 于是就想找一个像python-markdown一样的org mode的解析器. 后来找了几个感觉都不太合适, 索性就利用emacs org mode直接生成html上传. 然后blogproj接收并保存html后, 再生成目录和摘要. 期间遇到的一个问题是, 直接在 `admin ModelForm` 里上传文件到 `html_file FileField`, 然后在 `save_model` 里写到database的时候总是会报错, `ValueError: I/O operation on closed file`. 原因是Django在 `ModelForm` 的request结束后就关闭了 `request.FILE`. 最后在 `ModelForm` 中添加了一个额外的FileField `original_file` 用于接收上传的文件, 再根据上传文件的后缀名决定如何转换到html.

然后是图片的存储和显示. 我希望在写作时不需要考虑图片的重名和url, 所以在文章内链接图片的时候, 只在当前文章的关联图片里查找 **包含** 给定名称的图片, 因为django会对上传的重名图片自动加一个后缀, 这样就可以避免链接到错误图片或着找不到图片.

需要注意, 在修改 `model.py` 之后, 重新注册应用需要三步:

    python manage.py migrate
    python manage.py makemigrations
    python manage.py migrate

必要的时候要修改 `migrations/xxx__auto__XXXX.py` 文件.

最后整个博客的文件结构大概这样:

    
    ├── blog
    │   ├── admin.py
    │   ├── apps.py
    │   ├── feeds.py
    │   ├── __init__.py
    │   ├── models.py
    │   ├── search_indexes.py
    │   ├── static
    │   │   └── blog
    │   ├── templatetags
    │   │   ├── blog_tags.py
    │   │   └── __init__.py
    │   ├── tests.py
    │   ├── urls.py
    │   ├── views.py
    │   └── whoosh_cn_backend.py
    ├── blogproj
    │   ├── __init__.py
    │   ├── settings.py
    │   ├── urls.py
    │   └── wsgi.py
    ├── collected_static
    │   ├── admin
    │   └── blog
    ├── comments
    │   ├── admin.py
    │   ├── apps.py
    │   ├── forms.py
    │   ├── __init__.py
    │   ├── models.py
    │   ├── tests.py
    │   ├── urls.py
    │   └── views.py
    ├── db.sqlite3
    ├── manage.py
    ├── README.md
    ├── requirements.txt
    └── templates
        ├── base.html
        ├── blog
        │   ├── about.html
        │   ├── blog.html
        │   ├── contact.html
        │   ├── detail.html
        │   └── index.html
        └── search
    	├── indexes
    	│   └── blog
    	│       └── post_text.txt
    	└── search.html


# 部署

部署前需要检查一下django settings: `python manager.py check --deploy`. 这里我参考laike9m的做法, 根据机器的名称判断如果是在VPS上, 就从环境变量里读取相关设置.

其他的设置参照了digitalocean [Tutorial: How To Set Up Django with Postgres, Nginx, and Gunicorn on Ubuntu 16.04](https://www.digitalocean.com/community/tutorials/how-to-set-up-django-with-postgres-nginx-and-gunicorn-on-ubuntu-16-04), 创建postgres用户, 配置gunicorn和nginx.

需要注意的是, postgres默认都是本机的用户, 如果要用用户名和密码登陆的话, 需要在 `/etc/postgresql/<version>/main/pg_hba.conf` 中将 `peer` 修改为 `md5`:

    # "local" is for Unix domain socket connections only
    local   all             all                                     peer

然后在利用systemd启动 `/etc/systemd/system/gunicorn.service` 的时候, gunicorn 无法读取环境变量, 解决方法是在 `gunicorn.service` 加上 `Environment` 关键词:

    [Unit]
    Description=gunicorn daemon
    After=network.target
    
    [Service]
    User=user
    Group=www-data
    Environment="Environment Variables"
    WorkingDirectory=/path/to/blogproj
    ExecStart=/path/to/gunicorn --access-logfile - --workers 3 --bind unix:/path/to/blogproj/blogproj.sock blogproj.wsgi:application
    
    [Install]
    WantedBy=multi-user.target

启动 `gunicorn.service`:

    sudo systemctl daemon-reload
    sudo systemctl enable gunicorn
    sudo systemctl start gunicorn

nginx的 `blogproj.conf`:

    server {
        listen 80;
        server_name ip_or_domain_name;
    
        location = favicon.ico {access_log off; log_not_found off; }
        location /static/ { alias /path/to/blogproj/collected_static/; }
        location / {
    	include proxy_params;
    	proxy_pass http://unix:/path/to/blogproj/blogproj.sock;
        }
    }

然后 链接 `blogproj.conf` 到 `/etc/nginx/sites-enabled/`, 再重启nginx:

    sudo ln -s -T /path/to/blogproj.conf /etc/nginx/sites-enabled/blogproj.conf
    sudo systemctl restart nginx


# 小结

根据 [luoshao23/blogproj](https://github.com/luoshao23/blogproj) 尝试使用Python Django搭建了博客, 并利用gunicorn和nginx部署在VPS上；简单地了解了网站接收和响应请求的过程.
开始好好地写博客吧!

