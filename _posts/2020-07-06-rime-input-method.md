---
layout: post
title: 你好, RIME
categories: linux
---

因为在 Arch Linux 里使用的搜狗拼音候选框总是无法完全显示, 一直没有好的办法.
根据[这里](https://bbs.archlinuxcn.org/viewtopic.php?pid=43081#p43081)的
建议, 尝试了优麒麟版搜狗. 问题确实可以解决, 无奈功能有点简陋, 所以还是不太合适.

搜索 Linux 里常用的中文输入法, 发现了 RIME (名字也是递归梗), 而且被
[Arch wiki](https://wiki.archlinux.org/index.php/Fcitx_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)#%E4%B8%AD%E6%96%87)
称为著名输入法 (英文版倒是没加著名, 不过作为中文输入法我更相信中文版
wiki). 不过 RIME 居然在 Linux, Windows 和 MacOS 里分别有不同的名字, 分
别叫小狼毫, 鼠须管什么的, 实在让人心生疑虑. 不过好在
Arch 已经有对应的包, 所以倒可以先试试.

## 安装

直接安装 librime 就好, 配合 fcitx-rime 或者 ibus-rime 使用. 需要双拼方
案的话再安装 rime-double-pinyin.

## 配置

RIME 的[文档](https://github.com/rime/home/wiki/CustomizationGuide)有
点随性, 但仔细阅读的话很有用. 和通常的 Linux 程序一样, RIME 也分为默认
设置 (多在 `/usr/share/rime-data`, 一般无需修改) 和用户设置 (fcitx
的话一般在 `$HOME/.config/fcitx/rime`). 用户设置又分为两种,
针对所有 RIME 输入法 (schema) 的放在 `default.custom.yaml`, 针对特定输入法的放在
`<schema>.custom.yaml`, 将 `<schema>` 换成相应输入法的 schema (我这里是 `double_pinyin_flypy`).

RIME 的配置其实就是给已定义好的设置打补丁 (patch). 一个文件只能有一个
关键词 patch, 其他的项都在这个 patch 下面. 比如改变默认候选词数量的设
定就是

```yaml
# default.custom.yaml
patch:
  "menu/page_size": 7
```

指的是, 将默认 "menu/page_size" 的值改为 7. 相关的设置选项可以在
`/usr/share/rime-data/default.yaml` 以及相应的输入法 schema 文件中找到.

我用的几个设置是:

#### 使用简体中文

默认的 RIME 输入法都是繁体. 查看我使用的 `double_pinyin_flypy.schema.yaml` 里面, 有 simplification switch:

```yaml
switches:
  - name: ascii_mode
    reset: 0
    states: [ 中文, 西文 ]
  - name: full_shape
    states: [ 半角, 全角 ]
  - name: simplification
    states: [ 漢字, 汉字 ]
  - name: ascii_punct
    states: [ 。，, ．， ]
```

所以按[这里](https://github.com/rime/home/wiki/CustomizationGuide#%E4%B8%80%E4%BE%8B%E5%AE%9A%E8%A3%BD%E7%B0%A1%E5%8C%96%E5%AD%97%E8%BC%B8%E5%87%BA), 将这个 switch (第三个选项) 永远设为 1 即可 (在 `double_pinyin_custom.yaml` 里面):

```yaml
# double_pinyin_custom.yaml
patch:
  "switches/@2/reset": 1
```

#### 中文输入也使用西文标点

文档里是指定了一份自定义的标点来实现. 不过 RIME 会自带一个 `punctuation.yaml` 的文件, 所以我们可以偷个懒, 直接把输入法的标点指定为 ascii style.

```yaml
# double_pinyin_custom.yaml
patch:
  "punctuator/half_shape":
    __include: punctuation:/ascii_style
```


#### 自定义短语

仿照 `luna_pinyin.schema.yaml` 里的写法, 在 `double_pinyin_custom.yaml` 里面加上:

```yaml
patch:
  engine/translators:
    - punct_translator
    - reverse_lookup_translator
    - script_translator
    - table_translator@custom_phrase
    - table_translator

  custom_phrase:
    dictionary: ""
    user_dict: custom_phrase
    db_class: stabledb
    enable_completion: false
    enable_sentence: false
    initial_quality: 1
```

然后在用户的 rime 文件夹下添加 `custom_phrase.txt` 即可. 文件格式参加
[这里](https://gist.github.com/lotem/5440677). 需要注意的是, 每行短语
定义需要用 TAB 分隔, 空格是不行滴.

## 小结

至此, 输入法的问题得以解决. 目前的设置已足以满足需求. 你好, RIME.
