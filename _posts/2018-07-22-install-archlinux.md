---
layout: post
title: Arch Linux 安装笔记
categories: Linux
---

ArchWiki 上的 [Installation Guide](https://wiki.archlinux.org/index.php/Installation_guide#Update_the_system_clock) 和 [General Recommendations](https://wiki.archlinux.org/index.php/General_recommendations) 详尽地描述了Arch Linux的安装过程以及一些必要的系统配置. 其实把指南看作一个系统组件的说明更为合适, 日后如果某部分的设置出现了问题或需要更改, 可以直接参考相应的内容, 而不必费时费力地重装. 这里简要记录一下安装过程和遇到的一些问题, 算是使用Arch Linux的笔记. 以下几乎所有的 shell 操作都需要root权限.


# 刻录系统镜像

安装系统前需要一张光盘镜像, 也可以用U盘替代. 刻录前需要先下载Arch Linux的镜像文件, 并且确保U盘能正常使用并且接触良好. 注意刻录的时候U盘上的数据会被销毁.

Windows上要使用 Rufus或USBWriter等工具来刻录.

Linux上可以使用 `dd` 命令, 可以先用 `lsblk` 查看 usb:

```shell
dd if=/path/to/image of=/dev/sdx bs=4M status=progress && sync
```


# 建立网络连接

建立网络链接是安装中的关键一步. 本地连接一般很容易, 但无线网卡如果遇到驱动问题就会非常麻烦, 所以在安装前一定要查看一下Linux是否支持自己的网卡, 或是否有相应的解决办法.

查看网卡和驱动模块: 

```shell
lspci -v | awk -v RS='' '/[Ee]thernet/'

00:1f.6 Ethernet controller: Intel Corporation Ethernet Connection (2) I219-V (rev 31)
    ...
    Kernel driver in use: e1000e
    Kernel modules: e1000e
```

检查驱动是否加载:

``` shell
dmesg | grep e1000e

...
[    5.107773] e1000e: enpxxxxxx NIC Link is Up 100 Mbps Full Duplex, Flow Control: Rx/Tx
```

有些网卡驱动可能和内核模块的加载顺序有关. 比如以前遇到的Broadcom无线网卡需要在内核模块 `tg3` 之前先加载 `broadcom` 模块.

``` shell
modprobe -r tg3
modprobe broadcom
modprobe tg3
```

可以创建 `/etc/modprobe.d/broadcom.conf` 来指明加载顺序(参见 [problem with tg3 and broadcom modules in system.d](https://forums.gentoo.org/viewtopic-t-969162-start-0.html)):


    softdep tg3 pre: broadcom

利用Arch Linux自带的 `iproute` 包可以查看网络接口: 

    ip link show

    ...
    2: enpxxxxxx: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc fq_codel state UP mode DEFAULT group default qlen 1000

本地连接的接口以 `enp` 开头, 无线网络一般以 `wlp` 开头.
如果网络状态 `<BROADCAST,MULTICAST,UP,LOWER_UP>` 不是 `UP` 的话需要启用网络接口:

    ip link set network_interface up

本地连接直接通过 `dhcpcd` 建立连接就可以了: `dhcpcd eth0`

无线网络需要先验证身份, 可以利用 `wpa_supplicant`:

    wpa_supplicant -B -i wifi0 -c /path/to/config.file

`config.file` 大概这样:

    network={
      ssid="wifi_name"
      psk="password"
    }

注意等号前后不能有空格.
然后同样使用 `dhcpcd` 建立连接.

可以用 `systemd` 来自动连接网络:

``` shell
systemctl enable dhcpcd@enpxxxxxx.service
systemctl start dhcpcd@enpxxxxxx.service 
```

最后, `ping` 一下 `www.archlinux.org` 看一下网络连接是否大功告成:

    ping -c 5 www.archlinux.org

注意, 在系统安装完成重新启动后, 需要在新系统里重新设置网络, 可以参考以上方法, 也可以安装 `networkmanager` 等工具自动设置.


# 安装系统

安装系统前需要先在硬盘上分出相应的区域. 目前有两种不同的分区方案, 一种是 Master Boot Record(MBR), 另一种是 GUID Partition Table(GPT). 它们的应用范围和分区方式可以参见 Archwiki上的 [Partitioning](https://wiki.archlinux.org/index.php/Partitioning) 和 [EFI system partition](https://wiki.archlinux.org/index.php/Unified_Extensible_Firmware_Interface)(不太懂, 不赘述了). UEFI 下需要创建一个 EFI System Partiton(ESP), 推荐512MiB, 对于高级格式化4K本地驱动器, 最小为256MiB.

分区方案要因人而异, 一开始我只分了 `/boot` (ESP) 和 `/` 两个分区, 后来加了块大硬盘, 又分出了 `/home`. 其他的 `swap` 之类的分区可以视情况而定, 但我觉得个人的电脑可以简单点.

分区的步骤主要如下:

1.  利用 `fdisk` 分区, 创建一个至少256MiB的 ESP分区(假设为 `/dev/sda1`), 并标记为 bootable 以及其他分区(假定为 `/dev/sda2`);
2.  格式化相应分区: `mkfs.fat -F32 /dev/sda1 && mkfs.ext4 /dev/sda2`
3.  挂载分区: `mkdir /mnt/boot && mount /dev/sda1 /mnt/boot && mount /dev/sda2 /mnt`

然后按照 [Installation Guide](https://wiki.archlinux.org/index.php/Installation_guide#Update_the_system_clock) 安装系统就可以了:

1.  在 `/etc/pacman.d/mirrorlist` 中取消要使用的镜像站前面的注释.
2.  安装系统: `pacstrap /mnt base`.
3.  生成要挂载的文件系统: `genfstab -U /mnt >> /mnt/etc/fstab`.
4.  进入新安装的系统: `arch-chroot /mnt`.


# Boot loader

Boot loader 就是启动系统时候的引导程序, 一般都会使用 `GRUB` 的吧. 安装 `grub` 之后, 需要配置一下:

    grub-install --target=i386-pc /dev/sda
    grub-mkconfig -o /boot/grub/grub.cfg

有时候原先硬盘的MBR会被某些程序写入一些乱七八糟的东西, 比如 [Sector 32 being in use by FlexNet](https://ubuntuforums.org/showthread.php?t%3D1661254) 这个问题. 这时候需要清空 MBR, 当然要先备份以防万一:

    dd if=/dev/sda of=/archive/file bs=512 count=63
    dd if=/dev/zero of=/dev/sda bs=512 count=63

开机的时候可以编辑 `GRUB` 中的启动条目来进入不同的模式, 适合在弄崩了图形界面之后进入, 省去使用安装盘.

在启动条目上按 `e` 在下面这行后面加上数字进入不同的 run level(这里是3, 带网络连接的命令行模式):

    linux /boot/vmlinuz quiet 3 # boot into run level 3

不同模式可以参见下表:

<table id="org6ff7645" border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-right" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-right">run level number</th>
<th scope="col" class="org-left">description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-right">0</td>
<td class="org-left">halt the system</td>
</tr>


<tr>
<td class="org-right">1</td>
<td class="org-left">single user model</td>
</tr>


<tr>
<td class="org-right">2</td>
<td class="org-left">local multiuser without network service started</td>
</tr>


<tr>
<td class="org-right">3</td>
<td class="org-left">full multiuser with network service started</td>
</tr>


<tr>
<td class="org-right">4</td>
<td class="org-left">not used</td>
</tr>


<tr>
<td class="org-right">5</td>
<td class="org-left">full multiuser with network service and X windows(GUI)</td>
</tr>


<tr>
<td class="org-right">6</td>
<td class="org-left">reboot</td>
</tr>
</tbody>
</table>


# 系统语言

可以按以下步骤来设置系统语言(locale):

1.  在 `/etc/locale-gen` 取消需要的locale前面的注释.
2.  生成locale: `locale-gen`.
3.  设置 `$LANG`: `echo "LANG=en_US.UTF-8" > /etc/locale.conf`.
4.  生成locale encodings: `localedef -v -c -i en_US -f UTF-8 en_US.UTF-8`.

忽略最后一步的话可能会在 terminal 里出现 "cannot set LC\_TYPE/LC\_MESSAGE/LC\_ALL to default locale".


# 时区和时间

设置时区, 国内是 `Asia/Shanghai`.

    ln -sf /usr/share/zoneinfo/Region/City /etc/localtime

常用的查看/更改时间的命令有:

1.  查看系统时间: `timedatectl`.
2.  查看硬件时间: `hwclock --show`.
3.  更改系统时间: `date MMDDhhmm`.
4.  更改硬件时间: `hwclock --systohc`.

可以利用 `systemd-timesyncd` 来进行时钟同步:

    timedatectl set-ntp true

利用 `timedatectl status` 可以查看时钟同步状态:

    timedatectl status

                          Local time: Mon 2018-07-23 21:38:55 CST
                      Universal time: Mon 2018-07-23 13:38:55 UTC
                            RTC time: Mon 2018-07-23 13:38:55
                           Time zone: Asia/Shanghai (CST, +0800)
           System clock synchronized: yes
    systemd-timesyncd.service active: yes
                     RTC in local TZ: no


# 桌面环境


## Xorg

个人电脑肯定是要装桌面的. Arch Linux 默认的安装里不包含桌面, 需要自己选择. 首先安装 `xorg-server` 和 `xorg-server-utils` (包含 `xterm`, `xinit` 等工具). 如果安装了桌面环境, 可以通过 `/etc/X11/xinit/xinitrc` 自动启动(以 `lxde` 为例):

    exec startlxde

也可以使用 display manager 来图形化登陆. 可以参考 [Desktop Environment](https://wiki.archlinux.org/index.php/Desktop_environment) 和 [Display Manager](https://wiki.archlinux.org/index.php/Display_manager).


## 输入法

中文输入法需要设置几个环境变量, 一般放在 `~/.xprofile` 里, Xorg 启动后会从中读取, 这里以 `fcitx` 为例

    export GTK_IM_MODULE=fcitx
    export XMODIFIERS=@im=fcitx
    export QT_IM_MODULE=fcitx

注意, 有些软件可能并不从 `shell` 或者相关文件中读取这些环境变量, 比如, 利用 `systemd` 启动 `emacs --dameon` 的时候需要指定 `XMODIFIERS`:

    [Unit]
    Description=Emacs Daemon
    
    [Service]
    Type=simple
    Environment="LC_CTYPE=zh_CN.UTF-8" "XMODIFIERS=@im=fcitx"
    ExecStart=/usr/bin/emacs --fg-daemon
    ExecStop=/usr/bin/emacsclient --eval "(kill-emacs)"
    Restart=always
    
    [Install]
    WantedBy=default.target


## 声音

Linux的声音驱动程序是 `ALSA`, 可以安装 `alsa-utils`, 然后用 `alsamixer` 来设置声音.

`alsamixer` 有个简单的界面, 里面标上 `MM` 的都处于静音状态, 如图:

![img](/pics/20180723_183524.jpg)

# 小结

简要叙述了 Arch Linux 安装过程和一些必要的配置. 随着使用经验和相关知识的增加, 有些不懂的地方会豁然开朗. 也有很多时候因为折腾一些问题而有点灰心. 就像玩具一样, 喜欢的人自然会乐在其中了.

