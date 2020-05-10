#!/bin/sh

# See https://wiki.archlinux.org/index.php/Xinit#xinitrc
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

xbindkeys

xrdb -merge .Xresources

trayer --edge top --align right --SetDockType true --SetPartialStrut true \
 --expand true --width 10 --transparent true --tint 0x191970 --height 18 &

#xsetroot -solid yellow
#nitrogen --restore &
~/.fehbg &

xscreensaver -no-splash &

if [ -x /usr/bin/xfce4-power-manager ] ; then
   sleep 3
   xfce4-power-manager &
fi

systemctl --user start xfce4-notifyd

exec xmonad
