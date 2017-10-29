#!/bin/sh

# XTerm settings
xrdb -merge $HOME/.Xresources
xmodmap ~/.Xmodmap

# fcitx settings
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
export DefaultIMModule=fcitx
fcitx-autostart &

# xorg settings
xset dpms 0 0 0;xset s off
xrandr --output HDMI-1 --mode 1920x1080

# start some nice programs
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# twm &
# xclock -geometry 50x50-1+1 &
# xterm -geometry 80x50+494+51 &
# xterm -geometry 80x20+494-0 &
# exec xterm -geometry 80x66+0+0 -name login

exec i3
#exec openbox-session
