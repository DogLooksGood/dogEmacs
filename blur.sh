#!/bin/bash
# Works with KDE.

for pid in $(pgrep emacs)
do
        for wid in $(xdotool search --pid $pid); do
            xprop -f _KDE_NET_WM_BLUR_BEHIND_REGION 32c -set _KDE_NET_WM_BLUR_BEHIND_REGION 0 -id $wid;
        done
done
