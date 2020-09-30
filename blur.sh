#!/bin/bash
emacsPID=$(pgrep emacs)
if [ -n "$emacsPID" ]; then
        echo "Emacs is running!"
        for wid in $(xdotool search --pid $emacsPID); do
            xprop -f _KDE_NET_WM_BLUR_BEHIND_REGION 32c -set _KDE_NET_WM_BLUR_BEHIND_REGION 0 -id $wid;
        done
fi
