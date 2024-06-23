#!/usr/bin/env sh

# enable dbus

if test -z "$DBUS_SESSION_BUS_ADDRESS"; then
	eval $(dbus-launch --exit-with-session --sh-syntax)
fi
systemctl --user import-environment DISPLAY XAUTHORITY

if command -v dbus-update-activation-environment >/dev/null 2>&1; then
        dbus-update-activation-environment DISPLAY XAUTHORITY
fi

# setup monitors
xrandr --output HDMI-2 --left-of DP-1 --mode 1920x1080 --rate 60
xrandr --output DP-1 --primary --mode 1920x1080 --rate 60

openbox
