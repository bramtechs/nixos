;; enable second monitor
(start-process-shell-command
             "xrandr" nil "xrandr --output HDMI-2 --left-of DP-1 --mode 1920x1080 --rate 60")

;; configure main monitor
(start-process-shell-command
             "xrandr" nil "xrandr --output DP-1 --primary --mode 1920x1080 --rate 60")

(require 'exwm)
(require 'exwm-config)
(exwm-config-default)


;; stop fat-fingering suspend shortcut, making emacs hang on exwm
(global-unset-key "\C-x\C-z")
(put 'suspend-frame 'disabled t)

;; turn this distracting mode off again
(ido-mode -1)
