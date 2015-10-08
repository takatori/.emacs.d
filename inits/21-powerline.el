;;; Powerline Setting
(require 'powerline)
(setq ns-use-srgb-colorspace nil)
(setq powerline-color1 "#073642")
(setq powerline-color2 "#002b36")
(set-face-attribute 'mode-line nil
                    :foreground "#fff"
                    :background "#5e98c5"
                    :box nil)
(set-face-attribute 'mode-line-buffer-id nil
		    :foreground "#fff"
		    :bold t)
(set-face-attribute 'mode-line-inactive nil
                    :box nil)
(powerline-default-theme)
