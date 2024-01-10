(add-to-list 'load-path "~/Internet/Git/Emacs/emms/")

(require 'emms)
(require 'emms-history)
(require 'emms-playlist-mode)
(require 'emms-player-mpd)
(require 'emms-setup)

(add-to-list 'emms-info-functions 'emms-info-mpd)
(add-to-list 'emms-player-list 'emms-player-mpd)
(customize-set-variable 'emms-player-mpd-music-directory "~/Media/Musica/")
(customize-set-variable 'emms-source-file-default-directory "~/Media/Musica/")

(emms-history-load)
(emms-player-mpd-connect)

(emms-player-set emms-player-mpd
		 'regex
		 "\\(flac\\|mp3\\|ape\\)$")
