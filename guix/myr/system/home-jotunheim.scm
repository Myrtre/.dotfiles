(define-module (myr system home-jotunheim))
(use-modules (gnu)
	     (gnu home)
	     (gnu home services)
	     (gnu home services desktop)
	     (gnu home services sound)
	     (gnu home services shells)
	     (guix gexp)
	     (guix transformations))



(use-package-modules fonts web-browsers gnuzilla gnupg mail pulseaudio
		     gstreamer compton image-viewers linux music kde
		     gimp inkscape graphics compression version-control
		     guile guile-xyz emacs emacs-xyz audio video rust-apps
		     gnome gnome-xyz kde-frameworks
		     curl wget ssh)

(define %font-packages
  (list font-hermit        ;;|--> gnu packages fonts
	font-iosevka-aile
	font-google-noto   ;;:icons
	font-google-noto-emoji
	font-google-noto-sans-cjk
	font-google-material-design-icons))

(define %media-packages
  (list gstreamer       ;;|--> gnu packages gstreamer
	gst-plugins-ugly
	gst-plugins-bad
	gst-plugins-base
	gst-plugins-good
	gst-libav
	alsa-utils       ;;|--> gnu packages linux
	pipewire
	wireplumber
	brightnessctl
	picom            ;;|--> gnu packages compton
	pavucontrol      ;;|--> gnu packages pulseaudio
	mpv              ;;|--> gnu packages video :apps
	mpv-mpris
	playerctl        ;;|--> gnu packages music
	sxiv))           ;;|--> gnu packages image-viewer

(define %editor-packages
  (list gimp-next       ;;|--> gnu packages gimp
	blender         ;;|--> gnu packages graphics
	inkscape        ;;|--> gnu packages inkscape
	audacity        ;;|--> gnu packages audio
	kdenlive))      ;;|--> gnu packages kde

(define %general-packages
  (list curl            ;;|--> gnu packages curl
	ripgrep         ;;|--> gnu packages rust-apps
	wget            ;;|--> gnu packages wget
	openssh         ;;|--> gnu packages ssh
	zip             ;;|--> gnu packages compression
	unzip
	git             ;;|--> gnu packages version-control
	gnupg           ;;|--> gnu packages gnupg
	isync           ;;|--> gnu packages mail
	msmtp
	mu
	nyxt))          ;;|--> gnu packages web-browsers

(define %appearance-packages
  (list adwaita-icon-theme    ;;|--> gnu packages gnome
	papirus-icon-theme    ;;|--> gnu packages gnome-xyz
	breeze-icons))        ;;|--> gnu packages kde-frameworks

(define %emacs-packages
  (list guile-next         ;;|--> gnu packages guile
	guile-ares-rs      ;;|--> gnu packages guiile-xyz
	emacs              ;;|--> gnu packages emacs
	emacs-next-pgtk))

(home-environment
 (packages (append %media-packages
		   %editor-packages
		   %general-packages
		   %appearance-packages
		   %emacs-packages))
 (services
  (list (service home-dbus-service-type)
	(service home-pipewire-service-type)
	(service home-x11-service-type)
	(simple-service 'env-vars home-environment-variables-service-type
			'(("EDITOR" . "emacs")
			  ("BROWSER" . "nyxt")
			  ("XDG_SESSION_TYPE" . "x11")
			  ("XDG_SESSION_DESKTOP" . "stumpwm")
			  ("XDG_CURRENT_DESKTOP" . "stumpwm")
			  ("XDG_DOWNLOAD_DIR" . "~/downloads")
			  ("XDG_CACHE_DIR" . "~/.cache")))
	(service home-bash-service-type
		 (home-bash-configuration
		  (guix-defaults? #f)
		  (aliases '(("grep" . "grep --color=auto")
			     ("ls" . "ls -p --color=auto")
			     ("ll" . "ls -l")
			     ("la" . "ls -la")
			     ("ghr" . "guix home reconfigure")
			     ("gsr" . "sudo guix system reconfigure")
			     ("gup" . "guix pull && guix upgrade")
			     ("gud" . "guix system delete-generations")
			     ("ghd" . "guix home delete-generations")))
		  (bashrc
		   (list (local-file "../../dot-bashrc.sh"
				     #:recursive? #t)))
		  (bash-profile
		   (list (local-file "../../dot-bash_profile.sh"
				     #:recursive? #t))))))))