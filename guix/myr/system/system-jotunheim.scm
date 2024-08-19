(define-module (myr system jottunheim))
(use-modules (gnu)
	     (gnu packages)
	     (gnu services)
	     (gnu system)
	     (gnu system nss)
	     (gnu system setuid)
	     (guix download)
	     (guix packages)
	     (nongnu packages linux)
	     (nongnu system linux-initrd))

(use-package-modules lisp lisp-xyz wm xorg xdisorg linux fonts
		     cups suckless networking curl emacs vim
		     version-control nfs file-systems compression
		     package-management libusb)

(use-service-modules cups ssh desktop xorg nix
		     networking)

(define %keyboard-layout
  (keyboard-layout "hu"))

(define %davy-user-account
  (list (user-account
	 (name "davy")
	 (comment "MYRtré")
	 (group "users")
	 (supplementary-groups '("wheel"
				 "netdev"
				 "lp"
				 "tty"
				 "kvm"
				 "input"
				 "realtime"
				 "audio" "video"))
	 (home-directory "/home/davy"))))

(define %jottunheim-file-system
  (list (file-system
         (mount-point "/")
         (device (file-system-label "guix-linux"))
	 (options "subvol=@,compress=lzo,discard=async,space_cache=v2")
         (type "btrfs"))
	(file-system
         (mount-point "/home")
         (device (file-system-label "guix-linux"))
	 (options "subvol=@home,compress=zstd")
         (type "btrfs"))
	(file-system
         (mount-point "/gnu")
         (device (file-system-label "guix-linux"))
	 (options "subvol=@gnu,compress=lzo,discard=async,space_cache=v2")
         (type "btrfs"))
	(file-system
         (mount-point "/var/logs")
         (device (file-system-label "guix-linux"))
	 (options "subvol=@logs,compress=lzo,space_cache=v2")
         (type "btrfs"))
        (file-system
         (mount-point "/boot/efi")
         (device (uuid "D259-76BF"
                       'fat32))
         (type "vfat"))))


(define %stumpwm-packages
  (list sbcl                   ;;|--> gnu packages lisp
	sbcl-slynk             ;;|--> gnu packages lisp-xyz
	sbcl-parse-float
	sbcl-local-time
	sbcl-cl-ppcre
	sbcl-zpng
	sbcl-salza2
	sbcl-clx
	sbcl-zpb-ttf
	sbcl-cl-vectors
	sbcl-cl-store
	sbcl-trivial-features
	sbcl-bordeaux-threads
	sbcl-cl-fad
	sbcl-clx-truetype
	stumpwm+slynk          ;;|--> gnu packages wm
	sbcl-stumpwm-ttf-fonts ;;:stumpwm-contrib/utils
	sbcl-stumpwm-kbd-layouts
	sbcl-stumpwm-swm-gaps
	sbcl-stumpwm-globalwindows
	sbcl-stumpwm-cpu
	sbcl-stumpwm-mem
	sbcl-stumpwm-wifi
	sbcl-stumpwm-battery-portable))

(define %x11-util-packages
  (list font-hermit        ;;|--> gnu packages fonts
	font-jetbrains-mono
	xterm               ;;|--> gnu packages xorg
	transset
	xhost
	xsetroot
	xinput
	xrdb
	xrandr
	xinit
	xclip              ;;|--> gnu packages xdisorg
	xsel
	xss-lock))

(define %system-packages
  (list curl               ;;|--> gnu packages curl
	git                ;;|--> gnu packages version-control
	bluez              ;;|--> gnu packages networking
	blueman
	exfat-utils        ;;|--> gnu packages file-systems
	fuse-exfat         ;;|--> gnu packages linux
	btrfs-progs
	ntfs-3g
	brightnessctl
	stow               ;;|--> gnu packages stow
	emacs              ;;|--> gnu packages emacs
	vim                ;;|--> gnu packages vim
	zip unzip))        ;;|--> gnu packages compression

(define (substitutes->services config)
  (guix-configuration
   (inherit config)
   (substitute-urls
    (cons* "https://substitutes.nonguix.org"
	   %default-substitute-urls))
   (authorized-keys
    (cons* (origin
	    (method url-fetch)
	    (uri "https://substitutes.nonguix.org/signing-key.pub")
	    (file-name "nonguix.pub")
	    (sha256
	     (base32
	      "0j66nq1bxvbxf5n8q2py14sjbkn57my0mjwq7k1qm9ddghca7177")))
	   %default-authorized-guix-keys))))

(define %system-services
  (cons* (service nix-service-type)
	 ;;(service elogind-service-type)
	 (service screen-locker-service-type
		  (screen-locker-configuration
		   (name "slock")
		   (program (file-append slock "/bin/slock"))))
	 (service bluetooth-service-type
		  (bluetooth-configuration
		   (auto-enable? #t)))
	 (service cups-service-type
		  (cups-configuration
		   (web-interface? #t)
		   (default-paper-size "Letter")))
	 ;;(service network-manager-service-type)
	 ;;(service wpa-supplicant-service-type)
	 ;;(service modem-manager-service-type)

	 (simple-service 'mount-setuid-helpers
			 setuid-program-service-type
			 (map (lambda (program)
				(setuid-program
				 (program program)))
			      (list (file-append nfs-utils "/sbin/mount.nfs")
				    (file-append ntfs-3g "/sbin/mount.ntfs-3g"))))
	 (service console-font-service-type
		  (map (lambda (tty)
			 (cons tty (file-append font-terminus
						"/share/consolefonts/ter-220n")))
		       '("tty1" "tty2" "tty3")))

	 (service greetd-service-type
		  (greetd-configuration
		   (greeter-supplementary-groups '("video" "input"))
		   (terminals
		    (list (greetd-terminal-configuration (terminal-vt "1") (terminal-switch #t))
			  (greetd-terminal-configuration (terminal-vt "2"))
			  (greetd-terminal-configuration (terminal-vt "3"))))))

	 (service pam-limits-service-type
		  (list
		   (pam-limits-entry "@realtime" 'both 'rtprio 99)
		   (pam-limits-entry "@realtime" 'both 'nice -19)
		   (pam-limits-entry "@realtime" 'both 'memlock 'unlimited)))

	 (simple-service 'mtp udev-service-type (list libmtp))

	 (udev-rules-service 'pipewire-add-udev-rules pipewire)
	 (udev-rules-service 'brightnessctl-udev-rules brightnessctl)
	 
	 ;;(service x11-socket-directory-service-type)
	 (service xorg-server-service-type)
	 
	 (service openssh-service-type)
	 (modify-services %desktop-services
			  (delete login-service-type)
			  (delete mingetty-service-type)
			  (delete console-font-service-type)
			  (delete gdm-service-type)
			  (guix-service-type
			   config =>
			   (substitutes->services config)))))
	     
(operating-system
 (kernel linux-6.10)
 (initrd microcode-initrd)
 ;; Fixes Xorg Lagg
 (kernel-arguments (cons "i915.enable_psr=0" %default-kernel-arguments))
 (firmware (list linux-firmware
		 amdgpu-firmware
		 iwlwifi-firmware
		 ibt-hw-firmware))
 (locale "en_US.utf8")
 (timezone "Europe/Budapest")
 (keyboard-layout %keyboard-layout)
 (host-name "jotunheim")

 (users (append
	 %davy-user-account
	 %base-user-accounts))

 (groups (cons (user-group (system? #t) (name "realtime"))
	       %base-groups))
 
 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))
 
 (swap-devices (list (swap-space
                      (target (uuid
                               "09451d9c-5cc9-4d1a-aacf-f48da883157e")))))

 (file-systems (append
		%jottunheim-file-system
		%base-file-systems))

 (packages (append
	    %stumpwm-packages
	    %x11-util-packages
	    %system-packages
	    %base-packages))

 (services %system-services)
 
 (name-service-switch %mdns-host-lookup-nss))
