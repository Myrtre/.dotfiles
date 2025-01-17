{ config, lib, pkgs, userSettings, ... }:
let
  themePath = "../../../themes"+("/"+userSettings.theme+"/"+userSettings.theme)+".yaml";
  themePolarity = lib.removeSuffix "\n" (builtins.readFile (./. + "../../../themes"+("/"+userSettings.theme)+"/polarity.txt"));
  backgroundPath = (./. + "../../../themes"+("/"+userSettings.theme));
  background = (if lib.pathIsRegularFile (backgroundPath+"/background.gif") 
		then (backgroundPath)+"/background.gif" 
		else (if lib.pathIsRegularFile ((backgroundPath)+"/background.webp") 
			then (backgroundPath)+"/background.webp"
			else (./. + "../../../themes")+"/default.webp"));
in {
  home.file.".currentTheme".text = userSettings.theme;
  stylix =  {
    autoEnable = false;
    polarity = themePolarity;
    image = background;
    base16Scheme = ./. + themePath;
    
    fonts = {
      monospace = {
        name = userSettings.font;
        package = userSettings.fontPkg;
      };
      serif = {
        name = userSettings.font;
        package = userSettings.fontPkg;
      };
      sansSerif = {
        name = userSettings.font;
        package = userSettings.fontPkg;
      };
      emoji = {
        name = "Noto Color Emoji";
        package = pkgs.noto-fonts-emoji;
      };
      sizes = {
        terminal = 18;
        applications = 12;
        popups = 12;
        desktop = 12;
      };
    };

    ## Targets - Where use the theme    
    targets = {
      foot.enable = true;
      gnome.enable = true;
      gtk.enable = true;
      emacs.enable = true;
      firefox.enable = true;
 
      rofi.enable = true;
      sway.enable = true;
      swaylock.enable = true;
      #swaylock.useImage = true;
      mako.enable = true;
      waybar.enable = true;
      
      sxiv.enable = true;
    };
  };  

  home.packages = with pkgs; [ 
    swww				# Background
    #papirus-nord			# Icon-theme
    #nordzy-cursor-theme		# Cursor
  ];
  home.file.".background-stylix" = {
    text = ''
      swww img ''+config.stylix.image+'';
    '';
    executable = true;
  };
 
  # DConf
  dconf = {
    enable = true;
    settings = {
      "org/gnome/desktop/interface" = {
        color-scheme = if (themePolarity == "dark") then "prefer-dark" else "prefer-light";
      };
    };
  };
  # Cursor
  home.pointerCursor = {
    package = lib.mkForce pkgs.nordzy-cursor-theme;
    name = lib.mkForce "Nordzy-cursors";
    size = lib.mkForce 24;
    x11 = {
      enable = true;
      defaultCursor = "Nordzy-cursors";
    };
    gtk.enable = true;
  };  

  # GTK Application Styling
  gtk = {
    enable = true;
    iconTheme = {
      package = pkgs.papirus-icon-theme;
      name = "Papirus-Nord";
    };
    cursorTheme = {
      package = pkgs.nordzy-cursor-theme;
      name = "Nordzy-cursors";
      size = 24;
    };
    theme = {
      package = lib.mkForce pkgs.gnome.gnome-themes-extra;
      name = lib.mkForce "Adwaita";
    };
     
    # -- GTK*i version -- #
    gtk3.extraConfig = {
      Settings = ''
        gtk-application-prefer-dark-theme = ${if (themePolarity == "dark") then "1" else "0" };
      '';
    };
    gtk4.extraConfig = {
      Settings = ''
        gtk-application-prefer-dark-theme = ${if (themePolarity == "dark") then "1" else "0" };
      '';
    };
  };
  home.sessionVariables.GTK_THEME = "Adwaita";
  home.sessionVariables.GS_CURSOR_THEME = "Nordzy-cursor";

  # QT Application Styling
  qt = {
    enable = true;
    platformTheme = lib.mkForce "gnome";
    style = {
      package = pkgs.adwaita-qt;
      name = "adwaita-dark";
    };
  };
  home.sessionVariables = {
    QT_CURSOR_THEME = "Nordzy-cursor";
  };
}
