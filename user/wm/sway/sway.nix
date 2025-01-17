{ config, lib , pkgs, userSettings, ... }:
let 
  theme = "";
  fontConf = {
    names = [ "Roboto" ];
    size = 11.0;
  };
  swaylock = "${pkgs.swaylock}/bin/swaylock";
  buttons = {
    mod = "Mod4";
    alt = "Mod1";
    dot = "Period";
  };
in { 
  imports = [
    (./. + "../../../app/terminal"+("/"+userSettings.term)+".nix")
    ./waybar/waybar.nix
    ./swaylock/swaylock.nix
    ./notify/mako.nix
  ];

  ## Sway WM
  wayland.windowManager.sway = {
    enable = true;
    xwayland = true;
    config = {
        modifier = buttons.mod;
      	terminal = userSettings.term;
        #menu = userSettings.launcher; 
        gaps = {
          smartGaps = true;
          inner = 15;
        };
        bars = lib.singleton {
          command = "${pkgs.waybar}/bin/waybar";
          position = "top";
          fonts = fontConf;
        };
        window = {
          titlebar = true;
          hideEdgeBorders = "both";
        };
        ## == input
        input = {
          "type:keyboard" = {
            xkb_layout = "hu";
          };
          "type:touchpad" = {
            tap = "enabled";
            click_method = "clickfinger";
            natural_scroll = "enabled";
          };
        };
        ## == Workspace
        defaultWorkspace = "workspace number 1";
        workspaceOutputAssign = [
          {  }
        ];
        ## == keybinds
        keybindings =
         let 
           mod = config.wayland.windowManager.sway.config.modifier;
           inherit (config.wayland.windowManager.sway.config)
             left
             down
             up
             right
             menu
             terminal;
         in {
          # -- Basic stuff -- #
          "${mod}+Shift+Return" = "exec ${terminal}";
          "${mod}+Shift+Q" = "kill"; 
          "${mod}+l" = "exec swaylock";
          
          # -- Rofi Launcher -- # 
          "${mod}+p" = "exec rofi -modes 'file-browser-extended' -show file-browser-extended -file-browser-dir ~";
          "--release Super_L" = "exec rofi -modes 'drun' -show drun"; 
          "${mod}+Tab" = "exec rofi -modes 'window' -show window";
          "${mod}+Shift+C" = "exec rofi -modes 'calc' -show calc -no-show-match -no-sort";
          "${mod}+${buttons.dot}" = "exec rofi -modes 'emoji' -show emoji";

          # --= [ Window ] =-- #
          # Focus - Move
          # - Vim 
          #"${mod}+h" = "focus left";
          #"${mod}+j" = "focus down";
          #"${mod}+k" = "focus up";
          #"${mod}+l" = "focus right";
          # - Arrow 
          "${mod}+left" = "focus left";
          "${mod}+down" = "focus down";
          "${mod}+up" = "focus up";
          "${mod}+right" = "focus right";

          # Container - Move
          # - Vim 
          "${mod}+Shift+H" = "move left";
          "${mod}+Shift+J" = "move down";
          "${mod}+Shift+K" = "move up";
          "${mod}+Shift+L" = "move right";
          # - Arrow 
          "${mod}+Shift+Left" = "move left";
          "${mod}+Shift+Down" = "move down";
          "${mod}+Shift+Up" = "move up";
          "${mod}+Shift+Right" = "move right";

          # --= [ Workspace ]=-- #
          # Open Workspace
          "${mod}+1" = "workspace 1";
          "${mod}+2" = "workspace 2";
          "${mod}+3" = "workspace 3";
          "${mod}+4" = "workspace 4";
          "${mod}+5" = "workspace 5";
          # Move Container to Workspace
          "${mod}+Shift+1" = "move container to workspace 1";
          "${mod}+Shift+2" = "move container to workspace 2";
          "${mod}+Shift+3" = "move container to workspace 3";
          "${mod}+Shift+4" = "move container to workspace 4";
          "${mod}+Shift+5" = "move container to workspace 5";

          # --= [ Layout ] =-- # 
          # - Float          
          "${mod}+Shift+Space" = "floating toggle";
          "${mod}+space" = "focus mode_toggle"; 
          # - Switch Layout
          "${mod}+s" = "stacking";
          "${mod}+w" = "tabbed";
          "${mod}+e" = "toggle split";
          # - Split object
          "${mod}+v" = "splitv";
          "${mod}+b" = "splith";
          # - Fullscreen
          "${mod}+f" = "fullscreen";
          
          # - Focus Parent
          "${mod}+a" = "focus parent";
        };
        output = {
          "*" = {
            mode = "1920x1080@60Hz";
          };
        };
        startup = [
          # -- Here are goes the startups 
          { command = "swww init"; always = true; }
          { command = "~/.background-stylix"; always = true; }
        ];
    }; 
    extraSessionCommands = ''
      export SDL_VIDEODRIVER=wayland
      export QT_QPA_PLATFORM=wayland
      export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
      export _JAVA_AWT_WM_MONREPARENTING=1

      export SWWW_TRANSITION_STEP=5
      export SWWW_TRANSITION_FPS=30
    '';
    extraConfig = ''
      # Assign Application to Workspace
      assign [app_id="firefox"] workspace number 2
      assign [app_id="Firefox"] workspace number 2
      assign [app_id="nyxt"] workspace number 2
    
      assign [app_id="steam"] workspace number 3
      assign [app_id="lutris"] workspace number 3

      
      seat seat0 xcursor_theme Nordzy-cursors 24
    '';
  };
}
