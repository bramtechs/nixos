{ config, lib, pkgs, ... }:

{
  programs.home-manager.enable = true;

  # variables
  home.sessionVariables = {
    EDITOR = "nvim";
    BROWSER = "google-chrome-stable";
    TERMINAL = "xterm";
  };

  # unfree
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreePredicate = (_: true);

  programs.git = {
    package = pkgs.gitAndTools.gitFull;
    enable = true;
    userName = "bramtechs";
    userEmail = "bramtech@telenet.be";
    ignores = [ "*~" "*ghcid.txt" "TAGS" ".idea" ".vscode" ];
    extraConfig = {
      core.editor = "vi";
      pull.rebase = false;
      # Allow keybase git protocol.
      protocol.keybase.allow = "always";
    };
  };

  programs.tmux = {
    enable = true;
    escapeTime = 0;
    mouse = true;
  };

  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = [ "qemu:///system" ];
      uris = [ "qemu:///system" ];
    };
  };

  programs.bash = {
    enable = true;
    historyIgnore = [ "l" "ls" "cd" "exit" ];
    historyControl = [ "erasedups" ];
    shellAliases = {
      cdnix = "cd $HOME/dev/monolith/nixos";
      cdmono = "cd $HOME/dev/monolith";
      screenfetch = "hyfetch";
    };

    initExtra = ''
      PATH="$PATH:$HOME/dev/nixos/scripts"

      # add private scripts
      mkdir -p ~/dev/scripts
      PATH="$PATH:$HOME/dev/scripts"

      # set wallpaper
      gsettings set org.gnome.desktop.background picture-uri file:///home/bram/dev/nixos/misc/wallpaper_red.png 
    '';
  };

  # dicks
  programs.hyfetch = {
    enable = true;
    settings = {
      preset = "rainbow";
      mode = "rgb";
      light_dark = "dark";
      lightness = 0.65;
      color_align = {
          mode = "horizontal";
          custom_colors = [];
          fore_back = null;
      };
      backend = "neofetch";
      distro = null;
      pride_month_shown = [];
      pride_month_disable = false;
    };
  };

  programs.fzf = {
    enable = true;
    enableBashIntegration = true;
  };
}
