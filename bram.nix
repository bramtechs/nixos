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
    ignores = [ "*~" "*ghcid.txt" ];
    extraConfig = {
      core.editor = "vi";
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
    };
    initExtra = ''
      if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then
        . ~/.nix-profile/etc/profile.d/nix.sh;
        export NIX_PATH=$HOME/.nix-defexpr/channels''${NIX_PATH:+:}$NIX_PATH
      fi # added by Nix installer
      PATH="$PATH:$HOME/dev/nixos/scripts"

      # add private scripts
      mkdir -p ~/dev/scripts
      PATH="$PATH:$HOME/dev/scripts"

      # set wallpaper
      gsettings set org.gnome.desktop.background picture-uri file:///home/bram/dev/nixos/misc/wallpaper_red.png      
    '';
  };

  programs.fzf = {
    enable = true;
    enableBashIntegration = true;
  };

  # Automounter for removable media.
  services.udiskie = {
    automount = true;
    notify = true;
  };

  # The state version is required and should stay at the version you
  # originally installed.
  home.stateVersion = "21.11";
}
