{ pkgs, ... }:

{
  imports = [ <home-manager/nixos> ];

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreePredicate = _: true;
  nixpkgs.config.permittedInsecurePackages = [ "nix-2.15.3" ];
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  boot.loader.systemd-boot.configurationLimit = 10;

  # Enable the X11 windowing system and configure i3
  environment.pathsToLink = [ "/libexec" ];

  # general X settings
  programs.light.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  services.libinput.enable = true;
  services.xserver = {
    enable = true;

    # Configure keymap in X11
    xkb = {
      layout = "us";
      options = "ctrl:nocaps"; # remember to use caps instead of control!
    };
  };

  # Set your time zone.
  time.timeZone = "Europe/Brussels";

  networking = {
    hostName = "doomhowl"; # Define your hostname.
  };

  console = { keyMap = "us"; };

  users.users.bram = {
    isNormalUser = true;
    extraGroups = [
      "kvm"
      "adbusers"
      "libvirtd"
      "fuse"
      "video"
      "wheel"
      "networkmanager"
      "docker"
      "sudo"
      "tss"
    ]; # Enable ‘sudo’ for the user.
  };

  home-manager.users.bram = { config, lib, pkgs, ... }: {
    imports = [ ./bram-emacs.nix ./bram-git.nix ];
    home.stateVersion = "23.11";
  };

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
    };
  };

  programs = { dconf.enable = true; };

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 22 3000 ];
  #networking.firewall.allowedUDPPorts = [ ... ];
  networking.firewall.enable = true;

  system.fsPackages = [ pkgs.sshfs ];
}

