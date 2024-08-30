{ pkgs, ... }:

let
  private_hosts_file = ./private_hosts;
  zerotier_file = ../zerotier.nix;
in {
  imports = [ (import ./modules.nix { }).home-manager ]
    ++ (if builtins.pathExists zerotier_file then
      [ (import zerotier_file) ]
    else
      [ ]);

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreePredicate = _: true;
  nixpkgs.config.permittedInsecurePackages = [ "nix-2.15.3" ];
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  boot.loader.systemd-boot.configurationLimit = 10;

  # Enable the X11 windowing system and configure i3
  environment.pathsToLink = [ "/libexec" ];

  # general X settings
  programs.light.enable = true;

  # compilation cache
  programs.ccache.enable = true;

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
    imports = [
      ./bram-emacs.nix
      ./bram-git.nix
      ./bram.nix
      ./bram-nvim.nix
      ./bram-vscode.nix
    ];
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

  networking = {
    nameservers = [ "1.1.1.1" "1.0.0.1" ];
    networkmanager.enable = true;
    wireless.enable = false;
    extraHosts = ''
      192.168.0.149     nas
      192.168.0.214     doomhowl.local
      192.168.192.157   doomhowl.global
    '' + (if builtins.pathExists private_hosts_file then
      builtins.readFile private_hosts_file
    else
      "");
  };

  programs.steam = {
    enable = true;
    remotePlay.openFirewall =
      true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall =
      true; # Open ports in the firewall for Source Dedicated Server
  };

  programs = {
    virt-manager.enable = true;
    adb.enable = true;
    dconf.enable = true;
  };

  services.udev.packages = [ pkgs.android-udev-rules ];

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 22 3000 ];
  #networking.firewall.allowedUDPPorts = [ ... ];
  networking.firewall.enable = true;

  system.fsPackages = [ pkgs.sshfs ];
}
