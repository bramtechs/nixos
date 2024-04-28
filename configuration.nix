{ config, lib, pkgs, callPackage, ... }:

{
  imports =
    [
      <home-manager/nixos>
      ./zen.nix # don't
      ./mount-nas.nix
    ];

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreePredicate = _: true;

  # dual booting
  time.hardwareClockInLocalTime = true;

  # mesa
  hardware.opengl.driSupport = true;

  # tpm bs
  security.tpm2.enable = true;
  security.tpm2.pkcs11.enable = true; # expose /run/current-system/sw/lib/libtpm2_pkcs11.so
  security.tpm2.tctiEnvironment.enable = true; # TPM2TOOLS_TCTI and TPM2_PKCS11_TCTI env variables

  # bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Enable the X11 windowing system and configure i3
  environment.pathsToLink = [ "/libexec" ];

  # general X settings
  programs.light.enable = true;
  services.xserver = {
      enable = true;

      # Enable touchpad support (enabled default in most desktopManager).
      libinput.enable = true;

      # Configure keymap in X11
      layout = "us";
      xkbOptions = "ctrl:nocaps"; # remember to use caps instead of control!
  };

  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    packages = with pkgs; [
      font-awesome
      alacritty
      anonymousPro
      corefonts
      dejavu_fonts
      freefont_ttf
      google-fonts
      inconsolata
      liberation_ttf
      powerline-fonts
      source-code-pro
      terminus_font
      ttf_bitstream_vera
      ubuntu_font_family
    ];
  };

  # Set your time zone.
  time.timeZone = "Europe/Brussels";

  networking = {
      hostName = "doomhowl"; # Define your hostname.
      networkmanager.enable = true;
      wireless.enable = false;
      nameservers = [ "1.1.1.1" "1.0.0.1" ];
  };

  console = {
    keyMap = "us";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  users.users.bram = {
    isNormalUser = true;
    extraGroups = [ "kvm" "adbusers" "libvirtd" "fuse" "video" "wheel" "networkmanager" "docker" "sudo" "tss" ]; # Enable ‘sudo’ for the user.
  };

  home-manager.users.bram = { config, lib, pkgs, ... }: {
      imports =
        [
          ./bram.nix
          ./bram-i3.nix
          ./bram-nvim.nix
          ./bram-alacritty.nix
          ./bram-vscode.nix
          ./bram-emacs.nix
          ./bram-librewolf.nix
        ];
  };

  # nsa backdoor
  nixpkgs.config.permittedInsecurePackages = [
                  "nix-2.15.3"
                ];

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # extra hosts
  networking.extraHosts =
    ''
      192.168.0.149 nas
      127.0.0.1 server.test
      127.0.0.1 tirematch.local
    '';
  
  programs = {
    dconf.enable = true;
  };

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 22 3000 ];
  #networking.firewall.allowedUDPPorts = [ ... ];
  networking.firewall.enable = true;

  system.fsPackages = [ pkgs.sshfs ];
}

