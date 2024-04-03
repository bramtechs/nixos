{ config, lib, pkgs, callPackage, ... }:

{
  imports =
    [
      # ./i3.nix
      ./cinnamon.nix
      ./packages.nix
      <home-manager/nixos>
    ];

  # kernel additions
  boot.initrd.kernelModules = [ "cifs" ];
  boot.extraModulePackages = [ config.boot.kernelPackages.rtl8821ce ]; # it took me a while to get wifi working...
  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "ntfs" ];

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

  # battery
  services.auto-cpufreq.enable = true;
  services.auto-cpufreq.settings = {
    battery = {
      governor = "powersave";
      turbo = "never";
    };
    charger = {
      governor = "performance";
      turbo = "auto";
    };
  };

  # general X settings
  programs.light.enable = true;
  services.xserver = {
      enable = true;

      # Enable touchpad support (enabled default in most desktopManager).
      libinput.enable = true;


      # Configure keymap in X11
      layout = "us";
      xkbOptions = "ctrl:nocaps";
  };

  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    packages = with pkgs; [
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
    extraGroups = [ "libvirtd" "fuse" "video" "wheel" "networkmanager" "docker" "sudo" "tss" ]; # Enable ‘sudo’ for the user.
  };

  home-manager.users.bram = { config, lib, pkgs, ... }: {
      imports =
        [
          ./bram.nix
          ./bram-i3.nix
          ./bram-nvim.nix
          ./bram-vscode.nix
          ./bram-firefox.nix
        ];
  };

  systemd.user.services.start-apps = {
    description = "start apps";
    serviceConfig.PassEnvironment = "DISPLAY";
    script = ''
      discord &
    '';
    wantedBy = [ "multi-user.target" ]; # starts after login
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # extra hosts
  networking.extraHosts =
    ''
      192.168.0.149 nas
    '';

  services.picom = {
    enable = true;
    backend = "glx";
    settings = {
      blur = true;
      blurExclude = [ ];
      inactiveDim = "0.05";
      noDNDShadow = false;
      noDockShadow = false;
      # shadow-radius = 20
      # '';
      # shadow-radius = 20
      # corner-radius = 10
      # blur-size = 20
      # rounded-corners-exclude = [
      # "window_type = 'dock'",
      # "class_g = 'i3-frame'"
      # ]
      # '';
    };
    fade = false;
    inactiveOpacity = 1.0;
    menuOpacity = 1.0;
    opacityRules = [
      "0:_NET_WM_STATE@[0]:32a = '_NET_WM_STATE_HIDDEN'" # Hide tabbed windows
    ];
    shadow = false;
    shadowExclude = [ ];
    shadowOffsets = [ (-10) (-10) ];
    shadowOpacity = 0.5;
    vSync = true;
  };

  # virtualization
  virtualisation = {
    libvirtd = {
      enable = true;
      qemu = {
        package = pkgs.qemu_kvm;
        runAsRoot = true;
        swtpm.enable = true;
        ovmf = {
          enable = true;
          packages = [
            (pkgs.OVMF.override {
              secureBoot = true;
              tpmSupport = true;
            }).fd
          ];
        };
      };
    };
  };

  programs.virt-manager.enable = true;
  virtualisation.docker.enable = true;

  programs.dconf.enable = true;

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 22 ];
  #networking.firewall.allowedUDPPorts = [ ... ];
  networking.firewall.enable = true;

  system.fsPackages = [ pkgs.sshfs ];
  fileSystems =
    let
      # Use the user's gpg-agent session to query
      # for the password of the SSH key when auto-mounting.
      sshAsUser =
        pkgs.writeScript "sshAsUser" ''
          user="$1"; shift
          exec ${pkgs.sudo}/bin/sudo -i -u "$user" \
            ${pkgs.openssh}/bin/ssh "$@"
        '';
      options =
        [
          "user"
          "uid=bram"
          "gid=users"
          "allow_other"
          "exec" # Override "user"'s noexec
          "noatime"
          "nosuid"
          "_netdev"
          "ssh_command=${sshAsUser}\\040bram"
          "noauto"
          "x-gvfs-hide"
          "x-systemd.automount"
          #"Compression=yes" # YMMV
          # Disconnect approximately 2*15=30 seconds after a network failure
          "ServerAliveCountMax=1"
          "ServerAliveInterval=15"
          "dir_cache=no"
          "IdentityFile=/home/bram/.ssh/id_ed25519"
          "reconnect"
        ];
    in
    {
      "/mnt/nas" = {
        device = "${pkgs.sshfs-fuse}/bin/sshfs#bram@nas:/data";
        fsType = "fuse";
        inherit options;
      };
    };
  systemd.automounts = [
    { where = "/mnt/nas"; automountConfig.TimeoutIdleSec = "1 min"; }
  ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
}

