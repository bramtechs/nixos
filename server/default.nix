{ config, pkgs, ... }: {
  imports = [
    (import ../modules.nix { }).home-manager

    (fetchTarball
      "https://github.com/nix-community/nixos-vscode-server/tarball/master")

    ./packages.nix
    ./gitea.nix
    ./filebrowser.nix
    ./jenkins.nix
    ./nginx.nix
    ./samba.nix
    #./mount-nas-server.nix
    ../../zerotier.nix
  ];

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreePredicate = _: true;
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # kernel additions
  boot.initrd.kernelModules = [ "cifs" ];
  boot.extraModulePackages = [
    config.boot.kernelPackages.rtl8821ce
  ]; # it took me a while to get wifi working...
  boot.initrd.availableKernelModules =
    [ "nvme" "xhci_pci" "ahci" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];

  time.hardwareClockInLocalTime = true;
  time.timeZone = "Europe/Brussels";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "ntfs" ];

  networking = {

    hostName = "doomhowl-server";
    nameservers = [ "1.1.1.1" "1.0.0.1" ];
    # leaked ips
    extraHosts = ''
      192.168.0.149   nas
      127.0.0.1       doomhowl.local
      16.0.0.239      node
    '';
    useDHCP = false;
    defaultGateway = "16.0.0.1";
    interfaces = {
      enp2s0.ipv4.addresses = [{
        address = "16.0.0.100";
        prefixLength = 8;
      }];
    };
  };

  virtualisation = { docker.enable = true; };

  console = { keyMap = "us"; };

  users.users.server = {
    isNormalUser = true;
    extraGroups = [
      "kvm"
      "libvirtd"
      "fuse"
      "video"
      "wheel"
      "networkmanager"
      "docker"
      "sudo"
      "tss"
    ];
  };

  home-manager.users.server = { config, lib, pkgs, ... }: {
    nixpkgs.config.allowUnfree = true;
    nixpkgs.config.allowUnfreePredicate = _: true;
    imports = [ ../bram-git.nix ../bram-vscode.nix ../bram-nvim.nix ];
    home.stateVersion = "24.05";
  };

  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = true;
      PermitRootLogin = "no";
    };
  };
  services.vscode-server.enable = true;

  networking.firewall.allowedTCPPorts = [ 22 80 443 8006 3389 3000 9000 9002 6881 ];
  networking.firewall.allowedUDPPorts = [ 3389 53 9000 6881 9002 ];
  networking.firewall.enable = true;

  system.fsPackages = [ pkgs.sshfs ];

  fileSystems."/mnt/extra" = {
    device = "/dev/disk/by-partlabel/extra";
    fsType = "ext4";
    options = [ "nofail" "users" ];
  };

  # run backup script often
  services.cron = {
    enable = true;
    systemCronJobs = [
      "0 */3 * * *     root    ${pkgs.bash}/bin/bash ${builtins.toString ./backup}"
    ];
  };
}
