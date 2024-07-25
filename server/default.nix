{ config, lib, pkgs, callPackage, ... }:
{
  imports =
  [
  ];

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreePredicate = _: true;

  # kernel additions
  boot.initrd.kernelModules = [ "cifs" ];
  boot.extraModulePackages = [ config.boot.kernelPackages.rtl8821ce ]; # it took me a while to get wifi working...
  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];

  time.hardwareClockInLocalTime = true;
  time.timeZone = "Europe/Brussels";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "ntfs" ];

  networking = {
    hostName = "doomhowl";
    nameservers = [ "1.1.1.1" "1.0.0.1" ];
    networkmanager.enable = true;
    wireless.enable = false;
    extraHosts = ''
      192.168.0.149 nas
    '';
  };

  virtualisation = {
    docker.enable = true;
  };

  console = {
    keyMap = "us";
  };

  users.users.server = {
    isNormalUser = true;
    extraGroups = [ "kvm" "libvirtd" "fuse" "video" "wheel" "networkmanager" "docker" "sudo" "tss" ];
  };

  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = true;
      PermitRootLogin = "no";
    };
  };

  networking.firewall.allowedTCPPorts = [ 22 ];
  networking.firewall.enable = true;

  system.fsPackages = [ pkgs.sshfs ];
}
