{ config, lib, pkgs, callPackage, ... }:
{
  imports =
    [
      <home-manager/nixos>
      ./minimal-install.nix
      ../packages-heavy.nix
      ../irc.nix
    ];

  # kernel additions
  boot.initrd.kernelModules = [ "cifs" ];
  boot.extraModulePackages = [ config.boot.kernelPackages.rtl8821ce ]; # it took me a while to get wifi working...
  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];

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

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "ntfs" ];

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

  # virtualization
  virtualisation = {
    vmware = {
      host.enable = true;
    };
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
    spiceUSBRedirection.enable = true;
    docker.enable = true;
  };

  networking = { 
    nameservers = [ "1.1.1.1" "1.0.0.1" ]; 
    networkmanager.enable = true;
    wireless.enable = false;
    nameservers = [ "1.1.1.1" "1.0.0.1" ];
  };

  home-manager.users.bram = { config, lib, pkgs, ... }: {
    imports =
      [
        ./bram.nix
        ./bram-i3.nix
        ./bram-nvim.nix
        ./bram-alacritty.nix
        ./bram-vscode.nix
        ./bram-firefox.nix
        ./bram-librewolf.nix
      ];
  };

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
  };

  programs = {
    virt-manager.enable = true;
    adb.enable = true;
  };

  services.udev.packages = [
    pkgs.android-udev-rules
  ];
}
