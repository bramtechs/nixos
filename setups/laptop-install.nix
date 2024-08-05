{ config, pkgs, ... }:
{
  imports =
    [
      ../packages-heavy.nix
      ../packages-linux.nix
      ../packages.nix
      # ../startx.nix
      ../budgie.nix
      ../configuration.nix
      ../mount-nas.nix
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

}
