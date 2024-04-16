{ config, lib, pkgs, callPackage, ... }:

{
  imports =
    [
      ../configuration.nix
      ../cinnamon.nix
      ../packages.nix
      ../packages-heavy.nix
    ];

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

  programs = {
    virt-manager.enable = true;
  }
}