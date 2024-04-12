{ config, lib, pkgs, ... }:

{
    # https://nixos.wiki/wiki/Creating_a_NixOS_live_CD

    imports = [
        <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>
        "${builtins.getEnv "PWD"}/configuration.nix"
    ];

    # use fastest compression
    isoImage.squashfsCompression = "lz4";
}