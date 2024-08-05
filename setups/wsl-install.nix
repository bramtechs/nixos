{ ... }:

{
  imports =
    [
        (fetchTarball "https://github.com/nix-community/NixOS-WSL/tarball/main")

        ../configuration.nix
        ../packages.nix
    ];

    wsl.enable = true;
    wsl.defaultUser = "bram";

    hardware.pulseaudio.enable = false; # remove annoying beeps

    services.xserver = {
        enable = true;
        layout = "us";
        xkbOptions = "ctrl:nocaps"; # remember to use caps instead of control!
    };
}
