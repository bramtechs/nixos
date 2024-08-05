{ ... }:
{
    imports =
        [
            (import ../modules.nix {}).home-manager
            ./packages.nix
        ];

    services.nix-daemon.enable = true;
    programs.zsh.enable = true;

    users.users.bram = {
        name = "bram";
        home = "/Users/bram";
    };

    home-manager.users.bram = { config, lib, pkgs, ... }: {
      imports =
        [
          ./bram-emacs.nix
        ];

        home.stateVersion = "23.11";
  };

}
