{...}:

let
  modules = {
    home-manager = "${builtins.fetchTarball {
      url = "https://github.com/nix-community/home-manager/archive/release-24.05.tar.gz";
      sha256 = "0c83di08nhkzq0cwc3v7aax3x8y5m7qahyzxppinzwxi3r8fnjq3"; 
    }}/nixos";

    doomhowl = builtins.fetchTarball { url="https://www.github.com/bramtechs/nix-channel/archive/main.tar.gz"; };
  };
in
  modules