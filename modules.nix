{ pkgs ? import <nixpkgs> { } }:

let
  modules = {
    home-manager = "${
        builtins.fetchTarball {
          url =
            "https://github.com/nix-community/home-manager/archive/release-24.05.tar.gz";
          sha256 = "0c83di08nhkzq0cwc3v7aax3x8y5m7qahyzxppinzwxi3r8fnjq3";
        }
      }/nixos";

    doomhowl = pkgs.fetchFromGitHub {
      owner = "bramtechs";
      repo = "nix-channel";
      rev = "b9cc261f45224e88cdf77abedd689ce31a0c378c";
      sha256 = "sha256-wphlWjFImyrVwrXbExzRAC7J0oPMlabaeT7Bwi/Pj1g=";
    };
  };
in modules
