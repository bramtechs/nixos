{ pkgs ? import <nixpkgs> { config.allowUnfree = true; } }:

# nix shell with distracting software

with pkgs;

mkShell
  {
    buildInputs = [
      google-chrome
      cmus
      osu-lazer-bin
      audacious
      spotify
      discord
    ];
  }

