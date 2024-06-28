{ config, lib, pkgs, ... }:

{
  home.file.".xinitrc".source = ./xinitrc;

  services.picom = {
    enable = true;
    vSync = true;
  };

  # misc config files
  home.file."${config.xdg.configHome}" = {
    source = ./config;
    recursive = true;
  };

}
