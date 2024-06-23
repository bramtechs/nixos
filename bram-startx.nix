{ config, lib, pkgs, ... }:

{
  home.file.".xinitrc".source = ./xinitrc;
}
