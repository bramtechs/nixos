{ config, lib, pkgs, ... }:

let
  # Fetch the remote text file
  extraHostsSource = builtins.fetchurl "https://raw.githubusercontent.com/DWW256/distracting-websites/main/distracting-websites.txt";

  # Split the text file into an array of lines and filter out empty lines
  extraHostsLines = lib.filter (line: line != "") (lib.splitString "\n" (builtins.readFile extraHostsSource));

  # Map each line to prepend "0.0.0.0" to it
  extraHostsMapped = lib.concatMapStringsSep "\n" (domain: "0.0.0.0 " + (builtins.replaceStrings ["*"] [""] domain)) extraHostsLines;
in
{
  networking.extraHosts = ''
    ${extraHostsMapped}
  '';
}
