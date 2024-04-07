{ config, lib, pkgs, callPackage, ... }:

{
  programs.vscode = {
    enable = true;
    enableExtensionUpdateCheck = false;
    enableUpdateCheck = false;
    userSettings = {
      "files.autoSave" = "onFocusChange";
      #"workbench.colorTheme" = "Gruvbox Dark Medium";
      "editor.minimap.enabled" = false;
      "editor.lineNumbers" = "off";
      "window.zoomLevel" = 1;
      "nix.enableLanguageServer" = true;
      "nix.serverPath" = "rnix-lsp";
      "security.workspace.trust.enabled" = false;
      "git.openRepositoryInParentFolders" = "always";
      "cmake.configureOnOpen"= true;
      "extensions.ignoreRecommendations"= true;
      "git.autofetch"= true;
      "[javascript]" = {
        "editor.defaultFormatter"= "esbenp.prettier-vscode";
      };
    };
    extensions = with pkgs.vscode-extensions; [
      jnoortheen.nix-ide
      jdinhlife.gruvbox
      esbenp.prettier-vscode
      ms-vscode.cpptools
      vadimcn.vscode-lldb
    ];
  };

  imports = [
    # Source: https://gist.github.com/piousdeer/b29c272eaeba398b864da6abf6cb5daa
    # Make vscode settings writable

    (import
      (builtins.fetchurl {
        url = "https://gist.githubusercontent.com/piousdeer/b29c272eaeba398b864da6abf6cb5daa/raw/41e569ba110eb6ebbb463a6b1f5d9fe4f9e82375/mutability.nix";
        sha256 = "4b5ca670c1ac865927e98ac5bf5c131eca46cc20abf0bd0612db955bfc979de8";
      })
      { inherit config lib; })

    (import
      (builtins.fetchurl {
        url = "https://gist.githubusercontent.com/piousdeer/b29c272eaeba398b864da6abf6cb5daa/raw/41e569ba110eb6ebbb463a6b1f5d9fe4f9e82375/vscode.nix";
        sha256 = "fed877fa1eefd94bc4806641cea87138df78a47af89c7818ac5e76ebacbd025f";
      })
      { inherit config lib pkgs; })
  ];
}
