{ config, lib, pkgs, ... }:

let 
  system = builtins.currentSystem;
  extensions =
    (import (builtins.fetchGit {
      url = "https://github.com/nix-community/nix-vscode-extensions";
      ref = "refs/heads/master";
      rev = "c43d9089df96cf8aca157762ed0e2ddca9fcd71e";
    })).extensions.${system};
  extensionsList = with extensions.vscode-marketplace; [
      rust-lang.rust-analyzer
      rust-lang.rust
      jnoortheen.nix-ide
      jdinhlife.gruvbox
      esbenp.prettier-vscode
      ms-python.python
      ms-vscode-remote.remote-ssh
      leonardssh.vscord
      mads-hartmann.bash-ide-vscode
      tomoki1207.pdf
      twxs.cmake
      ms-azuretools.vscode-docker
      mechatroner.rainbow-csv
      christian-kohler.path-intellisense
      wayou.vscode-todo-highlight
      gruntfuggly.todo-tree
      benszabo.hotline-vice
      maarti.jenkins-doc
  ];
in
{
  programs.vscode = {
    enable = true;
    enableExtensionUpdateCheck = false;
    enableUpdateCheck = false;
    userSettings = {
      "files.autoSave" = "onFocusChange";
      #"workbench.colorTheme" = "Gruvbox Dark Medium";
      "workbench.colorTheme"="Hotline Vice";
      "editor.minimap.enabled" = false;
      "editor.lineNumbers" = "off";
      "editor.formatOnSave"=true;
      "window.zoomLevel" = 1;
      "security.workspace.trust.enabled" = false;
      "git.openRepositoryInParentFolders" = "always";
      "extensions.ignoreRecommendations"= true;
      "git.autofetch"= true;
      "vscord.status.idle.resetElapsedTime"= true;
      "vscord.behaviour.suppressNotifications"= true;
      "C_Cpp.clang_format_fallbackStyle"="Webkit";
      "cmake.configureOnEdit"=false;
      "cmake.configureOnOpen"=false;
      "cmake.enableAutomaticKitScan"=false;
      "cmake.showConfigureWithDebuggerNotification"=false;
      "nix.enableLanguageServer"= true;
      "nix.serverPath"= "nil";
      "vscord.status.details.text.idle"="Procrastinating";
      "[javascript]" = {
        "editor.defaultFormatter"= "esbenp.prettier-vscode";
      };
      "editor.defaultFormatter"= "esbenp.prettier-vscode";
    };
    extensions = extensionsList;
  };

  imports = [
    # Source: https://gist.github.com/piousdeer/b29c272eaeba398b864da6abf6cb5daa
    # HACK to make vscode settings writable

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
