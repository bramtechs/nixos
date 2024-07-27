{ pkgs, ... }: {

  programs.git = {
    package = pkgs.gitAndTools.gitFull;
    enable = true;
    userName = "bramtechs";
    userEmail = "bramtech@telenet.be";
    ignores = [ "*~" "*ghcid.txt" "TAGS" ".idea" ".vscode" ];
    extraConfig = {
      core.editor = "vim";
      pull.rebase = false;
      protocol.keybase.allow = "always";
    };
  };
}
