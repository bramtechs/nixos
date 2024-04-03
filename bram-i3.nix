{ config, lib, pkgs, ... }:

let
  mod = "Mod4";
in
{
  xsession.numlock.enable = true;
  xsession.windowManager.i3 = {
    enable = true;
    config = {
      modifier = mod;
      keybindings = lib.mkOptionDefault {
        "${mod}+p" = "exec ${pkgs.dmenu}/bin/dmenu_run";
        "${mod}+x" = "exec sh -c '${pkgs.maim}/bin/maim -s | xclip -selection clipboard -t image/png'";
        "${mod}+Shift+x" = "exec sh -c '${pkgs.i3lock}/bin/i3lock -c 222222 & sleep 5 && xset dpms force of'";

        # Focus
        "${mod}+j" = "focus left";
        "${mod}+k" = "focus down";
        "${mod}+l" = "focus up";
        "${mod}+semicolon" = "focus right";

        # Move
        "${mod}+Shift+j" = "move left";
        "${mod}+Shift+k" = "move down";
        "${mod}+Shift+l" = "move up";
        "${mod}+Shift+semicolon" = "move right";

        # My multi monitor setup
        "${mod}+m" = "move workspace to output DP-2";
        "${mod}+Shift+m" = "move workspace to output DP-5";
      };
      bars = [
        {
          position = "top";
          trayOutput = "primary";
          statusCommand = "i3status";
          fonts = {
            names = [ "DejaVu Sans Mono" ];
            style = "Bold Semi-Condensed";
            size = 10.0;
          };
        }
      ];
      startup = [{
        command = "systemctl --user restart picom";
        always = true;
        notification = false;
      }];
    };
  };

  gtk = {
    enable = true;
    theme = {
      name = "Materia-dark";
      package = pkgs.materia-theme;
    };
  };
}
