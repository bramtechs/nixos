{ pkgs, specialArgs, ... }:


{
  programs.alacritty = {
    enable = true;
    settings = {
      bell = {
        animation = "EaseOutExpo";
        duration = 5;
        color = "#ffffff";
      };
      colors = {
        primary = {
          background = "#040404";
          foreground = "#c5c8c6";
        };
      };
      font = {
        size = 14;
      };
      keyboard.bindings = [
        { key = 53; mods = "Shift"; mode = "Vi"; action = "SearchBackward"; }
        #{ key = "Return"; mods = "Shift"; chars = "\\x1b[13;2u"; }
        #{ key = "Return"; mods = "Control"; chars = "\\x1b[13;5u"; }
      ];
      selection.save_to_clipboard = true;
      window = {
        decorations = "full";
        opacity = 0.85;
        padding = {
          x = 5;
          y = 5;
        };
      };
    };
  };
}
