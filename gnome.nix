{ pkgs, ...} :
{
  services.xserver = {
    enable = true;
    displayManager.gdm.enable = true;
    desktopManager.gnome.enable = true;
  };

  environment.gnome.excludePackages = (with pkgs; [
    gnome-photos
    gnome-tour
    evolution
  ]) ++ (with pkgs.gnome; [
    cheese
    gnome-music
    epiphany
    geary
    gnome-characters
    tali
    iagno
    hitori
    atomix
  ]);

}