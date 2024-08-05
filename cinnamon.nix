{ pkgs, ...} :
{
  services.xserver = {
    enable = true;
    displayManager.gdm.enable = true;
    desktopManager.cinnamon.enable = true;
  };
}
