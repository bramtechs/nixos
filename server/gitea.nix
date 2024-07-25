{ config, pkgs, ... }:

{
  services.nginx = {
    enable = true;
    virtualHosts."doomhowl.local" = {
      locations."/git" = { proxyPass = "http://localhost:3001/"; };
    };
  };

  services.gitea = {
    enable = true;
    appName = "Doomhowl Interactive";
    dump = {
      enable = true;
      interval = "hourly";
    };
    settings.server = {
      DOMAIN = "doomhowl.local";
      ROOT_URL = "http://doomhowl.local/git";
      HTTP_PORT = 3001;
    };

  };
}
