{ config, pkgs, ... }:

{
  services.nginx = {
    enable = true;
    virtualHosts."git.doomhowl.local" = {
      locations."/" = { proxyPass = "http://localhost:3001/"; };
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
      DOMAIN = "git.doomhowl.local";
      ROOT_URL = "http://git.doomhowl.local/";
      HTTP_PORT = 3001;
    };

  };
}
