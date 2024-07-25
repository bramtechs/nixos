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
    appName = "Doomohowl Interactive";
    settings.server = {
      DOMAIN = "git.doomhowl.local";
      ROOT_URL = "http://git.doomhowl.local/";
      HTTP_PORT = 3001;
    };
  };

  systemd.timers."gitea-backup" = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "10s";
      OnUnitActiveSec = "30m";
      Unit = "gitea-backup.service";
    };
  };

  systemd.services."gitea-backup" = {
    script = builtins.readFile ./backup-gitea;
    serviceConfig = {
      Type = "oneshot";
      User = "root";
    };
  };
}
