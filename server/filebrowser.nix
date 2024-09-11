{ config, pkgs, ... }: {

  # TODO: should be a systemd service
  services.cron = {
    enable = true;
    systemCronJobs = [
      "@reboot      server    ${pkgs.filebrowser}/bin/filebrowser --config /home/server/nixos/server/filebrowser.json"
    ];
  };

  networking.firewall.allowedTCPPorts = [ 9001 ];
}
