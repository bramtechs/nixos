{ config, pkgs, ... }: {
 
  # access on port 8080
  # don't know how to setup nginx reverse proxy

  environment.systemPackages = with pkgs; [
    pkgs.openjdk17-bootstrap
  ];

  # nixos provided jenkins does not work work out of the box
  # .war file is used instead

  # TODO: should be a systemd service
  services.cron = {
    enable = true;
    systemCronJobs = [
      "@reboot      server    ${pkgs.bash}/bin/bash ${builtins.toString ./jenkins_run}"
    ];
  };

  networking.firewall.allowedTCPPorts = [ 8080 ];
}
