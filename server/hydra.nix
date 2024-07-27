{ config, ... }: {
  services.nginx = {
    enable = true;
    virtualHosts."doomhowl.local" = {
      locations."/" = {
        proxyPass = "http://localhost:3030";
        recommendedProxySettings = true;
      };
    };
  };

  services.hydra = {
    enable = true;
    hydraURL = "http://doomhowl.local";
    notificationSender = "hydra@localhost";
    buildMachinesFiles = [ ];
    port = 3030;
    useSubstitutes = true;
  };
}
