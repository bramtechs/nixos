{ ... }: {
  options.services.adguardhome = {
    enable = true;
    openFirewall = true;
  };
}
