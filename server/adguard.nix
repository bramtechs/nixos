{ ... }: {
  # TODO investigate: package might be broken upstream
  options.services.adguardhome = {
    enable = true;
    openFirewall = true;
  };
}
