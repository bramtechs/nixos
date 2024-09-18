{...}:
{
    services.nginx.enable = true;
    services.nginx.virtualHosts."doomhowl.local" = {
        root = "/var/www/doomhowl.local";
    };
}
