{ ... }:

{
  services.nginx = {
    enable = true;
    virtualHosts."doomhowl.local" = {
      locations."~ ^/(git|v2)($|/)".extraConfig = ''
        client_max_body_size 512M;

        # make nginx use unescaped URI, keep "%2F" as-is, remove the "/git" sub-path prefix, pass "/v2" as-is.
        set $clean_uri $request_uri;
        if ($clean_uri ~ "^/git(/.*)$") {
          set $clean_uri $1;
        }

        proxy_pass http://127.0.0.1:3001$clean_uri;

        # other common HTTP headers
        proxy_set_header Connection $http_connection;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
      '';
    };
  };

  services.gitea = {
    enable = true;
    appName = "Doomhowl Interactive";
    dump = {
      enable = true;
      interval = "hourly";
    };
    settings = {
        webhook = {
          ALLOWED_HOST_LIST = "*";
        };
        server = {
          DOMAIN = "doomhowl.local";
          ROOT_URL = "http://doomhowl.local/git";
          HTTP_PORT = 3001;
        };
    };
  };
}
