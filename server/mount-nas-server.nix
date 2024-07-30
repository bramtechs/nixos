{ config, lib, pkgs, callPackage, ... }:

# Make sure ssh keys are installed or this will hang awkwardly...
{
  fileSystems = let
    # Use the user's gpg-agent session to query
    # for the password of the SSH key when auto-mounting.
    sshAsUser = pkgs.writeScript "sshAsUser" ''
      user="$1"; shift
      exec ${pkgs.sudo}/bin/sudo -i -u "$user" \
        ${pkgs.openssh}/bin/ssh "$@"
    '';
    options = [
      "user"
      "uid=server"
      "gid=users"
      "allow_other"
      "exec" # Override "user"'s noexec
      "noatime"
      "nosuid"
      "_netdev"
      "ssh_command=${sshAsUser}\\040server"
      "noauto"
      "x-gvfs-hide"
      "x-systemd.automount"
      #"Compression=yes" # YMMV
      # Disconnect approximately 2*15=30 seconds after a network failure
      "ServerAliveCountMax=1"
      "ServerAliveInterval=15"
      "dir_cache=no"
      "IdentityFile=/home/server/.ssh/id_ed25519"
      "reconnect"
    ];
  in {
    "/mnt/nas/git" = {
      device = "${pkgs.sshfs-fuse}/bin/sshfs#bram@nas:/git";
      fsType = "fuse";
      inherit options;
    };
    "/mnt/nas/data" = {
      device = "${pkgs.sshfs-fuse}/bin/sshfs#bram@nas:/data";
      fsType = "fuse";
      inherit options;
    };
  };
  systemd.automounts = [{
    where = "/mnt/nas";
    automountConfig.TimeoutIdleSec = "1 min";
  }];
}
