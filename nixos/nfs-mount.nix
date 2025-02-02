{ ... }:
let
  options = ["noatime" "x-systemd.automount" "x-systemd.idle-timeout=600"
             "_netdev" "rw" "auto" "noatime" "ac" "retry=100000" "fsc" "tcp"];
  mkNFS = path: {
    fsType = "nfs4";
    device = "nfs.lain.li:${path}";
    inherit options;
  };
in
{
  # required for NFSv4
  services.rpcbind.enable = true;

  fileSystems."/mnt/storage/archive" = mkNFS "/mnt/storage/archive";
  fileSystems."/mnt/storage/media" = mkNFS "/mnt/storage/media";
  fileSystems."/mnt/storage/share" = mkNFS "/mnt/storage/share";
  fileSystems."/mnt/storage/curious" = mkNFS "/mnt/storage/curious";
}
