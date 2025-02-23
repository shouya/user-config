{ inputs, ... }:
{
  imports =
    [
      inputs.nixos-hardware.nixosModules.gigabyte-b650
      inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate
      inputs.nixos-hardware.nixosModules.common-cpu-amd
      inputs.nixos-hardware.nixosModules.common-pc-ssd

      ./common.nix

      ./nfs-mount.nix
      ./mrnix/configuration.nix
      ./mrnix/hardware-configuration.nix
      ./mrnix/extra.hidden.nix
    ];
}
