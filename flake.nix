{
  description = "Home Manager configuration of shou";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/release-24.11";
    nixos-hardware.url = "github:shouya/nixos-hardware/fix-gigabyte-b650-suspend";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixGL.url = "github:nix-community/nixGL";
    malakal.url = "github:shouya/malakal";
  };

  outputs = { nixpkgs, nixpkgs-stable, home-manager, nixos-hardware, ... }@inputs:
    let
      system = "x86_64-linux";
      stable-pkgs = import nixpkgs-stable { inherit system; config.allowUnfree = true; };
      pins = _final: _prev: {
        # khal = stable-pkgs.khal;
      };
      pkgs = (import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      }).extend pins;
    in {
      nixosConfigurations.mrnix = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = {inherit inputs system;};
        modules = [
          ./nixos/mrnix
        ];
      };

      homeConfigurations."shou@mrnix" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          { _module.args = { inherit inputs system; }; }
          { _module.args = { host = "mrnix"; }; }
          inputs.nix-index-database.hmModules.nix-index
          ./home/nixos.nix
          ./home
        ];

      };

      homeConfigurations."shou@herbian" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          { _module.args = { inherit inputs system; }; }
          { _module.args = { host = "herbian"; }; }
          inputs.nix-index-database.hmModules.nix-index
          ./home/non-nixos.nix
          ./home
        ];
      };

    };
}
