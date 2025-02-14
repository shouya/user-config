{
  description = "Home Manager configuration of shou";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/release-24.11";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    malakal.url = "github:shouya/malakal";

    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, nixpkgs-stable, home-manager, ... }@inputs:
    let
      system = "x86_64-linux";
      stable-pkgs = import nixpkgs-stable { inherit system; config.allowUnfree = true; };
      pins = final: prev: {
        # tracking: https://github.com/NixOS/nixpkgs/issues/380562
        khal = stable-pkgs.khal;
      };
      pkgs = (import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      }).extend pins;
    in {
      nixosConfigurations.mrnix = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          { _module.args = { inherit inputs system; }; }
          ./nixos/configuration.nix
        ];
      };

      homeConfigurations.shou = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          { _module.args = { inherit inputs system; }; }
          inputs.nix-index-database.hmModules.nix-index
          ./home.nix
        ];

      };
    };
}
