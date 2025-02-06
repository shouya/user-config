{
  description = "Home Manager configuration of shou";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    malakal.url = "github:shouya/malakal";
  };

  outputs = { nixpkgs, home-manager, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config = {
          allowUnfree = true;
        };
      };
    in {
      nixosConfigurations.mrnix = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          ./nixos/configuration.nix
        ];
      };

      homeConfigurations.shou = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          { _module.args = { inherit inputs; inherit system; }; }
          ./home.nix
        ];

      };
    };
}
