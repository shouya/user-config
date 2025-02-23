{
  # allow proprietary packages
  nixpkgs.config.allowUnfree = true;
  # build package with CUDA support
  nixpkgs.config.cudaSupport = true;

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # https://discourse.nixos.org/t/cuda-cache-for-nix-community/56038
  nix.settings.substituters = [
    "https://cache.nixos.org"
    "https://nix-community.cachix.org" # CUDA
    "https://spray8696.cachix.org" # malakal, etc
  ];
  nix.settings.trusted-public-keys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    "spray8696.cachix.org-1:1ZAU6SPy6bjak0g2KZq4lf/Z9hbgRFud04HeReFKnHk="
  ];

  nix.gc = {
    automatic = true;
    dates = "daily";
    options = "--delete-older-than 30d";
  };
  nix.optimise = {
    dates = ["weekly"];
    automatic = true;
  };

  # locale
  i18n.defaultLocale = "en_US.UTF-8";

  # globally available aliases
  environment.shellAliases = {
    ll = "ls -lh";
    l = "ls";
    lt = "ls -lhtr";
    rg = "rg --smart-case";
    less = "less -R";
  };
}
