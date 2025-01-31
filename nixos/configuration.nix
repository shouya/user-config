# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, lib, pkgs, ... }:
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # allow proprietary packages
  nixpkgs.config.allowUnfree = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  # boot.plymouth.enable = true; # boot animation
  boot.kernelModules = [ "nvidia" ];

  networking.hostName = "mrnix"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.

  # Set your time zone.
  time.timeZone = "Asia/Seoul";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkb.options in tty.
  # };

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    autoRepeatInterval = 45;
    autoRepeatDelay = 200;
    videoDrivers = [ "amdgpu" "nvidia" ];
  };
  hardware.graphics.enable = true;
  hardware.graphics.extraPackages = [pkgs.nvidia-vaapi-driver];
  hardware.nvidia = {
    open = false; # use proprietary driver
    modesetting.enable = true;
    powerManagement.enable = true;
    powerManagement.finegrained = true;
    nvidiaSettings = true;
    prime = {
      offload.enable = true;
      offload.enableOffloadCmd = true; # generate the nvidia-offload command

      nvidiaBusId = "PCI:1:0:0";
      amdgpuBusId = "PCI:16:0:0";
    };
  };
  # services.xserver.desktopManager.xmonad = {
  #   enable = true;
  #   enableContribAndExtras = true;
  # }
  # services.xserver.desktopManager.xfce.enable = true;
  services.xserver.windowManager.session = [{
    name = "xsession";
    manage = "window";
    start = "exec ~/.xsession";
  }];

  # Configure keymap in X11
  # services.xserver.xkb.layout = "us";
  # services.xserver.xkb.options = "eurosign:e,caps:escape";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # hardware.pulseaudio.enable = true;
  # OR
  services.pipewire = {
    enable = true;
    pulse.enable = true;
    extraConfig.pipewire = {
      "99-disable-bell" = {
        "context.properties"= {
          "module.x11.bell" = false;
        };
      };
    };
  };
  services.udisks2.enable = true; # required by udiskie

  # Enable touchpad support (enabled default in most desktopManager).
  # services.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.shou = {
    isNormalUser = true;
    extraGroups = [ "wheel" "video" "audio" "disk" "networkmanager" ];
    uid = 1000;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINqp5GiCayavtH8l4rfvg5Wi57yGxedOoXZd64oAy9Hv shou@herbian"
    ];
    shell = pkgs.fish;
    packages = with pkgs; [];
  };


  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # programs.firefox.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    delta
    fd
    file
    git
    gnumake
    htop
    lm_sensors
    lshw
    ncdu
    pciutils
    ripgrep
    tree
    vim
    xclip
    nix-search

    # for convenient tools
    python313Full
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryPackage = pkgs.pinentry-rofi;
  };
  programs.fish.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    settings.PermitRootLogin = "yes";
  };

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 22 ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # disable sudo password
  security.sudo.wheelNeedsPassword = false;

  services.udev.extraRules =
    let mkRule = lib.concatStringsSep ", ";
    in with pkgs; lib.concatStringsSep "\n" [
      # turn off rotational disk after 5 minutes (60 * 5) and set power
      # saving level to 128
      (mkRule [
        ''ACTION=="add|change"''
        ''SUBSYSTEM=="block"''
        ''KERNEL=="sd[a-z]", ATTR{queue/rotational}=="1"''
        ''RUN+="${hdparm}/bin/hdparm -B 90 -S 60 /dev/%k"''
      ])
    ];
  networking.interfaces.enp6s0.wakeOnLan.enable = true;

  powerManagement.resumeCommands = ''
    echo "This should show up in the journal after resuming."
  '';

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true; # incompatible with flake, disabled

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "24.11"; # Did you read the comment?
}
