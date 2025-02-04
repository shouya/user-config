# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, lib, pkgs, ... }:
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./nfs-mount.nix
    ];

  # allow proprietary packages
  nixpkgs.config.allowUnfree = true;
  # build package with CUDA support
  nixpkgs.config.cudaSupport = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  # boot.plymouth.enable = true; # boot animation
  boot.kernelModules = [ "nvidia" ];

  # resume from swap
  boot.resumeDevice = "/dev/disk/by-uuid/8e5a49c0-3bf6-4728-ba92-727acdd442a9";

  # luks unlock with usb stick
  boot.initrd.luks.devices.nixos.keyFile = "/dev/disk/by-partuuid/8df4dcb5-02";
  boot.initrd.luks.devices.nixos.keyFileSize = 4096;
  boot.initrd.luks.devices.nixos.keyFileOffset = 151243;
  boot.initrd.luks.devices.nixos.fallbackToPassword = true;

  # Power management configuration
  powerManagement.enable = true;
  networking.interfaces.eth.name = "enp6s0";
  networking.interfaces.eth.wakeOnLan.enable = true;
  powerManagement.resumeCommands = ''
    echo "This should show up in the journal after resuming."
  '';

  networking.hostName = "mrnix"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;

  time.timeZone = "Asia/Seoul";

  i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkb.options in tty.
  # };

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    videoDrivers = [ "amdgpu" "nvidia" ];
    displayManager.lightdm.enable = true;
    desktopManager.xterm.enable = true;
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
      # disable audible bell
      "99-disable-bell" = {
        "context.properties"= {
          "module.x11.bell" = false;
        };
      };
    };
  };
  services.udisks2.enable = true; # required by udiskie

  # bluetooth
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
    # for battery status
    settings = {General = {Experimental = true;};};
  };
  services.blueman.enable = true;

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
  };
  fileSystems."/home/shou/tmp" = {device = "tmpfs"; fsType = "tmpfs";};


  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # mostly for CUDA cache
  # https://discourse.nixos.org/t/cuda-cache-for-nix-community/56038
  nix.settings.substituters = [
    "https://nix-community.cachix.org"
  ];
  nix.settings.trusted-public-keys = [
    # Compare to the key published at https://nix-community.org/cache
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
  ];

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
    bluez-experimental
    nix-alien

    # for convenient tools
    python313Full
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.mtr.enable = true;
  programs.fish.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    settings.PermitRootLogin = "yes";
  };

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 22 11434 ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # disable sudo password
  security.sudo.wheelNeedsPassword = false;
  security.pam.services.login.enableGnomeKeyring = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryPackage = pkgs.pinentry-gnome3; # use gnome-keyring
  };

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
