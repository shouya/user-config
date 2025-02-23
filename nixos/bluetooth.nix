{
  # bluetooth
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
    # for battery status
    settings = {General = {Experimental = true;};};
  };

  services.blueman.enable = true;
}
