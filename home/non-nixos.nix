{ config, pkgs, lib, ... }:
{
  _module.args = {
    wrapGL = config.lib.nixGL.wrap;
  };
}
