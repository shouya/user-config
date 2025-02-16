{ config, pkgs, lib, ... }:
{
  _module.args = {
    wrapGL = pkg: pkg;
  };
}
