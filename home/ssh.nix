{ pkgs, linkConf, ... } :
let keys = pkgs.fetchurl {
      url = "https://github.com/shouya.keys";
      hash = "sha256-x5hHwpK6WtkAify0EkubOhvrKrJHmAD0mXYobf0WWNM=";
    };
in {
  home.file.".ssh/authorized_keys".source = keys;
}
