function nix-pkg-size --arg name
    set -l info (__nix-pkg-size_info $name)
    set -l nar_size (echo $info | jq -r '.narSize')
    set -l download_size (echo $info | jq -r '.downloadSize')

    echo "Nar size: "(numfmt --to=iec $nar_size)
    echo "Download size: "(numfmt --to=iec $download_size)
end

function __nix-pkg-size_info --arg name
    nix path-info --store https://cache.nixos.org/ --json \
        (nix eval --raw "nixpkgs#"$name) \
        | jq 'first(.[])'
end
