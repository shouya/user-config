if test -e ~/.nix-profile/etc/profile.d/nix.fish
    . ~/.nix-profile/etc/profile.d/nix.fish
end

# infrequently used tools
if not type -qf wakeonlan
    alias wakeonlan 'nix run nixpkgs#wakeonlan --'
    alias wakeonlan-herbian 'wakeonlan a0:ce:c8:10:64:ea'
    alias wakeonlan-mrnix 'wakeonlan 10:ff:e0:3c:6d:ec'
end

function nix-shell-here --description 'Load nix-shell env without a subshell'
    eval (nix-shell $argv --run "direnv dump fish")
end

function nixos-conf-repl --description 'Start a nixos configuration repl'
    # run in a subshell so cwd is not changed
    fish -c '
    cd ~/projects/user-config
    echo "Hint: NixOS config visible under config.<TAB>"
    nix repl ".#nixosConfigurations."(hostname)
    '
end
