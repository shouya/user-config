if test -e ~/.nix-profile/etc/profile.d/nix.fish
    . ~/.nix-profile/etc/profile.d/nix.fish
end

# infrequently used tools
if not type -qf wakeonlan
    alias wakeonlan 'nix run nixpkgs#wakeonlan --'
    alias wakeonlan-herbian 'wakeonlan a0:ce:c8:10:64:ea'
    alias wakeonlan-mrnix 'wakeonlan 10:ff:e0:3c:6d:ec'
end

alias nix-shell-fish 'nix-shell --command fish'
alias nix-develop-fish 'nix develop--command fish'

function nixos-conf-repl --description 'Start a nixos configuration repl'
    # run in a subshell so cwd is not changed
    fish -c '
    cd ~/projects/user-config
    echo "Hint: NixOS config visible under config.<TAB>"
    nix repl ".#nixosConfigurations."(hostname)
    '
end
