if test -e ~/.nix-profile/etc/profile.d/nix.fish
    . ~/.nix-profile/etc/profile.d/nix.fish
end

# infrequently used tools
if not type -qf wakeonlan
    alias wakeonlan 'nix run nixpkgs#wakeonlan --'
    alias wakeonlan-herbian 'wakeonlan a0:ce:c8:10:64:ea'
    alias wakeonlan-mrnix 'wakeonlan 10:ff:e0:3c:6d:ec'
end

# run nix-shell environment without a subshell
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

# automatically run commands from packages without installing
# https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/programs/command-not-found/command-not-found.nix
# prefix command with a "," to run without installing
# set -x NIX_AUTO_RUN 1
