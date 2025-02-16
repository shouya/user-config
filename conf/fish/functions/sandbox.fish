# unshare based sandbox

function _sandbox-net
    echo "[$argv $(test -z $argv; and echo 1; or echo 2)]"

    test -z "$argv"
    and set -l cmdline $SHELL
    or set -l cmdline $argv

    echo "running [$cmdline]"
    unshare --net --map-current-user --keep-caps $cmdline
end

function sandbox -a cmd
    set -l argv $argv[2..]

    switch $cmd
        case net
            _sandbox-net $argv
        case '*'
            echo "Usage: sandbox [net]"
            return 1
    end
end

complete -c sandbox -f -a net
