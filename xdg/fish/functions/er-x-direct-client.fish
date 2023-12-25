#!/usr/bin/env fish

function _gen-er-x-set-direct-client
    set -l client_ip $argv[1]
    echo configure
    echo set firewall group address-group direct-clients address $client_ip
    echo 'compare; commit; exit'
    echo exit
end

function _gen-er-x-delete-direct-client
    set -l client_ip $argv[1]
    echo configure
    echo delete firewall group address-group direct-clients address $client_ip
    echo 'compare; commit; exit'
    echo exit
end


function er-x-direct-client -a cmd -a client_ip
    switch $cmd
        case add
            # the -tt option is used to force allocate a pseudo-tty,
            # otherwise edge router's shell won't load utilities in
            # /opt/vyatta/bin/vyatta-op-cmd-wrapper automatically.
            _gen-er-x-set-direct-client $client_ip | ssh -tt er-x
        case del
            _gen-er-x-delete-direct-client $client_ip | ssh -tt er-x
        case reset
            _gen-er-x-delete-direct-client | ssh -tt er-x
        case '*'
            echo "Usage: er-x-direct-client [add|del|reset] [client_ip]"
    end
end
