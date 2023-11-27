# This file was partly copied from
# https://github.com/ThisisYoYoDev/Copilot-CLI-fish-configuration/blob/main/copilot_cli.fish

if not status is-interactive
    return
end

set copilot_cli_path (which github-copilot-cli)

if test -z "$copilot_cli_path"
    return
end

function copilot_what-the-shell
    set TMPFILE (mktemp)
    trap 'rm -f $TMPFILE' EXIT

    # there are error output for me blocking some telemetry endpoints
    if $copilot_cli_path $argv[1] (echo $argv[2..-1]) --shellout $TMPFILE 2>/dev/null
        if test -e "$TMPFILE"
            eval (cat $TMPFILE | sed ':a;N;$!ba;s/\n/ ; /g')
            if test "$status" -ne 0
                echo "Apologies! The command failed, log is available at $TMPFILE"
            end
        else
            echo "Apologies! Extracting command failed"
        end
    else
        return 1
    end
end

# copilot what the shell
alias '!!' 'copilot_what-the-shell what-the-shell'
