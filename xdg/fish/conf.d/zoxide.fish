if not status is-interactive
    return
end

if set -q fish_private_mode
    set -gx _ZO_EXCLUDE_DIRS '/*'
end

set -gx _ZO_MAXAGE 10000

function _z_cd
    cd $argv
    or return $status

    commandline -f repaint

    if test "$_ZO_ECHO" = 1
        echo $PWD
    end
end

function z
    set argc (count $argv)

    if test $argc -eq 0
        _z_cd $HOME
    else if test $argc -eq 1; and test $argv[1] = -
        _z_cd -
    else
        set -l _zoxide_result (zoxide query -- $argv)
        and _z_cd $_zoxide_result
    end
end

complete -c z -x -a '(zoxide query -l -- $argv 2>/dev/null | head -n 20)'

function zi
    set -l _zoxide_result (zoxide query -i -- $argv)
    and _z_cd $_zoxide_result
end

function _zoxide_hook --on-variable PWD
    set -q fish_private_mode
    or zoxide add (pwd -L)
end
