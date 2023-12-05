# source env file in bash variable assignment format
function bash-source
    if test -f "$argv[1]"
        exec bash -c "source '$argv[1]'; exec fish"
    else
        echo "File not found: $argv[1]"
    end
end
