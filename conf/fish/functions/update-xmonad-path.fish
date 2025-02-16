
function update-xmonad-path \
    -d "Update the PATH environment variable on the xmonad process to current $PATH" \
    -a xmonad_pid

    test -z "$xmonad_pid"; and set xmonad_pid (pgrep xmonad)

    echo "Setting path on: $xmonad_pid"
    set value (string join : $PATH)

    echo "New path: $value"
    set cmd1 "call (char*)getenv(\"PATH\")"
    set cmd2 "call (int)setenv(\"PATH\", \"$value\", 1)"

    echo -e "Running on GDB:\n$cmd1\n$cmd2"
    gdb -p $xmonad_pid --batch --command=(echo -e "$cmd\n$cmd2" | psub -s .gdb)
end

complete -c update-xmonad-path -x -d "xmonad process id" -a "(pgrep xmonad)"
