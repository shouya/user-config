function dmesg-prior --wraps journalctl
    journalctl -o short-precise -k -b -1
end
