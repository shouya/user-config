# adapt my custom fish build to the system version

# enables "man <fish-command>"
if test (count $__fish_data_dir/man/man1/*) -eq 0;
    and test (count /usr/share/fish/man/man1/*) -gt 0
    set __fish_data_dir /usr/share/fish
end
