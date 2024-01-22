# my custom compiled fish installation is in /usr/local prefix, but I
# also want to make use of the fish completions that come with the
# distro.

for p in $fish_complete_path
    if string match -q '/usr/local/share/fish/*' $p
        set -l new_path (string replace '/usr/local/share/fish/' '/usr/share/fish/' $p)
        set -a fish_complete_path $new_path
    end
end
