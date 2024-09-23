if not status is-interactive
    return
end

# LS related
alias l ls
alias ll 'ls -lh'
alias lt 'ls -lhtr'

# view file in vim with syntax highlighting
alias vr 'vim -R'

## \(o w o)/-- E.M.A.C.S!
alias eacm emacs
alias eamc emacs
alias eamcs emacs
alias eamsc emacs
alias emac emacs
alias emas emacs
alias emasc emacs
alias emcas emacs
alias emcs emacs
alias emsc emacs

alias emacsnw 'emacs -wn'
alias emnw 'emacs -nw'
alias ec 'emacsclient -n'
alias ecc 'emacsclient -nc'
alias ect 'emacsclient -nt'
alias emacsserver 'emacs --daemon'
alias emacskill "emacsclient -e '(kill-emacs)'"

# extracting arbitrary archives
alias x 'atool --explain --extract'
alias xl 'atool --explain --list'

# kubectl
alias k kubectl

# aria
alias a2c 'aria2c -x10 -s10 -c --file-allocation=none'

# yt-dlp
alias yt 'yt-dlp --no-progress --no-mtime --min-filesize 200k --cookies-from-browser firefox --downloader aria2c --verbose'

# ripgrep, default to smart case
alias rg 'rg --smart-case'

# render terminal escape sequences
alias less 'less -R'

# use eix to search for packages even in Debian
# type -qf is fish built-in as equivalence to which >/dev/null
if type -qf apt
    alias eix 'apt search'
end

# shortcut for pbcopy/pbpaste/wl-copy/wl-paste
# type -qf is fish built-in as equivalence to which >/dev/null
if type -qf wl-copy; and set -q WAYLAND_DISPLAY
    alias pbcopy wl-copy
    alias pbpaste wl-paste
else if type -qf xclip; and set -q DISPLAY
    alias pbcopy 'xclip -sel clip'
    alias pbpaste 'xclip -sel clip -o'
    alias wl-copy 'xclip -sel clip'
    alias wl-paste 'xclip -sel clip -o'
end

# auto-expand abbreviations for faster typing
# to allow expansion after "sudo"
abbr --add sys --position anywhere -- systemctl
abbr --add sysu --position anywhere -- systemctl --user
abbr --add j --position anywhere -- journalctl --since=\"14 days ago\" -u
abbr --add ju --position anywhere -- journalctl --since=\"14 days ago\" --user -u
abbr --add jf --position anywhere -- journalctl -fu
abbr --add jfu --position anywhere -- journalctl --user -fu

# faster way to type /dev/null
abbr --add nul --position anywhere -- /dev/null

alias yt-dlp-a2c "yt-dlp --output '%(title).200B%(title.201B&…|)s.%(ext)s' --no-mtime --min-filesize 200k --downloader aria2c"
alias yt-dlp-audio 'yt-dlp -x'
