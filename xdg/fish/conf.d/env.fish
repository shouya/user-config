if test -d ~/.kube/config.d
    set -x KUBECONFIG (echo ~/.kube/config.d/* | string split ' ' | string join ':')
else
    set -x KUBECONFIG ~/.kube/config
end

# pnpm home
set -gx PNPM_HOME "$HOME/.local/share/pnpm"

# [-a ""]: start emacs server if not running
set -x EDITOR 'emacsclient -a "" --tty'
set -x PAGER less
