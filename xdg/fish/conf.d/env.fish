if test -d ~/.kube/config.d
    set -x KUBECONFIG (echo ~/.kube/config.d/* | string split ' ' | string join ':')
else
    set -x KUBECONFIG ~/.kube/config
end


set -x EDITOR vim
set -x PAGER less
