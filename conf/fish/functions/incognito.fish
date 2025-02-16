function incognito
    if not set -q fish_private_mode
        set -g fish_private_mode 1
        # starship prompt indicator
        set -gx incognito_mode I
        echo "Incognito mode enabled"
    else
        # -e: erase
        set -ge fish_private_mode
        # --global --unexport
        set -gu incognito_mode
        echo "Incognito mode disabled"
    end

    history delete --exact --case-sensitive incognito
end
