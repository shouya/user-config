function lt1 --wraps='ls -ht | head -n 1' --description 'alias lt1 ls -ht | head -n 1'
    ls -ht $argv | head -n 1
end
