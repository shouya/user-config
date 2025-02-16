" https://www.hillelwayne.com/post/intermediate-vim/
"

" Load old vim config
set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
source ~/.vimrc

" Initialize plug
"   See https://github.com/junegunn/vim-plug
call plug#begin("~/.local/share/nvim/plugged")

Plug 'elixir-editors/vim-elixir'
Plug 'sbdchd/neoformat'

call plug#end()

" show in realtime what changes your ex command should make
set inccommand=nosplit

" System clipboard integraion
"
" set clipboard+=unnamedplus
"
" Or use `"+y` or `"+p`


imap jk <Esc>
imap kj <Esc>

