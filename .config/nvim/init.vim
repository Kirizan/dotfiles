" sets tab key to insert two spaces insead of a tab character
set expandtab
set tabstop=2
set softtabstop=-1
set shiftwidth=0
set shiftround

" sets line numbers to be displayed
set nu
set rnu

" toggles relative and absolute line numbers base on insert mode
augroup numbertoggle
  autocmd!
  autocmd BufEnter,FocusGained,InsertLeave,WinEnter * if &nu && mode() != "i" | set rnu   | endif
  autocmd BufLeave,FocusLost,InsertEnter,WinLeave   * if &nu                  | set nornu | endif
augroup END


" moves the viminfo file into the ~/.vim directory
if has("gui_macvim")
  " neovim and macvim viminfo files are incompatible, so this sets a different
  " viminfo file for macvim
  set viminfo+=n~/.config/vim/macviminfo
else
  set viminfo+=n~/.config/vim/viminfo
endif


" enables status line
set laststatus=2

" adds columns to the status line
set statusline+=col:\ %c,

" sets fontsize
set guifont=Menlo\ Regular:h18

" sets scroll by viewing lines
map k gk
map j gj
map <Up> gk
map <Down> gj

" sets cursor to change between insert and normal mode
let &t_SI = "\<Esc>]50;CursorShape=1\x7"
let &t_SR = "\<Esc>]50;CursorShape=2\x7"
let &t_EI = "\<Esc>]50;CursorShape=0\x7"

" sets cursor timeout
set ttimeout
set ttimeoutlen=1
set ttyfast

" sets tab key to insert tabs
nnoremap <Tab> >>_
nnoremap <S-Tab> <<_
inoremap <S-Tab> <C-D>
vnoremap <Tab> >gv
vnoremap <S-Tab> <gv

" sets system clipboard as default
nnoremap y "+y
vnoremap y "+y
nnoremap p "+p
vnoremap p "+p
nnoremap P "+P
vnoremap P "+P
nnoremap x "+x
vnoremap x "+x


" sets word wrap to only wrap on spaces
set wrap linebreak breakat&vim

" sets r# to create has box
nnoremap r#<space> 20I#<Esc>5a<Space><Esc>5A<Space><Esc>20A#<Esc>yy2P<C-V>$r#2j.

" sets :C to clear search
command C let @/=""
