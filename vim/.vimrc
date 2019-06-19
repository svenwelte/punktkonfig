" vim:fenc=utf-8

filetype off

" Bootstrap plug:
" curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim


call plug#begin('~/.vim/plugged')
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'tpope/vim-sensible'
Plug 'Lokaltog/vim-powerline'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-haml'
Plug 'tpope/vim-markdown'
Plug 'scrooloose/syntastic'
Plug 'dfxyz/CandyPaper.vim'
Plug 'jremmen/vim-ripgrep'
Plug 'vim-ruby/vim-ruby'
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree'
Plug 'kana/vim-textobj-user'
Plug 'nelstrom/vim-textobj-rubyblock'
Plug 'godlygeek/tabular'
Plug 'chrismetcalf/vim-yankring'
Plug 'pangloss/vim-javascript'
Plug 'vim-scripts/smartword'
Plug 'guns/xterm-color-table.vim'
Plug 'fcpg/vim-orbital'
Plug 'vim-scripts/bufkill.vim'
Plug 'editorconfig/editorconfig-vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'othree/html5.vim'
Plug 'leafgarland/typescript-vim'
Plug 'Quramy/tsuquyomi'
Plug 'justinmk/vim-sneak'
call plug#end()


" leader to ,
let maplocalleader = ","
let mapleader = ','


set hidden

" escape sequences
map <Esc>OH <Home>
map! <Esc>OH <Home>
map <Esc>OF <End>
map! <Esc>OF <End>

" smartwords
"map w  <Plug>(smartword-w)
"map b  <Plug>(smartword-b)
map e  <Plug>(smartword-e)
"map ge  <Plug>(smartword-ge)

" window mapping
noremap <silent> <Leader>h :wincmd h<CR>
noremap <silent> <Leader>j :wincmd j<CR>
noremap <silent> <Leader>k :wincmd k<CR>
noremap <silent> <Leader>l :wincmd l<CR>
noremap <silent> <Leader>q :close<CR>

" special mappings
noremap <silent> <Leader>y :YRShow<CR>
noremap <Leader>g :Rg<Space>
vnoremap <Leader>g y:Rg <C-R>*
noremap <silent> <Leader>d :BD<CR>

" old surround behaviour
vmap s S

" command line optimizations
cmap <C-A> <HOME>

" reselect just pasted text
nnoremap <leader>v V`]

" leave insert mode on:
imap jj <esc>
imap kk <esc>
imap hh <esc>
imap jk <esc>
imap kj <esc>


" shortcuts
nnoremap <silent> <Leader>f  :GFiles <CR>
nnoremap <silent> <Leader>F  :Files <CR>
nnoremap <silent> <Leader>b  :Buffers <CR>
noremap <silent> ,cw :cclose<CR>
nnoremap <silent> <C-S-j> :cnext<CR>
nnoremap <silent> <C-S-k> :cprev<CR>
map <F2> :e ~/.vimrc<CR>
nnoremap <silent> <Space> :nohlsearch<Bar>:echo<CR>


" general setup
filetype plugin indent on  " Automatically detect file types.
set ic                     " ignore case for search
set nocompatible           " We're running Vim, not Vi!

set cf                 " Enable error files & error jumping.
set clipboard=unnamed
"set clipboard=unnamedplus  " Yanks go on X11-clipboard instead.
set autowrite          " Writes on make/shell commands
set nu                 " Line numbers on
set wrap               " Line wrapping off
set timeoutlen=550     " Time to wait after ESC (default causes an annoying delay)
set guioptions=egm     " remove menu, scrollbars

set directory=~/tmp    " no swapfiles in current dir

" Formatting (some of these are for coding in C and C++)
set ts=2  " Tabs are 2 spaces
set shiftwidth=2  " Tabs under smart indent
set cinoptions=:1,p0,t0
set cinwords=if,else,while,do,for,switch,case
set cindent
set smartindent
set expandtab
set shortmess=at

" display options
"set showmatch  " Show matching brackets.
set mat=5  " Bracket blinking.
set list
set listchars=tab:▸\ ,trail:·,nbsp:·
"set cursorline

" 'sound' options
set noerrorbells      " no noise.
set vb                " use visual bell
set t_vb=
au GUIEnter * set t_vb= " forcing t_vb

set hlsearch      " always highlight search results

set gdefault      " always do s/.../.../g

" wildmenu config
set wildmode=list:longest,full
set wildignore+=tmp/**,*.scssc,*.sassc,*.class,log/**

" use relative adressing
"set relativenumber

" gui stuff
set t_Co=256
" colorscheme wombat256mod
set background=dark
colorscheme orbital
let g:airline_theme='distinguished'
"CandyPaper

highlight LineNr term=underline ctermfg=008 ctermbg=233 guifg=lightgray guibg=black

function! g:CodeReview()
  let g:lucius_style="light"
  colorscheme lucius
:endfunction
command! CodeReview :call g:CodeReview()

" command-t config
let g:CommandTMatchWindowAtTop=1

" remove whitespace
function! TrimWhiteSpace()
  %s/\s*$//
  ''
:endfunction
map <F5> :call TrimWhiteSpace()<CR>

set autowriteall


" diffmode
if &diff
  map <leader>1 :diffget LOCAL<CR>
  map <leader>2 :diffget BASE<CR>
  map <leader>3 :diffget REMOTE<CR>
endif

" setup sneak
map f <Plug>Sneak_s
map F <Plug>Sneak_S
let g:sneak#label = 1
let g:sneak#use_ic_scs = 1


let NERDTreeShowHidden=1

