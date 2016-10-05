" vim:fenc=utf-8

" Disable Paren Matching for Speed
let g:loaded_matchparen = 1

filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

let unite_locate_command = 'mdfind -onlyin . .| sed "s_`pwd`/__g" | agrep -p %s'
let g:ctrlp_user_command = 'ack -f %s'
let g:ctrlp_use_caching = 1

autocmd Filetype go set makeprg=go\ build

" Bootstrap vundle:
" git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
"
Plugin 'VundleVim/Vundle.vim'
Plugin 'Lokaltog/vim-powerline'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-haml'
Plugin 'tpope/vim-markdown'
Plugin 'scrooloose/syntastic'
Plugin 'dfxyz/CandyPaper.vim'
Plugin 'mileszs/ack.vim'
Plugin 'vim-ruby/vim-ruby'
Plugin 'scrooloose/nerdcommenter'
Plugin 'scrooloose/nerdtree'
Plugin 'kana/vim-textobj-user'
Plugin 'nelstrom/vim-textobj-rubyblock'
Plugin 'edsono/vim-matchit'
Plugin 'godlygeek/tabular'
Plugin 'chrismetcalf/vim-yankring'
Plugin 'pangloss/vim-javascript'
Plugin 'vim-scripts/smartword'
Plugin 'guns/xterm-color-table.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'jonathanfilip/vim-lucius'
Plugin 'vim-scripts/bufkill.vim'
Plugin 'othree/html5.vim'
call vundle#end()

set ttymouse=xterm2

"Plugin 'vim-scripts/octave.vim'

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
"map e  <Plug>(smartword-e)
"map ge  <Plug>(smartword-ge)

" window mapping
noremap <silent> <Leader>h :wincmd h<CR>
noremap <silent> <Leader>j :wincmd j<CR>
noremap <silent> <Leader>k :wincmd k<CR>
noremap <silent> <Leader>l :wincmd l<CR>
noremap <silent> <Leader>q :close<CR>

" special mappings
noremap <silent> <Leader>y :YRShow<CR>
noremap <Leader>a :Ack<Space>
vnoremap <Leader>a y:Ack <C-R>*
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

" do not manage directories
let g:ctrlp_working_path_mode = 0
let g:ctrlp_max_height = 20


" let g:ctrlp_use_caching = 0
" shortcuts
nnoremap <silent> <Leader>t  :CtrlP <CR>
nnoremap <silent> <Leader>b  :CtrlPBuffer <CR>
noremap <silent> ,cw :cclose<CR>
nnoremap <silent> <C-S-j> :cnext<CR>
nnoremap <silent> <C-S-k> :cprev<CR>
map <F2> :e ~/.vimrc<CR>
nnoremap <silent> <Space> :nohlsearch<Bar>:echo<CR>


" surround for jquery selectors
let g:surround_36 = "$('.\r')"

" general setup
filetype plugin indent on  " Automatically detect file types.
syntax enable
set ic             " ignore case for search
set nocompatible   " We're running Vim, not Vi!

set cf                 " Enable error files & error jumping.
set clipboard=unnamed
"set clipboard=unnamedplus  " Yanks go on X11-clipboard instead.
set history=256        " Number of things to remember in history.
set autowrite          " Writes on make/shell commands
set autoread           " Read stuff that changed on disk
set ruler              " Ruler on
set nu                 " Line numbers on
set wrap               " Line wrapping off
set timeoutlen=550     " Time to wait after ESC (default causes an annoying delay)
set guioptions=egm     " remove menu, scrollbars

set directory=~/tmp    " no swapfiles in current dir

" Formatting (some of these are for coding in C and C++)
set ts=2  " Tabs are 2 spaces
set bs=2  " Backspace over everything in insert mode
set shiftwidth=2  " Tabs under smart indent
set nocp incsearch
set cinoptions=:1,p0,t0
set cinwords=if,else,while,do,for,switch,case
set formatoptions=tcqr
set cindent
"set autoindent
set smartindent
set smarttab
set expandtab
set shortmess=at

" display options
"set showmatch  " Show matching brackets.
set mat=5  " Bracket blinking.
set list
set listchars=tab:▸\ ,trail:·,nbsp:·
" set listchars+=eol:¬
"set cursorline

" 'sound' options
set noerrorbells      " no noise.
set vb                " use visual bell
set t_vb=
au GUIEnter * set t_vb= " forcing t_vb

set laststatus=2  " always show status line.
set hlsearch      " always highlight search results

set gdefault      " always do s/.../.../g

" wildmenu config
set wildmenu
set wildmode=list:longest,full
let g:ctrlp_custom_ignore = '\.git$\|\.hg$\|\.svn$'
set wildignore+=public/system/**,tmp/**,*.scssc,*.sassc,*.class,log/**,server/**,*/vendor/bundle/**,*/.rsync_cache/*,*/server/*

" use relative adressing
"set relativenumber

" gui stuff
set t_Co=256
" colorscheme wombat256mod
set background=dark
colorscheme CandyPaper

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

" rails specific setup
"let g:rails_ctags_arguments='--exclude=.rsync_cache'
set tags=TAGS
set autowriteall

" clojure specific setup
let vimclojure#WantNailgun = 1
let vimclojure#ParenRainbow = 1
let vimclojure#DynamicHighlighting = 1
let vimclojure#SplitPos = "bottom"
let vimclojure#SplitSize = "10"

" lisp-words for clojure (should be project local)
autocmd FileType clojure setlocal lw+=defroutes,deftest,defelem,defhtml,with-group,form-to,fact


" diffmode
if &diff
  map <leader>1 :diffget LOCAL<CR>
  map <leader>2 :diffget BASE<CR>
  map <leader>3 :diffget REMOTE<CR>
endif
