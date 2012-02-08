" vim:fenc=utf-8

filetype off

set rtp+=~/punktkonfig/bundle.vim/vundle/
call vundle#rc()

" Bootstrap vundle: git clone http://github.com/gmarik/vundle.git " ~/punktkonfig/bundle.vim/vundle
Bundle 'gmarik/vundle'

Bundle 'tpope/vim-rails.git'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-haml'
Bundle 'tpope/vim-rake'

Bundle 'mileszs/ack.vim'
Bundle 'scrooloose/syntastic'

Bundle 'vim-ruby/vim-ruby'
Bundle 'gaffneyc/vim-cdargs'
Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/nerdtree'

Bundle 'kana/vim-textobj-user'
Bundle 'nelstrom/vim-textobj-rubyblock'
Bundle 'edsono/vim-matchit'

Bundle 'godlygeek/tabular'
Bundle 'wincent/Command-T'
Bundle 'chrismetcalf/vim-yankring'
Bundle 'pangloss/vim-javascript'
Bundle 'vim-scripts/smartword'
Bundle 'guns/xterm-color-table.vim'

Bundle 'larssmit/vim-lucius'

" leader to ,
let maplocalleader = ","
let mapleader = ','

set hidden

" smartwords
map w  <Plug>(smartword-w)
map b  <Plug>(smartword-b)
map e  <Plug>(smartword-e)
map ge  <Plug>(smartword-ge)

" window mapping
noremap <silent> <Leader>h :wincmd h<CR>
noremap <silent> <Leader>j :wincmd j<CR>
noremap <silent> <Leader>k :wincmd k<CR>
noremap <silent> <Leader>l :wincmd l<CR>
noremap <silent> <Leader>q :close<CR>

" special mappings
noremap <silent> <Leader>y :YRShow<CR>
noremap <silent> <Leader>a :Ack<Space>
vnoremap <silent> <Leader>a y:Ack "<C-R>*"

" cdargs shortcut
cnoreabbrev cv Cdb

" reselect just pasted text
nnoremap <leader>v V`]

" leave insert mode on:
imap jj <esc>
imap kk <esc>
imap hh <esc>
imap jk <esc>
imap kj <esc>

" shortcuts
nnoremap <silent> <Leader>t  :CommandT <CR>
nnoremap <silent> <Leader>b  :CommandTBuffer <CR>
noremap <silent> ,cc :close<CR>
noremap <silent> ,cw :cclose<CR>
map <F2> :e ~/.vimrc<CR>
map <F3> :Cdb w<cr>:cd src<cr>
nnoremap <silent> <Space> :nohlsearch<Bar>:echo<CR>


" surround for jquery selectors
let g:surround_36 = "$('.\r')"

" general setup
filetype plugin indent on  " Automatically detect file types.
syntax enable
set ic             " ignore case for search
set nocompatible   " We're running Vim, not Vi!

set cf                 " Enable error files & error jumping.
set clipboard=unnamed  " Yanks go on clipboard instead.
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
set showmatch  " Show matching brackets.
set mat=5  " Bracket blinking.
set list
set listchars=tab:▸\ ,trail:·,nbsp:·
" set listchars+=eol:¬
set cursorline

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
set wildignore+=public/system/**,tmp/**,*.scssc,*.sassc,*.class,log/**,server/**,vendor/bundle/**

" use relative adressing
set relativenumber

" gui stuff
set t_Co=256
" colorscheme wombat256mod
set background=dark
colorscheme lucius
highlight LineNr ctermfg=008
highlight LineNr ctermbg=233

function! g:CodeReview()
  let g:lucius_style="light"
  colorscheme lucius
:endfunction
command! CodeReview :call g:CodeReview()

if has('gui_running')
  set guifont=Inconsolata:h12
  set guitablabel=%t
end

" command-t config
let g:CommandTMatchWindowAtTop=1

" remove whitespace
function! TrimWhiteSpace()
  %s/\s*$//
  ''
:endfunction
map <F5> :call TrimWhiteSpace()<CR>

" rails specific setup
let g:rails_ctags_arguments='--exclude=*.js --exclude=vendor --exclude=.rsync_cache'
set autowriteall

" clojure specific setup
let vimclojure#WantNailgun = 1
let vimclojure#ParenRainbow = 1
let vimclojure#DynamicHighlighting = 1
let vimclojure#SplitPos = "bottom"
let vimclojure#SplitSize = "10"

" lisp-words for clojure (should be project local)
autocmd FileType clojure setlocal lw+=defroutes,deftest,defelem,defhtml,with-group,form-to,fact
