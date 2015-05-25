if filereadable("$VIMRUNTIME/debian.vim")
  source $VIMRUNTIME/debian.vim
endif

set nocompatible              " always use vim extensions
set modelines=0               " modelines have security risks, 0 == disabled
set encoding=utf-8            " always utf8
behave xterm                  " behavior compatible with xterm
set visualbell                " no noise
set selectmode=mouse          " enable mouse select
set noexpandtab               " don't expand tabs
set shiftwidth=3              " indent at 3 spaces
set tabstop=3                 " tab characters tab by 8
set textwidth=0               " use width of window
set wrapmargin=0              " wrap chars before end line
set autoindent                " automatically indent
"set copyindent                " .. and copy indent structure of existing line
set writebackup               " backup current file
set backup                    " delete old backup file
set grepprg=git\ grep          " use git grep for grepping
set makeef=~/.cache/vim/vim##.err   " file to use for make errors
set errorformat^=%-GIn\ file\ included\ from\ %.%#
set hidden                    " allow hidden bufs to be modified w/o save required
set ignorecase                " ignore case in searches
set smartcase                 " .. unless search term has upper case
set noincsearch               " don't search incrementally
set hlsearch                  " highlight search terms (nohls temp. turns off)
set autoread                  " read changed files w/o prompt
set autowrite                 " write changed files under certain circumstances
set tildeop                   " allow ~ to work as operator
set background=light          " use colors that look good on a light background
set shell=/bin/bash           " use bash for shelling out
set shellslash                " use forward slash for expanding file names
set cmdheight=3               " 3 line command area
set shortmess=at              " shorten displayed text
set laststatus=2              " show status in last window
set scrolloff=5               " minimum # lines to keep above and below cursor
set mousemodel=popup          " right mouse clicks pops up a menu
set formatoptions+=n          " recognize lists when formatting text
set formatoptions+=q          " recognize comments when formatting text
set formatoptions+=r          " automatically insert comment leader after <enter>
set formatoptions+=2          " for text, use indent of second line for remaining lines
set noshowmatch               " don't show matching brackets
set nostartofline             " keep current column if possible
set undofile                  " use persistant undo
set fillchars+=vert:\         " use black for vertical split
syntax on                     " enable syntax highlighting
set wildmenu                  " enhanced command line completion (use TAB)
set wildmode=list:longest,list:full                                  " what to do in wild mode
set wildignore+=*/.git/*,*/.hg/*,*/.dep/*,*/.svn/*,*.o,*.a           " file patterns to ignore in wild mode
set sessionoptions+=winpos,resize,slash,globals,localoptions         " session information saved, other than the default
set listchars=nbsp:·,tab:▹\    " nbsp as dot, tabs as arrow
set list                       " show special characters as defined in listchars
hi SpecialKey ctermfg=7 guifg=LightGray  " colors for listchars
"set autochdir                  " change to directory of current buffer
filetype plugin indent on      " turn on filetype plugins and indentation
set guioptions-=T              " remove the mostly useless toolbar
set spelllang=en_us            " set spelling language
set nospell                    " turn spell off by default

" Except for ~/.vimrc, these make vim XDG compatible 
set backupdir=~/.cache/vim,~/,/tmp   " dirs to use for backup files
set undodir=~/.cache/vim,~/,/tmp     " dirs to use for undo files
set directory=~/.cache/vim//         " where to put .swp files (// makes unique name)
set viminfo+=n~/.cache/vim/viminfo   " where to put viminfo
set runtimepath=~/.config/vim,~/.config/vim/after,$VIM,$VIMRUNTIME


" fix backspace under cygwin
if &term == "rxvt"
   set t_kb=
   fixdel
endif

" 256 color support if possible
if $TERM == "xterm-256color" || $TERM == "screen-256color" || $COLORTERM == "gnome-terminal"
  set t_Co=256
endif

" set font for gvim
if has("macunix")
  set guifont=DejaVu_Sans_Mono:h12
elseif has("unix")
  set guifont=DejaVu\ Sans\ Mono\ 10
else
  set guifont=Monospace\ 8
endif


"""""""""""""""""""""" Plugin Options """""""""""""""""""""""
" set up pathogen
runtime bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect()
call pathogen#helptags()

" show spacing errors in various languages
let c_space_errors=1
let python_space_error_highlight = 1
let ruby_space_errors=1 

" Options for bufmru
let g:bufmru_switchkey = "<CR>"

" Options for tagbar
let g:tagbar_compact = 1
let g:tagbar_singleclick = 1



"Options for NERDTree
let NERDTreeDirArrows=1
let NERDTreeIgnore=['.o$[[file]]','.a$[[file]]','autom4te.cache','Makefile.in$','Makefile$']

" Options for ctrlp
let g:ctrlp_user_command = { 'types': {
			\ 1: ['.git', 'cd %s && git ls-files'],
			\ 2: ['.hg', 'hg --cwd %s locate -I .'],
			\ },
			\ 'fallback': 'find %s -type f' }
let g:ctrlp_match_window = 'order:ttb,bottom'

" ignore lines that don't match any patterns defined for gcc
let g:compiler_gcc_ignore_unmatched_lines=1

" keep paren matching from loading, too slow on mac
if has("macunix")
  let loaded_matchparen=1
endif

" Options for buffer browser
let g:bufExplorerSplitVertical='v'       " Split vertically.
let g:bufExplorerUseCurrentWindow=1          " Open using current window.

" A.vim
let g:alternateExtensions_m = "h"
let g:alternateExtensions_mm = "h"
let g:alternateExtensions_h = "c,cpp,cc,cxx,m,mm"

" turn off netrw
let g:loaded_netrw       = 1
let g:loaded_netrwPlugin = 1

" turn off c curly brace errors so c++0x lambdas aren't bleeding red
let c_no_curly_error = 1

" plantuml
let g:plantuml_executable_script = 'java -jar ~/local/share/java/plantuml.jar'

" airline
let g:airline_left_sep=' '
let g:airline_right_sep=' '
let g:airline_detect_modified=1
let g:airline#extensions#whitespace#mixed_indent_algo = 2


"""""""""""""""""""""""" Mappings """""""""""""""""""""""""""

let mapleader="\<Space>"               " set character for <leader> sequences

" switch to previous buffer
nnoremap <TAB> 

" make j,k traverse screen lines
nnoremap j gj
nnoremap k gk

" Let . apply to all lines in visual mode 
vnoremap . :norm.<CR>

" scroll window instead of cursor
nnoremap <C-J> <C-e>j
nnoremap <C-K> <C-y>k

" turn off search highlighting
nnoremap <leader>/ :nohlsearch<CR>

" toggle relative numbers
nnoremap <leader># :set relativenumber!<CR>:set number!<CR>

"remove trailing spaces
nnoremap <leader>rs :%s/\s*$//<CR>:nohlsearch<CR>

" Quickly edit/reload the vimrc file
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>rv :so $MYVIMRC<CR>

" ctrl-p searches
nmap <leader>b :CtrlPBuffer<CR>
nmap <leader>f :CtrlP<CR>
nmap <leader>r :CtrlPMRU<CR>

" fugitive mappings
nnoremap <leader>gb :Gblame<CR>

" Define clipboard and primary register aliases
" for easier typing (compose with y,d or p)
nnoremap <leader>c "+
nnoremap <leader>m "*

" Next older, next newer jump
" Mnemonic ->    g; g, are next older, next newer change
nnoremap <leader>; <C-O>
nnoremap <leader>, <C-I>

" Increment/Decrement number
nnoremap <leader>= <C-A>
nnoremap <leader>- <C-X>

" Turn spell check on and off
nnoremap <leader>ss :set spell!<CR>

" fix spelling error
nnoremap <leader>sf 1z=

" Easier window commands
nnoremap <leader>w <C-W>

" visual select last paste
nnoremap <leader>v V`]

" write file using sudo
cnoremap w!! w !sudo tee % >/dev/null

map <C-X> :BD<CR>
map <C-F12> :call Print()<CR>

" Next, prev tag
map <F9> :tn<CR>
map <S-F9> :tp<CR>

" Next, prev error
map <F8> :cn<CR>
map <S-F8> :cp<CR>

" run make
map <F7> :make<CR>

" Look up tag
map <F6> <C-]>

" Alternate file
map <F5> :A<CR>
map <S-F5> :AN<CR>

" Various plugin windows
map <F4> :NERDTreeToggle<CR>
map <F3> :BufExplorer<CR>
map <F2> :TagbarToggle<CR>


"""""""""""""""""""""" AutoCommands """"""""""""""""""""""""""

if has("autocmd")

augroup vimrc
	autocmd!

	autocmd BufRead,BufNewFile *.ino set filetype=cpp     " arduino files
	autocmd FileType cpp set comments=:///,://      " handle doxygen comments better
	autocmd FileType cpp,c set list                   " always show trailing spaces
	autocmd SwapExists * let v:swapchoice = "o"       " always open ro when swap file exists
	autocmd VimLeavePre * call Resave_session()       " save current session if previously done
	autocmd FocusLost * :wa                           " save all buffers on focus lost

	" executing current file
	"following interferes with normal make
	"autocmd FileType cpp let &l:makeprg="g++ -Wall -Wextra -std=c++11 ". expand("%") . " && ./a.out"
	autocmd FileType ruby let &l:makeprg="ruby ".expand("%")
	autocmd FileType lua let &l:makeprg="lua ".expand("%")
	autocmd FileType python let &l:makeprg="python ".expand("%")
	autocmd FileType sql let &l:makeprg="sqlite3 ".expand("%")
	autocmd FileType ruby let &l:makeprg="ruby ".expand("%")
	autocmd FileType bash let &l:makeprg="bash ".expand("%")
	autocmd FileType sh let &l:makeprg="bash ".expand("%")
	autocmd Filetype plantuml let &l:makeprg=g:plantuml_executable_script." ".fnameescape(expand("%"))

	" tabstops
	autocmd FileType python setl expandtab shiftwidth=4 softtabstop=4

	" possibly helps performance issue due to syntax highlighting
	if version >= 702
	  autocmd BufWinLeave * call clearmatches()
	endif
augroup END


endif  " autocmds

"""""""""""""""""""""""" Functions """"""""""""""""""""""""""

" Save session if session.vim exists in cwd.
if 0 == exists("*Resave_session")  " can't load this while it is running
function! Resave_session()
   let s:vim_session_path = getcwd() . "/session.vim"
   if filereadable(s:vim_session_path)
   	execute ":mks! " . s:vim_session_path
   endif
endfunction
endif

function! Print()
  let path = bufname("%")
  let fn = substitute(path, " ", "\\\\ ", "")
  execute ":!a2ps --columns=2 --portrait -Phplj --underlay='UNCONFIRMED' " . fn
endfunction
