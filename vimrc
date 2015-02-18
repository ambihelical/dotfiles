if filereadable("$VIMRUNTIME/debian.vim")
  source $VIMRUNTIME/debian.vim
endif

set nocompatible              " always use vim extensions
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
"set makeprg=xcodebuild\ -activeconfiguration       " command to use for make
"set makeprg=run_make           " command to use for make
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
set scrolloff=5               " minimum # lines to keep above and below cursor
set mousemodel=popup          " right mouse clicks pops up a menu
set formatoptions+=n          " recognize lists when formatting text
set noshowmatch               " don't show matching brackets
set nostartofline             " keep current column if possible
syntax on                     " enable syntax highlighting
set wildmenu                  " enhanced command line completion (use TAB)
set wildmode=list:longest,list:full " what to do in wild mode
set wildignore+=*/.git/*,*/.hg/*,*/.dep/*,*.o,*.a           " file patterns to ignore in wild mode
" session information saved
set sessionoptions=winpos,resize,winsize,slash,folds,globals,tabpages,localoptions,buffers
"let mapleader=','             " use , for <leader> sequences
set listchars=trail:·,tab:▹\ ,nbsp:·  " show trailing & nb spaces as a dot, tabs as arrow
set list                       " show special characters as defined in listchars
hi SpecialKey ctermfg=7 guifg=LightGray  " colors for listchars nbsp,tab,trail
"set autochdir                  " change to directory of current buffer
filetype plugin indent on      " turn on filetype plugins and indentation
set guioptions-=T              " remove the mostly useless toolbar

" Except for ~/.vimrc, these make vim XDG compatible 
set backupdir=~/.cache/vim,~/,/tmp   " dirs to use for backup files
set directory=~/.cache/vim//         " where to put .swp files (// makes unique name)
set viminfo+=n~/.cache/vim/viminfo   " where to put viminfo
set runtimepath=~/.config/vim,~/.config/vim/after,$VIM,$VIMRUNTIME

" fix backspace under cygwin
if &term == "rxvt"
   set t_kb=
   fixdel
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

"Options for NERDTree
let NERDTreeDirArrows=1
let NERDTreeIgnore=['.o$[[file]]','.a$[[file]]','autom4te.cache','Makefile.in$','Makefile$']

" Options for ctrlp
let g:ctrlp_map = '<c-p>'
let g:ctrlp_working_path_mode = 2
let g:ctrlp_match_window_reversed = 0
let g:ctrlp_cache_dir = $HOME.'/.cache/ctrlp'
let g:ctrlp_dotfiles = 0
let g:ctrlp_user_command = ['.git/', 'cd %s && git ls-files -c']

" ignore lines that don't match any patterns defined for gcc
let g:compiler_gcc_ignore_unmatched_lines=1

" keep paren matching from loading, too slow on mac
if has("macunix")
  let loaded_matchparen=1
endif


" Options for tag list add-on
let Tlist_Show_One_File=1
"let Tlist_Inc_Winwidth=0
let Tlist_Use_SingleClick=1
"let Tlist_GainFocus_On_ToggleOpen=1
"let Tlist_Close_On_Select=1
let Tlist_WinWidth=20
let Tlist_Auto_Open=0
let Tlist_Use_Right_Window=1
let Tlist_Compact_Format=1
let Tlist_Display_Tag_Scope=0
let Tlist_Enable_Fold_Column=0
"let Tlist_Display_Prototype=1
"let Tlist_File_Fold_Auto_Close=1
let Tlist_Exit_OnlyWindow = 1

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

"""""""""""""""""""""""" Mappings """""""""""""""""""""""""""

map __ 
map _q a"<ESC>'ai"<ESC>
map _h :nohlsearch<CR>
map _/ :'a,.s/^[^\/]/\/\//g<CR>
map _? :'a,.s/^\/\///g<CR>
map _s :%s/\s*$//<CR>:nohlsearch<CR>
map  }
map <C-J> <C-e>j
map <C-K> <C-y>k
map <C-X> :bdelete<CR>
map <C-TAB> :tabn<CR>
map <C-S-TAB> :tabp<CR>
map <C-T> :tabnew<CR>
map <C-F12> :call Print()<CR>
map <F11> :ptn<CR>
map <S-F11> :ptp<CR>
map <F10> :bnext<CR>
map <S-F10> :bprevious<CR>
map <F9> <CR>
map <S-F9> W<CR>
map <F8> :cn<CR>
map <S-F8> :cp<CR>
" S-F7 mapped differently for each filetype
map <F7> :make<CR>
map <F6> yw:call Tag()<CR>
map <F6> yw:call Tag()<CR>
map <S-F6> }P
map <F5> :A<CR>
map <S-F5> :AN<CR>
map <f4> :NERDTreeToggle<CR>
map <F3> :BufExplorer<CR>
map <F2> :TlistToggle<CR>
map <S-F1> :help cheats<CR>

" This is needed to handle windows bizaarness during input
"imap  <BS>


"""""""""""""""""""""" AutoCommands """"""""""""""""""""""""""

if has("autocmd")

augroup vimrc
	autocmd!

	autocmd BufRead,BufNewFile *.ino set filetype=cpp     " arduino files
	autocmd FileType cpp set comments=:///,://      " handle doxygen comments better
	autocmd FileType cpp,c set list                   " always show trailing spaces
"	autocmd VimLeavePre * call Save_session()         " save current session
"	autocmd VimEnter * call Load_session()         " load previously saved session

	" executing current file
	autocmd FileType cpp let &l:makeprg="g++ -Wall -Wextra -std=c++11 ". expand("%") . " && ./a.out"
	autocmd FileType ruby let &l:makeprg="ruby ".expand("%")
	autocmd FileType lua let &l:makeprg="lua ".expand("%")
	autocmd FileType python let &l:makeprg="python ".expand("%")
	autocmd FileType sql let &l:makeprg="sqlite3 ".expand("%")
	autocmd FileType ruby let &l:makeprg="ruby ".expand("%")
	autocmd Filetype plantuml let &l:makeprg=g:plantuml_executable_script." ".fnameescape(expand("%"))

	" tabstops
	autocmd FileType python setl expandtab shiftwidth=4 softtabstop=4
augroup END

endif  " autocmds

"""""""""""""""""""""""" Functions """"""""""""""""""""""""""

" Load previous session, also load any files specified
"if 0 == exists("*Load_session")  " can't load this while it is running
"function! Load_session()
"   let s:vim_session_path = $HOME . "/.cache/vim/sessions" . getcwd()
"   if (argc() == 0) && filereadable(s:vim_session_path . "/session")
"     execute ":source " . s:vim_session_path . "/session"
"   endif
"endfunction
"endif
"
"if 0 == exists("*Save_session")  " can't load this while it is running
"function! Save_session()
"   if filewritable(s:vim_session_path) == 0
"     call mkdir(s:vim_session_path, "p")
"   endif
"   execute ":set viminfo='20,<50,/25,h,n" . s:vim_session_path . "/viminfo"
"   execute ":mks! " . s:vim_session_path . "/session"
"endfunction
"endif

function! Tag()
    execute ":tag " . @"
endfunction

function! Print()
  let path = bufname("%")
  let fn = substitute(path, " ", "\\\\ ", "")
  execute ":!a2ps --columns=2 --portrait -Phplj --underlay='UNCONFIRMED' " . fn
endfunction

