"
"  Super simple script for folding comment and control blocks of Calypso's control files 
"
"    After opening control files on vim, enter the following command:
"        :source PATH/Folding_Calypso.vim (Set correct PATH for this file)
"
"    The basic commands for folding in vim are the following:
"      za – Toggles the current fold open or closed. – The most useful command to know of all of these.
"      zA – Same as 'za' except it toggles all folds beneath as well.
"          Since folds can be nested (such as with indent folding), 
"          this will toggle the state of all the folds underneath of it,
"          not just the current fold.
"      zc – Close the current fold.
"      zC – Same as 'zc', but closes folds nested underneath as well.
"      zo – Open the current fold.
"      zO – Same as 'zo', but opens folds nested underneath as well.

setlocal foldmethod=expr
setlocal foldnestmax=10
setlocal foldexpr=Fold_func(v:lnum)

set number
set foldcolumn=7
set fillchars=vert:\|
set foldtext=FoldTexttt()

func! Fold_func(lnum)
  let bw = 'begin'
  let this_line=getline(a:lnum)
  let prev_line=getline(a:lnum-1)
  let next_line=getline(a:lnum+1)
  
  if    (prev_line !~ '^\s*!' && this_line =~? '^\s*!,*' && next_line =~? '^\s*!')
    return "a1"
  elseif(prev_line=~? '^\s*!' && this_line=~? '^\s*!.*' && next_line!~ '^\s*!')
    return "s1"
  endif
  
  if this_line =~? '^\s*end'
    return "s1"
  elseif this_line =~? '^\s*array'
    return "a1"
  elseif this_line =~? '^\s*begin'
    return "a1"
  else
    return '='
  endif
endfunc


function! FoldTexttt()
  let this_line=getline(v:foldstart)
  let next_line=getline(v:foldstart+1)
  if(this_line =~? '^\s*!\s*$')
    let sub = substitute(next_line, '/\*\|\*/\|{{{\d\=', '', 'g')
  else
    let sub = substitute(this_line, '/\*\|\*/\|{{{\d\=', '', 'g')
  endif
  return v:folddashes .. sub
endfunction
