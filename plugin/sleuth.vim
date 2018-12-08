" sleuth.vim - Heuristically set buffer options

if exists('g:loaded_sleuth') || v:version < 700 || &cp
  finish
endif
let g:loaded_sleuth = 1

let g:sleuth_neighbor_limit = get(g:, 'sleuth_neighbor_limit', 20)
let g:sleuth_line_guess_limit = get(g:, 'sleuth_line_guess_limit', 1024)

" Finds the most common space indentation size. Given a dict where:
"   key: Difference in leading spaces between sequential lines.
"   value: The number of times the key/delta above has appeared.
" Returns the indentation length of the most common key
function! s:most_common_indent(delta_sizes) abort
  let max_indent = 0
  let max_count = 0
  for [indent_size, indent_count] in items(a:delta_sizes)
    if indent_count > max_count
      let max_indent = indent_size
      let max_count = indent_count
    endif
  endfor

  return max_indent
endfunction

" Guess the most likely indentation scheme based on the provided buffer lines.
" Returns an 'options' array of vim indentation settings.
function! s:guess(lines) abort
  let ccomment = 0
  let podcomment = 0
  let triplequote = 0
  let backtick = 0
  let xmlcomment = 0

  let heuristics = {'spaces': 0, 'sizes': {}, 'hard': 0}
  let last_leading_spaces = 0

  for line in a:lines
    " Whitespace-only lines are ignored
    if !len(line) || line =~# '^\s*$'
      continue
    endif

    " Common multi-line comments are ignored (Python, C, XML, etc)
    if line =~# '^\s*/\*'
      let ccomment = 1
    endif
    if ccomment
      if line =~# '\*/'
        let ccomment = 0
      endif
      continue
    endif

    if line =~# '^=\w'
      let podcomment = 1
    endif
    if podcomment
      if line =~# '^=\%(end\|cut\)\>'
        let podcomment = 0
      endif
      continue
    endif

    if triplequote
      if line =~# '^[^"]*"""[^"]*$'
        let triplequote = 0
      endif
      continue
    elseif line =~# '^[^"]*"""[^"]*$'
      let triplequote = 1
    endif

    if backtick
      if line =~# '^[^`]*`[^`]*$'
        let backtick = 0
      endif
      continue
    elseif line =~# '^[^`]*`[^`]*$'
      let backtick = 1
    endif

    if line =~# '^\s*<\!--'
      let xmlcomment = 1
    endif
    if xmlcomment
      if line =~# '-->'
        let xmlcomment = 0
      endif
      continue
    endif

    " Computes total number of lines with leading tabs
    if line =~# '^\t'
      let heuristics.hard += 1
      continue
    endif

    " Computes total number of lines with leading spaces
    if line =~# '^ '
      let heuristics.spaces += 1

      " Determine how many spaces constitute an indent-level. Compute the most common
      " deltas in the quantity of leading spaces between sequential lines. The most
      " frequent length is shiftwidth. Inspired by 'Comparing Lines' described at:
      " https://medium.com/firefox-developer-tools/detecting-code-indentation-eff3ed0fb56b
      if line =~# '^ \+\t'
        " Tab size not known, so mixed indent lines are not useful data
        continue
      endif

      let leading_spaces = len(matchstr(line, '^ *'))
      let indent_size = abs(leading_spaces - last_leading_spaces)
      if indent_size > 1
        let heuristics.sizes[indent_size] = get(heuristics.sizes, indent_size, 0) + 1
        let last_leading_spaces = leading_spaces
      endif
    endif
  endfor

  " No heuristics, unable to determine indentation scheme
  if !(heuristics.hard) && !(heuristics.spaces)
    return {}
  endif

  " More tabs than spaces, use tab indentation
  if heuristics.hard > heuristics.spaces
    return {'expandtab': 0, 'shiftwidth': &tabstop}
  endif

  " More spaces than tabs, use space indentation
  if len(heuristics.sizes) > 0
    let indent_size = s:most_common_indent(heuristics.sizes)
    return {'expandtab': 1, 'shiftwidth': indent_size}
  endif

  " Unable to determine indentation scheme
  return {}
endfunction

" Given a vim 'syntax' string, find all associated file types.
" Returns a list of buffer name glob patterns.
function! s:patterns_for(type) abort
  if a:type ==# ''
    return []
  endif

  redir => capture
  silent autocmd BufRead
  redir END

  " Search for 'setf {type}', match all leading '*.{extension}'
  let patterns = []
  let setf_regex  = '^\s*\(\S\+\).*setf.' . a:type
  for line in split(capture, '\n')
    let match = matchlist(line, setf_regex)
    if !empty(match)
      call add(patterns, match[1])
    endif
  endfor

  " Sleuth will not accept patterns containing '/', remove
  call filter(patterns, 'v:val !~# "/"')

  return patterns
endfunction

" Apply 'options' if it contains valid indentation settings
function! s:apply_if_ready(options) abort
  if !has_key(a:options, 'expandtab') || !has_key(a:options, 'shiftwidth')
    return 0
  endif

  for [option, value] in items(a:options)
    call setbufvar('', '&'.option, value)
  endfor
  return 1
endfunction

" Determine indentation for the current buffer based on the settings of its neighbors.
"   Looks at the current value of 'filetype' for all associated file types. Moving from
"   the current directory to the root directory, 's:guess' is applied until a file is found
"   with valid indentation settings. Only 'g:sleuth_neighbor_limit' files are examined.
" Returns an 'options' array of vim indentation settings.
function! s:detect_options_neighbors() abort
  let file_limit = g:sleuth_neighbor_limit
  if file_limit <= 0
    return {}
  endif

  let patterns = s:patterns_for(&filetype)

  " Without file type associations, nothing can be done
  if len(patterns) == 0
    return {}
  endif

  let options = {}
  let current_dir = expand('%:p:h')
  while isdirectory(current_dir) && current_dir !=# fnamemodify(current_dir, ':h') && file_limit > 0
    for pattern in patterns
      for neighbor in split(glob(current_dir.'/'.pattern), '\n')[0:7]
        if neighbor ==# expand('%:p')
          continue
        endif
        if filereadable(neighbor)
          call extend(options, s:guess(readfile(neighbor, '', 256)), 'keep')
          let file_limit -= 1
        endif
        if len(options) > 0
          let b:sleuth_culprit = neighbor
          return options
        endif
        if file_limit <= 0
          break
        endif
      endfor
      if file_limit <= 0
        break
      endif
    endfor
    let current_dir = fnamemodify(current_dir, ':h')
  endwhile

  return {}
endfunction

function! s:detect() abort
  " Vim provides sane defaults for certain buffers
  if &buftype ==# 'help' || &buftype ==# 'quickfix'
    return
  endif

  " Examine current buffer for indentation scheme
  let options = s:guess(getline(1, g:sleuth_line_guess_limit))
  if s:apply_if_ready(options)
    return
  endif

  " Examine surrounding files for indentation scheme
  let options = s:detect_options_neighbors()
  if s:apply_if_ready(options)
    return
  endif
endfunction

" Sleuth global settings defaults, allow the user to override
if get(g:, 'sleuth_default_options', 1) && !exists('g:sleuth_did_defaults')
  setglobal smarttab
  filetype indent on
  setbufvar('', '&tabstop', 8)
  let g:sleuth_did_defaults = 1
endif

function! SleuthIndicator() abort
  let sw = &shiftwidth ? &shiftwidth : &tabstop
  if &expandtab
    return 'sw='.sw
  elseif &tabstop == sw
    return 'ts='.&tabstop
  else
    return 'sw='.sw.',ts='.&tabstop
  endif
endfunction

augroup sleuth
  autocmd!
  autocmd FileType *
        \ if get(b:, 'sleuth_automatic', get(g:, 'sleuth_automatic', 1))
        \ | call s:detect() | endif
  autocmd User Flags call Hoist('buffer', 5, 'SleuthIndicator')
augroup END

command! -bar -bang Sleuth call s:detect()

" vim:set et sw=2:
