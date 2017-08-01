function! func#replace_symbol_in_function(symbol, replacement, confirm)
    let window = winsaveview()

    if &ft == 'python'
        normal [m
    else
        normal [[
    endif

    let start_line = line('.')

    if &ft == 'python'
        normal ]m
        let end_line = line('.')

        if end_line != line('$')
            let end_line = end_line - 1
        endif
    else
        normal ][
        let end_line = line('.')
    endif

    if end_line == start_line
        echoerr "Missing end of function ?"
        call winrestview(window)
        return
    endif

    let extra_flags = ''
    if a:confirm == '!'
        let extra_flags = extra_flags . 'c'
    endif

    execute ":" . start_line . "," . end_line . "s/\\<" . a:symbol . "\\>/" . a:replacement . "/gI" . extra_flags

    call winrestview(window)
endfunction
