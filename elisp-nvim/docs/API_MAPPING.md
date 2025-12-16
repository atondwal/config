# Emacs API → Neovim API Mapping

This document maps Emacs Lisp functions to their Neovim equivalents.

## Buffer Operations

| Emacs Function | Neovim Equivalent | Status | Notes |
|----------------|-------------------|--------|-------|
| `(buffer-string)` | `nvim_buf_get_lines()` | 🟡 Partial | Need to join lines |
| `(buffer-substring start end)` | `nvim_buf_get_text()` | ⚪ TODO | |
| `(insert text)` | `nvim_buf_set_lines()` | 🟡 Partial | At cursor position |
| `(insert-buffer-substring buf)` | `nvim_buf_get_lines()` + `nvim_buf_set_lines()` | ⚪ TODO | |
| `(erase-buffer)` | `nvim_buf_set_lines(0, -1, false, {})` | ⚪ TODO | |
| `(current-buffer)` | `nvim_get_current_buf()` | ✅ Done | |
| `(set-buffer buf)` | `nvim_set_current_buf()` | ⚪ TODO | |
| `(get-buffer name)` | `nvim_list_bufs()` + filter | ⚪ TODO | |
| `(get-buffer-create name)` | `nvim_create_buf()` | ⚪ TODO | |
| `(kill-buffer buf)` | `nvim_buf_delete()` | ⚪ TODO | |
| `(buffer-name)` | `nvim_buf_get_name()` | ✅ Done | |
| `(rename-buffer name)` | `nvim_buf_set_name()` | ✅ Done | |
| `(buffer-file-name)` | `nvim_buf_get_name()` | ✅ Done | Same in Neovim |
| `(buffer-modified-p)` | `nvim_buf_get_option('modified')` | ⚪ TODO | |
| `(set-buffer-modified-p)` | `nvim_buf_set_option('modified', val)` | ⚪ TODO | |

## Point and Mark

| Emacs Function | Neovim Equivalent | Status | Notes |
|----------------|-------------------|--------|-------|
| `(point)` | `nvim_win_get_cursor()` | ⚪ TODO | Returns [row, col] |
| `(point-min)` | `1` | ✅ Trivial | Always 1 in Neovim |
| `(point-max)` | `nvim_buf_line_count()` | ⚪ TODO | |
| `(goto-char pos)` | `nvim_win_set_cursor()` | ⚪ TODO | Convert to [row, col] |
| `(forward-char n)` | `nvim_win_set_cursor()` | ⚪ TODO | Calculate new pos |
| `(beginning-of-line)` | `nvim_win_set_cursor([row, 0])` | ⚪ TODO | |
| `(end-of-line)` | `nvim_win_set_cursor([row, col_max])` | ⚪ TODO | |
| `(line-beginning-position)` | Calculate from cursor | ⚪ TODO | |
| `(line-end-position)` | Calculate from cursor | ⚪ TODO | |

## Windows

| Emacs Function | Neovim Equivalent | Status | Notes |
|----------------|-------------------|--------|-------|
| `(selected-window)` | `nvim_get_current_win()` | ⚪ TODO | |
| `(select-window win)` | `nvim_set_current_win()` | ⚪ TODO | |
| `(window-buffer win)` | `nvim_win_get_buf()` | ⚪ TODO | |
| `(set-window-buffer win buf)` | `nvim_win_set_buf()` | ⚪ TODO | |
| `(split-window)` | `nvim_open_win()` or `:split` | ⚪ TODO | |
| `(delete-window)` | `nvim_win_close()` | ⚪ TODO | |
| `(other-window n)` | Iterate `nvim_list_wins()` | ⚪ TODO | |
| `(window-list)` | `nvim_list_wins()` | ⚪ TODO | |

## Display and Output

| Emacs Function | Neovim Equivalent | Status | Notes |
|----------------|-------------------|--------|-------|
| `(message fmt ...)` | `nvim_echo()` or `print()` | 🟡 Partial | |
| `(princ obj)` | `nvim_out_write()` | ⚪ TODO | |
| `(print obj)` | `nvim_out_write()` + newline | ⚪ TODO | |
| `(redisplay)` | `nvim_command('redraw')` | ⚪ TODO | |

## Files

| Emacs Function | Neovim Equivalent | Status | Notes |
|----------------|-------------------|--------|-------|
| `(find-file path)` | `nvim_command('edit ' .. path)` | ⚪ TODO | |
| `(find-file-noselect path)` | `nvim_create_buf()` + read file | ⚪ TODO | |
| `(save-buffer)` | `nvim_command('write')` | ⚪ TODO | |
| `(write-file path)` | `nvim_command('write ' .. path)` | ⚪ TODO | |
| `(file-exists-p path)` | Lua `vim.loop.fs_stat()` | ⚪ TODO | |
| `(directory-files dir)` | Lua `vim.fn.readdir()` | ⚪ TODO | |

## Text Properties / Overlays

| Emacs Function | Neovim Equivalent | Status | Notes |
|----------------|-------------------|--------|-------|
| `(make-overlay start end)` | `nvim_buf_set_extmark()` | ⚪ TODO | Very different API |
| `(overlay-put ov prop val)` | Extmark options | ⚪ TODO | |
| `(delete-overlay ov)` | `nvim_buf_del_extmark()` | ⚪ TODO | |
| `(put-text-property start end prop val)` | `nvim_buf_set_extmark()` | ⚪ TODO | |

## Keymaps

| Emacs Function | Neovim Equivalent | Status | Notes |
|----------------|-------------------|--------|-------|
| `(define-key map key cmd)` | `nvim_set_keymap()` | ⚪ TODO | Different key syntax |
| `(local-set-key key cmd)` | `nvim_buf_set_keymap()` | ⚪ TODO | |
| `(global-set-key key cmd)` | `nvim_set_keymap('n', ...)` | ⚪ TODO | |

## Major/Minor Modes

| Emacs Concept | Neovim Equivalent | Status | Notes |
|---------------|-------------------|--------|-------|
| Major modes | `filetype` | ⚪ TODO | Very different |
| Minor modes | Plugins | ⚪ TODO | No direct equiv |
| `(define-derived-mode ...)` | Custom filetype | ⚪ TODO | |

## Hooks

| Emacs Function | Neovim Equivalent | Status | Notes |
|----------------|-------------------|--------|-------|
| `(add-hook hook fn)` | `nvim_create_autocmd()` | ⚪ TODO | |
| `(remove-hook hook fn)` | `nvim_del_autocmd()` | ⚪ TODO | |
| `find-file-hook` | `BufRead` autocmd | ⚪ TODO | |
| `after-save-hook` | `BufWritePost` autocmd | ⚪ TODO | |

## Interactive / Minibuffer

| Emacs Function | Neovim Equivalent | Status | Notes |
|----------------|-------------------|--------|-------|
| `(read-string prompt)` | `vim.fn.input()` | ⚪ TODO | |
| `(read-file-name prompt)` | `vim.fn.input()` with completion | ⚪ TODO | |
| `(y-or-n-p prompt)` | `vim.fn.confirm()` | ⚪ TODO | |
| `(completing-read ...)` | Telescope / fzf-lua | ⚪ TODO | |

## Command Execution

| Emacs Function | Neovim Equivalent | Status | Notes |
|----------------|-------------------|--------|-------|
| `(call-interactively cmd)` | `nvim_command()` | ⚪ TODO | |
| `(execute-kbd-macro keys)` | `nvim_feedkeys()` | ⚪ TODO | |

## Dired-Specific

| Emacs Function | Neovim Equivalent | Status | Notes |
|----------------|-------------------|--------|-------|
| `(dired dirname)` | Custom buffer + formatter | 🟡 Partial | Proof of concept exists |
| `(dired-get-filename)` | Parse buffer line | ⚪ TODO | |
| `(dired-do-delete)` | Lua `vim.loop.fs_unlink()` | ⚪ TODO | |
| `(dired-do-rename)` | Lua `vim.loop.fs_rename()` | ⚪ TODO | |

## Legend

- ✅ **Done**: Implemented and working
- 🟡 **Partial**: Basic implementation exists, needs work
- ⚪ **TODO**: Not yet implemented
- 🔴 **Blocked**: Fundamental incompatibility

## Implementation Strategy

1. **Phase 1**: Core buffer/window operations (in progress)
2. **Phase 2**: Point/mark, cursor movement
3. **Phase 3**: File operations
4. **Phase 4**: Display (overlays → extmarks)
5. **Phase 5**: Interactive commands
6. **Phase 6**: Hooks and keymaps
7. **Phase 7**: Complex packages (dired, magit, org)

## Notes

### Key Differences

1. **Indexing**: Emacs uses 1-based, Neovim uses 0-based for lines (but 1-based for some APIs)
2. **Positions**: Emacs uses single integer, Neovim uses [row, col] tuples
3. **Buffers**: Emacs buffers are objects, Neovim uses integer handles
4. **Async**: Neovim is async-first, Emacs historically synchronous
5. **No direct overlay equivalent**: Must use extmarks creatively

### Challenges

- **Synchronization**: Keeping Emacs buffer state in sync with Neovim
- **Keybindings**: Emacs uses complex key chord system
- **Performance**: RPC overhead for every operation
- **Faces**: Emacs faces → Neovim highlight groups mapping
