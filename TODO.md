## Items to Look Into:

- How do I toggle the line-wrap gutter?
- Can I get ido-mode working with ':e' from evil-mode?
- Get bindings setup for CMD-+ / CMD-- to increase/decrease font size.
- Look into Sunrise Commander
- Look into a way to use org-mode for note-taking:
    - Maybe a ~/Notes directory, and some elisp to launch in the directory?
    - There was that HN (or Reddit /r/emacs) thread about some custom 'edit this file in this mode, etc' elisp.
        (defun todo-list ()
         (interactive)
         (make-frame '((name . "TODO")))
         (set-background-color "yellow")
         (switch-to-buffer (find-file "~/TODO"))
         (org-mode)
        )
        SOURCE: https://www.reddit.com/r/emacs/comments/2ne8v4/do_you_use_orgmode_when_your_teammates_dont/
- Look into usage of Emacs desktops

## Work To Do

- Get FIXME/XXX/TODO working.
    - There is fic-mode, but it's not very nice / dynamic.
    - It would be preferable if there was a way to get this working that was dynamic with the current theme.

## Notes

- To replace Vim's visual-line selection/':sort' command, do the same in evil-mode, but use ':sort-lines' command.
- 