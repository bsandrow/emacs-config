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

[filtering text through a command shell in Emacs](http://stackoverflow.com/questions/206806/filtering-text-through-a-shell-command-in-emacs)

## Reference

- [markdown-mode](http://jblevins.org/projects/markdown-mode/)
- [Making Emacs Work for Me](http://zeekat.nl/articles/making-emacs-work-for-me.html)
- [Example: org-mode .emacs file](https://github.com/rodw/.dotfiles/blob/master/emacs/.rods-dot-emacs.org)

## Work To Do

- Get FIXME/XXX/TODO working.
    - There is fic-mode, but it's not very nice / dynamic.
    - It would be preferable if there was a way to get this working that was dynamic with the current theme.

- LustyExplorer: http://www.emacswiki.org/emacs/lusty-explorer.el
    - https://github.com/knevcher/emacs-config/blob/3fbcf52d9966db44a1e785d726e92fa76abf4df7/setup/lusty-setup.el
- https://gitorious.org/evil/pages/DevelopmentEx
- evil-numbers: https://github.com/cofi/evil-numbers/blob/master/evil-numbers.el
- hideshow.el: http://www.emacswiki.org/HideShow
- http://emacs.stackexchange.com/questions/2119/absolute-fold-level-in-emacs
- evil-org-mode: https://github.com/edwtjo/evil-org-mode
- http://stackoverflow.com/questions/2399612/why-is-there-no-code-folding-in-emacs
- http://stackoverflow.com/questions/1511737/how-do-you-list-the-active-minor-modes-in-emacs 


## Notes

- To replace Vim's visual-line selection/':sort' command, do the same in evil-mode, but use ':sort-lines' command.
  - I've added an evil-ex-mode command ':sort' that is bound to ':sort-lines'. Makes things easier.

* TODO Convert this file to org-mode
* TODO Install python-mode.el
* TODO Get Evil's C-w working in minibuffer (i.e. in Ex Mode)
* TODO Look into ibuffer mode as a possible BufferExplorer replacement (http://www.emacswiki.org/emacs/IbufferMode)
* TODO tabbar-mode: http://www.emacswiki.org/emacs/TabBarMode
