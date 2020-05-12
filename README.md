My XMonad configuration. Most definitely a WIP. In particular, I haven't done
much with layouts yet, because I still have to figure out which layouts I
actually want to use. (I have resumed using XMonad after many years.)

## Using the configuration

This repository mirrors my `~/.xmonad` directory. The configuration code is
managed as a Cabal project, so that I can easily pull development versions of
`xmonad` and `xmonad-contrib` (see the `cabal.project` file), as well as
additional packages if need be. The `build` script, called by XMonad upon
reloading, recompiles the binary using `cabal exec ghc`, and copies it to
`~/.local/bin`, which is in my `$PATH`.  (I couldn't figure how to make it
work with `cabal install`, and even if I did I wouldn't have wanted to end up
with an arbitrary number of copies of the binary in my Cabal store.)

To run XMonad with this configuration, assuming all the non-Haskell parts of
the setup are already in place, run the `build` script a
single time to create the `xmonad` binary. Then, copy the reference
`xinitrc` to `~/.xinitrc` and, after your next login, start X with `startx`
(alternatively, use the provided `xinitrc` directly with `startx
~/.xmonad/xinitrc`).

A few words about some of the other files in this directory:

- The purpose of the `Xresources` file is making `xterm` more pleasant to
    use. If you care about that, copy it to `~/.Xresources`.

- The [Haskell Wiki XMonad cheatsheet PNG](https://wiki.haskell.org/File:Xmbindings.png)
    is actually used by the configuration. (Note that I have by and large
    preserved the default keybindings, so it is relevant in this context.)

## External dependencies

Things I rely on for this configuration include:

- [xmobar](https://hackage.haskell.org/package/xmobar), version 0.33,
    installed from Hackage with all extensions enabled.

- Programs used by XMonad's default configuration:

    - [dmenu](https://www.archlinux.org/packages/community/x86_64/dmenu/);
    - [xterm](https://www.archlinux.org/packages/extra/x86_64/xterm/);
    - [xmessage](https://www.archlinux.org/packages/extra/x86_64/xorg-xmessage/)
        (for displaying the error log upon recompiling XMonad through the
        Mod+q shortcut. Investigating alternatives is on my todo list).

- Given that I quite like Xfce, and even plan to keep it around as a secondary
    desktop environment, I use several of its core utilities, including
    [`xfce4-appfinder`](https://www.archlinux.org/packages/extra/x86_64/xfce4-appfinder/),
    [`xfce4-terminal`](https://www.archlinux.org/packages/extra/x86_64/xfce4-terminal/),
    [`thunar`](https://www.archlinux.org/packages/extra/x86_64/thunar/),
    [`xfce4-taskmanager`](https://www.archlinux.org/packages/extra/x86_64/xfce4-taskmanager/),
    [`xfce4-power-manager`](https://www.archlinux.org/packages/extra/x86_64/xfce4-power-manager/)
    and [`xfce4-notifyd`](https://www.archlinux.org/packages/extra/x86_64/xfce4-notifyd/).

- Programs used in my `.xinitrc`:

    - [trayer](https://www.archlinux.org/packages/extra/x86_64/trayer/);
    - [feh](https://www.archlinux.org/packages/extra/x86_64/feh/) (for the wallpaper);
    - [XScrenSaver](https://www.archlinux.org/packages/extra/x86_64/xscreensaver/).

- Miscellaneous programs:

    - [XSel](https://www.archlinux.org/packages/community/x86_64/xsel/) (for
        the Unicode prompt).
    - [`unicode-character-database`](https://www.archlinux.org/packages/extra/any/unicode-character-database/)
        (also for the Unicode prompt).

- Fonts:

    - [Hack](https://www.archlinux.org/packages/extra/any/ttf-hack/) (from the
        monospaced fonts I tried with xmobar, it was the best looking one by
        some distance).
    - [Noto CJK](https://www.archlinux.org/packages/extra/any/noto-fonts-cjk/)
        (note that I haven't investigated yet whether it can be used as a
        CJK fallback on xmobar yet).

