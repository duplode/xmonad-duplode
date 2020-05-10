module Main where

import System.IO
import System.Exit
import System.Directory
import System.FilePath
import Data.Monoid
import Data.List

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Hooks.Place
import XMonad.Layout.BoringWindows
import XMonad.Layout.Minimize
import XMonad.Hooks.Minimize
import XMonad.Actions.Minimize
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.FloatNext
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Man
import XMonad.Prompt.Unicode
import qualified XMonad.Prompt as Prompt
import qualified XMonad.Actions.GridSelect as Grid
import qualified XMonad.Actions.WindowBringer as Bringer

floatedHook = composeAll
    [ className =? "Gimp" --> doFloat
    , className =? "Xfce4-appfinder" --> doFloat
    , className =? "kruler" --> doFloat
    ]

myManageHook = placeHook (withGaps (20,0,20,0) (smart (0.25,0.25)))
    <+> floatedHook

-- Used with pinnedFocusEventHook, defined later in this file.
-- Can be extended with the usual X.ManageHook combinators.
needsPinnedFocus :: Query Bool
needsPinnedFocus =
    -- gpick needs a focusFollowsMouse override, as otherwise its window will
    -- lose focus when you attempt to pick colours from other windows.
    className =? "Gpick"

-- I couldn't figure out how to get the default X.Prompt font from the Arch
-- repositories, so this override is needed. Without it, the prompts fail
-- silently. This appears to be the same issue discussed at
-- https://www.reddit.com/r/xmonad/comments/bw2gzg/prompt_stopped_working/
promptBaseCfg = def { Prompt.font = "xft:Hack:size=10" }

-- The skeleton of this main originates from
-- https://wiki.haskell.org/Xmonad/Config_archive/John_Goerzen's_Configuration
main = do
    -- The xmobar configuration is coupled to the xmonad one, so I might as
    -- well keep it here too.
    xmobarPath <- (\dir -> dir </> "bin" </> "xmobar")
        <$> getAppUserDataDirectory "cabal"
    configDir <- getAppUserDataDirectory "xmonad"
    let xmobarConfigPath = configDir </> "xmobarrc"
    xmproc <- spawnPipe $ intercalate " " [xmobarPath, xmobarConfigPath]

    xmonad $ docks $ ewmh def
        { manageHook = floatNextHook <+> manageDocks
            <+> myManageHook <+> manageHook def
        , layoutHook = minimize . boringWindows . avoidStruts
            $ layoutHook def
        , logHook = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor "green" "" . shorten 50
            }
            >> updatePointer (0.5, 0.5) (0, 0)
        -- The default handleEventHook is mempty, so there is no need to
        -- include it here.
        , handleEventHook = pinnedFocusEventHook needsPinnedFocus
            <+> minimizeEventHook
            <+> fullscreenEventHook
        , modMask = mod4Mask
        -- There are pros and cons to focusFollowsMouse. I still haven't
        -- decided whether to leave it on.
        --, focusFollowsMouse = False
        --, clickJustFocuses = False
        } `additionalKeys`
        -- A prompt to stop me from accdientally killing XMonad.
        [ ((mod4Mask .|. shiftMask, xK_q), confirmPrompt promptBaseCfg "exit?" $
            io (exitWith ExitSuccess))
        -- Unminimize. The minimize counterpart is on the focusFollowsKeys list.
        , ((mod4Mask .|. shiftMask, xK_d), withLastMinimized maximizeWindowAndFocus)
        -- The GridSelect commands have been tweaked so that minimized windows
        -- can be conveniently restored.
        , ((mod4Mask, xK_g), unminimizeAndGoToSelected def)
        , ((mod4Mask .|. shiftMask, xK_g), unminimizeAndBringSelected def)
        , ((mod4Mask, xK_F1), manPrompt promptBaseCfg)
        -- The selected character will go to the "primary" X selection
        -- buffer, and can be pasted with the mouse middle button. To use the
        -- regular clipboard instead, use X.Prompt.Unicode.mkUnicodePrompt to
        -- pass the -b flag to xsel.
        , ((mod4Mask, xK_backslash)
            , unicodePrompt "/usr/share/unicode/UnicodeData.txt" promptBaseCfg)
        ]
        `additionalKeys` spawnKeys configDir
        `additionalKeys` focusFollowsKeys
        -- This is meant to be appended iff focusFollowsMouse is False.
        --`additionalKeys` focusDoesntFollowKeys

spawnKeys configDir =
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    -- Given that dmenu_run at Mod+p already provides a lightweight runner, I
    -- have replaced gmrun in this binding with the Xfce runner, which makes
    -- it possible to search by description and browse by category.
    , ((mod4Mask .|. shiftMask, xK_p), spawn "xfce4-appfinder")
    -- Mod+Return is left as is, with xterm, for one-off terminal windows.
    -- The Xfce terminal is an alternative for more involved tasks, such as
    -- programming.
    , ((mod4Mask, xK_s), spawn "xfce4-terminal")
    , ((mod4Mask, xK_x), spawn "thunar")
    , ((mod4Mask .|. shiftMask, xK_BackSpace), spawn "xfce4-taskmanager")
    -- For some reason, xmessage doesn't break lines properly when displaying
    -- the default help message.
    -- Source: https://wiki.haskell.org/File:Xmbindings.png
    , ((mod4Mask .|. shiftMask, xK_slash), toggleFloatNext
        >> spawn (intercalate " " ["feh", configDir </> "Xmbindings.png"]))
    ]

focusFollowsKeys =
    -- The Mod+Shift+c binding is the default one; I'm restating it for
    -- the sake of consistency.
    [ ((mod4Mask .|. shiftMask, xK_c), kill)
    , ((mod4Mask, xK_d), withFocused minimizeWindow)
    ]

-- Without focusFollowsKeys, it is rather easy to, for instance, close the
-- focused window when you meant to close the one under the pointer. This
-- alternative set of bidings aims at lowering that risk.
focusDoesntFollowKeys =
    [ ((mod4Mask .|. shiftMask, xK_c), atPointer killWindow)
    -- Good old Alt-F4, closes unconditionally.
    , ((mod1Mask, xK_F4), kill)
    , ((mod4Mask, xK_d), atPointer minimizeWindow)
    , ((mod4Mask, xK_F8), withFocused minimizeWindow)
    ]

-- Some combinators that might be of general interest.

-- Window operations at the pointer location.

queryPointerWindow :: X (Maybe Window)
queryPointerWindow = do
    root <- asks theRoot
    dpy <- asks display
    mxy <- asks mousePosition
    let retrieveWindow (x, y) = io $ (\(_,_,_,w) -> w)
            <$> translateCoordinates dpy root root
                (fromIntegral x) (fromIntegral y)
    traverse retrieveWindow mxy

atPointer :: (Window -> X ()) -> X ()
atPointer callback = do
    mw <- queryPointerWindow
    maybe (return ()) callback mw

-- Loosely based on X.A.UpdateFocus.focusOnMouseMove.
-- This might be used to make an even safer version of the close window
-- binding, which only closes the window if it is focused and under the
-- pointer.
pointerIsOnFocused :: X Bool
pointerIsOnFocused = do
    foc <- withWindowSet $ return . W.peek
    mw <- queryPointerWindow
    return (foc == mw)

-- X.A.GridSelect window switching that accounts for X.L.Minimize.

unminimizeAndGoToSelected :: Grid.GSConfig Window -> X ()
unminimizeAndGoToSelected =
    Grid.withSelectedWindow $ \w -> do
        maximizeWindow w
        windows (W.focusWindow w)

unminimizeAndBringSelected :: Grid.GSConfig Window -> X ()
unminimizeAndBringSelected =
    Grid.withSelectedWindow $ \w -> do
        windows (Bringer.bringWindow w)
        maximizeWindow w
        focus w
        windows W.shiftMaster

-- Acts as if focusFollowsMouse was False if the query holds.
-- Implemented as an override of the CrossingEvent cases in X.Main.handle.
-- For a very similar solution, see
-- https://wiki.haskell.org/Xmonad/Config_archive/Brent_Yorgey%27s_darcs_xmonad.hs
-- See also X.L.MagicFocus.followOnlyIf.
pinnedFocusEventHook :: Query Bool -> Event -> X All
pinnedFocusEventHook q e@(CrossingEvent {ev_event_type = t})
    | (t == enterNotify && ev_mode e == notifyNormal) || t == leaveNotify
        = do
            -- The focus hasn't changed yet.
            focusIsPinned <- withWindowSet $ \w ->
                maybe (return False) (runQuery q) (W.peek w)
            -- Run the default handler (thus changing the focus) only if the
            -- focus isn't pinned.
            return (All (not focusIsPinned))
    | otherwise = return (All True)
pinnedFocusEventHook _ _ = return (All True)

