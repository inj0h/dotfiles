import System.IO
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Accordion
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.SpawnOnce

myManageHook = composeAll (
    [ manageHook gnomeConfig
    , className =? "Unity-2d-panel" --> doIgnore
    , className =? "Unity-2d-launcher" --> doIgnore
    , className =? "Audacity" --> doFloat
    , className =? "Gimp" --> doFloat
    , manageDocks
    ])

myStartupHook = do
    spawnOnce "unity-settings-daemon"
    spawnOnce "gnome-settings-daemon"

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/eric/.xmobarrc"
    xmonad $ defaultConfig
        { manageHook = myManageHook
        , layoutHook = avoidStruts $ smartBorders $ layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                    { ppOutput = hPutStrLn xmproc
                    , ppTitle = xmobarColor "green" "" . shorten 50
                    }
        , startupHook = myStartupHook
        , modMask = mod4Mask
        , terminal    = "gnome-terminal"
        }
