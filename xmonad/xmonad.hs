import XMonad
import Data.Monoid
import System.Exit
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Hooks.ManageDocks

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myTerminal = "kitty"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth   = 0 

myModMask       = mod4Mask

myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  [ ((modm, xK_Return), spawn $ XMonad.terminal conf)
  , ((modm .|. shiftMask, xK_Return), spawn "firefox")
  , ((modm, xK_space), spawn "rofi -show drun")
  , ((modm, xK_q), kill)
  , ((modm, xK_Tab), sendMessage NextLayout)
  , ((modm, xK_n), refresh)
  , ((modm, xK_j), windows W.focusDown)
  , ((modm, xK_k), windows W.focusUp)
  , ((modm, xK_h), sendMessage Shrink)
  , ((modm, xK_l), sendMessage Expand)
  , ((modm, xK_t), withFocused $ windows . W.sink)
  , ((modm .|. shiftMask, xK_slash), io (exitWith ExitSuccess))
  , ((modm, xK_r), spawn "xmonad --recompile; xmonad --restart")
  ]
  ++

  [((m .|. modm, k), windows $ f i)
          | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
          , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

myLayout = avoidStruts (tiled ||| Mirror tiled ||| Full)
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100

myManageHook = composeAll
  [ className =? "TelegramDesktop"    --> doFloat
  , className =? "Gimp"               --> doFloat
  , resource  =? "desktop_window"     --> doIgnore
  , resource  =? "kdesktop"           --> doIgnore ]

myEventHook = mempty
myLogHook   = return ()

myStartupHook = do
  spawnOnce "nitrogen --restore &"
  spawnOnce "picom &"
  spawnOnce "setxkbmap pl &"

main = do
  xmproc <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc"
  xmonad $ docks defaults

defaults = def {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

        keys               = myKeys,
        mouseBindings      = myMouseBindings,

        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }


