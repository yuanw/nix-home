-- https://github.com/xmonad/xmonad/blob/master/TUTORIAL.md

import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab

main :: IO ()
main = xmonad $ def
  {
  modMask  = mod4Mask
                  }

       `additionalKeysP`
       [ ("M-S-z", spawn "xscreensaver-command -lock")
       , ("M-S-=", unGrab *> spawn "" )
       , ("M-]", spawn "firefox")
       ]
