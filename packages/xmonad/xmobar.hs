-- https://github.com/jaor/xmobar/blob/master/examples/xmobar.hs

import Xmobar
  ( Border (TopB),
    Command (Com),
    Config
      ( additionalFonts,
        alignSep,
        allDesktops,
        alpha,
        bgColor,
        border,
        borderColor,
        commands,
        fgColor,
        font,
        hideOnStart,
        iconOffset,
        iconRoot,
        lowerOnStart,
        overrideRedirect,
        persistent,
        pickBroadest,
        position,
        sepChar,
        template,
        textOffset
      ),
    Date (Date),
    Exec (alias, run),
    Monitors (Cpu, Memory, Network, Swap, Weather),
    Runnable (Run),
    XPosition (Top),
    defaultConfig,
    xmobar,
  )

-- Example user-defined plugin

data HelloWorld = HelloWorld
  deriving (Read, Show)

instance Exec HelloWorld where
  alias HelloWorld = "hw"
  run HelloWorld = return "<fc=red>Hello World!!</fc>"

-- Configuration, using predefined monitors as well as our HelloWorld
-- plugin:

config :: Config
config =
  defaultConfig
    { font = "xft:Sans Mono-9",
      additionalFonts = [],
      borderColor = "black",
      border = TopB,
      bgColor = "black",
      fgColor = "grey",
      alpha = 255,
      position = Top,
      textOffset = -1,
      iconOffset = -1,
      lowerOnStart = True,
      pickBroadest = False,
      persistent = False,
      hideOnStart = False,
      iconRoot = ".",
      allDesktops = True,
      overrideRedirect = True,
      commands =
        [ Run $
            Weather
              "EGPH"
              [ "-t",
                "<station>: <tempC>C",
                "-L",
                "18",
                "-H",
                "25",
                "--normal",
                "green",
                "--high",
                "red",
                "--low",
                "lightblue"
              ]
              36000,
          Run $
            Network
              "eth0"
              [ "-L",
                "0",
                "-H",
                "32",
                "--normal",
                "green",
                "--high",
                "red"
              ]
              10,
          Run $
            Network
              "eth1"
              [ "-L",
                "0",
                "-H",
                "32",
                "--normal",
                "green",
                "--high",
                "red"
              ]
              10,
          Run $
            Cpu
              [ "-L",
                "3",
                "-H",
                "50",
                "--normal",
                "green",
                "--high",
                "red"
              ]
              10,
          Run $ Memory ["-t", "Mem: <usedratio>%"] 10,
          Run $ Swap [] 10,
          Run $ Com "uname" ["-s", "-r"] "" 36000,
          Run $ Date "%a %b %_d %Y %H:%M:%S" "date" 10,
          Run HelloWorld
        ],
      sepChar = "%",
      alignSep = "}{",
      template =
        "%cpu% | %memory% * %swap% | %eth0% - %eth1% }\
        \ %hw% { <fc=#ee9a00>%date%</fc>| %EGPH% | %uname%"
    }

-- Config { font = "xft:PragmataPro:size=18:bold"
--          , borderColor = "black"
--          , border = TopB
--          , bgColor = "black"
--          , fgColor = "grey"
--          , allDesktops = True
--          , position = TopW L 100
--          , commands = [ Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 10
--                          -- Cpu usage in percent
--                          , Run Cpu ["-t", "<fn=1>\xf108</fn>  cpu: (<total>%)","-H","50","--high","red"] 20
--                          -- Ram used number and percent
--                          , Run Memory ["-t", "<fn=1>\xf233</fn>  mem: <used>M (<usedratio>%)"] 20
--                          -- Disk space free
--                          , Run DiskU [("/", "<fn=1>\xf0c7</fn>  hdd: <free> free")] [] 60
--                          , Run Swap [] 10
--                          , Run Wireless "wlp0s20f3" [ ] 20
--                          , Run DynNetwork [] 10
--                          , Run Com "uname" ["-s","-r"] "" 36000
--                          , Run Date "<fn=1>\xf133</fn> %a %b %_d %Y %H:%M:%S" "date" 10
--                          , Run Battery ["-t", "<acstatus>: <left>% - <timeleft>",
--                                   "--",--"-c", "charge_full"
--                                         "-O", "AC",
--                                         "-o", "Bat",
--                                         "-h", "green",
--                                         "-l", "red"] 10
--                          ]
--          , sepChar = "%"
--          , alignSep = "}{"
--          , template = " %battery% | %cpu% | %memory% * %swap% | %disku% | %date% | %uname% | %wlp0s20f3wi% %dynnetwork% "

main :: IO ()
main = xmobar config
