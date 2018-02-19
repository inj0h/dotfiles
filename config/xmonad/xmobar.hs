Config { font = "xft:Inconsolata Medium:size=13:antialias=true"
       , bgColor = "#202020"
       , fgColor = "grey"
       , position = TopW L 100
       , commands = [ Run Cpu ["-t", "<total>%", "-L","5","-H","40","--normal","green","--high","red"] 15
                    , Run BatteryP ["BAT0"] ["-t", "<acstatus> <left>% <timeleft>", "-h", "green", "-n", "yellow", "-l", "red"] 30
                    , Run Memory ["-t", "<usedratio>%"] 30
                    , Run Date "%a %b %_d %l:%M" "date" 30
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ CPU: %cpu% | MEM: %memory% | BAT: %battery% | DATE: %date%"
       , hideOnStart = False
       , lowerOnStart = False
       , overrideRedirect = False
       , allDesktops = True
       , persistent = True
       , alpha = 255
       , border = NoBorder
       , pickBroadest = True
       }
