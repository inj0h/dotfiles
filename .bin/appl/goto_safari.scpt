#!/usr/bin/osascript
# goto safari and refresh page

tell application "Safari" to activate
tell application "System Events" to keystroke "r" using {command down}
