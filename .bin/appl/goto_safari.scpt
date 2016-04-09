#!/usr/bin/osascript
# goto safari and refresh page

tell application "Safari" to activate
tell window 1 of application "Safari" to set current tab to tab 5
tell application "System Events" to keystroke "r" using {command down}
