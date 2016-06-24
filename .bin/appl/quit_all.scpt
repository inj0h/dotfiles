#!/usr/bin/osascript
# quit all apps except Finder, iTerm, and Terminal

tell application "System Events" to set the visible of every process to true
set white_list to {"Finder"}
try
tell application "Finder"
set process_list to the name of every process whose visible is true
end tell
repeat with i from 1 to (number of items in process_list)
set this_process to item i of the process_list
if this_process is not in white_list and name is not "Finder" or name is not "iTerm" or name is not "Terminal" then
tell application this_process
quit
end tell
end if
end repeat
end try
