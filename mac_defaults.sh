#!/bin/sh

# Enable the app switcher on all displays
defaults write com.apple.dock appswitcher-all-displays -bool true

# Faster Dock show / hide
# Relevant blog post - https://macmyths.com/how-to-speed-up-auto-showing-hiding-dock-on-mac
defaults write com.apple.dock autohide-delay -float 0.10
defaults write com.apple.dock autohide-time-modifier -float 0.5

# Don't retain app state
# Relveant blog post - https://apple.stackexchange.com/questions/54854/is-there-a-way-to-make-preview-not-open-all-previously-opened-files
defaults write com.apple.Preview ApplePersistenceIgnoreState YES
defaults write com.apple.Terminal ApplePersistenceIgnoreState YES
