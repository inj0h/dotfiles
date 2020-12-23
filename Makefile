.DEFAULT_GOAL := setup
.PHONY        := setup clean

msg_info_help := "Commands: \
									\n  clean -> unlink config files \
									\n  help  -> output this message \
									\n  setup -> link config files"

clean:
	./bin/connect-the-dots --unlink dotfiles.txt

help:
	@echo $(msg_info_help)

setup:
	./bin/connect-the-dots --link dotfiles.txt
