PORT=5212
HOST=127.0.0.1

.PHONY: app
app:
	fuser -k $(PORT)/tcp || exit 0
	Rscript -e "shiny::runApp('.', port = $(PORT), host = '$(HOST)')" &
	sleep 0.4
	nohup firefox http://$(HOST):$(PORT) &> /dev/null &


.PHONY: script
script:
	Rscript sincal.R


.PHONY: help
help:
	cat Makefile


.PHONY: style
style:
	Rscript -e "styler::style_dir('.')"
