.PHONY: help
help:
	cat Makefile


.PHONY: style
style:
	Rscript -e "styler::style_dir('.')"
