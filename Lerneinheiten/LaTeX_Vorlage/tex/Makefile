NAME=lehrbrief-vorlage

all:
	xelatex $(NAME)
	makeindex $(NAME)
	bibtex $(NAME)
	xelatex $(NAME)

