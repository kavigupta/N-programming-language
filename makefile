
.ONESHELL:

main: program notes

program:
	cabal build
	mv dist/build/N-programming-language/N-programming-language N

notes:
	cd charset
	pdflatex charset.tex
	cd ..
