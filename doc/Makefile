main: docs partial_clean

docs: docs_body Docs_body.tex
	xelatex Docs.tex

docs_body: docs.md
	pandoc -f markdown_github+raw_tex -t latex docs.md > Docs_body.tex

.PHONY : partial_clean
partial_clean:
	-rm -f *.log
	-rm -f Docs_body.tex
	-rm -f *.aux

.PHONY : clean
clean: partial_clean
	-rm -f Docs.pdf
