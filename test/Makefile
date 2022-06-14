babel.html: babel.org
	pandoc -o babel.html babel.org

sections: 
	pandoc --filter=pandoc-crossref sections.org

test3.html: test3.org
	pandoc --standalone --citeproc --bibliography=../references.bib --csl=../modern-language-association.csl -o $@ $<
