babel.html: babel.org
	pandoc -o babel.html babel.org

sections: 
	pandoc --filter=pandoc-crossref sections.org

test3.html: test3.org
	pandoc --standalone --citeproc --bibliography=../references.bib --csl=../modern-language-association.csl -o $@ $<

citations.html: citations.org
	pandoc --standalone --citeproc --bibliography=../references.bib --csl=../templates/modern-language-association.csl -o $@ $<

synsets.html: synsets.org
	pandoc --standalone --filter ../templates/synset-filter.hs --filter ../templates/hex-filter.hs -o $@ $<

figures.html: figures.org
	pandoc --standalone --filter pandoc-crossref -o $@ $<
