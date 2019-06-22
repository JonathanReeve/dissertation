ch-2.pdf: ch-2.org references.bib
	pandoc -o $@ $< --bibliography references.bib --pdf-engine=xelatex --filter pandoc-citeproc -V linestretch=1.7 -V fontsize=12p --toc
	xdg-open $@

books.org: ~/Dropbox/Org/Projects/books.org
	emacsclient -e "(progn (find-file \"$<\") (org-id-goto \"ae4e2aa3-7b26-4038-8691-2a7b97fa21b5\") (org-org-export-to-org nil t))"
	mv ~/Dropbox/Org/Projects/books.org.org ./books.org

references.bib: /home/jon/Dropbox/Papers/dissertation.bib
	cp $< $@

all: ch-2.pdf books.org references.bib
