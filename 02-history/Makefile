ch-2.pdf: ch-2.md references.bib
	pandoc -o $@ $< \
	--bibliography /home/jon/Dropbox/Papers/library.bib \
	--csl=modern-language-association.csl \
	--pdf-engine=tectonic \
	--filter pandoc-citeproc \
	--template default.tex \
	-V linestretch=1.8 -V fontsize=12p \
	--metadata date=`date --iso`
	xdg-open $@

ch-2.html: ch-2.md references.bib
	pandoc -o $@ $< \
	--to html5 \
	--bibliography references.bib \
	--csl=modern-language-association.csl \
	--filter pandoc-citeproc \
	-V linestretch=1.8 -V fontsize=12p \
  --template=template.html \
	--metadata date=`date --iso` \
	--standalone
	xdg-open $@

ch-2-html.pdf: ch-2.md references.bib
	pandoc -o $@ $< \
	--bibliography references.bib \
	--csl=modern-language-association.csl \
	--pdf-engine=weasyprint \
	--filter pandoc-citeproc \
	-V linestretch=1.8 -V fontsize=12p \
	--metadata date=`date --iso`
	xdg-open $@

ch-2-org.pdf: ch-2.org references.bib
	pandoc -o $@ $< \
	--bibliography references.bib \
	--csl=modern-language-association.csl \
	--pdf-engine=tectonic \
	--filter pandoc-citeproc \
	-V linestretch=1.8 -V fontsize=12p \
	--metadata date=`date --iso`
	xdg-open $@

books.org: ~/Dropbox/Org/Projects/books.org
	emacsclient -e "(progn (find-file \"$<\") (org-id-goto \"ae4e2aa3-7b26-4038-8691-2a7b97fa21b5\") (org-org-export-to-org nil t))"
	mv ~/Dropbox/Org/Projects/books.org.org ./books.org

references.bib: /home/jon/Dropbox/Papers/dissertation.bib
	cp $< $@

all: ch-2.pdf books.org references.bib
