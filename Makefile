ch-2.pdf: ch-2.org
	pandoc -o $@ $< --filter pandoc-citeproc -V linestretch=1.7 -V geometry=margin=1in 
	xdg-open $@
