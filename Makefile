fvn_test.html: fvn_test.Rmd fvn.R
	Rscript create_markdowns.R && rm fvn_test.md

clean:
	rm fvn_test.html
