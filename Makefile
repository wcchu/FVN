dnn_test.html: dnn_test.Rmd dnn.R
	Rscript dnn_test_markdown.R && rm dnn_test.md

clean:
	rm dnn_test.html
