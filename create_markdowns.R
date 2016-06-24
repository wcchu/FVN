require(knitr) # required for knitting from rmd to md
require(markdown) # required for md to html 
knit('dnn_test.Rmd', 'dnn_test.md') # creates md file
markdownToHTML('dnn_test.md', 'dnn_test.html') # creates html file
