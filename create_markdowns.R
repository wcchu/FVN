require(knitr) # required for knitting from rmd to md
require(markdown) # required for md to html 
knit('fvn_test.Rmd', 'fvn_test.md') # creates md file
markdownToHTML('fvn_test.md', 'fvn_test.html') # creates html file
