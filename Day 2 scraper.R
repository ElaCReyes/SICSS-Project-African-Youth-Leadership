#Author: Haroon


# dependencies
install.packages("rvest", repos='http://cran.us.r-project.org')

library("rvest")

# load page html
html <- read_html("https://www.cgcs.org/domain/321")

# get links
links <- html_nodes(html, css = ".ui-article-description li a:first-child")

for (link in links) {
  name = html_text(link)
  url = html_attr(link, 'href')
  output = sprintf('pdfs/%s.pdf', name)
  
  if (file.exists(output)) {
    next
  }
  
  if (startsWith(url, '/')) {
    url = sprintf('https://www.cgcs.org%s', url)
  }
  download.file(url, output)
}

