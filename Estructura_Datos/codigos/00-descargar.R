library(rvest)
library(xml2)
library(curl)
library(httr)

url_base <- "https://www.gob.mx/sesnsp/acciones-y-programas/datos-abiertos-de-incidencia-delictiva"
url_base <- GET(url_base, add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
html <- read_html(url_base)

# 1) body
body_node <- html_node(html, "body")
if (is.na(body_node) || is.null(body_node)) {
  stop("No se encontró el nodo <body>.")
}

# 2) main
main_node <- html_node(body_node, "main")
if (is.na(main_node) || is.null(main_node)) {
  stop("No se encontró el nodo <main> dentro del body.")
}

# 3) primer div con class "container"
container_node <- html_node(main_node, "div.container")
if (is.na(container_node) || is.null(container_node)) {
  stop("No se encontró el primer div.container dentro de main.")
}

# 4) primer div con class "row"
row_node <- html_node(container_node, "div.row")
if (is.na(row_node) || is.null(row_node)) {
  stop("No se encontró el primer div.row dentro de container.")
}

# 5) primer div con class "col-sm-7 pull-left"
col_node <- html_node(row_node, "div.col-sm-7.pull-left")
if (is.na(col_node) || is.null(col_node)) {
  stop("No se encontró el primer div.col-sm-7.pull-left dentro de row.")
}

# 6) primer div
first_div <- xml_find_first(col_node, "./div")
if (is.na(first_div) || is.null(first_div)) {
  stop("No se encontró el primer div dentro de col-sm-7.pull-left.")
}

# 7) primer div con class "-box"
article_body <- xml_find_first(first_div, ".//div[contains(concat(' ', normalize-space(@class), ' '), ' article-body ')]")
if (is.na(article_body) || is.null(article_body)) {
  stop("No se encontró el primer div con class '-box'.")
}


# 9) segundo ul
uls <- xml_find_all(article_body, "./ul")
if (length(uls) < 2) {
  stop("No se encontró al menos un segundo <ul> dentro de article-body.")
}
second_ul <- uls[[2]]

# 10) tercer li
lis <- xml_find_all(second_ul, "./li")
if (length(lis) < 3) {
  stop("No se encontró al menos un tercer <li> dentro del segundo <ul>.")
}
third_li <- lis[[3]]

# 11) primer <a>
a_node <- xml_find_first(third_li, "./a")
if (is.na(a_node) || is.null(a_node)) {
  stop("No se encontró el primer <a> dentro del tercer <li>.")
}

texto <- xml_text(a_node)
href <- xml_attr(a_node, "href")
base_href <- sub("\\?.*$", "", href)
download_url <- paste0(base_href, "?download=1")
curl_download(download_url, destfile = "../Datos/Preliminares/delitos_municipal.zip", quiet = FALSE)

unzip(zipfile ="../Datos/Preliminares/delitos_municipal.zip",exdir = "../Datos/Preliminares/" )
