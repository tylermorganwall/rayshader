pkgdown::build_site(run_dont_run = TRUE, lazy = FALSE)
system('echo "www.rayshader.com" >> docs/CNAME')
system("git checkout -b gh-pages")
library(magrittr)
twitter_card = list()
twitter_card[[1]] = xml2::read_xml("<meta />")
twitter_card[[2]] = xml2::read_xml("<meta />")
twitter_card[[3]] = xml2::read_xml("<meta />")
twitter_card[[4]] = xml2::read_xml("<meta />")
twitter_card[[5]] = xml2::read_xml("<meta />")

xml2::xml_attrs(twitter_card[[1]]) = c(property="twitter:image:src", content="https://www.rayshader.com/reference/figures/website.png")
xml2::xml_attrs(twitter_card[[2]]) = c(name="twitter:site", content="@github")
xml2::xml_attrs(twitter_card[[3]]) = c(name="twitter:card", content="summary_large_image")
xml2::xml_attrs(twitter_card[[4]]) = c(name="twitter:title", content="tylermorganwall/rayshader")
xml2::xml_attrs(twitter_card[[5]]) = c(name="twitter:description", content="R Package for Producing and Visualizing Hillshaded Maps from Elevation Matrices, in 2D and 3D - tylermorganwall/rayshader")

xml2::read_html("docs/index.html") %>%
  xml2::xml_find_first("head") %>% 
  xml2::xml_add_child(twitter_card[[1]]) %>%
  xml2::xml_add_sibling(twitter_card[[2]]) %>%
  xml2::xml_add_sibling(twitter_card[[3]]) %>%
  xml2::xml_add_sibling(twitter_card[[4]]) %>%
  xml2::xml_add_sibling(twitter_card[[5]]) %>%
  xml2::xml_parent() %>%
  xml2::xml_parent() %>%
  xml2::write_html("docs/index.html")

system("git add -f docs")
system('git commit -m "deploy to gh-pages"')
system("git push origin `git subtree split --prefix docs`:gh-pages --force")
system("git checkout master")
system("git branch -D gh-pages")

