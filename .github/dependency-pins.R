# March R-devel snapshots reject the symbol bindings used by vctrs 0.7.2.
pins = character()
if (any(R.version[["svn rev"]] %in% c("89623", "89629"))) {
  pins = c(pins, "vctrs@0.7.1")
}

# Jammy ships libstdc++ GDAL. Its C++ string ABI is incompatible with newer
# terra on the R-hub runners built with libc++.
if (any(grepl("/ubuntu-22\\.04/[^/]+/libc\\+\\+/?$", getOption("repos")))) {
  pins = c(pins, "terra@1.8-50")
}

cat(
  "packages=",
  paste(pins, collapse = " "),
  "\n",
  sep = "",
  file = Sys.getenv("GITHUB_OUTPUT"),
  append = TRUE
)
