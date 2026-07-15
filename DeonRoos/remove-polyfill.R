html_files <- list.files("_site", pattern = "\\.html$", full.names = TRUE, recursive = TRUE)
for (f in html_files) {
  lines <- readLines(f, warn = FALSE)
  cleaned <- lines[!grepl("polyfill\\.io", lines)]
  if (length(cleaned) < length(lines)) {
    writeLines(cleaned, f)
    message("Removed polyfill.io tag from: ", f)
  }
}
