# washr API drift check (Global-Health-Engineering/ghedatapublishing#12).
# The guide renders with `eval: false`, so nothing runs the washr calls it
# shows. This script reads the R code chunks of the guide's .qmd files and
# checks every washr call against the installed washr (CRAN in CI):
# - a call is a washr call when it is written `washr::name()`, or when `name`
#   is a bare call that the washr release recorded in WASHR_VERSION exports
#   (read from the NAMESPACE file of that release's CRAN source tarball);
# - the check fails when the installed washr no longer exports the function,
#   or no longer has a named argument the guide passes to it;
# - the installed version is printed next to the recorded one, so a new CRAN
#   release shows up in the next run.
# Usage: Rscript scripts/washr_drift.R ['glob' ...]   (default: '*.qmd')
# The globs are relative to the repository root. Base R plus washr.
root <- normalizePath(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])), ".."))
globs <- commandArgs(trailingOnly = TRUE)
if (!length(globs)) globs <- "*.qmd"
recorded <- trimws(readLines(file.path(root, "WASHR_VERSION"), warn = FALSE)[1])
if (!requireNamespace("washr", quietly = TRUE)) stop("washr is not installed")
installed <- as.character(packageVersion("washr"))
cat(sprintf("washr installed: %s; guide recorded against (WASHR_VERSION): %s\n", installed, recorded))

# Exports of the recorded release, from its CRAN source tarball.
recorded_exports <- function(version) {
  tarball <- sprintf("washr_%s.tar.gz", version)
  urls <- c(sprintf("https://cran.r-project.org/src/contrib/%s", tarball),
            sprintf("https://cran.r-project.org/src/contrib/Archive/washr/%s", tarball))
  dest <- file.path(tempdir(), tarball)
  ok <- FALSE
  for (u in urls) {
    ok <- tryCatch(utils::download.file(u, dest, quiet = TRUE, mode = "wb") == 0, error = function(e) FALSE, warning = function(w) FALSE)
    if (ok) break
  }
  if (!ok) stop(sprintf("could not download %s from CRAN; is WASHR_VERSION a released version?", tarball))
  utils::untar(dest, files = "washr/NAMESPACE", exdir = tempdir())
  ns <- readLines(file.path(tempdir(), "washr", "NAMESPACE"), warn = FALSE)
  sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", ns, value = TRUE))
}
rec_exports <- recorded_exports(recorded)
exports <- getNamespaceExports("washr")

# Every function call in the R chunks: file, line, package prefix, name, named arguments.
chunk_calls <- function(file) {
  txt <- readLines(file, warn = FALSE)
  starts <- grep("^\\s*```+\\s*\\{r[ ,}]", txt)
  fences <- grep("^\\s*```+\\s*$", txt)
  out <- list()
  for (s in starts) {
    e <- fences[fences > s][1]
    if (is.na(e) || e == s + 1) next
    code <- txt[(s + 1):(e - 1)]
    code[grepl("^\\s*#\\|", code)] <- ""
    pd <- tryCatch(utils::getParseData(parse(text = code, keep.source = TRUE)), error = function(err) err)
    if (inherits(pd, "error")) {
      out[[length(out) + 1]] <- data.frame(file = file, line = s, pkg = NA, name = NA, args = conditionMessage(pd))
      next
    }
    for (i in which(pd$token == "SYMBOL_FUNCTION_CALL")) {
      fn_expr <- pd$parent[i]
      pkg <- pd$text[pd$token == "SYMBOL_PACKAGE" & pd$parent == fn_expr]
      call_expr <- pd$parent[pd$id == fn_expr]
      args <- pd$text[pd$token == "SYMBOL_SUB" & pd$parent == call_expr]
      out[[length(out) + 1]] <- data.frame(file = file, line = s + pd$line1[i], pkg = if (length(pkg)) pkg else "",
                                           name = pd$text[i], args = paste(args, collapse = ","))
    }
  }
  do.call(rbind, out)
}

files <- unique(unlist(lapply(globs, function(g) Sys.glob(file.path(root, g)))))
if (!length(files)) stop("no files match: ", paste(globs, collapse = " "))
calls <- do.call(rbind, lapply(files, chunk_calls))
calls$file <- sub(paste0("^", root, "/"), "", calls$file)
status <- 0L

bad_parse <- calls[is.na(calls$name), ]
for (k in seq_len(nrow(bad_parse))) {
  status <- 1L
  cat(sprintf("PARSE: %s:%d: the R chunk does not parse, so its washr calls cannot be checked (%s)\n", bad_parse$file[k], bad_parse$line[k], bad_parse$args[k]))
}
calls <- calls[!is.na(calls$name), ]
washr_calls <- calls[calls$pkg == "washr" | (calls$pkg == "" & calls$name %in% rec_exports), ]

for (f in unique(calls$file)) {
  w <- washr_calls[washr_calls$file == f, ]
  cat(sprintf("%s: %s\n", f, if (nrow(w)) paste(sprintf("%s (line %d)", w$name, w$line), collapse = ", ") else "no washr calls"))
}

for (k in seq_len(nrow(washr_calls))) {
  cl <- washr_calls[k, ]
  if (!cl$name %in% exports) {
    status <- 1L
    cat(sprintf("DRIFT: %s:%d: %s() is not exported by washr %s\n", cl$file, cl$line, cl$name, installed))
    next
  }
  fmls <- names(formals(getExportedValue("washr", cl$name)))
  used <- if (nzchar(cl$args)) strsplit(cl$args, ",")[[1]] else character()
  if ("..." %in% fmls) next
  for (a in setdiff(used, fmls)) {
    status <- 1L
    cat(sprintf("DRIFT: %s:%d: %s() in washr %s has no argument `%s`\n", cl$file, cl$line, cl$name, installed, a))
  }
}
if (status == 0L) cat(sprintf("every washr call in the guide (%d distinct function(s)) is exported by washr %s with the arguments the guide uses\n", length(unique(washr_calls$name)), installed))

# Bare calls that are neither washr (at the recorded release) nor base R, for a human to glance at.
base_pkgs <- c("base", "utils", "stats", "methods", "graphics", "grDevices")
base_names <- unique(unlist(lapply(base_pkgs, function(p) getNamespaceExports(p))))
other <- calls[calls$pkg == "" & !calls$name %in% c(rec_exports, base_names), ]
if (nrow(other)) cat(sprintf("not checked (bare calls from other packages): %s\n", paste(sort(unique(other$name)), collapse = ", ")))

if (utils::compareVersion(installed, recorded) != 0) {
  cat(sprintf("NOTE: installed washr %s differs from WASHR_VERSION %s; review washr's NEWS.md, sync the guide, then update WASHR_VERSION\n", installed, recorded))
}
quit(status = status)
