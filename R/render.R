#' Render Analysis Report
#'
#' Render Quarto document and produce output files `.Rds` and `.html`.
#'
#' @inheritParams bmoe-package
#' @param outfile character. Name of output file(s); file extensions dropped.
#'
#' @return Silently return the input `object` for use with pipes.
#'
#' @export
render_bmoe_fit <- function(object, outfile) {
  stopifnot(inherits(object, "bmoe_fit"))
  requireNamespace("quarto", quietly = TRUE)

  out_nm <- gsub("\\.(rds|html)$", "", basename(outfile), ignore.case = TRUE)

  # All rendering to be done in temporary directory to avoid clashes
  cur_wd <- getwd()
  on.exit(setwd(cur_wd))

  out_dir <- file.path(cur_wd, dirname(outfile))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  temp_dir <- tempfile("bmoe\\", tempdir(), fileext = "")
  dir.create(temp_dir, recursive = TRUE)
  setwd(temp_dir)

  # Require analysis QMD and object saved as RDS
  bmoe_analysis_qmd <-
    system.file(
      "reports", "bmoe-analysis.qmd", mustWork = TRUE, package = "bmoe"
    )

  file.copy(bmoe_analysis_qmd, basename(bmoe_analysis_qmd))
  saveRDS(object, sprintf("%s.rds", out_nm))

  quarto::quarto_render(
    input = basename(bmoe_analysis_qmd),
    output_file = sprintf("%s.html", out_nm),
    execute_params = list(
      object = structure(sprintf('readRDS("%s.rds")', out_nm), tag = "!expr")
    )
  )

  unlink(basename(bmoe_analysis_qmd))

  output_files <- sprintf("%s%s", out_nm, c(".rds", ".html"))
  file.rename(output_files, file.path(out_dir, output_files))

  invisible(object)
}


#' Functions used in Analysis Report
#'
#' Functions not to be used anywhere other than `bmoe-analysis.qmd`.
#'
#' @inheritParams bmoe-package
#' @param fig.asp numeric. Default aspect ratio per **panel**, not per plot.
#'
#' @name bmoe-render-internal
#' @export
.set_bmoe_render_options <- function(object, fig.asp) {
  p_regr <- dim(object$output$regr)[3]
  p_wt <- dim(object$output$wt)[3]
  n_y <- dim(object$output$prec)[3]

  knitr::opts_chunk$set(
    echo = FALSE,
    message = FALSE,
    fig.asp = fig.asp,
    out.width = "100%"
  )

  knitr::opts_template$set(
    regr = list(fig.asp = fig.asp * p_regr),
    wt = list(fig.asp = fig.asp * p_wt),
    prec = list(fig.asp = fig.asp * n_y)
  )

  s3_register("knitr::knit_print", "list", bmoe::printer_tabset)

  ggplot2::theme_set(ggplot2::theme_minimal(base_size = 11))

  invisible(NULL)
}


#' Print Tabsets (via [`knitr::knit_print`])
#'
#' Converts a list to a Quarto / RMD tabset using the names as tab headings.
#'
#' @param x list. Each element is passed to `knit_print` within tabs.
#' @param options,... unused arguments required for `knit_print`.
#'
#' @references
#'   <https://github.com/nclJoshCowley/jcutils/blob/master/R/knitr-printers.R>
#'
#' @export
printer_tabset <- function(x, options, ...) {
  if (is.null(names(x))) names(x) <- seq_along(x)

  header <- ":::: {.panel-tabset}"
  footer <- "::::"

  res <- lapply(seq_along(x), function(i) {
    knitr::knit_child(
      text = c(
        "##### `r names(x)[i]`",
        "",
        "```{r}",
        "#| echo: false",
        "x[[i]]",
        "```"
      ),
      options = options["fig.asp"],
      envir = environment(),
      quiet = TRUE
    )
  })

  knitr::asis_output(paste(c(header, res, footer), collapse = "\n\n"))
}
