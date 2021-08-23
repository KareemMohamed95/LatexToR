#' Translate LaTeX formula to R code (standardMath).
#'
#' Take a LaTeX formula and attempts to find an equivalent representation in R.
#' If `interactive=TRUE` it pops up an interactive REPL that does the same, but interactively.
#'
#' @param x string
#' @param interactive boolean
#'
#' @return string
#' @export
#'
#' @examples
#' LatexToR("\\beta_1^{\\frac{x+1}{x^2 \\cdot y}}")
LatexToR = function(x, interactive = FALSE) {
  if (interactive) {
    Reina$new()$run_prompt()
  } else {
    Reina$new()$run_line(x)
  }
}

#' @export
latex2r <- function(l) {
  latex_no_spaced <- gsub(" ", "", l, fixed = TRUE)
  if(grepl("++", latex_no_spaced,fixed = TRUE) == TRUE
     ||grepl("+-", latex_no_spaced,fixed = TRUE) == TRUE
     ||grepl("--", latex_no_spaced,fixed = TRUE) == TRUE
     ||grepl("-+", latex_no_spaced,fixed = TRUE) == TRUE
     ||grepl(",,", latex_no_spaced,fixed = TRUE) == TRUE) {
      return("syntax error")
  }
  latex <- gsub("\\cdot", "*", l, fixed = TRUE)
  latex <- gsub("\\left[", "\\leftsquare", latex, fixed = TRUE)
  latex <- gsub("[", "\\leftsquare", latex, fixed = TRUE)
  latex <- gsub("\\right]", "\\rightsquare", latex, fixed = TRUE)
  latex <- gsub("]", "\\rightsquare", latex, fixed = TRUE)
  latex <- replace_abs(latex)
  if(latex == "syntax error")return(latex)
  
  tryCatch({
    l <- LatexToR(latex)
    l <- replace_all(l, c("leftsquare", "rightsquare"), c("[", "]"))
    l <- reverse_abs(l)
    return(l)
  },
  LatexToR.error = function(condition) {
    return("syntax error")
  })
}

#' @export
r2latex <- function(r) { 
  
  str2 <- Ryacas::yac_str(paste0("TexForm(", r, ")"))
  str2 <- gsub("$", "", str2, fixed = TRUE)
  str2 <- gsub("\n", "", str2, fixed = TRUE)
  str2 <- gsub(" ", "", str2, fixed = TRUE)
  return(str2)
}

#' @export
replace_all <- function(l, vec, vec2) {
  for(i in 1:length(vec)) {
    l <- gsub(paste0(vec[[i]]," *"), vec2[[i]], l, fixed = TRUE)
    l <- gsub(paste0("* ", vec[[i]]), vec2[[i]], l, fixed = TRUE)
    l <- gsub(vec[[i]], vec2[[i]], l, fixed = TRUE)
  }
  return(l)
}

#' @export
replace_abs <- function(str) {
  c <- 0
  str <- gsub("\\mid", "|", str, fixed = TRUE)
  str <- gsub("\\left|", "|", str, fixed = TRUE)
  str <- gsub("\\right|", "|", str, fixed = TRUE)
  str <- strsplit(str,"")[[1]]
  str2 <- ""
  for(i in 1:length(str)) {
    if(str[[i]] == "|") {
      c <- c+1
      if(c %% 2 == 0) {
        str2 <- paste0(str2, "\\closingabs")
      }
      else {
        str2 <- paste0(str2, "|")
      }
    }
    else {
      str2 <- paste0(str2, str[[i]])
    }
  }
  if(c%%2 == 1)return("syntax error")
  str2 <- gsub("|", "\\abs(", str2, fixed = TRUE)
  str2 <- gsub("\\closingabs", ")", str2, fixed = TRUE)
  return(str2)
}

#' @export
reverse_abs <- function(str) {
  str <- gsub("abs * ", "abs", str, fixed = TRUE)
  return(str)
}
