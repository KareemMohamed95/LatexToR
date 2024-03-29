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
  if(grepl("++", latex_no_spaced, fixed = TRUE) == TRUE
     ||grepl("+-", latex_no_spaced, fixed = TRUE) == TRUE
     ||grepl("--", latex_no_spaced, fixed = TRUE) == TRUE
     ||grepl("-+", latex_no_spaced, fixed = TRUE) == TRUE
     ||grepl(",,", latex_no_spaced, fixed = TRUE) == TRUE) {
      return("syntax error")
  }
  latex <- gsub("\\cdot", "*", l, fixed = TRUE)
  latex <- gsub("w", "\\omega", latex, fixed = TRUE)
  latex <- gsub("\\left[", "\\leftsquare", latex, fixed = TRUE)
  latex <- gsub("[", "\\leftsquare", latex, fixed = TRUE)
  latex <- gsub("\\right]", "\\rightsquare", latex, fixed = TRUE)
  latex <- gsub("]", "\\rightsquare", latex, fixed = TRUE)
  latex <- handle_trig_functions(latex)
  latex <- handle_exp_powers(latex)
  latex <- replace_abs(latex)
  if(latex == "syntax error")return(latex)
  first_pm <- handle_first_pm(latex)
  if(first_pm[[1]] == "TRUE") latex <- paste(first_pm[[2]], latex)

  tryCatch({
    l <- LatexToR(latex)
    if(first_pm[[1]] == "TRUE") l <- gsub(paste0(first_pm[[2]], " "), "", l, fixed = TRUE)
    l <- replace_all(l, c("leftsquare", "rightsquare"), c("[", "]"))
    l <- reverse_abs(l)
    l <- restore_log(l)
    return(l)
  },
  LatexToR.error = function(condition) {
    return("syntax error")
  })
}

replace_all <- function(l, vec, vec2) {
  for(i in 1:length(vec)) {
    l <- gsub(paste0(vec[[i]]," *"), vec2[[i]], l, fixed = TRUE)
    l <- gsub(paste0("* ", vec[[i]]), vec2[[i]], l, fixed = TRUE)
    l <- gsub(vec[[i]], vec2[[i]], l, fixed = TRUE)
  }
  return(l)
}

replace_abs <- function(str) {
  c <- 0
  str <- gsub("\\mid", "|", str, fixed = TRUE)
  str <- gsub("\\left|", "|", str, fixed = TRUE)
  str <- gsub("\\right|", "|", str, fixed = TRUE)
  str <- strsplit(str, "")[[1]]
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

reverse_abs <- function(str) {
  str <- gsub("abs * ", "abs", str, fixed = TRUE)
  return(str)
}

restore_log <- function(str) {
  str <- gsub("log * ", "log", str, fixed = TRUE)
  str <- gsub("loge * ", "loge", str, fixed = TRUE)
  str <- gsub("loga * ", "loga", str, fixed = TRUE)
  str <- gsub("logb * ", "logb", str, fixed = TRUE)
  str <- gsub("logc * ", "logc", str, fixed = TRUE)
  str <- gsub("logx * ", "logx", str, fixed = TRUE)
  str <- gsub("logy * ", "logy", str, fixed = TRUE)
  str <- gsub("logz * ", "logz", str, fixed = TRUE)
  for(i in 1:100) {
    temp_str <- paste0("log",toString(i))
    str <- gsub(paste0(temp_str, " * "), temp_str, str, fixed = TRUE)
  }
  return(str)
}

handle_exp_powers <- function(str) {
  str <- strsplit(str, "")[[1]]
  final_str <- ""
  i <- 1
  while(i <= length(str)) {
    if(str[[i]] == "e" && i+1 <= length(str) && i+2 <= length(str) && str[[i+1]] == "^" && str[[i+2]] != "{") {
      final_str = paste0(final_str, "e^{", str[[i+2]], "}")
      i <- i+3
    }
    else  {
      final_str <- paste0(final_str,  str[[i]])
      i <- i+1
    }
  }
  return(final_str)
}

handle_trig_functions <- function(str) {
  str <- gsub("\\sin", "sin", str, fixed = TRUE)
  str <- gsub("\\cos", "cos", str, fixed = TRUE)
  str <- gsub("\\tan", "tan", str, fixed = TRUE)
  str <- gsub("\\csc", "csc", str, fixed = TRUE)
  str <- gsub("\\sec", "sec", str, fixed = TRUE)
  str <- gsub("\\cot", "cot", str, fixed = TRUE)
  
  str <- gsub("sin", "\\sin", str, fixed = TRUE)
  str <- gsub("cos", "\\cos", str, fixed = TRUE)
  str <- gsub("tan", "\\tan", str, fixed = TRUE)
  str <- gsub("csc", "\\csc", str, fixed = TRUE)
  str <- gsub("sec", "\\sec", str, fixed = TRUE)
  str <- gsub("cot", "\\cot", str, fixed = TRUE)
}

handle_first_pm <- function(str) {
  if(substr(gsub(" ", "", str, fixed = TRUE), 1, 3) == "\\pm") 
    return(c("TRUE", runif(1)))
  return(c("FALSE", ""))
}

#' @export
r2latex <- function(r) { 
  
  str2 <- Ryacas::yac_str(paste0("TexForm(", r, ")"))
  str2 <- gsub("$", "", str2, fixed = TRUE)
  str2 <- gsub("\n", "", str2, fixed = TRUE)
  str2 <- gsub(" ", "", str2, fixed = TRUE)
  return(str2)
}
