# Helpers
is_digit = function(char) {
  grepl("[[:digit:]]", char)
}

is_alpha = function(char) {
  grepl("[a-zA-Z]", char)
}

is_alphanumeric = function(char) {
  is_alpha(char) || is_digit(char)
}

char_at = function(char, at) {
  substr(char, at, at)
}

stop_custom = function(.subclass, message, call = NULL, ...) {
  err = structure(
    list(
      message = message,
      call = call,
      ...
    ),
    class = c(.subclass, "condition")
  )
  stop(err)
}

get_log_numbers = function(range) {
  logs <- c()
  for(i in 1:range) {
    logs <- c(logs, paste0("log", toString(i)))
  }
  return(logs)
}

# Data
GREEK_KEYWORDS = paste0("\\", c(
  "alpha", "theta", "tau", "beta", "vartheta", "pi", "upsilon",
  "gamma", "varpi", "phi", "delta", "kappa", "rho",
  "varphi", "epsilon", "lambda", "varrho", "chi", "varepsilon",
  "mu", "sigma", "psi", "zeta", "nu", "varsigma", "omega", "eta",
  "xi", "Gamma", "Lambda", "Sigma", "Psi", "Delta", "Xi",
  "Upsilon", "Omega", "Theta", "Pi", "Phi", "phi", "circ", "degree",
  "infty", "leftsquare", "rightsquare", "abs",
  "log", "loge", "loga", "logb", "logc", "logx", "logy", "logz",
  get_log_numbers(100)
))

# TODO: Add \div operator, which is like a regular binary.
KEYWORDS = list(
  '\\frac' = 'FRAC',
  '\\sqrt' = 'SQRT',
  '\\log' = 'LOG',
  '\\cdot' = 'STAR',
  '\\times' = 'STAR',
  "\\pm" = "PM",
  "\\cup" = "CUP",
  '\\sin' = 'SIN',
  '\\cos' = 'COS',
  '\\tan' = 'TAN',
  '\\sinh' = 'SINH',
  '\\cosh' = 'COSH',
  '\\tanh' = 'TANH',
  "\\sec" = "SEC",
  "\\csc" = "CSC",
  "\\cot" = "COT"
)

KEYWORDS_LEXEMES = list(
  'STAR' = '*',
  'FRAC' = '/',
  'PM' = '±',
  'CUP' = 'cup',
  'SQRT' = 'sqrt',
  'LOG' = 'log',
  'SIN' = 'sin',
  'COS' = 'cos',
  'TAN' = 'tan',
  'SINH' = 'sinh',
  'COSH' = 'cosh',
  'TANH' = 'tanh',
  "SEC" = "sec",
  "CSC" = "csc",
  "COT" = "cot"
)

UNARY_FNS = c('SQRT', 'LOG', 'SIN', 'COS', 'TAN', 'SINH', 'COSH', 'TANH', "SEC", "CSC", "COT")

pkg_data = list(
  'GREEK_KEYWORDS' = GREEK_KEYWORDS,
  'KEYWORDS' = KEYWORDS,
  'KEYWORDS_LEXEMES' = KEYWORDS_LEXEMES,
  'UNARY_FNS' = UNARY_FNS
)

get_pkg_data = function(name = names(pkg_data)) {
  name = match.arg(name)
  pkg_data[[name]]
}

# TODO: Add TESTS

# https://github.com/KaTeX/KaTeX/blob/master/src/Parser.js
# Take into consideration.
