## ---- chatgpt selection-aware shortcuts ----
## Requirements:
## install.packages("chatgpt")
## install.packages("rstudioapi")  # only needed for selection/whole-file capture in RStudio

.chatgpt_cat <- function(x) {
  cat(x)
  invisible(x)
}

# Internal: fetch code from RStudio editor (selection first, else whole document)
.chatgpt_get_code <- function(prefer_selection = TRUE) {
  
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    stop(
      "RStudio editor context is not available.\n",
      "If you are not in RStudio, pass code as a string, e.g. ex('x <- 1')."
    )
  }
  
  # Try Source editor context first (often the most reliable)
  ctx <- NULL
  ctx <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
  
  # Fallback: active document context
  if (is.null(ctx)) {
    ctx <- tryCatch(rstudioapi::getActiveDocumentContext(), error = function(e) NULL)
  }
  
  if (is.null(ctx)) {
    stop(
      "Could not retrieve an editor context from RStudio.\n",
      "Click inside the Source pane (your script) and try again."
    )
  }
  
  selected_text <- ""
  if (!is.null(ctx$selection) && length(ctx$selection) >= 1) {
    selected_text <- ctx$selection[[1]]$text %||% ""
  }
  
  # Selection first
  if (prefer_selection && nzchar(selected_text)) {
    return(selected_text)
  }
  
  # Whole document fallback
  contents <- ctx$contents %||% ""
  if (nzchar(contents)) {
    return(contents)
  }
  
  # Diagnostics that typically reveal what's wrong
  stop(
    "No code found in the current RStudio editor context.\n\n",
    "Most common fixes:\n",
    "  1) Click inside your script in the Source pane (give it focus), then run again.\n",
    "  2) Ensure the script tab contains code and is not empty.\n",
    "  3) If you are working in the Console only, pass code as a string.\n"
  )
}

# Helper: provide %||% operator locally
`%||%` <- function(a, b) if (is.null(a)) b else a

# 1) Ask ChatGPT (your shortcut)
qn <- function(prompt) .chatgpt_cat(chatgpt::ask_chatgpt(prompt))

# 2) Comment selected code (selection-first; otherwise whole file)
cc <- function(code = NULL) {
  if (is.null(code)) code <- .chatgpt_get_code(prefer_selection = TRUE)
  .chatgpt_cat(chatgpt::comment_code(code))
}

# 3) Complete selected code
cmp <- function(code = NULL) {
  if (is.null(code)) code <- .chatgpt_get_code(prefer_selection = TRUE)
  .chatgpt_cat(chatgpt::complete_code(code))
}

# 4) Create unit tests
ut <- function(code = NULL) {
  if (is.null(code)) code <- .chatgpt_get_code(prefer_selection = TRUE)
  .chatgpt_cat(chatgpt::create_unit_tests(code))
}

# 5) Create variable name
vn <- function(code = NULL) {
  if (is.null(code)) code <- .chatgpt_get_code(prefer_selection = TRUE)
  .chatgpt_cat(chatgpt::create_variable_name(code))
}

# 6) Document code (roxygen2)
doc <- function(code = NULL) {
  if (is.null(code)) code <- .chatgpt_get_code(prefer_selection = TRUE)
  .chatgpt_cat(chatgpt::document_code(code))
}

# 7) Explain selected code
ex <- function(code = NULL) {
  if (is.null(code)) code <- .chatgpt_get_code(prefer_selection = TRUE)
  .chatgpt_cat(chatgpt::explain_code(code))
}

# 8) Find issues in code
fi <- function(code = NULL) {
  if (is.null(code)) code <- .chatgpt_get_code(prefer_selection = TRUE)
  .chatgpt_cat(chatgpt::find_issues_in_code(code))
}

# 9) Optimize code
opt <- function(code = NULL) {
  if (is.null(code)) code <- .chatgpt_get_code(prefer_selection = TRUE)
  .chatgpt_cat(chatgpt::optimize_code(code))
}

# 10) Refactor code
rf <- function(code = NULL) {
  if (is.null(code)) code <- .chatgpt_get_code(prefer_selection = TRUE)
  .chatgpt_cat(chatgpt::refactor_code(code))
}
