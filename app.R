library(shiny)
library(dplyr)
library(googlesheets4)
library(shinyjs)
library(cookies)

# Read the CSV file (expects 'data.csv' with columns 'key' and 'value')
data <- read.csv('data.csv', stringsAsFactors = FALSE)

safe_general_log_inputs <- function(input, session) {
  if (!log_enabled) return(invisible(NULL))
  tryCatch({
    input_names <- names(input)
    # Coerce all input values to length 1 character ("" for empty)
    input_values <- lapply(input_names, function(nm) {
      val <- input[[nm]]
      if (is.null(val) || length(val) == 0) "" else as.character(val)[1]
    })
    names(input_values) <- input_names
    input_values$timestamp <- as.character(Sys.time())
    input_values$session_id <- as.character(session$token)
    df <- as.data.frame(input_values, stringsAsFactors = FALSE)
    googlesheets4::sheet_append(sheet_id, df)
  }, error = function(e) {
    assign("log_enabled", FALSE, envir = .GlobalEnv)
    warning("Logging disabled: ", e$message)
  })
}

# Helper to read sheet_id from file
get_sheet_id <- function(path = "sheet_id.txt") {
  if (file.exists(path)) {
    id <- trimws(readLines(path, warn = FALSE, n = 1))
    if (nzchar(id)) return(id)
  }
  return(NA_character_)
}

sheet_id <- get_sheet_id()

# Helper: log all input values to Google Sheets
log_enabled <- TRUE
if (is.na(sheet_id) || !nzchar(sheet_id)) {
  log_enabled <- FALSE
  warning("Logging disabled: sheet_id is not set.")
}
if (log_enabled) {
  # --- Google Sheets authentication for deployment ---
  if (file.exists("service-account.json")) {
    googlesheets4::gs4_auth(path = "service-account.json")
  } else {
    googlesheets4::gs4_deauth() # fallback: read-only public sheets, or prompt in dev
  }
}

ui <- add_cookie_handlers(fluidPage(
  useShinyjs(),
  titlePanel("Key Lookup App"),
  sidebarLayout(
    sidebarPanel(
      textInput("user_key", "Enter key:", ""),
      actionButton("check_btn", "Check"),
      selectInput("unlocked_values", "Unlocked values:", choices = NULL)
    ),
    mainPanel(
      textOutput("result")
    )
  )
))

server <- function(input, output, session) {
  add_cookie_handlers(session)

  # Track unlocked values via cookies (reactive)
  unlocked <- reactive({
    cookie_val <- cookies::get_cookie(session, "unlocked")
    if (!is.null(cookie_val) && nzchar(cookie_val)) {
      split_val <- strsplit(cookie_val, "::", fixed=TRUE)[[1]]
      if (length(split_val) == 0 || (length(split_val) == 1 && split_val == "")) {
        character(0)
      } else {
        unique(split_val)
      }
    } else {
      character(0)
    }
  })

  observeEvent(input$check_btn, {
    req(input$user_key)
    match <- data %>% filter(key == input$user_key)
    if (nrow(match) > 0) {
      val <- match$value[1]
      # Add to unlocked if not already present
      new_unlocked <- unique(c(unlocked(), val))
      cookies::set_cookie(session, "unlocked", paste(new_unlocked, collapse = "::"))
    }
  })

  observe({
    updateSelectInput(session, "unlocked_values", choices = unlocked())
  })

  # Observe all input changes (except every keystroke)
  observe({
    # Only log when an input changes (not every keystroke)
    # This triggers on blur for textInput, and on change for other controls
    reactiveValuesToList(input) # depend on all inputs
    isolate({
      safe_general_log_inputs(input, session)
    })
  })

  result <- eventReactive(input$check_btn, {
    req(input$user_key)
    match <- data %>% filter(key == input$user_key)
    if (nrow(match) > 0) {
      paste("Value:", match$value[1])
    } else {
      "Key not found."
    }
  })
  output$result <- renderText({
    result()
  })
}

shinyApp(ui = ui, server = server)
