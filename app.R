library(shiny)
library(dplyr)
library(googlesheets4)
library(shinyjs)
library(cookies)

# Read the CSV file (expects 'data.csv' with columns 'key' and 'value')
data <- read.csv('data.csv', stringsAsFactors = FALSE)

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

ui <- fluidPage(
  useShinyjs(),
  useShinyCookies(),
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
)

server <- function(input, output, session) {
  # Track unlocked values via cookies
  unlocked <- reactiveVal({
    cookie_val <- cookies::get_cookie(session, "unlocked")
    if (!is.null(cookie_val) && nzchar(cookie_val)) {
      unique(strsplit(cookie_val, "::", fixed=TRUE)[[1]])
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
      unlocked(new_unlocked)
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

shinyApp(ui, server)
