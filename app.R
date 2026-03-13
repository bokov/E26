library(shiny)
library(dplyr)
library(googlesheets4)
library(shinyjs)
library(cookies)
library(bslib)

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

    # Add extra environment/session metadata
    client_ip <- NA_character_
    if (!is.null(session$request$HTTP_X_FORWARDED_FOR)) {
      client_ip <- as.character(session$request$HTTP_X_FORWARDED_FOR)
    } else if (!is.null(session$request$REMOTE_ADDR)) {
      client_ip <- as.character(session$request$REMOTE_ADDR)
    } else {
      client_ip <- "unknown"
    }
    user_agent <- if (!is.null(session$request$HTTP_USER_AGENT)) as.character(session$request$HTTP_USER_AGENT) else ""
    host <- if (!is.null(session$clientData$url_hostname)) as.character(session$clientData$url_hostname) else ""
    protocol <- if (!is.null(session$clientData$url_protocol)) as.character(session$clientData$url_protocol) else ""
    cookie_text <- if (!is.null(session$request$cookies)) {
      paste(names(session$request$cookies), collapse = ";")
    } else {
      ""
    }

    input_values$timestamp <- as.character(Sys.time())
    input_values$session_id <- as.character(session$token)
    input_values$client_ip <- client_ip
    input_values$user_agent <- user_agent
    input_values$host <- host
    input_values$protocol <- protocol
    input_values$cookie_names <- cookie_text

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

easter_theme <- bs_theme(
  version = 4,
  bg = "#f8f1e4",
  fg = "#3e2e15",
  primary = "#a66b2f",
  secondary = "#f7c77f",
  success = "#76c893",
  font_scale = 1.1,
  bootswatch = "flatly"
)

app_title <- "Easter Bunny Steampunk Clue Validator"

ui <- add_cookie_handlers(fluidPage(
  theme = easter_theme,
  useShinyjs(),
  tags$head(
    tags$style(HTML(
      ".shiny-input-container, .well { background: linear-gradient(135deg, #fdf3e7, #f6e5d5); border: 2px solid #b57834; box-shadow: 4px 4px 10px rgba(0,0,0,.16);}"
    )),
    tags$style(HTML(
      "body { background-image: url('https://i.imgur.com/cZHs71h.png'); background-size: cover; color: #333; }"
    )),
    tags$style(HTML(
      "h1, h2, h3, label { font-family: 'Cinzel', serif; }"
    ))
  ),
  titlePanel(app_title),
  sidebarLayout(
    sidebarPanel(
      textInput("user_key", "Enter key:", "", placeholder = "Type secret key here"),
      actionButton("check_btn", "Validate Key", class = "btn btn-warning"),
      selectInput("unlocked_values", "Unlocked clues:", choices = NULL),
      tags$p("Easter Bunny steampunk & mechanical mystery vibes: collect clues, unlock the next Hatchery challenge.")
    ),
    mainPanel(
      h3("Your next clue"),
      textOutput("result"),
      tags$div(style = "margin-top: 20px; padding: 10px; border: 2px dashed #a66b2f; background: rgba(247,199,127,0.23);",
               "Collected clues appear in the dropdown. Keep hunting!")
    )
  )
))

server <- function(input, output, session) {
  add_cookie_handlers(session)

  # Use a reactiveVal to store the result to display
  result_val <- reactiveVal("")
  unlocked_vals <- reactiveVal(character(0))

  # Track unlocked values via cookies (observe once per cookie change)
  observe({
    cookie_val <- cookies::get_cookie(session, "unlocked")
    loaded_values <- if (is.null(cookie_val) || is.na(cookie_val) || !nzchar(as.character(cookie_val))) {
      character(0)
    } else {
      split_val <- strsplit(as.character(cookie_val), "::", fixed=TRUE)[[1]]
      if (length(split_val) == 0 || (length(split_val) == 1 && split_val == "")) {
        character(0)
      } else {
        unique(split_val)
      }
    }
    unlocked_vals(loaded_values)
  })

  # observeEvent(input$check_btn, {
  #  req(input$user_key)
  #  kmatch <- data %>% filter(key == input$user_key)
  #  if (nrow(kmatch) > 0) {
  #    val <- kmatch$value[1]
  #  }
  #})

  # observe({
  #   updateSelectInput(session, "unlocked_values", choices = unlocked())
  # })

  # Observe all input changes (except every keystroke)
  observe({
    # Only log when an input changes (not every keystroke)
    # This triggers on blur for textInput, and on change for other controls
    reactiveValuesToList(input) # depend on all inputs
    isolate({
      safe_general_log_inputs(input, session)
    })
  })


  observeEvent(input$check_btn, {
    req(input$user_key)
    kmatch <- data %>% filter(key == input$user_key)
    if (nrow(kmatch) > 0) {
      next_clue <- paste("Your next clue:", kmatch$value[1])
      result_val(next_clue)
      # Add to unlocked if not already present
      new_unlocked <- unique(c(unlocked_vals(), kmatch$value[1]))
      cookies::set_cookie("unlocked", paste(new_unlocked, collapse = "::"))
      unlocked_vals(new_unlocked)
    } else {
      result_val("Clue not found. Try solving another key!")
    }
  })

  output$result <- renderText({
    result_val()
  })
}

shinyApp(ui = ui, server = server)
