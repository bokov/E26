library(shiny)
library(dplyr)
library(googlesheets4)
library(shinyjs)
library(cookies)
library(bslib)
library(ggplot2)

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

app_title <- "Easter Bunny Clue Validator"

ui <- add_cookie_handlers(fluidPage(
  theme = easter_theme,
  useShinyjs(),
  tags$head(
    tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg"),
    tags$style(HTML(
      ".shiny-input-container, .well { background: linear-gradient(135deg, #fdf3e7, #f6e5d5); border: 2px solid #b57834; box-shadow: 4px 4px 10px rgba(0,0,0,.16); }"
    )),
    tags$style(HTML(
      "body { background-image: url('egg_factory_background.svg'); background-size: cover; background-repeat: no-repeat; background-attachment: fixed; color: #333; }"
    )),
    tags$style(HTML(
      "body { font-family: 'Cinzel', serif; }"
    )),
    tags$style(HTML(
      "h1, h2, h3, label { font-family: 'Cinzel', serif; }"
    ))
  ),
  tags$div(style = 'display:flex; align-items:center; gap:12px; margin-bottom:16px; padding: 8px 6px; border: 2px solid #b57834; border-radius: 14px; background: rgba(247, 199, 127, 0.35); box-shadow: 0 0 12px rgba(0,0,0,.12);',
           tags$img(src='steampunk_egg.svg', width=72, height=72, style='border:2px solid #a66b2f; border-radius:15%; background:rgba(255,255,255,0.8);'),
           tags$div(
             tags$h1(app_title, style='margin:0; font-size:2rem; color:#3e2e15; text-shadow: 1px 1px 2px rgba(0,0,0,0.3);'),
             tags$p("Hosted by the Easter Bunny - submit your key and unlock your next clue!", style='margin: 0; color:#553f20;')
           )
  ),
  sidebarLayout(
    sidebarPanel(
      textInput("user_key", "Enter key:", "", placeholder = "Type secret key here"),
      actionButton("check_btn", "Validate Key", class = "btn btn-warning"),
      selectInput("unlocked_values", "Unlocked clues:", choices = NULL),
      tags$p("The egg factory is humming. Enter the right key and unroll your next bunny clue adventure."),
      tags$img(src='steampunk_egg.svg', width='120', alt='Easter egg logo', style='display:block; margin-top:16px;')
    ),
    mainPanel(
      h3("Your next clue"),
      textOutput("result"),
      tags$div(style = "margin-top: 10px; margin-bottom: 16px; padding: 10px; border: 2px dashed #a66b2f; background: rgba(247,199,127,0.23);",
               "Collected clues appear in the dropdown. Keep hunting!"),
      h4("Egg-ometer Progress"),
      plotOutput("progress_plot", height = "200px"),
      tags$div(style = "margin-top: 10px; padding: 9px; border-radius: 10px; background: rgba(255, 246, 209, 0.8); color: #4f2f0e;",
               textOutput("progress_text"))
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
    # friggin NEVER give get_cookie and set_cookie as session argument!!!!
    cookie_val <- cookies::get_cookie("unlocked")
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

  # Keep select input updated with unlocked clues
  observe({
    updateSelectInput(session, "unlocked_values", choices = unlocked_vals())
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

  output$progress_plot <- renderPlot({
    unlocked <- length(unlocked_vals())
    total <- max(1, nrow(data))
    remaining <- max(0, total - unlocked)
    progress_df <- data.frame(
      status = factor(c("Cleared", "Remaining"), levels = c("Cleared", "Remaining")),
      count = c(unlocked, remaining)
    )
    ggplot(progress_df, aes(x = 1, y = count, fill = status)) +
      geom_col(width = 1, color = "#503a1b") +
      coord_polar(theta = "y") +
      scale_fill_manual(values = c("#76c893", "#f7c77f")) +
      theme_void() +
      theme(legend.position = "bottom", legend.title = element_blank(), plot.background = element_rect(fill = 'transparent', color = NA)) +
      labs(title = "Steampunk Egg-ometer")
  })

  output$progress_text <- renderText({
    unlocked <- length(unlocked_vals())
    total <- max(1, nrow(data))
    paste0("You have unlocked ", unlocked, " clue", ifelse(unlocked == 1, "", "s"), " out of ", total, " total clues.")
  })
}

shinyApp(ui = ui, server = server)
