# app.R
# QUT-STAQ 7E Centre of Mass mini concept inventory
# Single-file Shiny app: participant pre/post test + presenter dashboard

required <- c("shiny", "bslib", "ggplot2", "dplyr", "tidyr", "DT")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Install required packages first: install.packages(c(",
       paste(sprintf('"%s"', missing), collapse = ", "), "))")
}

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)

options(shiny.maxRequestSize = 5 * 1024^2)

APP_TITLE <- "7E Centre of Mass Challenge"
PRESENTER_PIN <- Sys.getenv("PRESENTER_PIN", unset = "7E2026")
DATA_FILE <- Sys.getenv("COM_DATA_FILE", unset = "com_responses.csv")

questions <- tibble::tribble(
  ~item, ~short, ~question, ~A, ~B, ~C, ~D, ~correct,
  "q1", "COM in empty space",
  "A uniform cardboard letter C is shown. Where could its centre of mass be?",
  "It must be somewhere in the cardboard.",
  "It could lie in the empty space inside the C.",
  "It must be at the centre of its bounding rectangle.",
  "It must lie directly below the highest point.", "B",
  "q2", "Symmetry",
  "A uniform cardboard T has a vertical line of symmetry. Which statement must be true?",
  "The COM is at the exact centre of its bounding rectangle.",
  "The COM lies somewhere on the line of symmetry.",
  "The COM is at the intersection of the two strokes.",
  "The COM is at the widest section.", "B",
  "q3", "Suspension method",
  "An irregular lamina is suspended from point A and a vertical plumb line is drawn. The process is repeated from point B. Where is the COM?",
  "At point A.",
  "Halfway between A and B.",
  "At the intersection of the two vertical lines.",
  "At the lowest point of the object.", "C",
  "q4", "Adding mass",
  "A small mass is attached to the far-right edge of a uniform cardboard character. What happens to its COM?",
  "It moves towards the added mass.",
  "It moves away from the added mass.",
  "It does not move.",
  "It moves vertically downward only.", "A",
  "q5", "Balance condition",
  "An object balances steadily on a narrow support. Which is the best explanation?",
  "Equal amounts of material must exist on each side.",
  "Its geometric centre must be directly above the support.",
  "The vertical line through its COM is appropriately positioned over the support.",
  "The object must be symmetrical.", "C"
)

empty_data <- function() {
  data.frame(
    timestamp = character(), id = character(), phase = character(),
    confidence = integer(), q1 = character(), q2 = character(),
    q3 = character(), q4 = character(), q5 = character(),
    score = integer(), session = character(), stringsAsFactors = FALSE
  )
}

read_store <- function() {
  if (!file.exists(DATA_FILE)) return(empty_data())
  tryCatch(read.csv(DATA_FILE, stringsAsFactors = FALSE), error = function(e) empty_data())
}

write_store <- function(dat) {
  tmp <- paste0(DATA_FILE, ".tmp")
  write.csv(dat, tmp, row.names = FALSE, na = "")
  if (!file.rename(tmp, DATA_FILE)) {
    file.copy(tmp, DATA_FILE, overwrite = TRUE)
    unlink(tmp)
  }
}

score_answers <- function(ans) {
  sum(vapply(seq_len(nrow(questions)), function(i) {
    identical(unname(ans[[questions$item[i]]]), questions$correct[i])
  }, logical(1)))
}

clean_id <- function(x) toupper(gsub("[^A-Za-z0-9]", "", trimws(x)))

question_ui <- function(i) {
  q <- questions[i, ]
  choices <- setNames(c("A", "B", "C", "D"), c(q$A, q$B, q$C, q$D))
  div(
    class = "question-card",
    div(class = "question-number", paste0("Question ", i, " of ", nrow(questions))),
    h4(q$question),
    radioButtons(q$item, NULL, choices = choices, selected = character(0), width = "100%")
  )
}

theme <- bs_theme(
  version = 5, bootswatch = "flatly",
  primary = "#2F80ED", secondary = "#667085",
  success = "#219653", warning = "#F2994A", danger = "#EB5757",
  base_font = font_google("Atkinson Hyperlegible"),
  heading_font = font_google("Atkinson Hyperlegible")
)

ui <- page_navbar(
  title = div(class = "brand", APP_TITLE),
  theme = theme, collapsible = TRUE, id = "nav",
  header = tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("\n      body { background:#f4f7fb; }\n      .navbar { box-shadow:0 2px 12px rgba(23,50,77,.14); }\n      .brand { font-weight:800; }\n      .hero { background:linear-gradient(135deg,#17324D,#2F80ED); color:white; padding:1.4rem; border-radius:18px; margin-bottom:1rem; }\n      .hero h2 { font-weight:800; margin-bottom:.35rem; }\n      .question-card,.panel-card { background:white; border:1px solid #dde4ee; border-radius:16px; padding:1.1rem; margin-bottom:1rem; box-shadow:0 4px 14px rgba(31,41,55,.06); }\n      .question-number { color:#2F80ED; font-weight:800; text-transform:uppercase; letter-spacing:.05em; font-size:.78rem; }\n      .form-check { border:1px solid #e3e8ef; border-radius:10px; padding:.65rem .75rem .65rem 2.2rem; margin:.45rem 0; }\n      .form-check:hover { background:#f5f9ff; border-color:#9dc4fa; }\n      .big-score { font-size:3.2rem; font-weight:900; color:#17324D; line-height:1; }\n      .metric { background:white; border-radius:14px; padding:1rem; border:1px solid #dde4ee; height:100%; }\n      .metric-label { color:#667085; font-size:.85rem; }\n      .metric-value { color:#17324D; font-weight:850; font-size:1.8rem; }\n      .phase-pre { color:#9B51E0; font-weight:800; } .phase-post { color:#2F80ED; font-weight:800; }\n      .btn-lg { border-radius:12px; font-weight:800; }\n      @media (max-width:768px){ .container-fluid {padding-left:.65rem;padding-right:.65rem;} .hero{padding:1rem;} }\n    "))
  ),
  nav_panel(
    "Take the check",
    div(class = "container-fluid", style = "max-width:900px;padding-top:1rem;",
      uiOutput("participant_ui")
    )
  ),
  nav_panel(
    "Presenter",
    div(class = "container-fluid", style = "max-width:1250px;padding-top:1rem;",
      uiOutput("presenter_ui")
    )
  ),
#  footer = div(class = "text-center text-muted small py-3",
#               "Anonymous diagnostic check. Use the same four-character code for PRE and POST.")
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    page = "landing", phase = NULL, participant_id = NULL,
    last_score = NULL, presenter_ok = FALSE, data_version = 0
  )

  data_now <- reactive({
    rv$data_version
    read_store()
  })

  output$participant_ui <- renderUI({
    if (rv$page == "landing") {
      tagList(
        div(class = "hero",
            h2("Locate it. Test it. Explain it."),
            p("A five-item mini concept inventory for the centre-of-mass workshop. Allow about 90 seconds.")),
        div(class = "panel-card",
          textInput("participant_id", "Your anonymous four-character code",
                    placeholder = "Example: W7K2", width = "100%"),
          helpText("Use the same code for PRE and POST. Do not enter your name, email or staff number."),
          radioButtons("phase", "Which check are you completing?",
                       choices = c("PRE: before the practical" = "pre",
                                   "POST: after the practical" = "post"),
                       selected = character(0)),
          sliderInput("confidence", "How confident are you about centre of mass?",
                      min = 1, max = 5, value = 3, step = 1),
          actionButton("begin", "Begin", class = "btn-primary btn-lg w-100")
        )
      )
    } else if (rv$page == "quiz") {
      tagList(
        div(class = "hero",
            h2(if (rv$phase == "pre") "PRE concept check" else "POST concept check"),
            p("Choose the best answer for every question. Your code is ", strong(rv$participant_id), ".")),
        lapply(seq_len(nrow(questions)), question_ui),
        actionButton("submit", "Submit responses", class = "btn-success btn-lg w-100"),
        br(), br(), actionButton("cancel", "Return to start", class = "btn-outline-secondary w-100")
      )
    } else {
      post_msg <- if (rv$phase == "pre") {
        tagList(h4("Thank you. Keep the answers unresolved for now."),
                p("Join the hands-on investigation, then return using the same code for the POST check."))
      } else {
        tagList(h4("Thank you. Your paired result will contribute to the live class evidence."),
                p("The presenter can now reveal the aggregated PRE/POST pattern."))
      }
      tagList(
        div(class = "panel-card text-center",
          div(class = "big-score", paste0(rv$last_score, "/", nrow(questions))),
          p("Items correct"), post_msg,
          actionButton("again", "Return to start", class = "btn-primary btn-lg")
        )
      )
    }
  })

  observeEvent(input$begin, {
    id <- clean_id(input$participant_id %||% "")
    if (nchar(id) < 4 || nchar(id) > 8) {
      showNotification("Use a memorable code of 4 to 8 letters or numbers.", type = "error")
      return()
    }
    if (is.null(input$phase) || !input$phase %in% c("pre", "post")) {
      showNotification("Choose PRE or POST.", type = "error")
      return()
    }
    rv$participant_id <- id
    rv$phase <- input$phase
    rv$page <- "quiz"
  })

  observeEvent(input$cancel, { rv$page <- "landing" })
  observeEvent(input$again, { rv$page <- "landing" })

  observeEvent(input$submit, {
    items <- questions$item
    ans <- setNames(lapply(items, function(x) input[[x]]), items)
    unanswered <- items[vapply(ans, function(x) is.null(x) || !nzchar(x), logical(1))]
    if (length(unanswered)) {
      showNotification(paste("Please answer all five questions. Missing:", paste(unanswered, collapse = ", ")),
                       type = "error", duration = 6)
      return()
    }
    sc <- score_answers(ans)
    new_row <- data.frame(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
      id = rv$participant_id, phase = rv$phase,
      confidence = as.integer(input$confidence),
      q1 = ans$q1, q2 = ans$q2, q3 = ans$q3, q4 = ans$q4, q5 = ans$q5,
      score = sc, session = session$token, stringsAsFactors = FALSE
    )
    dat <- read_store()
    # Retain the latest submission for each anonymous code and phase.
    dat <- dat[!(dat$id == rv$participant_id & dat$phase == rv$phase), , drop = FALSE]
    dat <- bind_rows(dat, new_row)
    tryCatch(write_store(dat), error = function(e) {
      showNotification(paste("Could not save:", e$message), type = "error", duration = NULL)
      return()
    })
    rv$data_version <- rv$data_version + 1
    rv$last_score <- sc
    rv$page <- "complete"
  })

  output$presenter_ui <- renderUI({
    if (!rv$presenter_ok) {
      div(class = "panel-card", style = "max-width:520px;margin:auto;",
          h3("Presenter dashboard"),
          passwordInput("pin", "Presenter PIN"),
          actionButton("login", "Open dashboard", class = "btn-primary w-100"),
          helpText("Set the environment variable PRESENTER_PIN before deployment. The local default is 7E2026."))
    } else {
      tagList(
        fluidRow(
          column(3, div(class="metric", div(class="metric-label","PRE responses"), div(class="metric-value", textOutput("n_pre", inline=TRUE)))),
          column(3, div(class="metric", div(class="metric-label","POST responses"), div(class="metric-value", textOutput("n_post", inline=TRUE)))),
          column(3, div(class="metric", div(class="metric-label","Paired participants"), div(class="metric-value", textOutput("n_paired", inline=TRUE)))),
          column(3, div(class="metric", div(class="metric-label","Mean paired gain"), div(class="metric-value", textOutput("mean_gain", inline=TRUE))))
        ), br(),
        div(class="panel-card",
          fluidRow(
            column(8, h3("Live concept change"), p("Correct-response percentage by item.")),
            column(4, div(class="d-flex gap-2 justify-content-end",
              actionButton("refresh", "Refresh", class="btn-outline-primary"),
              downloadButton("download", "Download CSV", class="btn-outline-success"),
              actionButton("reset", "Reset data", class="btn-outline-danger")
            ))
          ),
          plotOutput("item_plot", height="430px")
        ),
        fluidRow(
          column(6, div(class="panel-card", h4("Score distributions"), plotOutput("score_plot", height="300px"))),
          column(6, div(class="panel-card", h4("Paired PRE to POST"), plotOutput("paired_plot", height="300px")))
        ),
        div(class="panel-card", h4("Item results"), DTOutput("item_table")),
        div(class="panel-card", h4("Response records"),
            p(class="text-muted small","Anonymous codes are shown so duplicate/mismatched entries can be diagnosed."),
            DTOutput("raw_table"))
      )
    }
  })

  observeEvent(input$login, {
    if (identical(input$pin, PRESENTER_PIN)) rv$presenter_ok <- TRUE
    else showNotification("Incorrect PIN.", type="error")
  })
  observeEvent(input$refresh, { rv$data_version <- rv$data_version + 1 })

  paired_data <- reactive({
    dat <- data_now()
    if (!nrow(dat)) return(data.frame())
    dat %>% select(id, phase, score, confidence) %>%
      distinct(id, phase, .keep_all = TRUE) %>%
      pivot_wider(names_from = phase, values_from = c(score, confidence)) %>%
      filter(!is.na(score_pre), !is.na(score_post)) %>%
      mutate(gain = score_post - score_pre,
             normalised_gain = ifelse(score_pre < nrow(questions),
                                      gain / (nrow(questions) - score_pre), NA_real_))
  })

  output$n_pre <- renderText(sum(data_now()$phase == "pre"))
  output$n_post <- renderText(sum(data_now()$phase == "post"))
  output$n_paired <- renderText(nrow(paired_data()))
  output$mean_gain <- renderText({
    p <- paired_data()
    if (!nrow(p)) return("–")
    sprintf("%+.2f / 5", mean(p$gain, na.rm=TRUE))
  })

  item_summary <- reactive({
    dat <- data_now()
    if (!nrow(dat)) return(data.frame())
    dat %>%
      select(phase, all_of(questions$item)) %>%
      pivot_longer(-phase, names_to="item", values_to="answer") %>%
      left_join(questions %>% select(item, short, correct), by="item") %>%
      mutate(is_correct = answer == correct,
             phase = factor(phase, levels=c("pre","post"), labels=c("PRE","POST"))) %>%
      group_by(item, short, phase) %>%
      summarise(n=n(), correct=sum(is_correct), percent=100*mean(is_correct), .groups="drop")
  })

  output$item_plot <- renderPlot({
    s <- item_summary()
    validate(need(nrow(s), "Waiting for responses."))
    ggplot(s, aes(x=reorder(short, item), y=percent, fill=phase)) +
      geom_col(position=position_dodge(width=.72), width=.64) +
      geom_text(aes(label=sprintf("%.0f%%", percent)),
                position=position_dodge(width=.72), vjust=-.35, fontface="bold", size=4) +
      coord_cartesian(ylim=c(0,108)) +
      scale_fill_manual(values=c("PRE"="#9B51E0", "POST"="#2F80ED")) +
      labs(x=NULL, y="Correct responses (%)", fill=NULL) +
      theme_minimal(base_size=14) +
      theme(panel.grid.major.x=element_blank(), legend.position="top")
  })

  output$score_plot <- renderPlot({
    dat <- data_now()
    validate(need(nrow(dat), "Waiting for responses."))
    d <- dat %>% mutate(phase=factor(phase, levels=c("pre","post"), labels=c("PRE","POST")))
    ggplot(d, aes(x=factor(score, levels=0:5), fill=phase)) +
      geom_bar(position="dodge") +
      scale_fill_manual(values=c("PRE"="#9B51E0", "POST"="#2F80ED")) +
      labs(x="Score out of 5", y="Participants", fill=NULL) +
      theme_minimal(base_size=13) + theme(legend.position="top", panel.grid.major.x=element_blank())
  })

  output$paired_plot <- renderPlot({
    p <- paired_data()
    validate(need(nrow(p), "Waiting for matched PRE and POST codes."))
    long <- p %>% select(id, PRE=score_pre, POST=score_post) %>%
      pivot_longer(c(PRE,POST), names_to="phase", values_to="score") %>%
      mutate(phase=factor(phase, levels=c("PRE","POST")))
    ggplot(long, aes(phase, score, group=id)) +
      geom_line(alpha=.35, colour="#667085") + geom_point(aes(colour=phase), size=3) +
      scale_colour_manual(values=c("PRE"="#9B51E0", "POST"="#2F80ED"), guide="none") +
      scale_y_continuous(breaks=0:5, limits=c(0,5)) +
      labs(x=NULL, y="Score out of 5") + theme_minimal(base_size=13)
  })

  output$item_table <- renderDT({
    s <- item_summary()
    validate(need(nrow(s), "Waiting for responses."))
    tab <- s %>% select(short, phase, n, percent) %>%
      pivot_wider(names_from=phase, values_from=c(n,percent), names_sep="_") %>%
      mutate(`Change (percentage points)` = percent_POST - percent_PRE) %>%
      transmute(Concept=short, `PRE n`=n_PRE, `PRE correct`=round(percent_PRE,1),
                `POST n`=n_POST, `POST correct`=round(percent_POST,1),
                `Change (percentage points)`=round(`Change (percentage points)`,1))
    datatable(tab, rownames=FALSE, options=list(dom="t", pageLength=10)) %>%
      formatPercentage(c("PRE correct","POST correct"), digits=1)
  })

  output$raw_table <- renderDT({
    dat <- data_now()
    datatable(dat %>% arrange(desc(timestamp)), rownames=FALSE,
              options=list(pageLength=10, scrollX=TRUE))
  })

  output$download <- downloadHandler(
    filename = function() paste0("centre_of_mass_responses_", Sys.Date(), ".csv"),
    content = function(file) write.csv(data_now(), file, row.names=FALSE, na="")
  )

  observeEvent(input$reset, {
    showModal(modalDialog(
      title="Reset all response data?",
      "Download the CSV first if you want to retain the results.",
      footer=tagList(modalButton("Cancel"), actionButton("confirm_reset","Delete all data",class="btn-danger"))
    ))
  })
  observeEvent(input$confirm_reset, {
    write_store(empty_data())
    rv$data_version <- rv$data_version + 1
    removeModal()
    showNotification("Response data reset.", type="message")
  })
}

shinyApp(ui, server)
