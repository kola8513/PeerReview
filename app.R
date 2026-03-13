# Load necessary libraries
library(shiny)
library(bslib)
library(DBI)
library(RPostgres)
library(shinydashboard)
library(shinyjs)
library(fresh)
library(shinydashboardPlus)
library(shinyBS)
library(DT)
library(reticulate)
library(plotly)
library(uuid)
library(bcrypt)
library(rmarkdown)
library(knitr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(fmsb)
library(htmlwidgets)
library(ggradar)
library(fmsb)
library(reshape2)


py_require(
  packages = c("kaleido"),
  python_version = "3.11.13"
)

reticulate::py_run_string("import sys")

# --------------------------------- App Theme ----------------------------------
apptheme <- create_theme(
  adminlte_color(light_blue = "#003B73"),
  adminlte_sidebar(
    width = "250px",
    dark_bg = "#136377",
    # Darker background for sidebar
    dark_hover_bg = "#333333",
    dark_color = "#FFFFFF" # White text for sidebar
  ),
  adminlte_global(content_bg = "#eaeaea")
)

DARK_BLUE <- "#003B73"

# Bootstrap 4
#apptheme <- bslib::bs_theme(version = 4)

#------------------Database Connection------------------------------------------ 

get_db_con <- function() {
  DBI::dbConnect(
    RPostgres::Postgres(),
    dbname   = Sys.getenv("DB_NAME"),
    host     = Sys.getenv("DB_HOST"),
    #port     = as.integer(Sys.getenv("DB_PORT")),
    port <- as.integer(Sys.getenv("PGPORT", "5433")),
    user     = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    #sslmode  = 'require'
    sslmode = "disable"
  )
}

#----------------------FMSB RADAR CHART FUNCTIONS-------------------------------

make_radar_df_from_inputs_fmsb <- function(input, questions, group_name = "Aktueller Stand",
                                           binary_qnums = integer()) {
  
  vals <- sapply(questions, function(q) {
    qid <- q$id
    qnum <- suppressWarnings(as.numeric(sub("^[A-Za-z]+", "", qid)))
    v <- input[[paste0("Freitext", qid)]]
    
    if (is.null(v) || v == "" || v == "N/A" || is.na(v)) return(NA_real_)
    
    if (!is.na(qnum) && qnum %in% binary_qnums) {
      if (v == "Ja") return(1)
      if (v == "Nein") return(0)
      return(NA_real_)
    }
    
    suppressWarnings(as.numeric(v))
  })
  
  # Replace NA with 0
  vals[is.na(vals)] <- 0
  
  # Get axis names (question numbers)
  axis_names <- sapply(questions, function(q) sprintf("%d", as.numeric(gsub("\\D", "", q$label))))
  
  # Create max/min row (for scale 0-5)
  max_min <- data.frame(matrix(c(5, 0), nrow = 2, ncol = length(vals)))
  colnames(max_min) <- axis_names
  rownames(max_min) <- c("Max", "Min")
  
  # Create data row
  data_row <- data.frame(t(vals))
  colnames(data_row) <- axis_names
  rownames(data_row) <- group_name
  
  # Bind max/min with data (required by fmsb)
  df <- rbind(max_min, data_row)
  
  return(df)
}

#-------------------Create beautiful radar chart (fmsb style)-------------------

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), 
                                        vlcex = 1.0,
                                        caxislabels = NULL, 
                                        title = NULL,
                                        ...) {
  
  # Set up plot parameters
  op <- par(mar = c(1, 1, 2, 1))
  
  radarchart(
    data, 
    axistype = 1,
    
    # Customize the polygon
    pcol = color,                              # Line color
    pfcol = scales::alpha(color, 0.5),        # Fill color (semi-transparent)
    plwd = 2,                                  # Line width
    plty = 1,                                  # Line type (solid)
    
    # Customize the grid
    cglcol = "grey",                          # Grid color
    cglty = 1,                                # Grid line type
    cglwd = 0.8,                              # Grid line width
    
    # Customize the axis
    axislabcol = "grey",                      # Axis label color
    
    # Variable labels
    vlcex = vlcex,                            # Label size
    vlabels = vlabels,                        # Label text
    
    # Axis labels
    caxislabels = caxislabels,                # Custom axis labels
    
    # Title
    title = title,
    
    ...
  )
  
  par(op)
}

#----------------Wrapper function for radar charts-----------------------------
make_fmsb_radar_plot <- function(radar_df, title, colour = "#003B73") {
  
  # Defensive checks
  if (is.null(radar_df) || nrow(radar_df) == 0) {
    plot.new()
    text(0.5, 0.5, "Keine Daten vorhanden", cex = 1.5)
    return(invisible(NULL))
  }
  
  # Check if we have the required Max/Min rows
  if (!all(c("Max", "Min") %in% rownames(radar_df))) {
    plot.new()
    text(0.5, 0.5, "Ungültiges Datenformat", cex = 1.5)
    return(invisible(NULL))
  }
  
  # Get the data (skip Max/Min rows)
  data_rows <- radar_df[!rownames(radar_df) %in% c("Max", "Min"), , drop = FALSE]
  
  # Check if all values are zero
  if (nrow(data_rows) > 0 && all(data_rows == 0, na.rm = TRUE)) {
    plot.new()
    text(0.5, 0.5, "Keine Daten vorhanden", cex = 1.5)
    return(invisible(NULL))
  }
  
  # Create the plot
  create_beautiful_radarchart(
    data = radar_df,
    color = colour,
    vlabels = colnames(radar_df),
    vlcex = 1.0,
    caxislabels = c("0", "1", "2", "3", "4", "5"),
    title = title
  )
}

#------------------------------Styling for mandatory fields---------------------
# define here mandatory fields
fieldsMandatory <- c("name",
                     "Berufsgruppe",
                     "Internetadresse",
                     "Trag",
                     "Ansprechpartner",
                     "Email")

#---------------------------- to show to the asterisk above the mandatory fields
labelMandatory <- function(label) {
  tagList(label, span("*", class = "mandatory_star"))
}

#-------------------------list of questions for the apps-----------------------

mit_binary_global_qnums <- c(10, 20, 24)

# Helper: Given a vector of labels and a prefix, generate a list of questions with numbering continuing from start_idx
generate_questions <- function(labels,
                               prefix,
                               start_idx = 1,
                               info = "Bitte geben Sie weitere Details oder Beispiele zu dieser Frage an.") {
  n <- length(labels)
  lapply(seq_along(labels), function(i) {
    global_number <- start_idx + i - 1  # Global: 7, 8, 9, 10...
    local_number <- i  # Local within section: 1, 2, 3, 4...
    
    list(
      id = paste0(prefix, global_number),  # ID uses global: M7, M8, M9...
      number = global_number,  # Global number for filtering binary questions
      local_number = local_number,  # Local number for display in plots
      label = labels[i],  # Clean label without number
      info = info
    )
  })
}

#------------------------------ Führung-----------------------------------------
fuehrung_labels <- c(
  "Welche Strategie und Ziele verfolgen Sie?",
  "Wie wird die Strategie kommuniziert?",
  "Haben Sie Ihre Risiken identifiziert und wie gehen Sie mit Risiken um?",
  "Welche Back-up-Konzepte gibt es?",
  "Wie gehen Sie mit Veränderungen um?",
  "Wie werden Mitarbeiter ermutigt, sich an Planungen und Umsetzungen von Veränderungen zu beteiligen?"
)
fuehrung_questions <- generate_questions(fuehrung_labels, "F", 1)

#----------------------------- Mitarbeitende -----------------------------------
mit_labels <- c(
  "Welche Überlegungen gibt es zur Personalplanung für einen längerfristigen Zeitraum?",
  "Betreiben Sie aktive Personalakquise Wenn ja, wie?",
  "Wie werden Dienst-, Urlaubs-, Fort-, Weiterbildungs- und andere wiederkehrenden Pläne erstellt und zwischen den Berufsgruppen abgestimmt?",
  "Gibt es regelmäßige Laborbesprechungen?",
  "Mit welchen Besprechungsstrukturen und Kommunikationswerkzeugen wird eine inhaltlich und zeitlich effiziente Kommunikation erreicht?",
  "Wie wird ein wertschätzender Umgang hierarchieübergreifend und interprofessionell erreicht und befördert?",
  "Welche Unterstützung wird geboten?",
  "Wie wird gefördert?",
  "Führt die Laborleitung regelmäßig strukturierte Mitarbeitergespräche?",
  "Wie entwickeln Sie Führungs- und Fachkompetenz (Fort- und Weiterbildung) in den einzelnen Berufsgruppen?",
  "Welche Strukturen und Maßnahmen fördern das selbstständige Handeln der Mitarbeitenden im Rahmen Ihrer fachlichen Qualifikation?",
  "Gibt es eine Übersicht über die benötigten Kompetenzen?",
  "Ist ausreichend qualifiziertes Personal für [Prozess/Aufgabe/System] vorhanden und sind Vertretungsregelungen etabliert?",
  "Werden Wissensmonopole vermieden?",
  "Kann das Personal ersetzt werden, während der alte Stelleninhaber noch da ist (direkte Informationsweitergabe)?",
  "Wie können Mitarbeitende auf Informationen zugreifen (z.B. Internet, Literatur)?",
  "Wie ist die Einarbeitung und Inübungshaltung räumlich, zeitlich und inhaltlich für die einzelnen Berufsgruppen organisiert?",
  "Gibt es Maßnahmen zur Teamentwicklung?",
  "Welche Maßnahmen gibt es zur Fehlerkultur?"
)
mit_questions <- generate_questions(mit_labels, "M", length(fuehrung_questions) + 1)

#------------------------- Patient und Angehörige-------------------------------
pat_labels <- c(
  "Wie werden Patienten eindeutig identifiziert?",
  "Werden Plausibilitätskontrollen durchgeführt?",
  "Wie gehen Sie mit Patientenstammdatenänderungen um?",
  "Welche Unterstützung gibt das Labor bei der korrekten Probengewinnung und beim Probentransport?",
  "Werden Aspekte des Patient Blood Management berücksichtigt?",
  "Wie ist die Umsetzung von Datenschutzaspekten im Labor geregelt?",
  "Wie ist die Befundauskunft geregelt?",
  "Gibt es Prozesse, in denen Patientenmaterial vom Laborpersonal herausgegeben wird?",
  "Wie wird bei Ihnen das Gendiagnostikgesetz umgesetzt?"
)
pat_questions <- generate_questions(pat_labels,
                                    "P",
                                    length(fuehrung_questions) + length(mit_questions) + 1)

#------------------- Einsender und Kooperationspartner--------------------------
ein_labels <- c(
  "Pflegen Sie den Kontakt zu Ihren Einsendern / Interessengruppen?",
  "Findet ein regelmäßiger Informationsaustausch mit Ihren Hauptlieferanten und Dienstleistern statt?",
  "Wie gehen Sie mit Lieferabrissen um?",
  "Wie ist in Ihrem Labor das Materialmanagement (Bestellwesen) geregelt?",
  "Beschreiben und bewerten Sie das Wartungskonzept Ihrer Laborgeräte.",
  "Werden Ausfallzeiten Ihrer Geräte systematisch registriert und ausgewertet?",
  "Beschreiben und bewerten Sie den Process des Fremdversandes.",
  "Wie geht das Labor mit Beschwerden um und erfolgt eine regelmäßige Auswertung?",
  "Betreut Ihr Labor die POCT Diagnostik Ihrer Einsender?",
  "Wie ist die Betreuung strukturiert?"
)
ein_questions <- generate_questions(
  ein_labels,
  "E",
  length(fuehrung_questions) + length(mit_questions) + length(pat_questions) + 1
)

#-------- Qualitätsindikatoren und Technische und Medizinische Validation-------
qual_labels <- c(
  "Welche Verfahren zur Auftragsgenerierung bietet das Labor dem Einsender an (z. B. order entry etc.)?",
  "Gibt es Ausfallkonzepte für die elektronische Auftragsgenerierung?",
  "Erfolgt nach Auftragsgenerierung eine Kontrolle der Anforderung für Blutprodukte?",
  "Wie werden Anforderungsprofile definiert?",
  "Sind bei Ihnen diagnostische Pfade definiert?",
  "Monitoring beim Probentransport?",
  "Welche präanalytischen Prüfungen erfolgen im Labor?",
  "Werden Rückmeldungen der Einsender bei der Auswahl / Durchführung der Analytik berücksichtigt?",
  "Welche Kriterien spielen bei der Auswahl, z.B. Neueinführung der Analytik eine Rolle?",
  "Wie sieht Ihr Probenfluss-Konzept aus?",
  "Kann für bestimmte Messgrößen die TAT im Labor ermittelt werden?",
  "Werden Analysenergebnisse ärztlicherseits ergänzend textlich befundet?",
  "Wie erfolgt die Probenarchivierung?",
  "Welche Service- und Beratungsleistungen erbringen Sie?",
  "Wie wird bei Ihnen die interne Qualitätssicherung durchgeführt und gibt es Maßnahmen, die über die Forderungen der Rili-BÄK hinausgehen?",
  "Wie wird bei Ihnen die externe Qualitätssicherung durchgeführt und gibt es regelmäßige Auswertungen der RV?",
  "Wie und wann erfolgt die technischen und der medizinischen Validation?"
)
qual_questions <- generate_questions(
  qual_labels,
  "Q",
  length(fuehrung_questions) + length(mit_questions) + length(pat_questions) + length(ein_questions) + 1
)

#------------------------------ UI generation-----------------------------------
make_ui <- function(question_list, score_prefix = "Freitext", text_prefix = "Freitext_text") {
  lapply(question_list, function(q) {
    btn_id <- paste0("info_", q$id)
    div(
      tags$h5(
        # ✅ Use GLOBAL number for UI display
        strong(paste0(q$number, ". ", q$label)),
        bsButton(btn_id, label = "", icon = icon("info-circle"), style = "info", size = "extra-small", class = "btn-xs info-button", `data-style` = "margin-left: 5px; padding: 2px 6px;")
      ),
      bsPopover(
        id = btn_id,
        title = paste("Info zu Frage", q$id),
        content = q$info,
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      ),
      radioButtons(
        paste0(score_prefix, q$id),
        label = NULL,
        choices = c("N/A", 1:5),
        selected = "N/A",
        inline = TRUE
      ),
      textAreaInput(
        paste0(text_prefix, q$id),
        label = NULL,
        placeholder = "Text eingeben...",
        height = "100px",
        width = "100%"
      )
    )
  })
}


input_prefixes <- list(
  fuehrung      = list(score = "Freitext", text = "Freitext_text"),
  mitarbeitende = list(score = "Freitext", text = "Freitext_text"),
  patienten     = list(score = "Freitext", text = "Freitext_text"),
  einsender     = list(score = "Freitext", text = "Freitext_text"),
  qualitaet     = list(score = "Freitext", text = "Freitext_text")
)


#-------------------- Define the topic_questions list -------------------------
topic_questions <- list(
  fuehrung      = fuehrung_questions,
  mitarbeitende = mit_questions,
  patienten     = pat_questions,
  einsender     = ein_questions,
  qualitaet     = qual_questions
)

#----------------- Friendly display labels for each topic----------------------
topic_labels <- c(
  fuehrung      = "Führung",
  mitarbeitende = "Mitarbeitende",
  patienten     = "Patienten und Angehörige",
  einsender     = "Einsender & Kooperationspartner",
  qualitaet     = "Qualitätsindikatoren & Validierung"
)

# Normalize short aliases to canonical keys
resolve_topic_key <- function(key) {
  alias <- c(
    pat  = "patienten",
    ein  = "einsender",
    qual = "qualitaet"
  )
  if (key %in% names(alias)) alias[[key]] else key
}

# Return the friendly label for a topic key
get_topic_label <- function(topic_key) {
  canonical <- resolve_topic_key(topic_key)
  if (canonical %in% names(topic_labels)) {
    return(unname(topic_labels[[canonical]]))
  }
  canonical
}

#----------------- Colors for each topic (for use in plots)---------------------

# Colors for topics
topic_colors <- c(
  fuehrung      = "#1976D2",  # Deep Blue
  mitarbeitende = "#F57C00",  # Deep Orange
  patienten     = "#388E3C",  # Green
  einsender     = "#D32F2F",  # Red
  qualitaet     = "#FBC02D"   # Amber
)

# ✅ Colorblind-friendly palette for Berufsgruppen
berufsgruppe_colors <- c(
  "MTL"                   = "#0072B2",  # Blue
  "Wissenschaftler(-in)"  = "#D55E00",  # Vermillion/Orange
  "Ärzte"                 = "#009E73",  # Bluish Green
  "IT"                    = "#CC79A7"   # Reddish Purple
)

# If you need more colors (for future expansion):
colorblind_palette <- c(
  "#999999",  # Gray
  "#E69F00",  # Orange
  "#56B4E9",  # Sky Blue
  "#009E73",  # Bluish Green
  "#F0E442",  # Yellow
  "#0072B2",  # Blue
  "#D55E00",  # Vermillion
  "#CC79A7"   # Reddish Purple
)
#------------------ Create UI lists for each topic------------------------------
fuehrung_ui <- make_ui(fuehrung_questions,
                       input_prefixes$fuehrung$score,
                       input_prefixes$fuehrung$text)

mit_ui <- make_ui(
  mit_questions,
  input_prefixes$mitarbeitende$score,
  input_prefixes$mitarbeitende$text
)

pat_ui <- make_ui(pat_questions,
                  input_prefixes$patienten$score,
                  input_prefixes$patienten$text)
ein_ui <- make_ui(ein_questions,
                  input_prefixes$einsender$score,
                  input_prefixes$einsender$text)
qual_ui <- make_ui(qual_questions,
                   input_prefixes$qualitaet$score,
                   input_prefixes$qualitaet$text)


#-------------------------------- Helper function for DRY project analysis tabs
project_dashboard_analysis_tabs <- function() {
  tabBox(
    width = 12,
    tabPanel("Führung", tabsetPanel(
      tabPanel(
        "Selbstbewertung Textantworten ansehen",
        DT::dataTableOutput("fuehrung_vergleich_table")
      ),
      tabPanel(
        "Plot",
        div(
          style = "width:100%; height:80vh;", # 80% of viewport height
          plotlyOutput("fuehrung_spider", width = "100%", height = "100%")
        )
      ),
      tabPanel(
        "Statistik & Textantworten",
        h4("Statistik (Min, Mittel, Max):"),
        tableOutput("fuehrung_stats_table"),
        h4("Alle Antworten (pro User, inkl. Begründung):"),
        DT::dataTableOutput("fuehrung_responses")
      )
    )),
    tabPanel(
      "Mitarbeitende",
      tabsetPanel(
        tabPanel(
          "Selbstbewertung Textantworten ansehen",
          DT::dataTableOutput("mitarbeitende_vergleich_table")
        ),
        tabPanel(
          "Plot",
          plotlyOutput("mitarbeitende_spider", width = "100%", height = "600px")
        ),
        tabPanel(
          "Statistik & Textantworten",
          h4("Statistik (Min, Mittel, Max):"),
          tableOutput("mitarbeitende_stats_table"),
          h4("Alle Antworten (pro User, inkl. Begründung):"),
          DT::dataTableOutput("mitarbeitende_responses")
        )
      )
    ),
    tabPanel(
      "Patienten & Angehörige",
      tabsetPanel(
        tabPanel(
          "Selbstbewertung Textantworten ansehen",
          DT::dataTableOutput("pat_vergleich_table")
        ),
        tabPanel(
          "Plot",
          plotlyOutput("pat_spider", width = "100%", height = "600px")
        ),
        tabPanel(
          "Statistik & Textantworten",
          h4("Statistik (Min, Mittel, Max):"),
          tableOutput("pat_stats_table"),
          h4("Alle Antworten (pro User, inkl. Begründung):"),
          DT::dataTableOutput("pat_responses")
        )
      )
    ),
    tabPanel(
      "Einsender & Kooperationspartner",
      tabsetPanel(
        tabPanel(
          "Selbstbewertung Textantworten ansehen",
          DT::dataTableOutput("ein_vergleich_table")
        ),
        tabPanel(
          "Plot",
          plotlyOutput("ein_spider", width = "100%", height = "600px")
        ),
        tabPanel(
          "Statistik & Textantworten",
          h4("Statistik (Min, Mittel, Max):"),
          tableOutput("ein_stats_table"),
          h4("Alle Antworten (pro User, inkl. Begründung):"),
          DT::dataTableOutput("ein_responses")
        )
      )
    ),
    tabPanel(
      "Qualitätsindikatoren",
      tabsetPanel(
        tabPanel(
          "Selbstbewertung Textantworten ansehen",
          DT::dataTableOutput("qual_vergleich_table")
        ),
        tabPanel(
          "Plot",
          plotlyOutput("qual_spider", width = "100%", height = "600px")
        ),
        tabPanel(
          "Statistik & Textantworten",
          h4("Statistik (Min, Mittel, Max):"),
          tableOutput("qual_stats_table"),
          h4("Kürzel-Legende:"),
          tableOutput("qual_table"),
          h4("Alle Antworten (pro User, inkl. Begründung):"),
          DT::dataTableOutput("qual_responses")
        )
      )
    )
  )
}

#--------------------------- Main UI Definition --------------------------------
ui <- dashboardPage(
  title = "Dashboard",
  header = dashboardHeader(title = "Peer Review", tags$li(
    class = "dropdown", actionLink("logout_button", "Logout", icon = icon("sign-out-alt"))
  )),
  sidebar = dashboardSidebar(
    minified = FALSE,
    collapsed = FALSE,
    uiOutput("sidebar_menu_ui")
  ),
  body = dashboardBody(
    useShinyjs(),

    use_theme(apptheme),
    
    tags$head(tags$style(
      HTML(
        "
        h1, h2, h3, h4, h5, h6, .question-title, .section-title, strong {
          color: #003B73 !important;
        }
        label { color: #003B73 !important; }
        .box .box-title { color: #003B73 !important; }
        .nav-tabs > li > a { color: #003B73 !important; }
        .content-wrapper, .right-side { background-color: #eaeaea !important; }
        .main-sidebar { background-color: #212121; }
        .sidebar-menu > li.active > a { border-left-color: #003B73 !important; }
        .sidebar-menu > li > a { color: #FFFFFF; }
        .sidebar-menu > li > a:hover { background-color: #333333 !important; }
        .sidebar-menu .treeview-menu > li > a { color: #FFFFFF; }
        .sidebar-menu .treeview-menu > li.active > a { color: #003B73 !important; }
        .sidebar-menu .treeview-menu > li > a:hover { background-color: #333333 !important; }
        .skin-blue .main-header .navbar .sidebar-toggle:hover { background-color: #003B73 !important; }
        .small-box.bg-aqua { background-color: #003B73 !important; color: #fff !important; }
        .small-box.bg-purple-custom { background-color: #6A057F !important; color: #fff !important; }
        .small-box.bg-green { background-color: #28a745 !important; color: #fff !important; }
        .small-box.bg-yellow { background-color: #ffc107 !important; color: #fff !important; }
        .small-box.bg-red { background-color: #dc3545 !important; color: #fff !important; }
        /* ---- Force white text on ALL buttons, all states ---- */
        .btn,
        .btn:hover,
        .btn:focus,
        .btn:active,
        .btn:visited,
        .btn:focus-visible {
          color: #ffffff !important;
        }
        
        /* If icons are used inside buttons */
        .btn .fa,
        .btn .glyphicon {
          color: #ffffff !important;
        }
        
        /* Optional: prevent Bootstrap focus outline color bleed */
        .btn:focus {
          outline: none !important;
          box-shadow: none !important;
        }"
      )
    )),
    
    
    # Explicitly set default selected tab
    tags$script(HTML("
    $(document).ready(function() {
      // Ensure start tab is selected on load
      if (!$('.sidebar-menu .active').length) {
        $('.sidebar-menu li[data-value=\"start\"]').addClass('active');
        $('.tab-pane[data-value=\"start\"]').addClass('active');
      }
    });
  ")),
    
    tags$head(tags$style(HTML(
      "
  "
    )), tags$script(
      HTML(
        "
    $(document).on('click', '.btn-copy', function() {
      var codeToCopy = $(this).data('code');
      var $tempInput = $('<input>');
      $('body').append($tempInput);
      $tempInput.val(codeToCopy).select();
      document.execCommand('copy');
      $tempInput.remove();

      // Send the copied code back to Shiny for notification
      Shiny.setInputValue('copied_code', codeToCopy, {priority: 'event'});

      // Optional: Give visual feedback on the button
      var originalText = $(this).text();
      $(this).text('Copied!').prop('disabled', true);
      var button = this;
      setTimeout(function() {
        $(button).text(originalText).prop('disabled', false);
      }, 1500);
    });
  "
      )
    )),
    
    tags$head(
      tags$style(HTML("
    .shiny-notification.custom-success {
      background-color: #4CAF50 !important;  /* Green */
      color: white !important;
      font-size: 1.5em !important;
      text-align: center !important;
      top: 40% !important;
      left: 50% !important;
      transform: translate(-50%, -50%) !important;
      min-width: 400px;
      max-width: 90vw;
      z-index: 9999;
    }
  "))
    ),
    
    tags$head(
      tags$style(HTML("
        .btn-file {
          background-color: #3c8dbc !important;
          color: white !important;
          border: none !important;
          padding: 6px 12px !important;
          font-weight: 500 !important;
          border-radius: 4px !important;
        }
        .btn-file:hover {
          background-color: #2f7cad !important;
        }
      "))
    ),
    
    tags$script(HTML("
  Shiny.addCustomMessageHandler('centeredSuccess', function(params) {
    var id = 'custom-centered-success-' + Math.floor(Math.random()*100000);
    var notif = $('<div></div>')
      .addClass('shiny-notification shiny-notification-message custom-success')
      .attr('id', id)
      .css({position: 'fixed'});
    notif.text(params.message);
    $('body').append(notif);
    setTimeout(function() {
      notif.fadeOut(400, function() { $(this).remove(); });
    }, params.duration || 5000);
  });
")),
    
    tags$head(tags$style(
      HTML(
        '
      .main-header .logo {
        font-family: "Georgia";
        font-weight: bold;
        font-size: 24px;
      }
    '
      )
    )),

    #----------------------------Guide for Admin-------------------------------
    
    tabItems(
      tabItem(
        tabName = "adminGuide",
        h2("Admin Guide: Verwaltung und Steuerung des Systems"),
        p(
          "Nutzen Sie die unten aufgeführten Schritte, um Ihr Peer Review-System effizient zu verwalten."
        ),
        hr(),
        fluidRow(
          box(
            title = tags$span("1. Peer Review erstellen und verwalten", style = "color:white;"),
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            status = "primary",
            icon = icon("folder-plus"),
            HTML("Verwalten Sie Projekte, um Teams und Mitglieder in den Workflow einzubeziehen."),
            tags$br(),
            tags$br(),
            actionButton(
              "goToCreateProject",
              "Projekte verwalten",
              icon = icon("arrow-right"),
              style = "background-color: #003B73; color: white; border: none; padding: 10px 20px; font-weight: 500;"
            )
          ),
          box(
            title = tags$span("2. Benutzer und Rollen verwalten", style = "color:white;"),
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            status = "success",
            icon = icon("user-cog"),
            HTML(
              "Erstellen, laden und assign Benutzer zu Projekten basierend auf Rollen."
            ),
            tags$br(),
            tags$br(),
            actionButton(
              "goToManageUsers",
              "Benutzerrollen verwalten",
              icon = icon("arrow-right"),
              style = "background-color: #00a65a; color: white; border: none; padding: 10px 20px; font-weight: 500;"
            )
          ),
          box(
            title = tags$span("3. Einladungscodes generieren", style = "color:white;"),
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            status = "info",
            icon = icon("ticket-alt"),
            HTML(
              "Generieren und verwalten Sie Einladungscodes für unterschiedliche Rollen wie Laborleitung, Kollegen, oder Peer-Reviewer."
            ),
            tags$br(),
            tags$br(),
            actionButton(
              "goToInvitationCodes",
              "Einladungscodes verwalten",
              icon = icon("arrow-right"),
              style = "background-color: #00c0ef; color: white; border: none; padding: 10px 20px; font-weight: 500;"
            )
          ),
          box(
            title = tags$span("4. Fortschritte überwachen und Berichte generieren", style = "color:white;"),
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            status = "warning",
            icon = icon("poll"),
            HTML(
              "Überwachen Sie Fortschritte, und laden Sie Berichte herunter, um Einblicke in Projektstatus zu erhalten."
            ),
            tags$br(),
            tags$br(),
            actionButton(
              "goToReports",
              "Fortschritte überwachen",
              icon = icon("arrow-right"),
              style = "background-color: #f39c12; color: white; border: none; padding: 10px 20px; font-weight: 500;"
            )
            # complete summary download
            # downloadButton(
            #   "downloadZusammenfassung",
            #   "Gesamtbericht herunterladen (Labinfo + Selbstbewertung)",
            #   icon = icon("file-pdf"),
            #   style = "background-color: #003B73; color: white; font-weight: bold; padding: 10px 20px;"
            # )
            
            
            
          )
        )
      )
    ),
    
  #----------------------------Laborleitung Guide------------------------------- 
  
  # status = "primary"   -> #3c8dbc (blue)
  # status = "success"   -> #00a65a (green)
  # status = "info"      -> #00c0ef (light blue/cyan)
  # status = "warning"   -> #f39c12 (orange)
  # status = "danger"    -> #dd4b39 (red)
    
    tabItems(
      tabItem(
        tabName = "laborLeiterGuide",
        h2("Laborleitung Guide: Ihre Schritte zum Erfolg im Peer Review"),
        p(
          "Folgen Sie diesen Schritten, um Ihre Peer Review und Teammitglieder effizient zu verwalten."
        ),
        hr(),
        fluidRow(
          box(
            title = tags$span("1. Peer Review erstellen", style = "color:white;"),
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            status = "primary",
            icon = icon("plus-circle"),
            HTML("Starten Sie einen neuen Peer Review, indem Sie ihn im System anlegen."),
            tags$br(),
            actionButton(
              "btnGoToCreateProject",
              "Peer Review erstellen",
              icon = icon("arrow-right"),
              style = "background-color: #003B73; color: white; border: none; padding: 10px 20px; font-weight: 500;"
              
            )
          ),
          
          box(
            title = tags$span("2. Laborinformation ausfüllen (einmal pro Peer Review)", style = "color:white;"),
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            status = "success",
            icon = icon("flask"),
            HTML(
              "Geben Sie die wesentlichen Informationen zu Ihrem Labor für das aktuelle Peer Review ein. Diese Angaben sind für Berichte notwendig und können später nicht geändert werden."
            ),
            tags$br(),
            actionButton(
              "btnGoToLabInfo",
              " Laborinformation ausfüllen",
              icon = icon("arrow-right"),
              style = "background-color: #00a65a; color: white; border: none; padding: 10px 20px; font-weight: 500;"
              
            )
          ),
          
          box(
            title = tags$span("3. Selbstbewertung ausfüllen", style = "color:white;"),
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            status = "danger",
            icon = icon("file-alt"),
            HTML(
              "Bitte füllen Sie den Fragebogen für das aktuelle Peer Review vollständig aus. Die angegebenen Informationen sind für die weitere Auswertung und Berichterstellung erforderlich."
            ),
            tags$br(),
            actionButton(
              "btnGoToFragebogen",
              "Selbstbewertung ausfüllen",
              icon = icon("arrow-right"),
              style = "background-color: #dd4b39; color: white; border: none; padding: 10px 20px; font-weight: 500;"
              
            )
          ),
          
          box(
            title = tags$span("4. Zusammenfassung Selbstbewertungen", style = "color:white;"),
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            status = "warning",
            icon = icon("poll-h"),
            HTML(
              "Behalten Sie den Fortschritt und die Eingaben Ihrer eingeladenen Kollegen im Blick."
            ),
            tags$br(),
            actionButton(
              "btnGoToResponses",
              "Fortschritt Selbstbewertung",
              icon = icon("arrow-right"),
              style = "background-color: #f39c12; color: white; border: none; padding: 10px 20px; font-weight: 500;"
              
            )
          ),
          
          box(
            title = tags$span("5. Leading Peer einladen", style = "color:white;"),
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            width = 12,
            icon = icon("user-plus"),
            HTML(
              "Laden Sie den Leading Peer ein, um das Peer Review Besuch zu organisieren. 
              Erst nach dem Abschluss der Datenerhebung kann der Leading Peer seine Einschätzung abgeben."
            ),
            tags$br(),
            actionButton(
              "btnGoToInviteUsers",
              "Leading Peer einladen",
              icon = icon("arrow-right"),
              style = "background-color: #003B73 !important; color: white !important; border: none !important; padding: 10px 20px !important; font-weight: 500 !important;"
            )
          ),
        )
      ),
      
      # box(
      #   title = tags$span("4. Kollegen einladen", style = "color:white;"),
      #   solidHeader = TRUE,
      #   collapsible = TRUE,
      #   width = 12,
      #   status = "primary",
      #   icon = icon("user-plus"),
      #   HTML(
      #     "Erstellen und versenden Sie Einladungscodes, um Kollegen und den Leading Peer zur Teilnahme am Peer Review einzuladen."
      #   ),
      #   tags$br(),
      #   actionButton(
      #     "btnGoToInviteUsers",
      #     "Benutzer einladen",
      #     icon = icon("arrow-right")
      #   )
      # ),
      
      # box(
      #   title = tags$span("5. Selbstbewertung abschließen", style = "color:white;"),
      #   solidHeader = TRUE,
      #   status = "danger",
      #   collapsible = TRUE,
      #   width = 12,
      #   icon = icon("check-double"),
      #   HTML(
      #     "Bitte überprüfen Sie alle erfassten Daten und schließen Sie die Datenerhebung für diesen Peer Review ab."
      #   ),
      #   tags$br(),
      #   actionButton(
      #     "btnConfirmFinalizeProject",
      #     "Selbstbewertung abschließen",
      #     icon = icon("arrow-right")
      #   )
      # ),
      
      # box(
      #   title = tags$span("7. Berichte herunterladen", style = "color:white;"),
      #   solidHeader = TRUE,
      #   status = "success",
      #   collapsible = TRUE,
      #   width = 12,
      #   icon = icon("chart-line"),
      #   HTML(
      #     "Hier können Sie die Berichte für Ihr Peer Review herunterladen. Wählen Sie das gewünschte Peer Review aus und laden Sie den entsprechenden Bericht als PDF herunter."
      #   ),
      #   tags$br(),
      #   tags$div(
      #     style = "display: flex; gap: 10px; flex-wrap: wrap;",
      #     actionButton(
      #       "btnGoToSelfAssessmentReport",
      #       "Selbstbewertung Bericht herunterladen",
      #       icon = icon("file-pdf")
      #       #class = "btn-primary"
      #     ),
      #     actionButton(
      #       "btnGoToCompleteReport",
      #       "Gesamten Bericht herunterladen",
      #       icon = icon("file-alt")
      #       #class = "btn-success"
      #     )
      #   )
      # )
      
#----------------------------- landing page ------------------------------------

tabItem(
  tabName = "start",

  # Page-level wrapper (full height + soft background)
  tags$div(
    style = "
      min-height: calc(100vh - 60px);
      padding: 24px 12px;
      background: linear-gradient(180deg, #f6f8fc 0%, #ffffff 60%);
      display: flex;
      justify-content: center;
      align-items: flex-start;
    ",

    tags$div(
      style = "width: 100%; max-width: 1100px;",

      # --- Minimal CSS for a modern look ---
      tags$style(HTML("
        .landing-card { border-radius: 16px; box-shadow: 0 10px 30px rgba(0,0,0,0.08); border: 0; }
        .landing-hero h1 { margin: 0 0 6px 0; font-size: 34px; font-weight: 800; color: #0f172a; }
        .landing-hero .subline { font-size: 18px; color: #334155; margin-bottom: 14px; }
        .landing-hero p { color: #475569; font-size: 15px; line-height: 1.65; margin-bottom: 10px; }
        .login-title { font-size: 18px; font-weight: 700; margin-bottom: 10px; color: #0f172a; }
        .btn-primary.custom-welcome-btn { border-radius: 10px; font-weight: 700; }
        .muted-note { font-size: 13px; color: #64748b; margin-top: 10px; }
        .feature-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 14px; }
        .feature-item { padding: 14px; border: 1px solid #e2e8f0; border-radius: 14px; background: #fff; }
        .feature-item .icon { font-size: 22px; margin-bottom: 6px; }
        .feature-item .title { font-weight: 700; color: #0f172a; margin-bottom: 3px; }
        .feature-item .text { color: #475569; font-size: 14px; line-height: 1.5; }
        @media (max-width: 992px) { .feature-grid { grid-template-columns: 1fr; } }
        .footer-links a { color: #475569; margin-right: 14px; }
        .footer-links a:hover { text-decoration: underline; }
      ")),

      # ---------------------------- Top: Logo row -----------------------------
      tags$div(
        style = "
          display:flex; justify-content: space-between; align-items: center;
          margin-bottom: 16px; padding: 0 6px;
        ",
        tags$div(
          style = "display:flex; align-items:center; gap:16px; flex-wrap: wrap;",
          tags$img(src = "UMO-Logo.svg", height = "80px", alt = "UMO Logo"),
          tags$img(src = "logo_inquam.gif", height = "50px", alt = "Inquam Logo")
        ),
        tags$div(style = "color:#64748b; font-size:13px;", "Peer Review App")
      ),

      # ------------------------- Main: Hero + Login card ----------------------
      fluidRow(
        column(
          width = 7,
          tags$div(
            class = "landing-hero",
            style = "padding: 8px 6px 0 6px;",

            tags$h1("Peer Review leicht gemacht"),
            tags$div(class = "subline", "In Kooperation mit der Ärztekammer Niedersachsen"),

            tags$p(
              "Willkommen in der Peer-Review-App. Wenn Sie die App zum ersten Mal nutzen, ",
              "registrieren Sie sich bitte mit Ihrem Einladungscode. Nach der Registrierung ",
              "können Sie sich jederzeit mit Ihrem Benutzernamen und Passwort anmelden."
            ),
            tags$p(
              "Falls Sie bereits ein Konto haben, können Sie sich direkt einloggen. ",
              "Bei Problemen wenden Sie sich bitte an: ",
              tags$a(href = "mailto:yadwinder.kaur@uol.de", "yadwinder.kaur[at]uol.de"),
              "."
            ),

            # Illustration placeholder
            tags$div(
              style = "
                margin-top: 20px;
                border-radius: 16px;
                border: 1px dashed #cbd5e1;
                background: #ffffff;
                display:flex;
                align-items:center;
                justify-content:center;
                color:#64748b;
              ",
               tags$img(src="peer review imagined.png", style = "max-width: 100%;height: auto;display: block;")
            )
          )
        ),

        column(
          width = 5,
          box(
            width = 12,
            class = "landing-card",
            status = NULL,
            solidHeader = FALSE,

            tags$div(
              style = "padding: 6px 8px 2px 8px;",
              tags$div(class = "login-title", "Login"),

              textInput("login_username", "Benutzername"),
              passwordInput("login_password", "Passwort"),

              actionButton(
                "login_button",
                "Einloggen",
                class = "btn btn-primary custom-welcome-btn",
                style = "width:100%;"
              ),

              tags$div(
                style = "text-align:center; margin-top:10px;",
                actionLink("show_register_link", "Registrieren →")
              ),

              tags$div(
                class = "muted-note",
                HTML("🔬 Entwickelt für die Laboratoriumsmedizin <br> 🔒 Nur für eingeladene Teilnehmende")
              )
            )
          )
        )
      ),

      tags$hr(style = "margin: 18px 6px;"),

      # ---------------- "So funktioniert es" ----------------
      tags$div(
        style = "padding: 0 6px;",
        tags$h3(
          style = "font-weight:800; color:#0f172a; margin-top:0;",
          "So funktioniert es"
        ),
        
        tags$div(
          class = "feature-grid",
          
          tags$div(
            class = "feature-item",
            tags$div(class = "icon", "1️⃣"),
            tags$div(class = "title", "Selbstbewertung"),
            tags$div(
              class = "text",
              "Durchführung einer strukturierten Selbstbewertung anhand des standardisierten INQUAM-Fragebogens."
            )
          ),
          
          tags$div(
            class = "feature-item",
            tags$div(class = "icon", "2️⃣"),
            tags$div(class = "title", "Peer-Dialog vor Ort"),
            tags$div(
              class = "text",
              "Kollegialer Austausch mit externen Peers im Rahmen eines Vor-Ort-Besuchs inklusive Fremdbewertung."
            )
          ),
          
          tags$div(
            class = "feature-item",
            tags$div(class = "icon", "3️⃣"),
            tags$div(class = "title", "Feedback & Bericht"),
            tags$div(
              class = "text",
              "Erstellung eines vertraulichen Peer-Review-Berichts mit konsentierten Empfehlungen zur Qualitätsverbesserung."
            )
          )
        )
      ),
      
      tags$hr(style = "margin: 18px 6px;"),
      

#----------------------------------- Footer ------------------------------------
      tags$div(
        style = "padding: 0 6px 10px 6px; display:flex; justify-content:space-between; flex-wrap:wrap; gap:10px;",
        tags$div(
          class = "footer-links",
          #tags$a(href = "#", "Kontakt"),
          tags$a(href = "https://uol.de/datenschutzerklaerung", "Datenschutz"),
          tags$a(href = "https://kc.uol.de/disclaimer/", "Impressum")
        ),
        tags$div(style = "color:#94a3b8; font-size:12px;", paste("©", format(Sys.Date(), "%Y"), "Peer Review App"))
      )
    )
  )
),


# ----------------------------------- Register Tab -----------------------------
tabItem(
  tabName = "register",
  
  tags$div(
    style = "
      min-height: calc(100vh - 60px);
      padding: 24px 12px;
      background: linear-gradient(180deg, #f6f8fc 0%, #ffffff 60%);
      display: flex;
      justify-content: center;
      align-items: flex-start;
    ",
    
    tags$div(
      style = "width: 100%; max-width: 560px;",
      
      # Optional: only if not already defined globally in dashboardBody(tags$head(...))
      tags$style(HTML("
        .landing-card { border-radius: 16px; box-shadow: 0 10px 30px rgba(0,0,0,0.08); border: 0; }
        .form-hint { font-size: 13px; color: #64748b; line-height: 1.5; margin-top: 10px; }
        .link-center { text-align: center; margin-top: 10px; }
      ")),
      
      box(
        width = 12,
        class = "landing-card",
        status = NULL,
        solidHeader = FALSE,
        
        tags$div(
          style = "padding: 6px 8px 2px 8px;",
          
          tags$h2(
            style = "margin: 0 0 6px 0; font-weight: 800; color: #0f172a;",
            "Registrierung"
          ),
          tags$div(
            style = "margin-bottom: 14px; color: #475569;",
            "Erstellen Sie ein Konto mit Einladungscode. Die Registrierung ist nur für eingeladene Teilnehmende möglich."
          ),
          
          textInput("reg_username", "Benutzername"),
          textInput("reg_invite_code", "Einladungscode"),
          passwordInput("reg_password", "Passwort"),
          passwordInput("reg_password_confirm", "Passwort bestätigen"),
          
          actionButton(
            "register_button",
            "Registrieren",
            class = "btn btn-primary custom-welcome-btn",
            style = "width:100%;"
          ),
          
          tags$div(
            class = "form-hint",
            HTML("🔒 Vertraulicher Zugang &nbsp;&middot;&nbsp; 🎓 Nur für eingeladene Teilnehmende")
          ),
          
          tags$div(
            class = "link-center",
            actionLink("show_login_link", "Sie haben bereits ein Konto? Hier anmelden →")
          )
        )
      )
    )
  )
),


#------------Einwilligungserklärung zur Nutzung der Peer-Review-App-------------
tabItem(
  tabName = "Einwilligungserklärung",
  
  # ✅ Add a blocking overlay style
  tags$style(HTML("
    .consent-required-overlay {
      position: fixed;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      background: rgba(255, 255, 255, 0.98);
      z-index: 9998;
      display: flex;
      justify-content: center;
      align-items: center;
    }
    .consent-box {
      max-width: 900px;
      max-height: 90vh;
      overflow-y: auto;
      background: white;
      padding: 30px;
      border-radius: 12px;
      box-shadow: 0 4px 20px rgba(0,0,0,0.15);
    }
  ")),
  
  tags$div(
    class = "consent-required-overlay",
    
    tags$div(
      class = "consent-box",
      
      tags$h2("Einwilligungserklärung zur Nutzung der Peer-Review-App", 
              style = "color: #003B73; margin-bottom: 20px; text-align: center;"),
      
      tags$div(
        style = "background: #fff3cd; border: 1px solid #ffc107; padding: 15px; border-radius: 8px; margin-bottom: 20px;",
        tags$strong("Wichtig:"),
        " Bitte lesen Sie die folgende Einwilligungserklärung sorgfältig durch. Sie müssen dieser zustimmen, um die App nutzen zu können."
      ),
      
      tags$div(
        style = "max-height: 400px; overflow-y: auto; border: 1px solid #ddd; padding: 20px; border-radius: 8px; margin-bottom: 20px; background: #f9f9f9;",
        
        tags$div(
          tags$strong("1. Zweck der Einwilligung"),
          tags$br(),
          "Im Rahmen des Peer Reviews in der Laboratoriumsmedizin wird erstmalig die neu entwickelte Peer-Review-App eingesetzt, erprobt und evaluiert..."
        ),
        
        tags$div(
          style = "margin-top: 15px;",
          tags$strong("2. Funktionen der App im Peer Review"),
          tags$br(),
          "Die App wird im Rahmen des Peer Reviews für folgende Aspekte eingesetzt:",
          tags$ul(
            tags$li("Durchführung der Selbstbewertung durch die internen Peers und Übermittelung dieser an die externen Peers"),
            tags$li("Durchführung der Fremdbewertung im Rahmen des Vor-Ort-Besuchs durch die externen Peers"),
            tags$li("Grafische Darstellung der Selbst- und Fremdbewertung in Form von Spinnendiagrammen"),
            tags$li("Erstellung der Peer-Review-Berichts inkl. SWOT-Analyse, Maßnahmen und Best Practices"),
            tags$li("Möglichkeit zum Herunterladen der jeweiligen Dokumente als PDF-Datei")
          )
        ),
        
        tags$div(
          style = "margin-top: 15px;",
          tags$strong("3. Umfang der Datenverarbeitung"),
          tags$br(),
          "Die für die Durchführung des Peer Reviews erforderlichen Daten werden in der App erfasst, verarbeitet und gespeichert..."
        ),
        
        tags$div(
          style = "margin-top: 15px;",
          tags$strong("4. Zugriffs- und Rechtemanagement"),
          tags$br(),
          "Die App liegt auf dem Server der Universität Oldenburg..."
        ),
        
        tags$div(
          style = "margin-top: 15px;",
          tags$strong("5. Löschung der Daten"),
          tags$br(),
          "Die Daten werden in der App für den Zweck des Vor-Ort-Besuchs, die Evaluation und ein mögliches Re-Review gespeichert. Die Löschung der Daten erfolgt nach fünf Jahren."
        ),
        
        tags$div(
          style = "margin-top: 15px;",
          tags$strong("6. Gesetzliche Regelungen des Datenschutzes"),
          tags$br(),
          "Die einschlägigen bundes- und landesdatenschutzrechtlichen Vorschriften und die DSGVO werden beachtet."
        ),
        
        tags$div(
          style = "margin-top: 15px;",
          tags$strong("7. Widerrufsrecht"),
          tags$br(),
          "Die Einwilligung in die Nutzung der App kann jederzeit mit Wirkung für die Zukunft schriftlich widerrufen werden."
        ),
        
        tags$div(
          style = "margin-top: 15px;",
          tags$strong("8. Bestätigung"),
          tags$br(),
          "Ich bestätige, dass ich über Zweck, Funktionen, Umfang der Datenverarbeitung, Zugriffs- und Rechtemanagement, den Datenschutz sowie das Widerrufsrecht der App-Nutzung informiert wurde, akzeptiere die Bedingungen und willige der Nutzung ein."
        )
      ),
      
      tags$hr(style = "margin: 30px 0;"),
      
      # Consent Form Fields
      fluidRow(
        column(
          width = 6,
          textInput(
            "consent_ort",
            "Ort *",
            placeholder = "z.B. Oldenburg"
          )
        ),
        column(
          width = 6,
          dateInput(
            "consent_datum",
            "Datum *",
            value = Sys.Date(),
            format = "dd.mm.yyyy",
            language = "de"
          )
        )
      ),
      
      checkboxInput(
        "consent_checkbox",
        HTML("<strong>Ich habe die Einwilligungserklärung gelesen und willige hiermit ein. *</strong>"),
        value = FALSE
      ),
      
      tags$div(
        style = "margin-top: 30px; text-align: center;",
        actionButton(
          "bestätigen_button",
          "Zustimmen und Fortfahren",
          class = "btn btn-primary btn-lg",
          icon = icon("check-circle"),
          style = "padding: 15px 40px; font-size: 18px;",
          disabled = TRUE
        )
      ),
      
      tags$div(
        style = "margin-top: 20px; text-align: center; color: #666;",
        tags$small("* Pflichtfelder - Sie können die App erst nach Zustimmung nutzen.")
      )
    )
  )
),

      
#------------------------ Dashboard for admin and laborleitung------------------
      tabItem(
        tabName = "dashboard",
        h2(textOutput("dashboard_title")),
        
        # Value Boxes
        fluidRow(
          valueBoxOutput("totalUsersBox"),
          valueBoxOutput("totalProjectsBox"),
          valueBoxOutput("totalSubmissionsBox")
        ),
        
        # User Statistics
        fluidRow(
          box(
            title = "Benutzerstatistik",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("user_registration_plot"),
            plotlyOutput("project_status_pie"),  
            uiOutput("dashboard_project_name"),
            DT::dataTableOutput("admin_submissions_table")  
          )
        ),
        
        # All Users
        fluidRow(
          box(
            title = "All Users",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            DT::dataTableOutput("admin_users_table")
          )
        ),
        
        # All Projects
        fluidRow(
          box(
            title = "All Projects",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            DT::dataTableOutput("admin_projects_table")
          )
        ),
        
        # ✅ NEW: Saved Drafts Section
        fluidRow(
          box(
            title = "Gespeicherte Entwürfe",
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            p("Hier sehen Sie alle gespeicherten Fragebogen-Entwürfe von allen Benutzern."),
            DT::dataTableOutput("admin_drafts_table")
          )
        ),
        
        # ✅ NEW: Draft Details (shown when a row is clicked)
        fluidRow(
          box(
            title = "Entwurf Details",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            p("Klicken Sie auf einen Entwurf oben, um die Details anzuzeigen."),
            uiOutput("draft_details_view")
          )
        )
      ),
    # ------------------------ Admin Invitations ------------------------
    tabItem(
      tabName = "admin_invitations",
      h2("Einladungscodes (Admin)"),
      fluidRow(
        box(
          title = tags$span("Codes generieren", style = "color:white;"),
          status = "info",
          solidHeader = TRUE,
          width = 6,
          
          selectInput(
            "invite_role",
            "Rolle für diesen Code:",
            choices = c("laborleitung", "colleague", "leading_peer", "co_peer"),
            selected = "colleague"
          ),
     selectInput("dashboard_project_selector_for_codes", "Peer Review auswählen:", choices = NULL),
          numericInput("num_codes", "Anzahl Codes:", value = 1, min = 1, max = 200),
          selectInput(
            "code_type",
            "Assessment-Typ:",
            choices = c("selbstbewertung", "fremdbewertung"),
            selected = "selbstbewertung"
          ),
          actionButton("generate_codes", "Codes generieren", icon = icon("key"),
                       style = "background-color:#003B73;color:white;border:none;"),
     
          uiOutput("code_generation_status"),
          DT::dataTableOutput("generated_codes_table")
        ),
        
        box(
          title = tags$span("Alle Codes", style = "color:white;"),
          status = "primary",
          solidHeader = TRUE,
          width = 6,
          DT::dataTableOutput("current_invitations_table")
        )
      )
    ),
#----------------------summary of Selbstbewertung-------------------------------
    tabItem(
      tabName = "labor_leiter_summary",
      h2(textOutput("summary_page_title")),
      fluidRow(
        box(
          title = tags$span("Peer Review Auswahl", style = "color:white;"),
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          uiOutput("antworten_project_selector_ui"),
          uiOutput("project_summary"),
          
          # ✅ Download button HERE (after project selector)
          tags$hr(),
          div(
            style = "margin-top: 15px; margin-bottom: 15px;",
            verbatimTextOutput("debug_project_id"),
            downloadButton(
              "downloadZusammenfassung",
              "Gesamtbericht herunterladen (Labinfo + Selbstbewertung)",
              icon = icon("file-pdf"),
              style = "background-color: #dd4b39; color: white; font-weight: bold; padding: 12px 24px; font-size: 16px;"
            ),
            tags$p(
              style = "margin-top: 10px; color: #666; font-size: 14px;",
              "Bitte wählen Sie zuerst ein Projekt aus der Dropdown-Liste aus."
            )
          )
        )
      ),
      fluidRow(project_dashboard_analysis_tabs())
    ),
          
#---------------------- project finalisation tab-------------------------------
      tabItem(
        tabName = "project_finalization",
        h2("Selbstbewertung abschließen"),
        fluidRow(
          box(
            title = "Peer Review Auswahl",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            uiOutput("project_finalization_project_selector_ui")
          )
        ),
        fluidRow(
          box(
            title = "Labordaten Übersicht",
            solidHeader = TRUE,
            status = "info",
            width = 12,
            uiOutput("finalization_lab_info_ui")
          )
        ),
        fluidRow(column(
          width = 12,
          div(style = "text-align:center; margin:20px;"),
          uiOutput("finalization_status_ui")
        ))
      ),
      
      
#----------------- Peer Review Management Tab----------------------
  tabItem(
    tabName = "create_project",
    h2("Peer Review"),
    box(
      title = tags$span("1. Peer Review beginnen", style = "color:white;"),
      solidHeader = TRUE,
      status = "primary",
      width = 8,
      
      textInput("project_name", "Zu besuchende Einrichtung"),
      textAreaInput("project_description", "Themenschwerpunkte"),
      textInput("Zeitraum", "Gewünschter Peer Review-Zeitraum"),
      
      actionButton(
        "create_project_button", 
        "Peer Review erstellen",
        icon = icon("arrow-right"),
        style = "background-color: #3c8dbc; color: white; border: none; padding: 10px 20px; font-weight: 500;"
      ),
      uiOutput("project_creation_status"),
      
      # ✅ Weiter button - blue color and proper styling
      div(
        style = "margin-top: 20px;",
        actionButton(
          "btn_project_to_labinfo",
          "Weiter zu Laborinformationen",
          icon = icon("arrow-right"),
          style = "background-color: #3c8dbc !important; color: white !important; border: none !important; padding: 10px 20px; font-weight: 500;"
        )
      )
    )
  ),
      # Benutzerverwaltung (User Admin) Tab
      tabItem(
        tabName = "user_admin",
        h2("Benutzerverwaltung"),
        uiOutput("user_admin_content") # Fill with your actual admin UI
      ),
      
      
#-------------------------- Kollegen einladen tab ------------------------------

tabItem(
  tabName = "labor_leiter_invite_colleagues",
  h2("Kollegen einladen"),
  uiOutput("invite_project_selector_ui_colleagues"),
  numericInput(
    "num_leiter_codes_colleagues",
    "Anzahl Einladungscodes (Kollegen):",
    value = 1, min = 1, max = 100
  ),
  actionButton("generate_leiter_codes_colleagues", "Einladungscodes für Kollegen generieren"),
  DT::dataTableOutput("leiter_codes_table_colleagues")
),

# --- Leading Peer einladen tab ---
tabItem(
  tabName = "labor_leiter_invite_leading_peer",
  h2("Leading Peer einladen"),
  uiOutput("invite_project_selector_ui_leading_peer"),
  numericInput(
    "num_leiter_codes_leading_peer",
    "Anzahl Einladungscodes (Leading Peer):",
    value = 1, min = 1, max = 10
  ),
  actionButton("generate_leiter_codes_leading_peer", "Einladungscodes für Leading Peer generieren"),
  DT::dataTableOutput("leiter_codes_table_leading_peer")
),
        

# Fill Lab Info Tab
        tabItem(tabName = "fill_labinfo", h2("Laborinformation"), uiOutput("lab_ui")),
# Project Finalization Tab
        tabItem(
          tabName = "project_finalization",
          h2("Peer Review finalisieren"),
          uiOutput("project_finalization_content")
        ),

#--------------------------------- Reports Tab----------------------------------

  tabItem(
    tabName = "reports", # For laborleiter and colleague
    h2("Berichte"),
    fluidRow(
      box(
        title = tags$span("Peer Review Bericht herunterladen", style = "color:white;"),
        width = 6,
        status = "success",
        solidHeader = TRUE,
        selectInput(
          "report_project_selector",
          "Peer Review für Bericht auswählen:",
          choices = NULL
        ),
        downloadButton("downloadPersonalReport_reports", "Mein Selbstbewertung herunterladen"),
        verbatimTextOutput("debug_project_id")
        #downloadButton("downloadZusammenfassung", "Selbstbewertung (Zusammenfassung) & Laborinformation herunterladen")
      )
    )
  ),
tabItem(
  tabName = "report", # For leading_peer and co_peer
  h2("Berichte"),
  fluidRow(
    box(
      title = tags$span("Peer Review Bericht herunterladen", style = "color:white;"),
      width = 6,
      status = "primary",  # ✅ Changed from "success" to "primary" (blue)
      solidHeader = TRUE,
      uiOutput("report_project_selector_leading_ui"),
      
      # ✅ Updated button styling
      tags$div(
        style = "margin-top: 15px; margin-bottom: 10px;",
        downloadButton(
          "downloadFremdbewertung", 
          "Meine Fremdbewertung herunterladen",
          icon = icon("file-pdf"),
          style = "background-color: #003B73; color: white; font-weight: bold; padding: 10px 20px; border: none; border-radius: 4px;"
        )
      ),
      
      tags$div(
        style = "margin-top: 10px; margin-bottom: 15px;",
        downloadButton(
          "downloadLeadingPeerGesamtbericht", 
          "Gesamten Bericht herunterladen",
          icon = icon("file-pdf"),
          style = "background-color: #003B73; color: white; font-weight: bold; padding: 10px 20px; border: none; border-radius: 4px;"
        )
      ),
      
      tags$p(
        style = "margin-top: 10px; color: #666; font-size: 13px; font-style: italic;",
        icon("info-circle"), " Bitte wählen Sie zuerst ein Projekt aus der Dropdown-Liste aus."
      )
    )
  )
),
      
#------------------------------Colleague (Kollege) Dashboard------------------------------
tabItem(
  tabName = "colleagueGuide",
  h2("Kollegen Guide: Schritte im Peer Review"),
  p("Nutzen Sie diese Übersicht, um alle Schritte schnell zu erreichen."),
  hr(),
  
  fluidRow(
    box(
      title = tags$span("1. Kollegen Dashboard", style = "color:white;"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      icon = icon("dashboard"),
      HTML("Übersicht: Projektname sehen und Status prüfen."),
      tags$br(),
      tags$button(
        id = "goColleagueDashboard",
        class = "btn action-button",
        style = "background-color: #3c8dbc !important; color: white !important; border: none !important; font-weight: 500; padding: 10px 20px;",
        icon("arrow-right"),
        " Zum Dashboard"
      )
    ),
    box(
      title = tags$span("2. Labordaten ansehen", style = "color:white;"),
      status = "warning",
      solidHeader = TRUE,
      width = 12,
      icon = icon("flask"),
      HTML("Laborinformationen ansehen (read-only)."),
      tags$br(),
      tags$button(
        id = "goColleagueLabData",
        class = "btn action-button",
        style = "background-color: #f39c12 !important; color: white !important; border: none !important; font-weight: 500; padding: 10px 20px;",
        icon("arrow-right"),
        " Zu Labordaten"
      )
    ),
    box(
      title = tags$span("3. Fragebogen ausfüllen", style = "color:white;"),
      status = "success",
      solidHeader = TRUE,
      width = 12,
      icon = icon("file-signature"),
      HTML("Selbstbewertung ausfüllen (Fragebogen)."),
      tags$br(),
      actionButton(
        inputId = "questionnaire",
        label = " Zum Fragebogen",
        icon = icon("arrow-right"),
        style = "background-color: #00a65a !important; color: white !important; border: none !important; font-weight: 500; padding: 10px 20px;"
      )
    )
    # box(
    #   title = tags$span("4. Berichte herunterladen", style = "color:white;"),
    #   status = "warning",
    #   solidHeader = TRUE,
    #   width = 12,
    #   icon = icon("chart-line"),
    #   HTML("PDF/Download: Selbstbewertung und Zusammenfassung."),
    #   tags$br(),
    #   tags$button(
    #     id = "goColleagueReports",
    #     class = "btn action-button",
    #     style = "background-color: #f39c12 !important; color: white !important; border: none !important; font-weight: 500; padding: 10px 20px;",
    #     icon("arrow-right"),
    #     " Zu Berichten"
    #   )
    # )
  )
),

tabItem(
  tabName = "colleague_dashboard",
  h2(textOutput("colleague_dashboard_project_title")),
  fluidRow(
    box(
      title = tags$span("Schnellzugriff", style = "color:white;"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      tags$button(
        id = "goColleagueLabData2",
        class = "btn action-button",
        style = "background-color: #00c0ef !important; color: white !important; border: none !important; font-weight: 500; padding: 10px 20px; margin-right: 10px;",
        icon("flask"),
        " Labordaten ansehen"
      ),
      actionButton(
        inputId = "questionnaire",
        label = " Fragebogen ausfüllen",
        icon = icon("file-signature"),
        style = "background-color: #00a65a !important; color: white !important; border: none !important; font-weight: 500; padding: 10px 20px; margin-right: 10px;"
      )
      # tags$button(
      #   id = "goColleagueReports2",
      #   class = "btn action-button",
      #   style = "background-color: #f39c12 !important; color: white !important; border: none !important; font-weight: 500; padding: 10px 20px;",
      #   icon("chart-line"),
      #   " Berichte herunterladen"
      # )
    )
  )
),


#-----------------------------leading peer dashboard----------------------------

tabItem(
  tabName = "leading_peer_dashboard",
  h2("Selbstbewertung Übersicht"),
  
  # Project Selector
  fluidRow(
    box(
      title = tags$span("Peer Review Auswahl", style = "color:white;"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      uiOutput("labor_leiter_summary_project_selector_ui")
    )
  ),
  
  # Custom CSS matching fragebogen_ui style
  tags$head(tags$style(HTML("
    body, .tab-content { background-color: #f8f9fa; }
    .dashboard-wrapper { background: white; border-radius: 6px; box-shadow: 0 1px 3px rgba(0,59,115,0.1); padding: 16px; margin: 16px 0; overflow-x: auto; }
    .dashboard-table { width: 100%; border-collapse: collapse; font-size: 13px; }
    .dashboard-table thead { background: #003B73; color: white; position: sticky; top: 0; z-index: 10; }
    .dashboard-table th { padding: 10px 8px; font-weight: 600; text-align: left; font-size: 13px; border: 1px solid #0056a6; color: white; }
    .dashboard-col-question { width: 20%; min-width: 180px; }
    .dashboard-col-selbst { width: 25%; min-width: 200px; }
    .dashboard-col-stats { width: 15%; min-width: 140px; }
    .dashboard-col-plot { width: 40%; min-width: 350px; }
    .dashboard-row { border-bottom: 1px solid #e9ecef; transition: background-color 0.15s; }
    .dashboard-row:hover { background-color: #f8f9fa; }
    .dashboard-table td { padding: 10px 8px; vertical-align: top; border: 1px solid #dee2e6; }
    .dashboard-col-question { font-size: 13px; line-height: 1.4; color: #212529; }
    .dashboard-col-question strong { color: #003B73; font-size: 13px; }
    
        
             /* ✅ Enhanced statistics table styling */
    .berufsgruppe-stats {
      background: #f8f9fa;
      border-radius: 8px;
      padding: 16px;
      border: 1px solid #e0e0e0;
      margin-top: 16px;
    }
    
    .berufsgruppe-stats h4 {
      margin: 0 0 12px 0;
      font-size: 14px;
      color: #333;
      font-weight: 600;
      text-align: center;
    }
    
    .stats-detail-table {
      width: 100%;
      border-collapse: collapse;
      margin: 0 auto;
      font-size: 12px;
    }
    
    .stats-detail-table thead {
      background: #003B73;
      color: white;
    }
    
    .stats-detail-table th {
      padding: 8px 6px;
      text-align: left;
      font-size: 12px;
      font-weight: 600;
      border: 1px solid #00508f;
    }
    
    .stats-detail-table td {
      padding: 6px 8px;
      border: 1px solid #e0e0e0;
    }
    
    .stats-label {
      text-align: left;
      color: #555;
      font-size: 12px;
      font-weight: 500;
      min-width: 140px;
    }
    
    .stats-n {
      text-align: center;
      color: #1976D2;
      font-weight: 500;
      min-width: 40px;
    }
    
    .stats-question-value {
      text-align: center;
      font-size: 11px;
      color: #555;
      min-width: 45px;
    }
    
    .stats-topic-avg {
      text-align: center;
      color: #1976D2;
      font-weight: 600;
      font-size: 13px;
      background: #f0f7ff;
      min-width: 60px;
    }
    
    .stats-total td {
      border-top: 2px solid #1976D2;
      padding-top: 10px;
      padding-bottom: 10px;
      font-weight: 600;
      background: #fafafa;
    }
    
    /* Responsive: Make table scrollable on small screens */
    @media (max-width: 1200px) {
      .berufsgruppe-stats {
        overflow-x: auto;
      }
    }
              
    /* Selbstbewertung styling - same as fragebogen_ui */
    .rating-inline { display: inline-flex; gap: 4px; align-items: center; flex-wrap: wrap; }
    .rating-active { background: #003B73; color: white; font-weight: 600; padding: 4px 7px; border-radius: 4px; font-size: 11px; display: inline-block; min-width: 28px; text-align: center; position: relative; }
    .rating-with-count { padding-right: 16px; }
    .text-count-badge { display: inline-block; margin-left: 6px; padding: 1px 6px; background: #c6dbef; color: #003B73; font-size: 10px; font-weight: 700; border-radius: 8px; vertical-align: middle; }
    .rating-count { position: absolute; top: -6px; right: -2px; font-size: 9px; color: #003B73; font-weight: 700; background: #c6dbef; padding: 1px 3px; border-radius: 6px; line-height: 1; }
    .rating-inactive { background: #e9ecef; color: #adb5bd; padding: 4px 7px; border-radius: 4px; font-size: 10px; display: inline-block; min-width: 28px; text-align: center; }
    .respondent-info { display: inline-block; margin-top: 6px; padding: 2px 8px; background: #c6dbef; border-radius: 12px; font-size: 11px; color: #003B73; font-weight: 600; }
    .respondent-info .fa { font-size: 10px; color: #003B73; }
    .selbst-text-container { margin-top: 8px; }
    .selbst-text-item { padding: 6px 10px; margin-bottom: 6px; background: #f8f9fa; border-left: 3px solid #003B73; border-radius: 0 4px 4px 0; font-size: 12px; line-height: 1.5; color: #495057; word-wrap: break-word; }
    .selbst-text-item .fa { color: #003B73; margin-right: 6px; font-size: 10px; }
    .no-data { color: #adb5bd; font-style: italic; font-size: 12px; }
    
    /* Stats column styling */
    .dashboard-col-stats { font-size: 11px; }
    .stat-item { margin-bottom: 4px; padding: 3px 6px; background: #f8f9fa; border-radius: 3px; }
    .stat-label { font-weight: 600; color: #495057; }
    .stat-value { color: #003B73; font-weight: 700; }
    
    /* Plot column styling */
    .dashboard-col-plot { text-align: center; padding: 5px !important; }
    .plot-container { width: 100%; height: auto; }
    
    /* Tab styling */
    .nav-tabs { border-bottom: 2px solid #dee2e6; margin-bottom: 0; }
    .nav-tabs > li > a { color: #495057; font-weight: 500; border: none; border-bottom: 3px solid transparent; padding: 10px 20px; font-size: 14px; transition: all 0.15s; }
    .nav-tabs > li > a:hover { background-color: #f8f9fa; border-bottom-color: #003B73; }
    .nav-tabs > li.active > a { color: #003B73; border-bottom-color: #003B73; background-color: white; font-weight: 600; }
    
    @media (max-width: 1400px) { 
      .dashboard-col-question { width: 18%; } 
      .dashboard-col-selbst { width: 24%; } 
      .dashboard-col-stats { width: 14%; }
      .dashboard-col-plot { width: 44%; }
    }
  "))),
  
  # Main Content - Tabs
  fluidRow(
    tabBox(
      width = 12,
      id = "leading_peer_dashboard_tabs",
      
      # FÜHRUNG TAB
      tabPanel(
        "Führung",
        uiOutput("leading_peer_dashboard_fuehrung")
      ),
      
      # MITARBEITENDE TAB
      tabPanel(
        "Mitarbeitende",
        uiOutput("leading_peer_dashboard_mitarbeitende")
      ),
      
      # PATIENTEN TAB
      tabPanel(
        "Patienten & Angehörige",
        uiOutput("leading_peer_dashboard_patienten")
      ),
      
      # EINSENDER TAB
      tabPanel(
        "Einsender & Kooperationspartner",
        uiOutput("leading_peer_dashboard_einsender")
      ),
      
      # QUALITÄT TAB
      tabPanel(
        "Qualitätsindikatoren & Validierung",
        uiOutput("leading_peer_dashboard_qualitaet")
      )
    )
  )
),
 
      tabItem(
        actionButton(
          "btnGoToOrtBesuch",
          "Zum Bericht-Download",
          icon = icon("arrow-right")
        ),

        tabName = "leading_peer_ort_besuch",
        h2("Vergleich Selbst-/Fremdbewertung – Bericht von Ort besuch"),
        h3("Führung"),
        DT::dataTableOutput("leading_peer_fuehrung_vergleich_table"),
        h3("Mitarbeitende"),
        DT::dataTableOutput("leading_peer_mitarbeitende_vergleich_table"),
        h3("Patienten & Angehörige"),
        DT::dataTableOutput("leading_peer_pat_vergleich_table"),
        h3("Einsender & Kooperationspartner"),
        DT::dataTableOutput("leading_peer_ein_vergleich_table"),
        h3("Qualitätsindikatoren"),
        DT::dataTableOutput("leading_peer_qual_vergleich_table")
      ),


      tabItem(
        tabName = "leading_peer_report_data",
        h2("Berichtsdaten"),
        textInput("report_teilnehmende", "Teilnehmende"),
        textInput("report_ort", "Ort"),
        textInput("report_datum", "Datum")
      ),

tabItem(
  tabName = "leading_peer_swot",
  h2("SWOT Analyse"),
  

  tags$head(tags$style(HTML("
    .swot .box-body, .swot .form-group { height: 100%; margin-bottom: 0; }
    .swot textarea.form-control { height: 100% !important; }
  "))),
  
  fluidRow(
    tagAppendAttributes(
      box(
        title = "Stärken (Strengths)", status = "success", solidHeader = TRUE,
        width = 12, height = "28vh",
        textAreaInput("swot_strengths", NULL, placeholder = "Stärken eintragen...", width = "100%")
      ),
      class = "swot"
    )
  ),
  fluidRow(
    tagAppendAttributes(
      box(
        title = "Schwächen (Weaknesses)", status = "danger", solidHeader = TRUE,
        width = 12, height = "28vh",
        textAreaInput("swot_weaknesses", NULL, placeholder = "Schwächen eintragen...", width = "100%")
      ),
      class = "swot"
    )
  ),
  fluidRow(
    tagAppendAttributes(
      box(
        title = "Chancen (Opportunities)", status = "info", solidHeader = TRUE,
        width = 12, height = "28vh",
        textAreaInput("swot_opportunities", NULL, placeholder = "Chancen eintragen...", width = "100%")
      ),
      class = "swot"
    )
  ),
  fluidRow(
    tagAppendAttributes(
      box(
        title = "Risiken (Threats)", status = "warning", solidHeader = TRUE,
        width = 12, height = "28vh",
        textAreaInput("swot_threats", NULL, placeholder = "Risiken eintragen...", width = "100%")
      ),
      class = "swot"
    )
  )
),
      tabItem(
        tabName = "leading_peer_best_practices",
        h2("Best Practices"),
        textAreaInput("best_practices", "Best Practices (Freitext)", width = "100%", height = "200px")
      ),
      tabItem(
        tabName = "leading_peer_final_remarks",
        h2("Final Remarks"),
        textAreaInput("final_remarks", "Final Remarks (Freitext)", width = "100%", height = "200px")
      ),
      
      
      tabItem(
        tabName = "leading_peer_invites",
        h2("Co-Peer Einladungen"),
        uiOutput("invite_project_selector_ui_leading_peer_co_peer"),
        numericInput(
          "num_leading_peer_codes_co_peer",
          "Anzahl Codes:",
          1,
          min = 1,
          max = 20
        ),
        actionButton(
          "generate_leading_peer_codes_co_peer",
          "Codes für Co-Peers generieren",
          icon = icon("key"),
          style = "background-color:#003B73;color:white;border:none;"
        ),
        DT::dataTableOutput("leading_peer_codes_table_co_peer")
      ),
      
#----------------------------- co-peer dashboard-------------------------------

tabItem(
  tabName = "co_peer_dashboard",
  h2("Selbstbewertung Übersicht"),
  
  # Project Selector
  fluidRow(
    box(
      title = tags$span("Peer Review Auswahl", style = "color:white;"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      uiOutput("labor_leiter_summary_project_selector_ui")
    )
  ),
  
  # Custom CSS for consistent look (reuse or reference leading_peer_dashboard styling)
  tags$head(tags$style(HTML("
    /* Copy custom dashboard styling from leading_peer_dashboard here */
  "))),
  
  # Main Content - Tabs
  fluidRow(
    tabBox(
      width = 12,
      id = "co_peer_dashboard_tabs",
      
      # FÜHRUNG TAB
      tabPanel(
        "Führung",
        uiOutput("co_peer_dashboard_fuehrung")
      ),
      
      # MITARBEITENDE TAB
      tabPanel(
        "Mitarbeitende",
        uiOutput("co_peer_dashboard_mitarbeitende")
      ),
      
      # PATIENTEN TAB
      tabPanel(
        "Patienten & Angehörige",
        uiOutput("co_peer_dashboard_patienten")
      ),
      
      # EINSENDER TAB
      tabPanel(
        "Einsender & Kooperationspartner",
        uiOutput("co_peer_dashboard_einsender")
      ),
      
      # QUALITÄT TAB
      tabPanel(
        "Qualitätsindikatoren & Validierung",
        uiOutput("co_peer_dashboard_qualitaet")
      )
    )
  )
),


# show this only for leading_peer and co_peer
tabItem(
  tabName = "fill_fragebogen",
  h2("Fremdbewertung ausfüllen"),
  #uiOutput("selbstbewertung_summary"),  # For leading_peer: show comparison, otherwise empty
  uiOutput("fragebogen_ui"),
  fluidRow(
    column(
      width = 12,
      conditionalPanel(
        condition = "output.userRole === 'leading_peer' || output.userRole === 'co_peer'",
        div(
          style = "display: flex; gap: 20px; margin-bottom: 20px;"
          
          # actionButton(
          #   "resume_draft_btn",
          #   "Resume",
          #   icon = icon("play"),
          #   style = "background-color: #3c8dbc; color: white; border: none; padding: 10px 20px; font-weight: 500;")
        )
      )
    )
  ),
  uiOutput("draft_status_ui"),
  uiOutput("start_qn"),
  uiOutput("questionnaire_submit_status")
),


#------------------------Labordaten ansehen------------------------------------
tabItem(
  tabName = "labordaten_ansehen",
  h3("Labordaten ansehen"),
  fluidRow(
    column(
      width = 12,
      uiOutput("labinfo_panel")
    )
  )
)

      )
    )
  )


#---------------------------- Server Logic -------------------------------------
  server <- function(input, output, session) {
    
    
#----------------------load draft of answers------------------------------------
    draft_loaded_once <- reactiveVal(FALSE)
    
    observeEvent(user_logged_in(), {
      if (!isTRUE(user_logged_in())) return()
      draft_loaded_once(FALSE)   # reset on login
    }, ignoreInit = TRUE)
    

    observeEvent(input$sidebar_menu, {
      req(user_logged_in())
      
      if (identical(input$sidebar_menu, "fill_questionnaire")) {
        if (!isTRUE(draft_loaded_once())) {
          resume_draft_from_db()
          draft_loaded_once(TRUE)
        }
      }
    }, ignoreInit = TRUE)
    
    
    
    
    observe({
      req(input$antworten_project_selector)
      if (!is.null(input$antworten_project_selector) && input$antworten_project_selector != "") {
        shinyjs::enable("downloadZusammenfassung")
      } else {
        shinyjs::disable("downloadZusammenfassung")
      }
    })
    

    #--------------Helper: Get Question-Level Statistics for PDF-------------------
    
    get_question_level_statistics <- function(project_id, questions_list, section_name) {
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      
      qids <- sapply(questions_list, function(q) q$id)
      qids_quoted <- paste0("'", qids, "'", collapse = ", ")
      
      query <- sprintf("
    SELECT 
      r.question_id,
      r.response_value as rating,
      SUM(COALESCE(CAST(qd.answers::json->>'group_size' AS INTEGER), 1)) as count
    FROM responses r
    JOIN users u ON u.id = r.user_id
    LEFT JOIN questionnaire_drafts qd 
      ON u.id::text = qd.user_id::text 
      AND qd.project_id::text = r.project_id::text
      AND qd.questionnaire_id = 'peer_review'
    WHERE r.project_id::text = '%s'
      AND r.assessment_type = 'selbstbewertung'
      AND r.question_id IN (%s)
      AND r.response_value != ''
      AND r.response_value != 'N/A'
    GROUP BY r.question_id, r.response_value
    ORDER BY r.question_id, r.response_value
  ", as.character(project_id), qids_quoted)
      
      result <- DBI::dbGetQuery(con, query)
      
      # Create full data with question labels
      full_data <- data.frame(
        Section = section_name,
        question_id = qids,
        Frage = sapply(questions_list, function(q) q$label),
        stringsAsFactors = FALSE
      )
      
      # Format statistics as readable text
      if (nrow(result) > 0) {
        stats_by_question <- split(result, result$question_id)
        
        full_data$Statistik <- sapply(full_data$question_id, function(qid) {
          stats <- stats_by_question[[qid]]
          if (!is.null(stats) && nrow(stats) > 0) {
            parts <- sapply(1:nrow(stats), function(i) {
              # ✅ FIX: Use %.0f instead of %d
              sprintf("Bewertung %s: %.0f Personen", stats$rating[i], as.numeric(stats$count[i]))
            })
            paste(parts, collapse = ", ")
          } else {
            "—"
          }
        })
      } else {
        full_data$Statistik <- "—"
      }
      
      return(full_data[, c("Section", "question_id", "Frage", "Statistik")])
    }
    
    #--------------Helper: Get Berufsgruppe Averages for PDF----------------------
    
    get_berufsgruppe_averages <- function(project_id, questions_list, section_name) {
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      
      qids <- sapply(questions_list, function(q) q$id)
      qids_quoted <- paste0("'", qids, "'", collapse = ", ")
      
      query <- sprintf("
    SELECT 
      r.question_id,
      COALESCE(qd.answers::json->>'Berufsgruppe', 'Nicht angegeben') as berufsgruppe,
      ROUND(
        SUM(CAST(r.response_value AS NUMERIC) * COALESCE(CAST(qd.answers::json->>'group_size' AS INTEGER), 1)) / 
        SUM(COALESCE(CAST(qd.answers::json->>'group_size' AS INTEGER), 1)),
        1
      ) as avg_rating,
      SUM(COALESCE(CAST(qd.answers::json->>'group_size' AS INTEGER), 1)) as group_count
    FROM responses r
    JOIN users u ON u.id = r.user_id
    LEFT JOIN questionnaire_drafts qd 
      ON u.id::text = qd.user_id::text 
      AND qd.project_id::text = r.project_id::text
      AND qd.questionnaire_id = 'peer_review'
    WHERE r.project_id::text = '%s'
      AND r.assessment_type = 'selbstbewertung'
      AND r.question_id IN (%s)
      AND r.response_value != ''
      AND r.response_value != 'N/A'
      AND r.response_value ~ '^[0-9]+$'
    GROUP BY r.question_id, COALESCE(qd.answers::json->>'Berufsgruppe', 'Nicht angegeben')
    ORDER BY r.question_id, berufsgruppe
  ", as.character(project_id), qids_quoted)
      
      result <- DBI::dbGetQuery(con, query)
      
      # Create full data
      full_data <- data.frame(
        Section = section_name,
        question_id = qids,
        Frage = sapply(questions_list, function(q) q$label),
        stringsAsFactors = FALSE
      )
      
      # Format group averages
      if (nrow(result) > 0) {
        groups_by_question <- split(result, result$question_id)
        
        full_data$Berufsgruppen <- sapply(full_data$question_id, function(qid) {
          groups <- groups_by_question[[qid]]
          if (!is.null(groups) && nrow(groups) > 0) {
            parts <- sapply(1:nrow(groups), function(i) {
              # ✅ FIX: Use %.0f instead of %d
              sprintf("%s: Ø %.1f (n=%.0f)", groups$berufsgruppe[i], as.numeric(groups$avg_rating[i]), as.numeric(groups$group_count[i]))
            })
            paste(parts, collapse = "; ")
          } else {
            "—"
          }
        })
      } else {
        full_data$Berufsgruppen <- "—"
      }
      
      return(full_data[, c("Section", "question_id", "Frage", "Berufsgruppen")])
    }
    
    
    #--------------Helper: Get Selbstbewertung Text Responses for PDF--------------
    
    get_selbstbewertung_text_responses <- function(project_id, questions_list, section_name) {
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      
      qids <- sapply(questions_list, function(q) q$id)
      qids_quoted <- paste0("'", qids, "'", collapse = ", ")
      
      query <- sprintf("
    SELECT 
      r.question_id,
      r.text_value,
      COALESCE(CAST(qd.answers::json->>'group_size' AS INTEGER), 1) as group_size
    FROM responses r
    JOIN users u ON u.id = r.user_id
    LEFT JOIN questionnaire_drafts qd 
      ON u.id::text = qd.user_id::text 
      AND qd.project_id::text = r.project_id::text
      AND qd.questionnaire_id = 'peer_review'
    WHERE r.project_id::text = '%s'
      AND r.assessment_type = 'selbstbewertung'
      AND r.question_id IN (%s)
      AND r.text_value != ''
      AND r.text_value IS NOT NULL
    ORDER BY r.question_id, r.text_value
  ", as.character(project_id), qids_quoted)
      
      result <- DBI::dbGetQuery(con, query)
      
      # Create full data with question labels
      full_data <- data.frame(
        Section = section_name,
        question_id = qids,
        Frage = sapply(questions_list, function(q) q$label),
        stringsAsFactors = FALSE
      )
      
      # Aggregate texts by question (with count)
      if (nrow(result) > 0) {
        texts_by_question <- split(result, result$question_id)
        
        full_data$Textantworten <- sapply(full_data$question_id, function(qid) {
          texts <- texts_by_question[[qid]]
          if (!is.null(texts) && nrow(texts) > 0) {
            # Aggregate identical texts
            text_agg <- aggregate(group_size ~ text_value, data = texts, FUN = sum)
            
            parts <- sapply(1:nrow(text_agg), function(i) {
              count <- as.numeric(text_agg$group_size[i])
              if (count > 1) {
                sprintf("• %s (×%.0f)", text_agg$text_value[i], count)
              } else {
                sprintf("• %s", text_agg$text_value[i])
              }
            })
            paste(parts, collapse = ";")
          } else {
            "—"
          }
        })
      } else {
        full_data$Textantworten <- "—"
      }
      
      return(full_data[, c("Section", "question_id", "Frage", "Textantworten")])
    }
    
#-------------------------- SPIDER PLOTS - All Topics----------------------------


    get_weighted_spider_data_local <- function(project_id, topic_key, questionnaires, binary_qnums = integer()) {
      message(">>> get_weighted_spider_data_local called")
      message("    project_id: ", project_id)
      message("    topic_key: ", topic_key)
      
      question_list <- questionnaires[[topic_key]]
      
      if (is.null(question_list)) {
        message("    ERROR: No question list for topic: ", topic_key)
        return(NULL)
      }
      
      message("    Questions found: ", length(question_list))
      
      # Filter binary questions
      if (length(binary_qnums) > 0) {
        question_list <- Filter(function(q) !(q$number %in% binary_qnums), question_list)
        message("    After binary filter: ", length(question_list))
      }
      
      if (length(question_list) == 0) {
        message("    No questions after filtering")
        return(NULL)
      }
      
      question_ids <- sapply(question_list, function(q) q$id)
      message("    Question IDs: ", paste(question_ids, collapse = ", "))
      
      # Get responses
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      sql <- sprintf(
        "SELECT user_id, question_id, response_value::numeric, group_size
     FROM responses
     WHERE project_id = $1
       AND assessment_type = 'selbstbewertung'
       AND question_id IN (%s)
       AND response_value IS NOT NULL
       AND response_value != ''",
        paste0("'", question_ids, "'", collapse = ", ")
      )
      
      message("    Executing query...")
      responses <- DBI::dbGetQuery(con, sql, params = list(project_id))
      message("    Raw responses: ", nrow(responses))
      
      if (nrow(responses) == 0) {
        message("    No responses found")
        return(NULL)
      }
      
      # Expand by group size
      message("    Expanding by group_size...")
      expanded_list <- lapply(1:nrow(responses), function(i) {
        row <- responses[i, ]
        gs <- if (is.na(row$group_size) || row$group_size < 1) 1 else as.integer(row$group_size)
        
        data.frame(
          user_id = rep(row$user_id, gs),
          question_id = rep(row$question_id, gs),
          response_value = rep(as.numeric(row$response_value), gs)
        )
      })
      
      expanded <- do.call(rbind, expanded_list)
      message("    Expanded to: ", nrow(expanded), " rows")
      
      # Calculate means
      means <- expanded %>%
        group_by(question_id) %>%
        summarise(mean_value = mean(response_value, na.rm = TRUE), .groups = "drop")
      
      message("    Means calculated for ", nrow(means), " questions")
      
      # Map to question order
      values <- sapply(question_list, function(q) {
        row <- means[means$question_id == q$id, ]
        if (nrow(row) > 0) row$mean_value else 0
      })
      
      labels <- sapply(question_list, function(q) as.character(q$number))
      
      message("    Values: ", paste(round(values, 2), collapse = ", "))
      message("    Labels: ", paste(labels, collapse = ", "))
      message(">>> get_weighted_spider_data_local END")
      
      return(list(values = values, labels = labels))
    }
    
    #--------------Spider Plots for All Topics (SUMMARY ANALYSIS)-----------------------------------
    
    message("========================================")
    message("INITIALIZING SPIDER PLOT OUTPUTS")
    message("========================================")
    
    #-------------- Führung Spider--------------------------
    output$fuehrung_spider <- renderPlotly({
      message("!!! FUEHRUNG_SPIDER TRIGGERED !!!")
      
      pid <- current_project_id()
      message("!!! Project ID: ", ifelse(is.null(pid), "NULL", as.character(pid)))
      
      req(pid)
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      question_ids <- sapply(fuehrung_questions, function(q) q$id)
      
      message("!!! Question IDs: ", paste(question_ids, collapse = ", "))
      
      # Simple query without expansion first
      responses <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT question_id, response_value::numeric as rv
       FROM responses
       WHERE project_id = $1
         AND assessment_type = 'selbstbewertung'
         AND question_id IN (%s)
         AND response_value IS NOT NULL
         AND response_value != ''",
          paste0("'", question_ids, "'", collapse = ", ")
        ),
        params = list(pid)
      )
      
      message("!!! Raw responses: ", nrow(responses))
      
      if (nrow(responses) == 0) {
        message("!!! NO DATA - Returning empty plot")
        return(plotly::plot_ly() %>% 
                 plotly::layout(title = "Keine Daten vorhanden für Führung"))
      }
      
      # Calculate means
      means <- responses %>%
        group_by(question_id) %>%
        summarise(mean_value = mean(rv, na.rm = TRUE), .groups = "drop")
      
      message("!!! Means calculated: ", nrow(means), " questions")
      
      # Map to question order
      values <- sapply(fuehrung_questions, function(q) {
        row <- means[means$question_id == q$id, ]
        if (nrow(row) > 0) row$mean_value else NA
      })
      
      labels <- sapply(fuehrung_questions, function(q) as.character(q$number))
      
      # Remove NA values
      valid_idx <- !is.na(values)
      values <- values[valid_idx]
      labels <- labels[valid_idx]
      
      message("!!! Valid values: ", length(values))
      message("!!! Values: ", paste(round(values, 2), collapse = ", "))
      
      if (length(values) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "Keine gültigen Daten"))
      }
      
      message("!!! Creating plotly object...")
      
      p <- plotly::plot_ly(
        type = 'scatterpolar',
        mode = 'lines+markers',
        r = values,
        theta = labels,
        fill = 'toself',
        fillcolor = "rgba(1, 115, 178, 0.3)",
        line = list(color = 'rgb(1, 115, 178)', width = 2)
      ) %>%
        plotly::layout(
          title = "Führung - Durchschnitt",
          polar = list(radialaxis = list(visible = TRUE, range = c(0, 5)))
        )
      
      message("!!! PLOT CREATED !!!")
      
      return(p)
    })
    
    # Mitarbeitende Spider
    output$mitarbeitende_spider <- renderPlotly({
      message("!!! MITARBEITENDE_SPIDER TRIGGERED !!!")
      
      pid <- current_project_id()
      req(pid)
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      # Filter out binary questions
      binary_qnums <- c(10, 20, 24)
      questions_filtered <- Filter(function(q) !(q$number %in% binary_qnums), mit_questions)
      question_ids <- sapply(questions_filtered, function(q) q$id)
      
      responses <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT question_id, response_value::numeric as rv
       FROM responses
       WHERE project_id = $1
         AND assessment_type = 'selbstbewertung'
         AND question_id IN (%s)
         AND response_value IS NOT NULL
         AND response_value != ''",
          paste0("'", question_ids, "'", collapse = ", ")
        ),
        params = list(pid)
      )
      
      message("!!! Mit responses: ", nrow(responses))
      
      if (nrow(responses) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "Keine Daten vorhanden"))
      }
      
      means <- responses %>%
        group_by(question_id) %>%
        summarise(mean_value = mean(rv, na.rm = TRUE), .groups = "drop")
      
      values <- sapply(questions_filtered, function(q) {
        row <- means[means$question_id == q$id, ]
        if (nrow(row) > 0) row$mean_value else NA
      })
      
      labels <- sapply(questions_filtered, function(q) as.character(q$number))
      
      valid_idx <- !is.na(values)
      values <- values[valid_idx]
      labels <- labels[valid_idx]
      
      if (length(values) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "Keine gültigen Daten"))
      }
      
      plotly::plot_ly(
        type = 'scatterpolar',
        mode = 'lines+markers',
        r = values,
        theta = labels,
        fill = 'toself',
        fillcolor = "rgba(222, 143, 5, 0.3)",
        line = list(color = 'rgb(222, 143, 5)', width = 2)
      ) %>%
        plotly::layout(
          title = "Mitarbeitende - Durchschnitt",
          polar = list(radialaxis = list(visible = TRUE, range = c(0, 5)))
        )
    })
    
    # Patienten Spider
    output$pat_spider <- renderPlotly({
      message("!!! PAT_SPIDER TRIGGERED !!!")
      
      pid <- current_project_id()
      req(pid)
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      question_ids <- sapply(pat_questions, function(q) q$id)
      
      responses <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT question_id, response_value::numeric as rv
       FROM responses
       WHERE project_id = $1
         AND assessment_type = 'selbstbewertung'
         AND question_id IN (%s)
         AND response_value IS NOT NULL
         AND response_value != ''",
          paste0("'", question_ids, "'", collapse = ", ")
        ),
        params = list(pid)
      )
      
      if (nrow(responses) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "Keine Daten vorhanden"))
      }
      
      means <- responses %>%
        group_by(question_id) %>%
        summarise(mean_value = mean(rv, na.rm = TRUE), .groups = "drop")
      
      values <- sapply(pat_questions, function(q) {
        row <- means[means$question_id == q$id, ]
        if (nrow(row) > 0) row$mean_value else NA
      })
      
      labels <- sapply(pat_questions, function(q) as.character(q$number))
      
      valid_idx <- !is.na(values)
      values <- values[valid_idx]
      labels <- labels[valid_idx]
      
      if (length(values) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "Keine gültigen Daten"))
      }
      
      plotly::plot_ly(
        type = 'scatterpolar',
        mode = 'lines+markers',
        r = values,
        theta = labels,
        fill = 'toself',
        fillcolor = "rgba(2, 158, 115, 0.3)",
        line = list(color = 'rgb(2, 158, 115)', width = 2)
      ) %>%
        plotly::layout(
          title = "Patienten & Angehörige - Durchschnitt",
          polar = list(radialaxis = list(visible = TRUE, range = c(0, 5)))
        )
    })
    
    # Einsender Spider
    output$ein_spider <- renderPlotly({
      message("!!! EIN_SPIDER TRIGGERED !!!")
      
      pid <- current_project_id()
      req(pid)
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      question_ids <- sapply(ein_questions, function(q) q$id)
      
      responses <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT question_id, response_value::numeric as rv
       FROM responses
       WHERE project_id = $1
         AND assessment_type = 'selbstbewertung'
         AND question_id IN (%s)
         AND response_value IS NOT NULL
         AND response_value != ''",
          paste0("'", question_ids, "'", collapse = ", ")
        ),
        params = list(pid)
      )
      
      if (nrow(responses) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "Keine Daten vorhanden"))
      }
      
      means <- responses %>%
        group_by(question_id) %>%
        summarise(mean_value = mean(rv, na.rm = TRUE), .groups = "drop")
      
      values <- sapply(ein_questions, function(q) {
        row <- means[means$question_id == q$id, ]
        if (nrow(row) > 0) row$mean_value else NA
      })
      
      labels <- sapply(ein_questions, function(q) as.character(q$number))
      
      valid_idx <- !is.na(values)
      values <- values[valid_idx]
      labels <- labels[valid_idx]
      
      if (length(values) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "Keine gültigen Daten"))
      }
      
      plotly::plot_ly(
        type = 'scatterpolar',
        mode = 'lines+markers',
        r = values,
        theta = labels,
        fill = 'toself',
        fillcolor = "rgba(204, 120, 188, 0.3)",
        line = list(color = 'rgb(204, 120, 188)', width = 2)
      ) %>%
        plotly::layout(
          title = "Einsender & Kooperationspartner - Durchschnitt",
          polar = list(radialaxis = list(visible = TRUE, range = c(0, 5)))
        )
    })
    
    # Qualität Spider
    output$qual_spider <- renderPlotly({
      message("!!! QUAL_SPIDER TRIGGERED !!!")
      
      pid <- current_project_id()
      req(pid)
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      question_ids <- sapply(qual_questions, function(q) q$id)
      
      responses <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT question_id, response_value::numeric as rv
       FROM responses
       WHERE project_id = $1
         AND assessment_type = 'selbstbewertung'
         AND question_id IN (%s)
         AND response_value IS NOT NULL
         AND response_value != ''",
          paste0("'", question_ids, "'", collapse = ", ")
        ),
        params = list(pid)
      )
      
      if (nrow(responses) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "Keine Daten vorhanden"))
      }
      
      means <- responses %>%
        group_by(question_id) %>%
        summarise(mean_value = mean(rv, na.rm = TRUE), .groups = "drop")
      
      values <- sapply(qual_questions, function(q) {
        row <- means[means$question_id == q$id, ]
        if (nrow(row) > 0) row$mean_value else NA
      })
      
      labels <- sapply(qual_questions, function(q) as.character(q$number))
      
      valid_idx <- !is.na(values)
      values <- values[valid_idx]
      labels <- labels[valid_idx]
      
      if (length(values) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "Keine gültigen Daten"))
      }
      
      plotly::plot_ly(
        type = 'scatterpolar',
        mode = 'lines+markers',
        r = values,
        theta = labels,
        fill = 'toself',
        fillcolor = "rgba(202, 145, 97, 0.3)",
        line = list(color = 'rgb(202, 145, 97)', width = 2)
      ) %>%
        plotly::layout(
          title = "Qualitätsindikatoren - Durchschnitt",
          polar = list(radialaxis = list(visible = TRUE, range = c(0, 5)))
        )
    })
    
    message("========================================")
    message("SPIDER PLOT OUTPUTS INITIALIZED")
    message("========================================")
    
#----------------helper function to generate spider plot images-----------------
    
    # Helper function to create spider plot and save as PNG
    create_spider_plot_image <- function(project_id, topic_key, questions_list, title, color, binary_qnums = integer()) {
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      # Filter binary questions if needed
      questions_filtered <- if (length(binary_qnums) > 0) {
        Filter(function(q) !(q$number %in% binary_qnums), questions_list)
      } else {
        questions_list
      }
      
      question_ids <- sapply(questions_filtered, function(q) q$id)
      
      # Get responses
      responses <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT question_id, response_value::numeric as rv
       FROM responses
       WHERE project_id = $1
         AND assessment_type = 'selbstbewertung'
         AND question_id IN (%s)
         AND response_value IS NOT NULL
         AND response_value != ''",
          paste0("'", question_ids, "'", collapse = ", ")
        ),
        params = list(project_id)
      )
      
      if (nrow(responses) == 0) {
        return(NULL)
      }
      
      # Calculate means
      means <- responses %>%
        group_by(question_id) %>%
        summarise(mean_value = mean(rv, na.rm = TRUE), .groups = "drop")
      
      # Map to questions
      values <- sapply(questions_filtered, function(q) {
        row <- means[means$question_id == q$id, ]
        if (nrow(row) > 0) row$mean_value else NA
      })
      
      labels <- sapply(questions_filtered, function(q) as.character(q$number))
      
      # Remove NAs
      valid_idx <- !is.na(values)
      values <- values[valid_idx]
      labels <- labels[valid_idx]
      
      if (length(values) == 0) {
        return(NULL)
      }
      
      # Create plotly object
      p <- plotly::plot_ly(
        type = 'scatterpolar',
        mode = 'lines+markers',
        r = values,
        theta = labels,
        fill = 'toself',
        fillcolor = paste0(color, ", 0.3)"),
        line = list(color = color, width = 2)
      ) %>%
        plotly::layout(
          title = title,
          polar = list(radialaxis = list(visible = TRUE, range = c(0, 5)))
        )
      
      return(p)
    }
    
    # Helper to get topic responses as data frame
    get_topic_responses_df <- function(project_id, topic_prefix, questions_list) {
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      df <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT u.username, r.question_id, r.response_value, r.text_value
       FROM responses r
       JOIN users u ON u.id = r.user_id
       WHERE r.assessment_type = 'selbstbewertung'
         AND r.project_id = $1
         AND r.question_id LIKE '%s%%'
       ORDER BY r.question_id, u.username",
          topic_prefix
        ),
        params = list(project_id)
      )
      
      if (nrow(df) > 0) {
        df$question <- sapply(df$question_id, function(qid) {
          q <- Find(function(x) x$id == qid, questions_list)
          if (!is.null(q)) q$label else qid
        })
        df <- df[, c("username", "question", "response_value", "text_value")]
        colnames(df) <- c("Nutzer", "Frage", "Bewertung", "Freitext")
      }
      
      return(df)
    }
    
    
    
    #----------------------- Next Button in the App for Selbstbewertung----------
    question_tabs <- c(
      "Berufsgruppe",
      "Führung",
      "Mitarbeitende",
      "Patient und Angehörige",
      "Einsender und Kooperationspartner",
      "Qualitätsindikatoren und Teschnishe und Medizinische Validation")
    
    output$question_next_nav_ui_inline <- renderUI({
      req(user_logged_in())
      req(input$question_info)
      
      all_tabs <- c("Berufsgruppe", "Führung", "Mitarbeitende", "Patient und Angehörige",
                    "Einsender und Kooperationspartner",
                    "Qualitätsindikatoren und Teschnishe und Medizinische Validation",
                    "Abschicken und Fragebogen herunterladen")
      
      current_idx <- match(input$question_info, all_tabs)
      
      # Don't show on Berufsgruppe (has its own button) or Abschicken (last tab)
      if (is.na(current_idx) || input$question_info == "Berufsgruppe" || 
          input$question_info == "Abschicken und Fragebogen herunterladen") {
        return(NULL)
      }
      
      # On Qualitätsindikatoren (last question tab): show "Weiter" to go to Abschicken
      actionButton(
        "next_tab_btn",
        "Weiter",
        icon = icon("arrow-right"),
        style = "background-color:#003B73;color:white;border:none;"
      )
    })
    
    observeEvent(input$next_tab_btn, {
      req(input$question_info)
      
      all_tabs <- c("Berufsgruppe", "Führung", "Mitarbeitende", "Patient und Angehörige",
                    "Einsender und Kooperationspartner",
                    "Qualitätsindikatoren und Teschnishe und Medizinische Validation",
                    "Abschicken und Fragebogen herunterladen")
      
      current_idx <- match(input$question_info, all_tabs)
      
      if (!is.na(current_idx) && current_idx < length(all_tabs)) {
        updateTabsetPanel(session, "question_info", selected = all_tabs[current_idx + 1])
      }
    }, ignoreInit = TRUE)
    
    # ✅ Conditionally show "Speichern" button
    output$save_button_ui <- renderUI({
      req(user_logged_in())
      req(input$question_info)
      
      # Show Speichern on all question tabs (NOT on Berufsgruppe or Abschicken)
      question_tabs <- c("Führung", "Mitarbeitende", "Patient und Angehörige",
                         "Einsender und Kooperationspartner",
                         "Qualitätsindikatoren und Teschnishe und Medizinische Validation")
      
      if (input$question_info %in% question_tabs) {
        actionButton(
          "save_draft_btn",
          "Speichern",
          icon = icon("save"),
          style = "background-color:#003B73;color:white;border:none;"
        )
      } else {
        NULL
      }
    })
    
    observeEvent(input$btn_project_to_labinfo, {
      message("Button clicked!")  # Debug message
      
      # Check if project exists
      pid <- current_project_id()
      message("Project ID: ", ifelse(is.null(pid), "NULL", pid))
      
      if (is.null(pid) || pid == "") {
        showNotification(
          "Bitte erstellen Sie zuerst ein Peer Review Projekt!", 
          type = "warning",
          duration = 5
        )
        return()
      }
      
      # Navigate to the correct tab
      updateTabItems(session, "sidebar_menu", "fill_labinfo")
      message("Navigating to fill_labinfo")
    })
    
    
    # Tab navigation
    observeEvent(input$btn_grundlagen_to_personal, {
      updateTabsetPanel(session, "labinfo_tabs", selected = "tab_personal")
    })
    
    observeEvent(input$btn_personal_back, {
      updateTabsetPanel(session, "labinfo_tabs", selected = "tab_grundlagen")
    })
    
    observeEvent(input$btn_personal_to_edv, {
      updateTabsetPanel(session, "labinfo_tabs", selected = "tab_edv")
    })
    
    observeEvent(input$btn_edv_back, {
      updateTabsetPanel(session, "labinfo_tabs", selected = "tab_personal")
    })
    
    observeEvent(input$btn_edv_to_input, {
      updateTabsetPanel(session, "labinfo_tabs", selected = "tab_input")
    })
    
    observeEvent(input$btn_input_back, {
      updateTabsetPanel(session, "labinfo_tabs", selected = "tab_edv")
    })
    
    observeEvent(input$btn_labinfo_complete, {
      showNotification("Laborinformationen erfolgreich abgeschlossen!", type = "message")
      updateTabItems(session, "sidebar", "invite_colleagues")  # Navigate to next main tab
    }) 
    
#---To ensure users have filled required fields before moving to the next tab:
    
    observeEvent(input$btn_grundlagen_to_personal, {
      # Check required fields
      if (is.null(input$name) || input$name == "") {
        showNotification("Bitte füllen Sie 'Name und Anschrift' aus!", type = "warning")
        return()
      }
      if (is.null(input$Internetadresse) || input$Internetadresse == "") {
        showNotification("Bitte füllen Sie 'Internetadresse' aus!", type = "warning")
        return()
      }
      
      # If all required fields are filled, proceed
      updateTabsetPanel(session, "labinfo_tabs", selected = "tab_personal")
    })

    
    #--------------------- next button for Fremdbewertung-----------------------
    # Define peer tabs
    peer_tabs <- c(
      "Führung",
      "Mitarbeitende",
      "Patient und Angehörige",
      "Einsender und Kooperationspartner",
      "Qualitätsindikatoren und Technische und Medizinische Validation"
    )
    
    # ✅ SAVE BUTTONS - one for each tab
    output$peer_save_button_fuehrung <- renderUI({
      req(user_logged_in())
      req(user_role() %in% c("leading_peer", "co_peer"))
      actionButton("save_peer_draft_btn_fuehrung", "Speichern", 
                   icon = icon("save"), class = "btn-fragebogen")
    })
    
    output$peer_save_button_mit <- renderUI({
      req(user_logged_in())
      req(user_role() %in% c("leading_peer", "co_peer"))
      actionButton("save_peer_draft_btn_mit", "Speichern", 
                   icon = icon("save"), class = "btn-fragebogen")
    })
    
    output$peer_save_button_pat <- renderUI({
      req(user_logged_in())
      req(user_role() %in% c("leading_peer", "co_peer"))
      actionButton("save_peer_draft_btn_pat", "Speichern", 
                   icon = icon("save"), class = "btn-fragebogen")
    })
    
    output$peer_save_button_ein <- renderUI({
      req(user_logged_in())
      req(user_role() %in% c("leading_peer", "co_peer"))
      actionButton("save_peer_draft_btn_ein", "Speichern", 
                   icon = icon("save"), class = "btn-fragebogen")
    })
    
    output$peer_save_button_qual <- renderUI({
      req(user_logged_in())
      req(user_role() %in% c("leading_peer", "co_peer"))
      actionButton("save_peer_draft_btn_qual", "Speichern", 
                   icon = icon("save"), class = "btn-fragebogen")
    })
    
    # ✅ NEXT BUTTONS - all tabs except last
    output$peer_next_button_fuehrung <- renderUI({
      req(user_logged_in())
      req(user_role() %in% c("leading_peer", "co_peer"))
      actionButton("peer_next_tab_btn_fuehrung", "Weiter →", 
                   icon = icon("arrow-right"), class = "btn-fragebogen")
    })
    
    output$peer_next_button_mit <- renderUI({
      req(user_logged_in())
      req(user_role() %in% c("leading_peer", "co_peer"))
      actionButton("peer_next_tab_btn_mit", "Weiter →", 
                   icon = icon("arrow-right"), class = "btn-fragebogen")
    })
    
    output$peer_next_button_pat <- renderUI({
      req(user_logged_in())
      req(user_role() %in% c("leading_peer", "co_peer"))
      actionButton("peer_next_tab_btn_pat", "Weiter →", 
                   icon = icon("arrow-right"), class = "btn-fragebogen")
    })
    
    output$peer_next_button_ein <- renderUI({
      req(user_logged_in())
      req(user_role() %in% c("leading_peer", "co_peer"))
      actionButton("peer_next_tab_btn_ein", "Weiter →", 
                   icon = icon("arrow-right"), class = "btn-fragebogen")
    })
    
    # ✅ LAST TAB: Submit button instead of Next
    output$peer_next_button_qual <- renderUI({
      req(user_logged_in())
      req(user_role() %in% c("leading_peer", "co_peer"))
      actionButton("submit_peer_fremdbewertung_all", "Fragebogen abschicken",
                   icon = icon("paper-plane"), class = "btn-fragebogen")
    })
    
    
    # ✅ NEXT TAB HANDLERS - separate observers (no delay)
    observeEvent(input$peer_next_tab_btn_fuehrung, {
      navigate_to_next_peer_tab()
    }, ignoreInit = TRUE)
    
    observeEvent(input$peer_next_tab_btn_mit, {
      navigate_to_next_peer_tab()
    }, ignoreInit = TRUE)
    
    observeEvent(input$peer_next_tab_btn_pat, {
      navigate_to_next_peer_tab()
    }, ignoreInit = TRUE)
    
    observeEvent(input$peer_next_tab_btn_ein, {
      navigate_to_next_peer_tab()
    }, ignoreInit = TRUE)
    
    # ✅ Extract navigation logic
    navigate_to_next_peer_tab <- function() {
      req(input$fragebogen_tabs_peer)
      
      peer_tabs <- c(
        "Führung",
        "Mitarbeitende",
        "Patient und Angehörige",
        "Einsender und Kooperationspartner",
        "Qualitätsindikatoren und Technische und Medizinische Validation"
      )
      
      current_tab <- input$fragebogen_tabs_peer
      current_idx <- match(current_tab, peer_tabs)
      
      if (!is.na(current_idx) && current_idx < length(peer_tabs)) {
        next_tab <- peer_tabs[current_idx + 1]
        message("→ Moving from '", current_tab, "' to '", next_tab, "'")
        updateTabsetPanel(session, "fragebogen_tabs_peer", selected = next_tab)
      }
    }
    
    # ✅ SUBMIT HANDLER (final tab)
    observeEvent(input$submit_peer_fremdbewertung_all, {
      message("\n========== SUBMIT FREMDBEWERTUNG ==========")
      
      req(user_logged_in())
      req(user_role() %in% c("leading_peer", "co_peer"))
      
      # First save the draft
      current_user_id <- user_id()
      project_id <- current_project_id()
      
      if (is.null(project_id) || project_id == "" || is.na(project_id)) {
        showNotification("Kein Projekt ausgewählt!", type = "error", duration = 3)
        return()
      }
      
      # Collect all answers
      all_answers <- list()
      
      all_question_lists <- list(
        fuehrung_questions,
        mit_questions,
        pat_questions,
        ein_questions,
        qual_questions
      )
      
      for (qlist in all_question_lists) {
        for (q in qlist) {
          qid <- q$id
          score_id <- paste0("FremdbewertungNum_", qid)
          text_id <- paste0("FremdbewertungText_", qid)
          
          if (!is.null(input[[score_id]]) && input[[score_id]] != "") {
            all_answers[[score_id]] <- input[[score_id]]
          }
          
          if (!is.null(input[[text_id]]) && input[[text_id]] != "") {
            all_answers[[text_id]] <- input[[text_id]]
          }
        }
      }
      
      message("Total answers to submit: ", length(all_answers))
      
      if (length(all_answers) == 0) {
        showNotification("Keine Daten zum Abschicken!", type = "warning", duration = 3)
        return()
      }
      
      answers_json <- jsonlite::toJSON(all_answers, auto_unbox = TRUE)
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      
      tryCatch({
        # Update draft to submitted status
        DBI::dbExecute(
          con,
          "UPDATE questionnaire_drafts 
       SET answers = $1, status = 'submitted', last_saved = NOW()
       WHERE user_id = $2 AND project_id = $3 AND questionnaire_id = 'peer_fremdbewertung'",
          params = list(answers_json, current_user_id, project_id)
        )
        
        # Also insert into responses table (if you want permanent record)
        # ... (optional) ...
        
        showNotification("Fremdbewertung erfolgreich abgeschickt! ✓", 
                         type = "message", duration = 5)
        
        message("✓ Fremdbewertung submitted")
        
      }, error = function(e) {
        message("ERROR submitting: ", e$message)
        showNotification(paste("Fehler beim Abschicken:", e$message), 
                         type = "error", duration = 10)
      })
      
      message("===========================================\n")
    }, ignoreInit = TRUE)
        
    
    
    
    #---------------LEADING PEER DASHBOARD - QUESTION DISPLAYS------------------

    # Führung Questions Display
    output$leading_peer_fuehrung_questions_display <- renderUI({
      questions_html <- lapply(fuehrung_questions, function(q) {
        tags$div(
          class = "dashboard-question-item",
          style = "border-left-color: #1976D2;",
          tags$strong(paste0(q$number, ". ")),
          tags$span(q$label)
        )
      })
      do.call(tagList, questions_html)
    })
    
    # Mitarbeitende Questions Display
    output$leading_peer_mitarbeitende_questions_display <- renderUI({
      questions_html <- lapply(mit_questions, function(q) {
        tags$div(
          class = "dashboard-question-item",
          style = "border-left-color: #F57C00;",
          tags$strong(paste0(q$number, ". ")),
          tags$span(q$label)
        )
      })
      do.call(tagList, questions_html)
    })
    
    # Patienten Questions Display
    output$leading_peer_patienten_questions_display <- renderUI({
      questions_html <- lapply(pat_questions, function(q) {
        tags$div(
          class = "dashboard-question-item",
          style = "border-left-color: #388E3C;",
          tags$strong(paste0(q$number, ". ")),
          tags$span(q$label)
        )
      })
      do.call(tagList, questions_html)
    })
    
    # Einsender Questions Display
    output$leading_peer_einsender_questions_display <- renderUI({
      questions_html <- lapply(ein_questions, function(q) {
        tags$div(
          class = "dashboard-question-item",
          style = "border-left-color: #D32F2F;",
          tags$strong(paste0(q$number, ". ")),
          tags$span(q$label)
        )
      })
      do.call(tagList, questions_html)
    })
    
    # Qualität Questions Display
    output$leading_peer_qualitaet_questions_display <- renderUI({
      questions_html <- lapply(qual_questions, function(q) {
        tags$div(
          class = "dashboard-question-item",
          style = "border-left-color: #FBC02D;",
          tags$strong(paste0(q$number, ". ")),
          tags$span(q$label)
        )
      })
      do.call(tagList, questions_html)
    })    
    
    
    
    #--------------LEADING PEER DASHBOARD - CONTENT GENERATION------------------

    # Helper to calculate WEIGHTED stats
    calc_weighted_stats <- function(df, questions_list, project_id) {
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      
      stats <- list()
      
      for (q in questions_list) {
        qid <- q$id
        
        # Get responses WITH group_size from questionnaire_drafts
        query <- "
      SELECT 
        r.response_value,
        CAST(qd.answers::json->>'group_size' AS INTEGER) as group_size
      FROM responses r
      JOIN users u ON u.id = r.user_id
      LEFT JOIN questionnaire_drafts qd 
        ON u.id::text = qd.user_id::text 
        AND qd.project_id = r.project_id
        AND qd.questionnaire_id = 'peer_review'
      WHERE r.project_id = $1 
        AND r.assessment_type = 'selbstbewertung'
        AND r.question_id = $2
        AND r.response_value != '' 
        AND r.response_value != 'N/A'
    "
        
        values_df <- DBI::dbGetQuery(con, query, params = list(project_id, qid))
        
        if (nrow(values_df) > 0) {
          # Convert to numeric
          values_df$response_numeric <- as.numeric(values_df$response_value)
          values_df <- values_df[!is.na(values_df$response_numeric), ]
          
          # Default group_size to 1 if missing
          values_df$group_size[is.na(values_df$group_size)] <- 1
          
          if (nrow(values_df) > 0) {
            # Calculate weighted mean
            weighted_sum <- sum(values_df$response_numeric * values_df$group_size)
            total_people <- sum(values_df$group_size)
            weighted_mean <- weighted_sum / total_people
            
            stats[[qid]] <- list(
              mean = weighted_mean,
              n = total_people,
              n_users = nrow(values_df)
            )
          }
        }
      }
      
      return(stats)
    }
    
    
    #-------------------Create Berufsgruppe Statistics Table--------------------

    #============================================================================
    # Create Complete Berufsgruppe Statistics Table (Per-Question + Average)
    #============================================================================
    #============================================================================
    # Create Complete Berufsgruppe Statistics Table (Per-Question + Average)
    #============================================================================
    create_berufsgruppe_stats_table_complete <- function(project_id, questions_list) {
      # Get the weighted data (same as radar plot uses)
      data <- get_berufsgruppe_radar_data_weighted(project_id, questions_list, integer())
      
      # 🔍 DEBUG
      cat("\n=== STATS TABLE DATA DEBUG ===\n")
      print("Data used for statistics table:")
      print(data)
      cat("===\n\n")
      
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }
      
      # Get breakdown for participant counts
      berufsgruppe_breakdown <- get_berufsgruppe_breakdown(project_id)
      
      # Separate Berufsgruppe column
      berufsgruppen <- data$berufsgruppe
      numeric_data <- data[, -1, drop = FALSE]
      
      # 🔍 DEBUG: Show calculated averages
      topic_averages <- rowMeans(numeric_data, na.rm = TRUE)
      cat("\n=== TOPIC AVERAGES DEBUG ===\n")
      for (i in 1:length(berufsgruppen)) {
        cat(berufsgruppen[i], ": ", sprintf("%.2f", topic_averages[i]), "\n")
      }
      cat("===\n\n")
      
      # Extract question numbers
      question_numbers <- sapply(questions_list, function(q) as.character(q$number))
      
      # Create table header
      header_row <- tags$tr(
        tags$th(rowspan = 2, "Berufsgruppe"),
        tags$th(rowspan = 2, "n"),
        tags$th(colspan = length(question_numbers), style = "text-align: center; border-bottom: 1px solid #00508f;", 
                "Durchschnitt pro Frage"),
        tags$th(rowspan = 2, HTML("Ø Thema<br/>(x&#772;<sub>w</sub>)"))
      )
      
      # Question number sub-header
      question_header_row <- tags$tr(
        lapply(question_numbers, function(qnum) {
          tags$th(style = "text-align: center; font-size: 11px; padding: 4px 6px;", qnum)
        })
      )
      
      # Create data rows
      data_rows <- lapply(1:length(berufsgruppen), function(i) {
        bg <- berufsgruppen[i]
        
        # Get n_people from breakdown
        n_people <- if (nrow(berufsgruppe_breakdown) > 0) {
          idx <- which(berufsgruppe_breakdown$berufsgruppe == bg)
          if (length(idx) > 0) berufsgruppe_breakdown$n_people[idx] else 0
        } else {
          0
        }
        
        tags$tr(
          tags$td(class = "stats-label", bg),
          tags$td(class = "stats-n", style = "text-align: center; color: #1976D2; font-weight: 500;", n_people),
          lapply(1:ncol(numeric_data), function(j) {
            val <- numeric_data[i, j]
            tags$td(
              class = "stats-question-value",
              style = "text-align: center; font-size: 12px; color: #555;",
              if (!is.na(val)) sprintf("%.2f", val) else "—"
            )
          }),
          tags$td(
            class = "stats-topic-avg",
            style = "text-align: center; color: #1976D2; font-weight: 600; font-size: 13px; background: #f0f7ff;",
            sprintf("%.2f", topic_averages[i])
          )
        )
      })
      
      # Overall totals row
      total_people <- sum(berufsgruppe_breakdown$n_people, na.rm = TRUE)
      overall_topic_avg <- mean(topic_averages, na.rm = TRUE)
      
      # Calculate overall average per question
      overall_question_avgs <- colMeans(numeric_data, na.rm = TRUE)
      
      total_row <- tags$tr(
        class = "stats-total",
        tags$td(tags$strong("Gesamt")),
        tags$td(style = "text-align: center; font-weight: 600;", tags$strong(total_people)),
        lapply(overall_question_avgs, function(val) {
          tags$td(
            style = "text-align: center; font-size: 12px; font-weight: 600;",
            if (!is.na(val)) sprintf("%.2f", val) else "—"
          )
        }),
        tags$td(
          style = "text-align: center; font-weight: 600; background: #e3f2fd;",
          tags$strong(sprintf("%.2f", overall_topic_avg))
        )
      )
      
      # Create complete table
      tags$div(
        class = "berufsgruppe-stats",
        tags$h4("Statistik nach Berufsgruppe"),
        tags$div(
          style = "overflow-x: auto;",
          tags$table(
            class = "stats-detail-table",
            tags$thead(
              header_row,
              question_header_row
            ),
            tags$tbody(
              data_rows,
              total_row
            )
          )
        )
      )
    }
    
    # Helper to get Berufsgruppe breakdown
   
    #============================================================================
    # Create Dashboard Rows with Weighted Statistics
    #============================================================================
    create_dashboard_rows <- function(df, questions_list, stats_list, plot_output_id, berufsgruppe_breakdown, project_id) {
      # ✅ Added project_id parameter ^
      
      question_rows <- lapply(seq_along(questions_list), function(idx) {
        q <- questions_list[[idx]]
        qid <- q$id
        q_num <- q$number
        
        selb_rows <- df[df$question_id == qid & df$response_value != "", ]
        
        # 🔍 DEBUG - Add this
        if (qid == "F1") {
          cat("\n=== DASHBOARD DEBUG F1 ===\n")
          print("Columns in selb_rows:")
          print(colnames(selb_rows))
          print("Data:")
          print(selb_rows[, c("response_value", "group_size")])
        }
        
        selb_display <- if (nrow(selb_rows) > 0) {
          
          # Check group_size exists
          if (!"group_size" %in% colnames(selb_rows)) {
            cat("ERROR: group_size missing in dashboard for", qid, "\n")
            selb_rows$group_size <- 1
          }
          selb_rows$group_size[is.na(selb_rows$group_size)] <- 1
          
          total_people <- sum(selb_rows$group_size, na.rm = TRUE)
          
          # Calculate weighted ratings
          rating_weighted <- aggregate(group_size ~ response_value, data = selb_rows, FUN = sum)
          rating_counts_weighted <- setNames(rating_weighted$group_size, rating_weighted$response_value)
          
          # 🔍 DEBUG for F1
          if (qid == "F1") {
            cat("rating_weighted:\n")
            print(rating_weighted)
            cat("rating_counts_weighted:\n")
            print(rating_counts_weighted)
          }
          
          rating_display <- tags$div(
            class = "rating-inline",
            lapply(c("N/A", "1", "2", "3", "4", "5"), function(j) {
              count <- rating_counts_weighted[as.character(j)]
              count <- if (is.na(count)) 0 else as.integer(count)
              
              if (count > 0) {
                if (count == 1) {
                  tags$span(class = "rating-active", j)
                } else {
                  tags$span(
                    class = "rating-active rating-with-count",
                    j,
                    tags$sup(class = "rating-count", paste0("\u00d7", count))
                  )
                }
              } else {
                tags$span(class = "rating-inactive", j)
              }
            })
          )
          
          respondent_info <- if (total_people > 1) {
            tags$div(
              class = "respondent-info",
              icon("users"),
              paste0(" ", total_people, " Teilnehmer")
            )
          } else {
            NULL
          }
          
          texts_with_group <- selb_rows[selb_rows$text_value != "" & !is.na(selb_rows$text_value), 
                                        c("text_value", "group_size")]
          
          text_display <- if (nrow(texts_with_group) > 0) {
            text_weighted <- aggregate(group_size ~ text_value, data = texts_with_group, FUN = sum)
            
            tags$div(
              class = "selbst-text-container",
              lapply(1:nrow(text_weighted), function(i) {
                txt <- text_weighted$text_value[i]
                count <- text_weighted$group_size[i]
                tags$div(
                  class = "selbst-text-item",
                  icon("comment"),
                  txt,
                  tags$span(class = "text-count-badge", paste0("\u00d7", count))
                )
              })
            )
          } else {
            NULL
          }
          
          tags$div(rating_display, respondent_info, text_display)
        } else {
          tags$span(class = "no-data", "\u2014")
        }
        
        # Stats with weighted average
        stat_info <- if (qid %in% names(stats_list)) {
          stat <- stats_list[[qid]]
          tags$div(
            class = "stats-container",
            tags$div(
              class = "stat-item stat-weighted",
              tags$span(class = "stat-label", HTML("x&#772;<sub>w</sub>: ")),
              tags$span(class = "stat-value", sprintf("%.2f", stat$mean))
            ),
            tags$div(
              class = "stat-item",
              tags$span(class = "stat-label", "n: "),
              tags$span(class = "stat-value", stat$n)
            )
          )
        } else {
          tags$span(class = "no-data", "\u2014")
        }
        
        # First row includes plot AND both tables
        if (idx == 1) {
          
          # Participant breakdown table (left side)
          # breakdown_table <- if (nrow(berufsgruppe_breakdown) > 0) {
          #   total_people <- sum(berufsgruppe_breakdown$n_people, na.rm = TRUE)
          #   
          #   tags$div(
          #     class = "berufsgruppe-breakdown",
          #     tags$h4("Teilnehmer nach Berufsgruppe"),
          #     tags$table(
          #       class = "breakdown-table",
          #       tags$tbody(
          #         lapply(1:nrow(berufsgruppe_breakdown), function(i) {
          #           row <- berufsgruppe_breakdown[i, ]
          #           tags$tr(
          #             tags$td(class = "breakdown-label", row$berufsgruppe),
          #             tags$td(class = "breakdown-value", 
          #                     paste0("n = ", row$n_people))
          #           )
          #         }),
          #         tags$tr(
          #           class = "breakdown-total",
          #           tags$td(class = "breakdown-label", tags$strong("Gesamt")),
          #           tags$td(class = "breakdown-value", tags$strong(paste0("n = ", total_people)))
          #         )
          #       )
          #     )
          #   )
          # } else {
          #   NULL
          #}
          
          # ✅ Statistics table (right side) - now project_id is available
          stats_table <- create_berufsgruppe_stats_table_complete(project_id, questions_list)
          
          # Container with both tables side by side
          tables_container <- tags$div(
            class = "berufsgruppe-tables-container",
            #breakdown_table,
            stats_table
          )
          
          tags$tr(
            class = "dashboard-row",
            tags$td(class = "dashboard-col-question",
                    tags$strong(paste0(q_num, ". ")), q$label),
            tags$td(class = "dashboard-col-selbst", selb_display),
            tags$td(class = "dashboard-col-stats", stat_info),
            tags$td(
              class = "dashboard-col-plot",
              rowspan = length(questions_list),
              tables_container,
              plotOutput(plot_output_id, height = "600px")
            )
          )
        } else {
          # Other rows (not first)
          tags$tr(
            class = "dashboard-row",
            tags$td(class = "dashboard-col-question",
                    tags$strong(paste0(q_num, ". ")), q$label),
            tags$td(class = "dashboard-col-selbst", selb_display),
            tags$td(class = "dashboard-col-stats", stat_info)
          )
        }
      })
      
      do.call(tagList, question_rows)
    }
    
    
    # 1. FÜHRUNG TAB
    output$leading_peer_dashboard_fuehrung <- renderUI({
      req(current_project_id())
      project_id <- current_project_id()
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      # ✅ UPDATED QUERY (includes group_size):
      fuehrung_df <- DBI::dbGetQuery(con, "
  SELECT 
    u.username, 
    u.id as user_id,
    r.question_id, 
    r.response_value, 
    r.text_value,
    CAST(qd.answers::json->>'group_size' AS INTEGER) as group_size
  FROM responses r
  JOIN users u ON u.id = r.user_id
  LEFT JOIN questionnaire_drafts qd 
    ON u.id::text = qd.user_id::text 
    AND qd.project_id::text = r.project_id::text
    AND qd.questionnaire_id = 'peer_review'
  WHERE r.project_id::text = $1::text 
    AND r.assessment_type = 'selbstbewertung'
    AND r.question_id LIKE 'F%'
  ORDER BY r.question_id, u.username
", params = list(as.character(project_id)))
      
      
      fuehrung_stats <- calc_weighted_stats(fuehrung_df, fuehrung_questions, project_id)
      berufsgruppe_breakdown <- get_berufsgruppe_breakdown(project_id)
      
      
      # 🔍 DEBUG: Check if breakdown has data
      cat("\n=== FÜHRUNG BERUFSGRUPPE BREAKDOWN ===\n")
      print(berufsgruppe_breakdown)
      print(paste("Number of rows:", nrow(berufsgruppe_breakdown)))
      cat("===\n")
      
      tags$div(
        class = "dashboard-wrapper",
        tags$table(
          class = "dashboard-table",
          tags$thead(
            tags$tr(
              tags$th(class = "dashboard-col-question", "Fragen"),
              tags$th(class = "dashboard-col-selbst", "Selbstbewertung"),
              tags$th(class = "dashboard-col-stats", "Statistik"),
              tags$th(class = "dashboard-col-plot", "Durchschnitt nach Berufsgruppe")
            )
          ),
          tags$tbody(
            create_dashboard_rows(
              fuehrung_df, 
              fuehrung_questions, 
              fuehrung_stats, 
              "overlay_radar_fuehrung_leading",
              berufsgruppe_breakdown,
              project_id
            )
          )
        )
      )
    })
    
    # 2. MITARBEITENDE TAB
    output$leading_peer_dashboard_mitarbeitende <- renderUI({
      req(current_project_id())
      project_id <- current_project_id()
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      mit_df <- DBI::dbGetQuery(con, "
  SELECT 
    u.username, 
    u.id as user_id,
    r.question_id, 
    r.response_value, 
    r.text_value,
    CAST(qd.answers::json->>'group_size' AS INTEGER) as group_size
  FROM responses r
  JOIN users u ON u.id = r.user_id
  LEFT JOIN questionnaire_drafts qd 
    ON u.id::text = qd.user_id::text 
    AND qd.project_id::text = r.project_id::text
    AND qd.questionnaire_id = 'peer_review'
  WHERE r.project_id::text = $1::text 
    AND r.assessment_type = 'selbstbewertung'
    AND r.question_id LIKE 'M%'
  ORDER BY r.question_id, u.username
", params = list(as.character(project_id)))
      
      
      
      mit_stats <- calc_weighted_stats(mit_df, mit_questions, project_id)
      berufsgruppe_breakdown <- get_berufsgruppe_breakdown(project_id)
      
      tags$div(
        class = "dashboard-wrapper",
        tags$table(
          class = "dashboard-table",
          tags$thead(
            tags$tr(
              tags$th(class = "dashboard-col-question", "Fragen"),
              tags$th(class = "dashboard-col-selbst", "Selbstbewertung"),
              tags$th(class = "dashboard-col-stats", "Statistik"),
              tags$th(class = "dashboard-col-plot", "Durchschnitt nach Berufsgruppe")
            )
          ),
          tags$tbody(
            create_dashboard_rows(
              mit_df, 
              mit_questions, 
              mit_stats, 
              "overlay_radar_mitarbeitende_leading",
              berufsgruppe_breakdown,
              project_id
            )
          )
        )
      )
    })
    
    # 3. PATIENTEN TAB
    output$leading_peer_dashboard_patienten <- renderUI({
      req(current_project_id())
      project_id <- current_project_id()
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      pat_df <- DBI::dbGetQuery(con, "
  SELECT 
    u.username, 
    u.id as user_id,
    r.question_id, 
    r.response_value, 
    r.text_value,
    CAST(qd.answers::json->>'group_size' AS INTEGER) as group_size
  FROM responses r
  JOIN users u ON u.id = r.user_id
  LEFT JOIN questionnaire_drafts qd 
    ON u.id::text = qd.user_id::text 
    AND qd.project_id::text = r.project_id::text
    AND qd.questionnaire_id = 'peer_review'
  WHERE r.project_id::text = $1::text 
    AND r.assessment_type = 'selbstbewertung'
    AND r.question_id LIKE 'P%'
  ORDER BY r.question_id, u.username
", params = list(as.character(project_id)))
      
      pat_stats <- calc_weighted_stats(pat_df, pat_questions, project_id)
      berufsgruppe_breakdown <- get_berufsgruppe_breakdown(project_id)
      
      tags$div(
        class = "dashboard-wrapper",
        tags$table(
          class = "dashboard-table",
          tags$thead(
            tags$tr(
              tags$th(class = "dashboard-col-question", "Fragen"),
              tags$th(class = "dashboard-col-selbst", "Selbstbewertung"),
              tags$th(class = "dashboard-col-stats", "Statistik"),
              tags$th(class = "dashboard-col-plot", "Durchschnitt nach Berufsgruppe")
            )
          ),
          tags$tbody(
            create_dashboard_rows(
              pat_df, 
              pat_questions, 
              pat_stats, 
              "overlay_radar_patienten_leading",
              berufsgruppe_breakdown,
              project_id
            )
          )
        )
      )
    })
    
    # 4. EINSENDER TAB
    output$leading_peer_dashboard_einsender <- renderUI({
      req(current_project_id())
      project_id <- current_project_id()
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      ein_df <- DBI::dbGetQuery(con, "
  SELECT 
    u.username, 
    u.id as user_id,
    r.question_id, 
    r.response_value, 
    r.text_value,
    CAST(qd.answers::json->>'group_size' AS INTEGER) as group_size
  FROM responses r
  JOIN users u ON u.id = r.user_id
  LEFT JOIN questionnaire_drafts qd 
    ON u.id::text = qd.user_id::text 
    AND qd.project_id::text = r.project_id::text
    AND qd.questionnaire_id = 'peer_review'
  WHERE r.project_id::text = $1::text 
    AND r.assessment_type = 'selbstbewertung'
    AND r.question_id LIKE 'E%'
  ORDER BY r.question_id, u.username
", params = list(as.character(project_id)))
      
      ein_stats <- calc_weighted_stats(ein_df, ein_questions, project_id)
      berufsgruppe_breakdown <- get_berufsgruppe_breakdown(project_id)
      
      tags$div(
        class = "dashboard-wrapper",
        tags$table(
          class = "dashboard-table",
          tags$thead(
            tags$tr(
              tags$th(class = "dashboard-col-question", "Fragen"),
              tags$th(class = "dashboard-col-selbst", "Selbstbewertung"),
              tags$th(class = "dashboard-col-stats", "Statistik"),
              tags$th(class = "dashboard-col-plot", "Durchschnitt nach Berufsgruppe")
            )
          ),
          tags$tbody(
            create_dashboard_rows(
              ein_df, 
              ein_questions, 
              ein_stats, 
              "overlay_radar_einsender_leading",
              berufsgruppe_breakdown,
              project_id
            )
          )
        )
      )
    })
    
    # 5. QUALITÄT TAB
    
    output$leading_peer_dashboard_qualitaet <- renderUI({
      req(current_project_id())
      project_id <- current_project_id()
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      qual_df <- DBI::dbGetQuery(con, "
  SELECT 
    u.username, 
    u.id as user_id,
    r.question_id, 
    r.response_value, 
    r.text_value,
    CAST(qd.answers::json->>'group_size' AS INTEGER) as group_size
  FROM responses r
  JOIN users u ON u.id = r.user_id
  LEFT JOIN questionnaire_drafts qd 
    ON u.id::text = qd.user_id::text 
    AND qd.project_id::text = r.project_id::text
    AND qd.questionnaire_id = 'peer_review'
  WHERE r.project_id::text = $1::text 
    AND r.assessment_type = 'selbstbewertung'
    AND r.question_id LIKE 'Q%'
  ORDER BY r.question_id, u.username
", params = list(as.character(project_id)))
      
      qual_stats <- calc_weighted_stats(qual_df, qual_questions, project_id)
      berufsgruppe_breakdown <- get_berufsgruppe_breakdown(project_id)
      
      tags$div(
        class = "dashboard-wrapper",
        tags$table(
          class = "dashboard-table",
          tags$thead(
            tags$tr(
              tags$th(class = "dashboard-col-question", "Fragen"),
              tags$th(class = "dashboard-col-selbst", "Selbstbewertung"),
              tags$th(class = "dashboard-col-stats", "Statistik"),
              tags$th(class = "dashboard-col-plot", "Durchschnitt nach Berufsgruppe")
            )
          ),
          tags$tbody(
            create_dashboard_rows(
              qual_df, 
              qual_questions, 
              qual_stats, 
              "overlay_radar_qualitaet_leading",
              berufsgruppe_breakdown,
              project_id
            )
          )
        )
      )
    })
#-------------------------------------------------------------------------------

    # Core session state (define ONCE, at the top!)
    user_logged_in <- reactiveVal(FALSE)
    user_id <- reactiveVal(NULL)
    user_role <- reactiveVal(NULL)
    current_username <- reactiveVal(NULL)
    current_project_id <- reactiveVal(NULL)
    current_project_name <- reactiveVal(NULL)
    current_submission_id <- reactiveVal(NULL)
    redeemed_code_info <- reactiveVal(NULL)
    project_data_changed <- reactiveVal(0)
    responses_data_changed <- reactiveVal(0)
    invitation_data_changed <- reactiveVal(0)
    project_title_val <- reactiveVal("")
    consent_given <- reactiveVal(FALSE) 
    
    # ---- outputs (define each output ONCE) ----
    output$userRole <- reactive({ user_role() })
    outputOptions(output, "userRole", suspendWhenHidden = FALSE)
    output$isAdmin <- reactive({ user_role() == "admin" })
    output$isLeiter <- reactive({ user_role() == "laborleitung" })
    output$isLeadingPeer <- reactive({ user_role() == "leading_peer" })
    output$isColleague <- reactive({ user_role() == "colleague" })
    output$isCoPeer <- reactive({ user_role() == "co_peer" })
    outputOptions(output, "isAdmin", suspendWhenHidden = FALSE)
    outputOptions(output, "isLeiter", suspendWhenHidden = FALSE)
    outputOptions(output, "isLeadingPeer", suspendWhenHidden = FALSE)
    outputOptions(output, "isColleague", suspendWhenHidden = FALSE)
    outputOptions(output, "isCoPeer", suspendWhenHidden = FALSE)
    
    
    is_co_peer <- reactive({
      user_role() == "co_peer"
    })
    # Returns TRUE if current user is a leading peer, otherwise FALSE
    is_leading_peer <- function() {
      !is.null(user_role()) && user_role() == "leading_peer"
    }
    
  
# ---------------- Admin Guide Navigation -------------------------------------
    observeEvent(input$goToCreateProject, {
      updateTabItems(session, "sidebar_menu", selected = "create_project")
    })
    
    observeEvent(input$goToManageUsers, {
      updateTabItems(session, "sidebar_menu", selected = "user_admin")
    })
    
    observeEvent(input$goToInvitationCodes, {
      updateTabItems(session, "sidebar_menu", selected = "admin_invitations")
    })
    
    observeEvent(input$goToReports, {
      updateTabItems(session, "sidebar_menu", selected = "monitor_progress")
    })
    
    
#--------------------landing page--------------------------------
    # observeEvent(input$go_to_login_from_landing, {
    #   updateTabItems(session, "sidebar_menu", selected = "login")
    # })
    
#------- Update current_project_id() when a project is selected ----
    observeEvent(input$selected_labinfo_project, {
      if (!is.null(input$selected_labinfo_project) && input$selected_labinfo_project != "") {
        current_project_id(input$selected_labinfo_project)
      }
    })    
    
#--------------Project Selector: Dynamic for each role-------------------------
    
    # Reactive value for triggering refresh after project creation
    project_data_changed <- reactiveVal(0)
    
    get_project_choices <- function(role, user_id = NULL) {
      if (is.null(role) || length(role) == 0)
        role <- ""
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      projs <- switch(
        role,
        "admin" = DBI::dbGetQuery(con, "SELECT id, title FROM projects ORDER BY created_at DESC"),
        "laborleitung" = DBI::dbGetQuery(
          con,
          "SELECT id, title FROM projects WHERE owner_id = $1 ORDER BY created_at DESC",
          params = list(user_id)
        ),
        "leading_peer" = DBI::dbGetQuery(con, "SELECT id, title FROM projects ORDER BY created_at DESC"),,
        # fallback
        DBI::dbGetQuery(con, "SELECT id, title FROM projects ORDER BY created_at DESC")
      )
      setNames(projs$id, projs$title)
    }
    
    output$dashboard_project_selector_ui <- renderUI({
      role <- user_role()
      uid <- user_id()
      if (is.null(role) || length(role) == 0)
        return(NULL)
      choices <- get_project_choices(role, uid)
      if (role == "admin") {
        selectInput(
          "dashboard_project_selector",
          "Peer Review auswählen:",
          choices = choices,
          selected = if (length(choices) > 0) choices[[1]] else NULL
        )
      } else if (role == "laborleitung") {
        selectInput(
          "laborleiter_dashboard_project_selector",
          "Peer Review auswählen:",
          choices = choices,
          selected = if (length(choices) > 0) choices[[1]] else NULL
        )
      } else if (role == "leading_peer") {
        selectInput(
          "leading_peer_dashboard_project_selector",
          "Peer Review auswählen:",
          choices = choices,
          selected = if (length(choices) > 0) choices[[1]] else NULL
        )
      } else {
        NULL
      }
    })
    
    output$refresh_dashboard_projects_ui <- renderUI({
      role <- user_role()
      if (is.null(role) || length(role) == 0)
        return(NULL)
      if (role == "admin") {
        actionButton("refresh_dashboard_projects",
                     "Peer Reviews aktualisieren",
                     icon = icon("refresh"))
      } else if (role == "laborleitung") {
        actionButton(
          "refresh_laborleiter_dashboard_projects",
          "Peer Reviews aktualisieren",
          icon = icon("refresh")
        )
      } else {
        NULL
      }
    })
    
    observe({
      project_data_changed() # triggers on change
      role <- user_role()
      uid <- user_id()
      if (is.null(role) || length(role) == 0)
        return()
      choices <- get_project_choices(role, uid)
      if (role == "admin") {
        updateSelectInput(
          session,
          "dashboard_project_selector",
          choices = choices,
          selected = if (length(choices) > 0)
            choices[[1]]
          else
            NULL
        )
      } else if (role == "laborleitung") {
        updateSelectInput(
          session,
          "laborleiter_dashboard_project_selector",
          choices = choices,
          selected = if (length(choices) > 0)
            choices[[1]]
          else
            NULL
        )
      }
    })
    
    observeEvent(input$refresh_dashboard_projects, {
      project_data_changed(project_data_changed() + 1)
    })
    observeEvent(input$refresh_laborleiter_dashboard_projects, {
      project_data_changed(project_data_changed() + 1)
    })
    
    # Reactive expression to get the currently selected project ID
    get_selected_project_id <- reactive({
      role <- user_role()
      if (role == "laborleitung") {
        # This should be the ID of the selectInput in the Labor Leiter's UI
        return(input$laborleiter_dashboard_project_selector)
      } else if (role == "admin") {
        # This should be the ID of the selectInput in the Admin's UI
        return(input$dashboard_project_selector)
      }
      return(NULL) # Return NULL if no relevant role or selector
    })
    
    output$dashboard_title <- renderText({
      role <- user_role()
      if (is.null(role) || length(role) == 0)
        return("Dashboard")
      if (role == "admin")
        "Dashboard"
      else if (role == "laborleitung")
        ""
      else
        ""
    })
    
    output$report_project_selector_leading_ui <- renderUI({
      req(user_logged_in())
      role <- user_role()
      uid <- user_id()
      choices <- get_project_choices(role, uid)
      if (length(choices) == 0) {
        return(h5("Keine Projekte verfügbar"))
      }
      selectInput(
        "report_project_selector_leading",
        "Peer Review für Bericht auswählen:",
        choices = choices,
        selected = choices[[1]]
      )
    })
    
#------------------------colleague dashboard part-------------------------------    
#--------------Display title of the project on the dashboard for colleagues<-----
    output$colleague_dashboard_project_title <- renderText({
      req(user_id())
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      pid <- DBI::dbGetQuery(
        con,
        "SELECT project_id FROM users WHERE id = $1",
        params = list(user_id())
      )$project_id[1]
      
      if (is.null(pid) || is.na(pid) || pid == 0) return("Kein Projekt zugeordnet")
      
      title <- DBI::dbGetQuery(
        con,
        "SELECT title FROM projects WHERE id = $1",
        params = list(as.integer(pid))
      )$title[1]
      
      if (is.null(title) || is.na(title) || title == "") "Kein Projekt zugeordnet" else title
    })
    
    
    # function to get the current user's project
    get_user_project_id <- function(user_id) {
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      DBI::dbGetQuery(con, "SELECT project_id FROM users WHERE id = $1", params = list(user_id))$project_id[1]
    }
    
    observe({
      # Only run when user is logged in and tab is "reports"
      req(user_id())
      role <- user_role()
      
      # For labor_leiter: show all their projects
      if (role == "colleague") {
        pid <- get_user_project_id(user_id())
        con <- get_db_con()
        on.exit(DBI::dbDisconnect(con))
        pname <- DBI::dbGetQuery(con, "SELECT title FROM projects WHERE id = $1", params = list(pid))$title[1]
        updateSelectInput(session, "report_project_selector",
                          choices = setNames(pid, pname),
                          selected = pid)
      }
      # Add any logic for other roles as needed
    })
    
    
    #-------- Make the guide buttons navigate for colleague dashboard-----------
    observeEvent(input$goColleagueDashboard, {
      updateTabItems(session, "sidebar_menu", selected = "colleague_dashboard")
    })
    
    observeEvent(input$goColleagueLabData, {
      updateTabItems(session, "sidebar_menu", selected = "labordaten_ansehen")
    })
    
    observeEvent(input$goColleagueQuestionnaire, {
      updateTabItems(session, "sidebar_menu", selected = "fill_questionnaire")
    })
    
    observeEvent(input$goColleagueReports, {
      updateTabItems(session, "sidebar_menu", selected = "reports")
    })
    
    # from dashboard quick buttons
    observeEvent(input$goColleagueLabData2, {
      updateTabItems(session, "sidebar_menu", selected = "labordaten_ansehen")
    })
    observeEvent(input$questionnaire, {
      updateTabItems(session, "sidebar_menu", selected = "fill_questionnaire")
    })
    observeEvent(input$goColleagueReports2, {
      updateTabItems(session, "sidebar_menu", selected = "reports")
    })
    
    
#--------------saving leading peer sidemenu entries----------------------------

    report_data <- reactiveValues(
      teilnehmende = NULL,
      ort = NULL,
      datum = NULL,
      swot = NULL,
      best_practices = NULL,
      final_remarks = NULL
    )
    
    # Save Report Data
    observeEvent(input$save_report_data, {
      report_data$teilnehmende <- input$report_teilnehmende
      report_data$ort <- input$report_ort
      report_data$datum <- input$report_datum
      showNotification("Berichtsdaten gespeichert.", type = "message")
    })
    
    # Save SWOT
    observeEvent(input$save_swot_analysis, {
      report_data$swot <- input$swot_analysis
      showNotification("SWOT Analyse gespeichert.", type = "message")
    })
    
    # Save Best Practices
    observeEvent(input$save_best_practices, {
      report_data$best_practices <- input$best_practices
      showNotification("Best Practices gespeichert.", type = "message")
    })
    
    # Save Final Remarks
    observeEvent(input$save_final_remarks, {
      report_data$final_remarks <- input$final_remarks
      showNotification("Final Remarks gespeichert.", type = "message")
    })
    
    
    
    
#--------------refresh button for leading_peer --------------------------------- 
    
    output$refresh_dashboard_projects_ui <- renderUI({
      role <- user_role()
      if (is.null(role) || length(role) == 0)
        return(NULL)
      if (role == "admin") {
        actionButton("refresh_dashboard_projects", "Peer Reviews aktualisieren", icon = icon("refresh"))
      } else if (role == "laborleitung") {
        actionButton("refresh_laborleiter_dashboard_projects", "Peer Reviews aktualisieren", icon = icon("refresh"))
      } else if (role == "leading_peer") {
        actionButton("refresh_leading_peer_dashboard_projects", "Peer Reviews aktualisieren", icon = icon("refresh"))
      } else {
        NULL
      }
    })
    
    observeEvent(input$refresh_leading_peer_dashboard_projects, {
      project_data_changed(project_data_changed() + 1)
    })
    
    
    observe({
      project_data_changed() # triggers on change
      role <- user_role()
      uid <- user_id()
      if (is.null(role) || length(role) == 0)
        return()
      choices <- get_project_choices(role, uid)
      if (role == "admin") {
        updateSelectInput(session, "dashboard_project_selector", choices = choices, selected = if (length(choices) > 0) choices[[1]] else NULL)
      } else if (role == "laborleitung") {
        updateSelectInput(session, "laborleiter_dashboard_project_selector", choices = choices, selected = if (length(choices) > 0) choices[[1]] else NULL)
      } else if (role == "leading_peer") {
        updateSelectInput(session, "leading_peer_dashboard_project_selector", choices = choices, selected = if (length(choices) > 0) choices[[1]] else NULL)
      }
    })
    
    get_selected_project_id <- reactive({
      role <- user_role()
      if (role == "laborleitung") {
        return(input$laborleiter_dashboard_project_selector)
      } else if (role == "admin") {
        return(input$dashboard_project_selector)
      } else if (role == "leading_peer") {
        return(input$leading_peer_dashboard_project_selector)
      }
      return(NULL)
    })
    
    
    # Redirect colleagues after login
    observeEvent(user_role(), {
      role <- user_role()
      if (!is.null(role) && role == "colleague") {
        updateTabItems(session, "sidebar_menu", selected = "colleague_dashboard")
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    
    # Use project_title in the report
#---------------------- fragebogen bearbeiten-----------------------------------
    
    output$labor_leiter_project_selector <- renderUI({
      projs <- labor_leiter_projects() # <-- this should fetch projects for labor_leiter
      if (nrow(projs) == 0)
        return(h5("Keine eigenen Peer Reviews vorhanden"))
      selectInput(
        "selected_labor_leiter_project",
        "Peer Review auswählen:",
        choices = setNames(projs$id, projs$title)
      )
    })
    
    observeEvent(input$selected_labor_leiter_project, {
      current_project_id(as.character(input$selected_labor_leiter_project))
      output$draft_status_ui <- renderUI(div(
        style = "color:blue",
        paste("Projekt gewählt:", input$selected_labor_leiter_project)
      ))
    })
    
    
    observeEvent(input$resume_draft_btn, {
      req(current_project_id())
      req(user_logged_in())
      output$draft_status_ui <- renderUI(div(style = "color:orange", "Fragebogen wird geladen..."))
      resume_draft_from_db()
      output$draft_status_ui <- renderUI(div(style = "color:green", "Fragebogen geladen. Sie können jetzt bearbeiten."))
    })
    
    
    # In server
    observeEvent(input$edit_questionnaire_btn, {
      updateTabItems(session, "sidebar_menu", selected = "fill_questionnaire")
      # Optionally, reset/disable the button after navigation if needed
    })
    
    
    # Track the last non-"Abschicken" questionnaire tab
    last_qn_tab <- reactiveVal(NULL)
    
    # Update last_qn_tab every time the user changes tabs
    observeEvent(input$active_questionnaire, {
      if (!is.null(input$active_questionnaire) &&
          input$active_questionnaire != "Abschicken") {
        last_qn_tab(input$active_questionnaire)
      }
    })
    
    
    #-----------------------------questionnaire mapping --------------------------
    qn_map <- c(
      "Führung" = "fuehrung",
      "Mitarbeitende" = "mitarbeitende",
      "Patient und Angehörige" = "patienten",
      "Einsender und Kooperationspartner" = "einsender",
      "Qualitätsindikatoren und Teschnishe und Medizinische Validation" = "qualitaet"
    )
    
    
    output$user_dashboard_content <- renderUI({
      req(user_logged_in())
      tagList(fluidRow(
        valueBoxOutput("fragebogenFilledUsersBox"),
        valueBoxOutput("myFragebogenStatusBox")
      ),
      fluidRow(
        box(
          title = "Mein Fragebogen: Bereichsauswertung",
          status = "primary",
          solidHeader = TRUE,
          width = 12
        )
      ))
    })
    
    output$project_title_box <- renderUI({
      req(current_project_name())
      box(
        title = NULL,
        solidHeader = TRUE,
        status = "info",
        width = 12,
        style = "margin-bottom: 15px; background-color: #e3f2fd; border-left: 5px solid #003B73;",
        h4(
          icon("folder-open"),
          "Aktuelles Projekt: ",
          tags$span(style = "color: #003B73; font-weight: bold;", current_project_name())
        )
      )
    })
    
    get_input_safe <- function(id) {
      val <- input[[id]]
      if (is.null(val) || length(val) == 0)
        return(NA_character_)
      if (is.logical(val) &&
          length(val) == 1 && !is.na(val))
        return(as.character(val))
      if (is.character(val) && length(val) == 1)
        return(val)
      if (is.numeric(val) &&
          length(val) == 1)
        return(as.character(val))
      return(NA_character_)
    }
    
    #-------------------------------------sidebar menu-----------------------------
    output$sidebar_menu_ui <- renderUI({
      role <- user_role()
      logged_in <- user_logged_in()
      
      message("=== SIDEBAR RENDER TRIGGERED ===")
      message("Logged in: ", logged_in)
      message("Role: ", ifelse(is.null(role), "NULL", role))
      message("User ID: ", ifelse(is.null(user_id()), "NULL", user_id()))
      
      # ✅ Not logged in - show public menu and auto-navigate to start
      if (!logged_in || is.null(role) || length(role) == 0 || is.na(role)) {
        message("Sidebar - RETURNING not-logged-in menu")
        
        # ✅ AUTO-NAVIGATE TO START PAGE
        if (is.null(session$userData$navigated_to_start)) {
          session$userData$navigated_to_start <- TRUE
          shinyjs::delay(100, {
            message("Auto-navigating to start page")
            updateTabItems(session, "sidebar_menu", selected = "start")
          })
        }
        
        menu <- sidebarMenu(
          id = "sidebar_menu",
          menuItem("Start", tabName = "start", icon = icon("home")),
          menuItem("Login", tabName = "login", icon = icon("sign-in-alt")),
          menuItem("Registrieren", tabName = "register", icon = icon("user-plus"))
        )
        
        return(menu)
      }
      
      # ✅ User is logged in - check consent
      has_consent <- consent_given()
      message("Sidebar - consent_given(): ", has_consent)
      
      # Check DB if not set
      if (!has_consent && !is.null(user_id()) && user_id() > 0) {
        message("Sidebar - checking DB for consent...")
        
        tryCatch({
          con <- get_db_con()
          consent_check <- DBI::dbGetQuery(con, 
                                           "SELECT * FROM user_consents WHERE user_id = $1", 
                                           params = list(user_id())
          )
          DBI::dbDisconnect(con)
          
          message("Sidebar - DB consent rows: ", nrow(consent_check))
          
          if (nrow(consent_check) > 0) {
            has_consent <- TRUE
            consent_given(TRUE)
            message("Sidebar - found consent, set to TRUE")
          }
        }, error = function(e) {
          message("Sidebar consent check error: ", e$message)
        })
      }
      
      message("Sidebar - final has_consent: ", has_consent)
      
      # ✅ No consent - show consent menu and auto-navigate
      if (!has_consent) {
        message("Sidebar - RETURNING consent-only menu")
        
        # Auto-navigate to consent page
        shinyjs::delay(100, {
          message("Auto-navigating to Einwilligungserklärung")
          updateTabItems(session, "sidebar_menu", selected = "Einwilligungserklärung")
        })
        
        menu <- sidebarMenu(
          id = "sidebar_menu",
          menuItem(
            "⚠ Einwilligungserklärung erforderlich", 
            tabName = "Einwilligungserklärung", 
            icon = icon("file-contract"),
            badgeLabel = "Erforderlich",
            badgeColor = "red"
          )
        )
        
        return(menu)
      }
      
      # ✅ Has consent - show role menu and auto-navigate to dashboard
      message("Sidebar - Has consent, showing role menu for: ", role)
      
      if (role == "laborleitung") {
        
        # Auto-navigate to dashboard
        shinyjs::delay(100, {
          message("Auto-navigating to laborLeiterGuide")
          updateTabItems(session, "sidebar_menu", selected = "laborLeiterGuide")
        })
        
        menu <- sidebarMenu(
          id = "sidebar_menu",
          
          menuItem(
            tags$span(style = "font-size: 10px;", "Mein Dashboard-Guide"),
            tabName = "laborLeiterGuide",
            icon = icon("book")
          ),
          
          menuItem(
            tags$span(style = "font-size: 10px;", "1. Peer Review erstellen"),
            tabName = "create_project",
            icon = icon("plus-circle")
          ),
          
          menuItem(
            tags$span(style = "font-size: 10px;", "2. Laborinformation ausfüllen"),
            tabName = "fill_labinfo",
            icon = icon("flask")
          ),
          
          menuItem(
            tags$span(style = "font-size: 10px;", "3. Selbstbewertung ausfüllen"),
            tabName = "fill_questionnaire",
            icon = icon("file-alt")
          ),
          
          menuItem(
            tags$span(style = "font-size: 10px;", "4. Zusammenfassung Selbstbewertungen"),
            tabName = "labor_leiter_summary",
            icon = icon("chart-bar")
          ),
          
          menuItem(
            tags$span(style = "font-size: 10px;", "5. Leading Peer einladen – für Ärztekammer"),
            tabName = "labor_leiter_invite_leading_peer",
            icon = icon("user-plus")
          )
        )
        
        return(menu)
        
      } else if (role == "admin") {
        
        shinyjs::delay(100, {
          updateTabItems(session, "sidebar_menu", selected = "dashboard")
        })
        
        menu <- sidebarMenu(
          id = "sidebar_menu",
          menuItem("Admin Guide", tabName = "adminGuide", icon = icon("book")),
          menuItem("Admin Dashboard", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("Projekte Verwalten", tabName = "create_project", icon = icon("folder-plus")),
          menuItem("Benutzer & Rollen", tabName = "user_admin", icon = icon("user-cog")),
          menuItem("Einladungscodes", tabName = "admin_invitations", icon = icon("ticket-alt")),
          menuItem("Fortschrittsüberwachung", tabName = "labor_leiter_summary", icon = icon("poll")),
          menuItem("Berichte", tabName = "reports", icon = icon("chart-line"))
        )
        return(menu)
        
      } else if (role == "colleague") {
        
        shinyjs::delay(100, {
          updateTabItems(session, "sidebar_menu", selected = "colleague_dashboard")
        })
        
        menu <- sidebarMenu(
          id = "sidebar_menu",
          menuItem("Kollegen Guide", tabName = "colleagueGuide", icon = icon("book")),
          menuItem("Kollegen Dashboard", tabName = "colleague_dashboard", icon = icon("dashboard")),
          menuItem("Labordaten ansehen", tabName = "labordaten_ansehen", icon = icon("flask")),
          menuItem("Fragebogen ausfüllen", tabName = "fill_questionnaire", icon = icon("file-signature"))
          #menuItem("Berichte herunterladen", tabName = "reports", icon = icon("chart-line"))
        )
        return(menu)
        
      } else if (role == "leading_peer") {
        
        shinyjs::delay(100, {
          updateTabItems(session, "sidebar_menu", selected = "leading_peer_dashboard")
        })
        
        menu <- sidebarMenu(
          id = "sidebar_menu",
          menuItem("Leading Peer Dashboard", tabName = "leading_peer_dashboard", icon = icon("dashboard")),
          menuItem("Co-Peer Einladungen", tabName = "leading_peer_invites", icon = icon("user-plus")),
          menuItem("Labordaten ansehen", tabName = "labordaten_ansehen", icon = icon("flask")),
          menuItem("Fremdbewertung ausfüllen", tabName = "fill_fragebogen", icon = icon("file-alt")),
          menuItem("Berichtsdaten eingeben", tabName = "leading_peer_report_data", icon = icon("edit")),
          menuItem("SWOT Analyse", tabName = "leading_peer_swot", icon = icon("balance-scale")),
          menuItem("Best Practices", tabName = "leading_peer_best_practices", icon = icon("lightbulb")),
          menuItem("Final Remarks", tabName = "leading_peer_final_remarks", icon = icon("comment-dots")),
          menuItem("Berichte", tabName = "report", icon = icon("chart-line"))
        )
        return(menu)
        
      } else if (role == "co_peer") {
        
        shinyjs::delay(100, {
          updateTabItems(session, "sidebar_menu", selected = "co_peer_dashboard")
        })
        
        menu <- sidebarMenu(
          id = "sidebar_menu",
          menuItem("Co-Peer Dashboard", tabName = "co_peer_dashboard", icon = icon("dashboard")),
          menuItem("Labordaten ansehen", tabName = "labordaten_ansehen", icon = icon("flask")),
          menuItem("Fremdbewertung ausfüllen", tabName = "fill_fragebogen", icon = icon("file-alt")),
          # Optionally: Remove if Co-Peer shouldn't download reports
          menuItem("Berichte herunterladen", tabName = "report", icon = icon("chart-line"))
        )
        return(menu)
        
      } else {
        # Fallback
        shinyjs::delay(100, {
          updateTabItems(session, "sidebar_menu", selected = "user_dashboard")
        })
        
        menu <- sidebarMenu(
          id = "sidebar_menu",
          menuItem("Dashboard", tabName = "user_dashboard", icon = icon("dashboard")),
          menuItem("Fragebogen ausfüllen", tabName = "fill_questionnaire", icon = icon("file-alt")),
          menuItem("Berichte", tabName = "reports", icon = icon("chart-line"))
        )
        return(menu)
      }
    })
    
    
    shinyBS::addTooltip(
      session,
      "btnGoToCreateProject",
      "Hier starten Sie ein neuen Peer Review.",
      placement = "right",
      trigger = "hover"
    )
    
    # Navigation Logic ----
    
    observeEvent(input$btnGoToCreateProject, {
      updateTabItems(session, "sidebar_menu", selected = "create_project")
    })
    observeEvent(input$btnGoToLabInfo, {
      updateTabItems(session, "sidebar_menu", selected = "fill_labinfo")
    })
    observeEvent(input$btnGoToFragebogen, {
      updateTabItems(session, "sidebar_menu", selected = "fill_questionnaire")
    })
    observeEvent(input$btnGoToInviteUsers, {
      updateTabItems(session, "sidebar_menu", selected = "labor_leiter_invite_colleagues")
    })
    observeEvent(input$btnGoToResponses, {
      updateTabItems(session, "sidebar_menu", selected = "labor_leiter_summary")
    })
    observeEvent(input$btnConfirmFinalizeProject, {
      updateTabItems(session, "sidebar_menu", selected = "project_finalization")
    })
    observeEvent(input$btnGoToInviteLeadingPeer, {
      updateTabItems(session, "sidebar_menu", selected = "labor_leiter_invite_leading_peer")
    })
    
    observeEvent(input$btnGoToReports, {
      updateTabItems(session, "sidebar_menu", selected = "reports")
    })
    
   # Leading Peer-specific navigation --
      observeEvent(input$btnGoToOrtBesuch, {
        updateTabItems(session, "sidebar_menu", selected = "leading_peer_ort_besuch")
      })
    observeEvent(input$btnGoToReportData, {
      updateTabItems(session, "sidebar_menu", selected = "leading_peer_report_data")
    })
    observeEvent(input$btnGoToSwot, {
      updateTabItems(session, "sidebar_menu", selected = "leading_peer_swot")
    })
    observeEvent(input$btnGoToBestPractices, {
      updateTabItems(session, "sidebar_menu", selected = "leading_peer_best_practices")
    })
    observeEvent(input$btnGoToFinalRemarks, {
      updateTabItems(session, "sidebar_menu", selected = "leading_peer_final_remarks")
    })
    observeEvent(input$btnGoToCoPeerInvites, {
      updateTabItems(session, "sidebar_menu", selected = "leading_peer_invites")
    })
    
#---------------------------Show register/login links---------------------------
    

    #-------------------------- Registration Logic -----------------------------
    observeEvent(input$show_register_link, {
      updateTabItems(session, "sidebar_menu", selected = "register")
    })
    observeEvent(input$show_login_link, {
      updateTabItems(session, "sidebar_menu", selected = "start")
    })
    
    #---------------------------------- Login Logic ----------------------------
    observeEvent(input$login_button, {
      req(input$login_username, input$login_password)
      
      # Try DB connection
      con <- tryCatch(
        get_db_con(),
        error = function(e) {
          print(paste("DB connection error:", e$message))
          showNotification("Fehler bei der Datenbankverbindung!", type = "error")
          return(NULL)
        }
      )
      if (is.null(con))
        return()
      on.exit(DBI::dbDisconnect(con))
      
      # DB query for user
      user_data <- tryCatch(
        DBI::dbGetQuery(
          con,
          "SELECT id, username, password_hash, role, activated, project_id FROM users WHERE username = $1",  # ✅ Added project_id
          params = list(input$login_username)
        ),
        error = function(e) {
          print(paste("DB query error:", e$message))
          showNotification("Fehler beim Abruf des Benutzers!", type = "error")
          return(NULL)
        }
      )
      if (is.null(user_data) || nrow(user_data) == 0) {
        showNotification("Ungültiger Benutzername oder Passwort.", type = "error")
        return()
      }
      
      # Password check
      pw_ok <- tryCatch(
        bcrypt::checkpw(input$login_password, user_data$password_hash[1]),
        error = function(e) {
          print(paste("bcrypt error:", e$message))
          showNotification("Fehler bei der Passwortprüfung!", type = "error")
          return(FALSE)
        }
      )
      
      if (pw_ok) {
        if (user_data$activated[1]) {
          # ✅ SET USER STATE
          user_logged_in(TRUE)
          user_id(user_data$id[1])
          user_role(user_data$role[1])
          current_username(user_data$username[1])
          consent_given(FALSE)
          
          message("\n=== LOGIN SUCCESS ===")
          message("User ID: ", user_data$id[1])
          message("Username: ", user_data$username[1])
          message("Role: ", user_data$role[1])
          
          # ✅ SET PROJECT_ID FROM DATABASE
          if (!is.na(user_data$project_id[1]) && !is.null(user_data$project_id[1])) {
            current_project_id(user_data$project_id[1])
            message("✓ Project ID from users table: ", user_data$project_id[1])
          } else {
            current_project_id(NULL)
            message("⚠ No project_id in users table")
          }
          
          # ✅ For colleagues/peers, get invitation code from invitation_codes table
          if (user_data$role[1] %in% c("colleague", "leading_peer", "co_peer")) {
            
            invitation_info <- DBI::dbGetQuery(
              con,
              "SELECT code, project_id 
         FROM invitation_codes 
         WHERE used_by_user_id = $1 
         ORDER BY used_at DESC 
         LIMIT 1",
              params = list(user_data$id[1])
            )
            
            if (nrow(invitation_info) > 0) {
              # Set invitation code
              if (!is.na(invitation_info$code[1])) {
                session$userData$invitation_code <- invitation_info$code[1]
                message("✓ Invitation code: ", invitation_info$code[1])
              }
              
              # Set project_id if not already set
              if (is.null(current_project_id()) && !is.na(invitation_info$project_id[1])) {
                current_project_id(invitation_info$project_id[1])
                message("✓ Project ID from invitation: ", invitation_info$project_id[1])
              }
            } else {
              message("⚠ No invitation found for user_id: ", user_data$id[1])
              session$userData$invitation_code <- NULL
            }
          } else {
            # Admin/Laborleitung don't need invitation codes
            session$userData$invitation_code <- NULL
          }
          
          message("Final state:")
          message("  project_id: ", current_project_id())
          message("  invitation_code: ", session$userData$invitation_code)
          message("=====================\n")
          
          showNotification(
            paste0("Willkommen, ", user_data$username[1], "!"),
            type = "message"
          )
          
        } else {
          showNotification(
            "Ihr Konto ist noch nicht aktiviert. Bitte überprüfen Sie Ihre E-Mails.",
            type = "warning"
          )
        }
      } else {
        showNotification("Ungültiger Benutzername oder Passwort.", type = "error")
      }
    })
    
    # --- Unified Logout Handler ---
    do_logout <- function() {
      user_logged_in(FALSE)
      user_id(NULL)
      user_role(NULL)
      current_username(NULL)
      current_project_id(NULL)
      current_submission_id(NULL)
      redeemed_code_info(NULL)
      # Clear any session$userData if used
      session$userData$user_id <- NULL
      session$userData$username <- NULL
      session$userData$invitation_code <- NULL
      showNotification("Erfolgreich abgemeldet.", type = "message")
      updateTabItems(session, "sidebar_menu", selected = "start")
      # Optionally, reload session for full reset:
      # session$reload()
    }
    
    # --- Logout triggers (menu, top button, sidebar link) ---
    observeEvent(input$sidebar_menu, {
      if (input$sidebar_menu == "logout")
        do_logout()
    })
    observeEvent(input$logout_button, {
      do_logout()
    })
    observeEvent(input$sidebar_logout_button, {
      do_logout()
    })
    
    
    observeEvent(input$register_button, {
      req(
        input$reg_username,
        input$reg_invite_code,
        input$reg_password,
        input$reg_password_confirm
      )
      
      # Password checks
      if (input$reg_password != input$reg_password_confirm) {
        showNotification("Passwörter stimmen nicht überein.", type = "error")
        return()
      }
      if (nchar(input$reg_password) < 6) {
        showNotification("Das Passwort muss mindestens 6 Zeichen lang sein.", type = "error")
        return()
      }
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      # --- Fetch invitation code and validate ---
      code_row <- DBI::dbGetQuery(
        con,
        "SELECT id, role, assessment_type, project_id
     FROM invitation_codes
     WHERE code = $1
       AND used_at IS NULL
       AND expires_at > NOW()
     LIMIT 1",
        params = list(input$reg_invite_code)
      )
      if (nrow(code_row) == 0) {
        showNotification("Einladungscode ist ungültig, bereits verwendet oder abgelaufen.", type = "error")
        return()
      }
      
      # --- Check if username already exists ---
      existing_user <- DBI::dbGetQuery(
        con,
        "SELECT id FROM users WHERE username = $1",
        params = list(input$reg_username)
      )
      if (nrow(existing_user) > 0) {
        showNotification("Benutzername ist bereits vergeben.", type = "error")
        return()
      }
      
      # --- Prepare registration info from invitation ---
      invite_role <- code_row$role[1]
      invite_project <- as.integer(code_row$project_id[1])
      invite_assessment <- code_row$assessment_type[1]
      
      if (is.na(invite_role) || invite_role == "") {
        showNotification(
          "Fehler: Dieser Einladungscode hat keine Rolle gespeichert. Bitte lassen Sie den Admin einen neuen Code erstellen.",
          type = "error",
          duration = NULL
        )
        return()
      }
      
      if (is.na(invite_assessment) || invite_assessment == "") {
        invite_assessment <- if (invite_role %in% c("leading_peer", "co_peer")) "fremdbewertung" else "selbstbewertung"
      }
      
      if (is.na(invite_project) || invite_project == "") {
        showNotification(
          "Fehler: Dem Einladungscode ist kein Projekt zugeordnet. Bitte wenden Sie sich an den Laborleiter.",
          type = "error"
        )
        return()
      }
      
      # Hash password
      hashed_password <- tryCatch(
        bcrypt::hashpw(input$reg_password, bcrypt::gensalt()),
        error = function(e) {
          showNotification("Fehler beim Hashen des Passworts.", type = "error")
          NULL
        }
      )
      if (is.null(hashed_password)) return()
      
      # --- Transaction ---
      DBI::dbBegin(con)
      on.exit(try(DBI::dbRollback(con), silent = TRUE), add = TRUE)
      
      new_user_id <- DBI::dbGetQuery(
        con,
        "INSERT INTO users (username, password_hash, role, assessment_type, project_id, registered_at, activated)
     VALUES ($1, $2, $3, $4, $5, NOW(), TRUE)
     RETURNING id",
        params = list(input$reg_username, hashed_password, invite_role, invite_assessment, invite_project)
      )$id[1]
      
      updated <- DBI::dbExecute(
        con,
        "UPDATE invitation_codes
     SET used_by_user_id = $1, used_at = NOW()
     WHERE id = $2
       AND used_at IS NULL
       AND expires_at > NOW()",
        params = list(new_user_id, code_row$id[1])
      )
      
      if (updated == 0) {
        showNotification("Einladungscode ist bereits verwendet oder abgelaufen.", type = "error")
        return()
      }
      
      DBI::dbCommit(con)
      
      showNotification("Registrierung erfolgreich! Bitte melden Sie sich an.", type = "message")
      updateTextInput(session, "reg_username", value = "")
      updateTextInput(session, "reg_invite_code", value = "")
      updateTextInput(session, "reg_password", value = "")
      updateTextInput(session, "reg_password_confirm", value = "")
      updateTabItems(session, "sidebar_menu", selected = "login")
    }, ignoreInit = TRUE)
    
    
    #--------------Server Logic for Einwilligungserklärung----------------------
    
    # Enable/disable consent button
    observe({
      if (!is.null(input$consent_checkbox) && input$consent_checkbox == TRUE &&
          !is.null(input$consent_ort) && nchar(trimws(input$consent_ort)) > 0) {
        shinyjs::enable("bestätigen_button")
      } else {
        shinyjs::disable("bestätigen_button")
      }
    })
    
    # Handle consent submission
    observeEvent(input$bestätigen_button, {
      req(input$consent_checkbox == TRUE)
      req(input$consent_ort)
      req(input$consent_datum)
      req(user_id())
      
      message("=== CONSENT BUTTON CLICKED ===")
      message("User ID: ", user_id())
      message("Ort: ", input$consent_ort)
      message("Datum: ", input$consent_datum)
      
      tryCatch({
        con <- get_db_con()
        
        DBI::dbExecute(con,
                       "INSERT INTO user_consents (user_id, ort, datum) 
       VALUES ($1, $2, $3)",
                       params = list(
                         user_id(),
                         trimws(input$consent_ort),
                         as.character(input$consent_datum)
                       )
        )
        
        DBI::dbDisconnect(con)
        
        # ✅ Mark consent as given - this triggers observe() to re-run
        consent_given(TRUE)
        
        showNotification("Einwilligung erfolgreich gespeichert.", type = "message", duration = 3)
        
        # The observe() block will automatically redirect to the appropriate dashboard
        
      }, error = function(e) {
        showNotification(paste("Fehler beim Speichern:", e$message), type = "error", duration = 10)
        message("Consent save error: ", e$message)
      })
    })
    
    
    # --- Login/Logout/Registration Logic ---
    observe({
      message("=== OBSERVE BLOCK TRIGGERED ===")
      
      role <- user_role()
      message("User logged in: ", user_logged_in())
      message("User role: ", ifelse(is.null(role), "NULL", role))
      message("User ID: ", ifelse(is.null(user_id()), "NULL", user_id()))
      
      if (user_logged_in()) {
        
        # ✅ CHECK CONSENT FIRST
        has_consent <- consent_given()
        message("Consent given (reactive value): ", has_consent)
        
        if (!has_consent) {
          message("Checking database for consent...")
          
          check_result <- isolate({
            tryCatch({
              con <- get_db_con()
              
              if (!DBI::dbExistsTable(con, "user_consents")) {
                DBI::dbExecute(con, "
              CREATE TABLE user_consents (
                id SERIAL PRIMARY KEY,
                user_id INTEGER NOT NULL,
                ort TEXT NOT NULL,
                datum TEXT NOT NULL,
                timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                FOREIGN KEY (user_id) REFERENCES users(id)
              )
            ")
              }
              
              consent_check <- DBI::dbGetQuery(con, 
                                               "SELECT * FROM user_consents WHERE user_id = $1", 
                                               params = list(user_id())
              )
              DBI::dbDisconnect(con)
              
              message("Consent check rows: ", nrow(consent_check))
              
              return(nrow(consent_check) > 0)
            }, error = function(e) {
              message("Consent check ERROR: ", e$message)
              return(TRUE)
            })
          })
          
          if (check_result) {
            message("Setting consent_given(TRUE)")
            consent_given(TRUE)
          } else {
            message("NO CONSENT - user will see consent form")
            # Sidebar will handle showing the consent form
            return()
          }
        }
        
        message("Consent given - sidebar will handle navigation")
        
      } else {
        # Not logged in - sidebar will show login page
        message("Not logged in")
      }
    })
    #-----------------------Save Draft for Selbstbewertung----------------------
    
    output$showDraftButtons <- reactive({
      # Change this to however you store the current user's role
      session$userData$user_role == "leading_peer"
    })
    outputOptions(output, "showDraftButtons", suspendWhenHidden = FALSE)
    
    
    questionnaire_id <- function() {
      "peer_review"
    }
    
    
    # When resuming, get the largest X from saved answers such as "BeschreibungX"
    # saved_ids <- names(saved_answers) # if you have a named list of saved answers
    # beschreibung_nums <- as.integer(gsub("^Beschreibung", "", saved_ids[grep("^Beschreibung\\d+$", saved_ids)]))
    # anmerkung_nums <- as.integer(gsub("^Anmerkung", "", saved_ids[grep("^Anmerkung\\d+$", saved_ids)]))
    # n_anmerkungen <- max(c(anmerkungen$counter, beschreibung_nums, anmerkung_nums), na.rm = TRUE)
    
    # 1.  Helper to get all question input IDs including dynamic 'Input durch Peer Review' fields
    get_all_question_ids <- function() {
      # Static IDs from all other tabs
      ids <- c(
        # Grundlagen und Organisation
        "name",
        "Internetadresse",
        "Trag",
        "Zug",
        "Ansprechpartner",
        "Email",
        "Telefonnummer",
        "Öffnungzeiten",
        "versorgung",
        "sonstiges_versorgung",
        "laborbereiche",
        "sonstiges_laborbereiche",
        "leistungsspektrum",
        "sonstiges_leistungsspektrum",
        "ausstattung",
        "hauptlieferanten",
        "question_binary_1",
        "farbcodesystem",
        "question_binary_2",
        "auftragsgenerierung",
        "präanalytik",
        "befundübermittlung",
        "beratungsleistungen",
        "question_binary_3",
        # Personal
        "Anzahl_total",
        "Anzahl_mit",
        "davon_Fachaerzte",
        "davon_weiterbildung",
        "Anzahl_Planstellen",
        "davon_unbesetzt",
        "Anzahl_tech",
        "Anzahl_TPlanstellen",
        "davon_Tunbesetzt",
        "Anzahl_natur",
        "Anzahl_NPlanstellen",
        "davon_Nunbesetzt",
        "Anzahl_IT",
        "Anzahl_IPlanstellen",
        "davon_Iunbesetzt",
        "Beschreibung",
        "WeitereInfo",
        # EDV und Kennzahlen
        "AnbieterInfo",
        "AnbieterOrder",
        "AnbieterMiddleware",
        "WeitereIT",
        "Angaben",
        "laufendenJahres",
        "Vorjahres",
        "Kompetenzschwerpunkte",
        "Organigramm",
        "Ergänzung",
        # Berufsgruppe
        "Berufsgruppe"
      )
      
      # Questionnaire questions (Führung, Mitarbeitende, etc.)
      sections <- list(
        fuehrung = fuehrung_questions,
        mitarbeitende = mit_questions,
        patienten = pat_questions,
        einsender = ein_questions,
        qualitaet = qual_questions
      )
      for (section in names(sections)) {
        for (q in sections[[section]]) {
          ids <- c(ids,
                   paste0("Freitext", q$id),
                   paste0("Freitext_text", q$id))
        }
      }
      
      # --- Dynamic fields for "Input durch Peer Review": BeschreibungX and AnmerkungX
      # Use the current value of anmerkungen$counter, or, if resuming a draft, the largest number present in saved data
      n_anmerkungen <- if (!is.null(anmerkungen$counter))
        anmerkungen$counter
      else
        1
      # Optionally, when resuming, use the max found in the saved answers (see resume logic below)
      ids <- c(ids, unlist(lapply(1:n_anmerkungen, function(i)
        c(paste0("Beschreibung", i), paste0("Anmerkung", i)))))
      
      ids
    }
    
    # 2. Collect answers from all questions
    
    get_qn_answers <- function() {
      ids <- get_all_question_ids()
      answers <- list()
      for (id in ids) {
        answers[[id]] <- input[[id]]
      }
      answers
    }
    
    # Server side
    current_project_id <- reactiveVal(NULL)
    # Wherever you have project selection (probably in fill_labinfo or dashboard)
    observeEvent(input$selected_project, {
      req(input$selected_project)
      current_project_id(input$selected_project)
      message("✓ Project ID set to: ", input$selected_project)
    })
    
    # Save draft (single source of truth)
    # Save draft to database
    save_draft_to_db <- function(status = "draft", show_notification = TRUE) {
      
      uid <- user_id()
      pid <- current_project_id()
      qid <- "peer_review"
      
      message("Saving draft - User: ", uid, ", Project: ", pid, ", Status: ", status)
      
      # Validation
      if (is.null(uid) || is.na(uid)) {
        if (show_notification) {
          showNotification("Bitte melden Sie sich an", type = "error", duration = 3)
        }
        return(FALSE)
      }
      
      if (is.null(pid) || is.na(pid)) {
        if (show_notification) {
          showNotification("Bitte wählen Sie zuerst ein Projekt aus", type = "warning", duration = 3)
        }
        return(FALSE)
      }
      
      tryCatch({
        con <- get_db_con()
        on.exit(DBI::dbDisconnect(con))
        
        # Get answers
        answers_list <- get_qn_answers()
        
        # ✅ Convert to JSON string (PostgreSQL will handle the JSONB conversion)
        answers_json <- jsonlite::toJSON(answers_list, auto_unbox = TRUE)
        
        message("Collected ", length(answers_list), " answers")
        
        # Check if draft exists
        existing <- DBI::dbGetQuery(con,
                                    "SELECT id FROM questionnaire_drafts 
       WHERE user_id = $1 AND project_id = $2 AND questionnaire_id = $3",
                                    params = list(as.integer(uid), as.integer(pid), qid)
        )
        
        if (nrow(existing) > 0) {
          # ✅ Update existing - removed ::text
          DBI::dbExecute(con,
                         "UPDATE questionnaire_drafts 
         SET answers = $1, status = $2, last_saved = NOW() 
         WHERE id = $3",
                         params = list(answers_json, status, existing$id[1])
          )
          message("Draft updated - ID: ", existing$id[1])
        } else {
          # ✅ Insert new - removed ::text
          DBI::dbExecute(con,
                         "INSERT INTO questionnaire_drafts 
         (user_id, project_id, questionnaire_id, answers, status, last_saved)
         VALUES ($1, $2, $3, $4, $5, NOW())",
                         params = list(as.integer(uid), as.integer(pid), qid, answers_json, status)
          )
          message("New draft created")
        }
        
        if (show_notification) {
          showNotification("Entwurf gespeichert ✓", type = "message", duration = 2)
        }
        
        return(TRUE)
        
      }, error = function(e) {
        message("Error saving draft: ", e$message)
        print(e)
        if (show_notification) {
          showNotification(paste("Fehler beim Speichern:", e$message), 
                           type = "error", duration = 5)
        }
        return(FALSE)
      })
    }
    
    # Resume draft (single source of truth)
    # 4. Resume draft from DB
    resume_draft_from_db <- function() {
      req(user_logged_in())
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      user_id_val <- user_id()
      proj_id_val <- current_project_id()
      qid_val <- "peer_review"
      
      message("\n========== LOAD DRAFT ==========")
      message("User: ", user_id_val)
      message("Project: ", proj_id_val)
      
      # Fetch draft
      draft <- NULL
      
      if (is.null(proj_id_val) || proj_id_val == "" || is.na(proj_id_val)) {
        draft <- DBI::dbGetQuery(
          con,
          "SELECT answers, project_id FROM questionnaire_drafts
       WHERE user_id = $1 AND questionnaire_id = $2 AND status IN ('draft', 'paused')
       ORDER BY last_saved DESC LIMIT 1",
          params = list(user_id_val, qid_val)
        )
        
        if (nrow(draft) > 0) {
          current_project_id(draft$project_id[1])
          proj_id_val <- draft$project_id[1]
          message("✓ Found draft, project: ", proj_id_val)
        }
      } else {
        draft <- DBI::dbGetQuery(
          con,
          "SELECT answers FROM questionnaire_drafts
       WHERE user_id = $1 AND project_id = $2 AND questionnaire_id = $3 AND status IN ('draft', 'paused')
       ORDER BY last_saved DESC LIMIT 1",
          params = list(user_id_val, proj_id_val, qid_val)
        )
      }
      
      if (nrow(draft) == 0) {
        message("No draft found")
        output$draft_status_ui <- renderUI(div(style = "color:gray", "Kein Entwurf gefunden."))
        return()
      }
      
      answers <- jsonlite::fromJSON(draft$answers[1])
      
      if (is.null(answers) || length(answers) == 0) {
        message("Draft is empty")
        return()
      }
      
      message("Loaded ", length(answers), " keys")
      
      # Show what's in draft
      message("\n--- DRAFT CONTENTS ---")
      if ("Berufsgruppe" %in% names(answers)) {
        message("  Berufsgruppe: ", answers$Berufsgruppe)
      }
      if ("alleine_oder_gruppe" %in% names(answers)) {
        message("  alleine_oder_gruppe: ", answers$alleine_oder_gruppe)
      }
      if ("gruppen_groesse" %in% names(answers)) {
        message("  gruppen_groesse: ", answers$gruppen_groesse)
      }
      message("----------------------\n")
      
      # Handle peer review dynamic fields
      peer_fields <- grep("^Beschreibung\\d+$|^Anmerkung\\d+$", names(answers), value = TRUE)
      if (length(peer_fields) > 0) {
        peer_numbers <- as.integer(gsub("[^0-9]", "", peer_fields))
        max_peer <- max(peer_numbers, na.rm = TRUE)
        if (!is.na(max_peer) && max_peer > 0) {
          anmerkungen$counter <- max_peer
        }
      }
      
      # Restore Berufsgruppe
      if ("Berufsgruppe" %in% names(answers)) {
        updateRadioButtons(session, "Berufsgruppe", selected = answers$Berufsgruppe)
      }
      
      # Restore group settings
      restore_gruppe <- FALSE
      gruppe_size_value <- NULL
      
      if ("alleine_oder_gruppe" %in% names(answers)) {
        updateRadioButtons(session, "alleine_oder_gruppe", selected = answers$alleine_oder_gruppe)
        message("✓ Restored alleine_oder_gruppe")
        
        if (answers$alleine_oder_gruppe == "gruppe" && "gruppen_groesse" %in% names(answers)) {
          restore_gruppe <- TRUE
          gruppe_size_value <- as.character(answers$gruppen_groesse)
          message("→ Will restore size: ", gruppe_size_value)
        }
      }
      
      # Restore all other inputs
      for (id in names(answers)) {
        if (id %in% c("Berufsgruppe", "alleine_oder_gruppe", "gruppen_groesse")) next
        
        val <- answers[[id]]
        if (is.null(val) || (is.character(val) && val == "")) next
        
        if (grepl("^Beschreibung\\d+$|^Anmerkung\\d+$", id)) {
          next  # Handle below
        } else if (grepl("_text$", id) || id %in% c("Beschreibung", "WeitereInfo", "Kompetenzschwerpunkte", "Ergänzung")) {
          updateTextAreaInput(session, id, value = val)
        } else if (grepl("^Freitext", id)) {
          updateRadioButtons(session, id, selected = val)
        } else if (id %in% c("versorgung", "laborbereiche", "leistungsspektrum")) {
          updateCheckboxGroupInput(session, id, selected = val)
        } else if (id %in% c("Anzahl_total", "Anzahl_mit", "davon_Fachaerzte", "davon_weiterbildung",
                             "Anzahl_Planstellen", "davon_unbesetzt", "Anzahl_tech", "Anzahl_TPlanstellen",
                             "davon_Tunbesetzt", "Anzahl_natur", "Anzahl_NPlanstellen", "davon_Nunbesetzt",
                             "Anzahl_IT", "Anzahl_IPlanstellen", "davon_Iunbesetzt")) {
          suppressWarnings(updateNumericInput(session, id, value = as.numeric(val)))
        } else {
          updateTextInput(session, id, value = val)
        }
      }
      
      message("✓ Restored all inputs")
      
      # Restore dynamic peer fields with delay
      shinyjs::delay(300, {
        counter <- anmerkungen$counter
        for (i in seq_len(counter)) {
          beschr_id <- paste0("Beschreibung", i)
          anmerk_id <- paste0("Anmerkung", i)
          
          if (!is.null(answers[[beschr_id]])) {
            updateTextAreaInput(session, beschr_id, value = answers[[beschr_id]])
          }
          if (!is.null(answers[[anmerk_id]])) {
            updateTextInput(session, anmerk_id, value = answers[[anmerk_id]])
          }
        }
      })
      
      # Restore gruppen_groesse with longer delay
      if (restore_gruppe && !is.null(gruppe_size_value)) {
        shinyjs::delay(700, {
          message("→ Restoring gruppen_groesse: ", gruppe_size_value)
          updateSelectInput(session, "gruppen_groesse", selected = gruppe_size_value)
          message("✓ DONE")
        })
      }
      
      message("========== DRAFT LOADED ==========\n")
      
      output$draft_status_ui <- renderUI(
        div(style = "color:blue", "Entwurf geladen.")
      )
      
      showNotification("Entwurf geladen!", type = "message", duration = 3)
    }
    
    # 5. Show draft buttons only for leading peer
    # Show draft buttons only for leading peer and co-peer
    # output$showDraftButtons <- reactive({
    #   user_role() %in% c("leading_peer", "co_peer")
    # })
    
    output$showDraftButtons <- reactive({
      isTRUE(user_logged_in()) && !is.null(user_role()) && !is.na(user_role())
    })
    
    
    outputOptions(output, "showDraftButtons", suspendWhenHidden = FALSE)
    
    # 6. Observe draft buttons
    observeEvent(input$save_draft_btn, {
      message("\n========== SAVE DRAFT ==========")
      
      req(user_logged_in())
      
      current_user_id <- user_id()
      project_id <- current_project_id()
      questionnaire_id <- "peer_review"
      
      message("User ID: ", current_user_id)
      message("Project ID: ", project_id)
      
      if (is.null(project_id) || project_id == "" || is.na(project_id)) {
        showNotification("Kein Projekt ausgewählt!", type = "error")
        return()
      }
      
      # Collect all answers
      all_answers <- list()
      
      # Berufsgruppe
      if (!is.null(input$Berufsgruppe) && input$Berufsgruppe != "") {
        all_answers[["Berufsgruppe"]] <- input$Berufsgruppe
        message("  Berufsgruppe: ", input$Berufsgruppe)
      }
      
      # ✅ Group settings
      if (!is.null(input$alleine_oder_gruppe) && input$alleine_oder_gruppe != "") {
        all_answers[["alleine_oder_gruppe"]] <- input$alleine_oder_gruppe
        message("  alleine_oder_gruppe: ", input$alleine_oder_gruppe)
      }
      
      if (!is.null(input$gruppen_groesse) && input$gruppen_groesse != "") {
        all_answers[["gruppen_groesse"]] <- input$gruppen_groesse
        message("  gruppen_groesse: ", input$gruppen_groesse)
      }
      
      # All question responses
      all_question_lists <- list(
        fuehrung_questions,
        mit_questions,
        pat_questions,
        ein_questions,
        qual_questions
      )
      
      for (qlist in all_question_lists) {
        for (q in qlist) {
          qid <- q$id
          score_id <- paste0("Freitext", qid)
          text_id <- paste0("Freitext_text", qid)
          
          if (!is.null(input[[score_id]]) && input[[score_id]] != "") {
            all_answers[[score_id]] <- input[[score_id]]
          }
          
          if (!is.null(input[[text_id]]) && input[[text_id]] != "") {
            all_answers[[text_id]] <- input[[text_id]]
          }
        }
      }
      
      message("Total answers: ", length(all_answers))
      
      if (length(all_answers) == 0) {
        showNotification("Keine Daten zum Speichern!", type = "warning")
        return()
      }
      
      answers_json <- jsonlite::toJSON(all_answers, auto_unbox = TRUE)
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      
      tryCatch({
        # Check if draft exists
        existing <- DBI::dbGetQuery(
          con,
          "SELECT id FROM questionnaire_drafts 
       WHERE user_id = $1 AND project_id = $2 AND questionnaire_id = $3",
          params = list(current_user_id, project_id, questionnaire_id)
        )
        
        if (nrow(existing) > 0) {
          # Update existing
          DBI::dbExecute(
            con,
            "UPDATE questionnaire_drafts 
         SET answers = $1, last_saved = NOW(), status = 'draft'
         WHERE id = $2",
            params = list(answers_json, existing$id[1])
          )
          message("✓ Draft updated - ID: ", existing$id[1])
        } else {
          # Insert new
          DBI::dbExecute(
            con,
            "INSERT INTO questionnaire_drafts (user_id, project_id, questionnaire_id, answers, status, last_saved) 
         VALUES ($1, $2, $3, $4, 'draft', NOW())",
            params = list(current_user_id, project_id, questionnaire_id, answers_json)
          )
          message("✓ New draft created")
        }
        
        showNotification("Entwurf gespeichert!", type = "message", duration = 3)
        
      }, error = function(e) {
        message("ERROR: ", e$message)
        showNotification(paste("Fehler:", e$message), type = "error", duration = 10)
      })
      
      message("=================================\n")
    })
    
    
# --- Helper: check if user has already submitted ---
    
    user_has_submitted <- function(user_id, tag_value, con) {
      FALSE
    }
  #   user_has_submitted <- function(user_id, tag_value, con) {
  #     n <- DBI::dbGetQuery(
  #       con,
  #       "
  #   SELECT COUNT(*) as n FROM responses WHERE user_id = $1 AND tag = $2
  # ",
  #       params = list(user_id, tag_value)
  #     )$n
  #     n > 0
  #   }

    
 #-----------------------Save Draft for Fremdbewertung--------------------------
    
    # 1. Collect all fremdbewertung answers
    get_fremdbewertung_answers <- function() {
      answers <- list()
      
      all_question_lists <- list(
        fuehrung_questions,
        mit_questions,
        pat_questions,
        ein_questions,
        qual_questions
      )
      
      for (qlist in all_question_lists) {
        for (q in qlist) {
          qid <- q$id
          score_id <- paste0("FremdbewertungNum_", qid)
          text_id <- paste0("FremdbewertungText_", qid)
          
          if (!is.null(input[[score_id]]) && input[[score_id]] != "") {
            answers[[score_id]] <- input[[score_id]]
          }
          
          if (!is.null(input[[text_id]]) && input[[text_id]] != "") {
            answers[[text_id]] <- input[[text_id]]
          }
        }
      }
      
      answers
    }
    
    # 2. SAVE FREMDBEWERTUNG DRAFT
    # ✅ SAVE HANDLER - FIXED: no duplicates
    observeEvent(input$save_peer_draft_btn_fuehrung, {
      save_fremdbewertung_draft()
    }, ignoreInit = TRUE)
    
    observeEvent(input$save_peer_draft_btn_mit, {
      save_fremdbewertung_draft()
    }, ignoreInit = TRUE)
    
    observeEvent(input$save_peer_draft_btn_pat, {
      save_fremdbewertung_draft()
    }, ignoreInit = TRUE)
    
    observeEvent(input$save_peer_draft_btn_ein, {
      save_fremdbewertung_draft()
    }, ignoreInit = TRUE)
    
    observeEvent(input$save_peer_draft_btn_qual, {
      save_fremdbewertung_draft()
    }, ignoreInit = TRUE)
    
    # ✅ Extract the save logic to a function
    save_fremdbewertung_draft <- function() {
      message("\n========== SAVE PEER DRAFT ==========")
      
      req(user_logged_in())
      req(user_role() %in% c("leading_peer", "co_peer"))
      
      current_user_id <- user_id()
      project_id <- current_project_id()
      questionnaire_id <- "peer_fremdbewertung"
      
      message("User: ", current_user_id)
      message("Project: ", project_id)
      
      if (is.null(project_id) || project_id == "" || is.na(project_id)) {
        showNotification("Kein Projekt ausgewählt!", type = "error", duration = 3)
        return()
      }
      
      # Collect all fremdbewertung answers
      all_answers <- list()
      
      all_question_lists <- list(
        fuehrung_questions,
        mit_questions,
        pat_questions,
        ein_questions,
        qual_questions
      )
      
      for (qlist in all_question_lists) {
        for (q in qlist) {
          qid <- q$id
          score_id <- paste0("FremdbewertungNum_", qid)
          text_id <- paste0("FremdbewertungText_", qid)
          
          if (!is.null(input[[score_id]]) && input[[score_id]] != "") {
            all_answers[[score_id]] <- input[[score_id]]
          }
          
          if (!is.null(input[[text_id]]) && input[[text_id]] != "") {
            all_answers[[text_id]] <- input[[text_id]]
          }
        }
      }
      
      message("Total answers: ", length(all_answers))
      
      if (length(all_answers) == 0) {
        showNotification("Keine Daten zum Speichern!", type = "warning", duration = 3)
        return()
      }
      
      answers_json <- jsonlite::toJSON(all_answers, auto_unbox = TRUE)
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      
      tryCatch({
        existing <- DBI::dbGetQuery(
          con,
          "SELECT id FROM questionnaire_drafts 
       WHERE user_id = $1 AND project_id = $2 AND questionnaire_id = $3",
          params = list(current_user_id, project_id, questionnaire_id)
        )
        
        if (nrow(existing) > 0) {
          DBI::dbExecute(
            con,
            "UPDATE questionnaire_drafts 
         SET answers = $1, last_saved = NOW(), status = 'draft'
         WHERE id = $2",
            params = list(answers_json, existing$id[1])
          )
          message("✓ Draft updated - ID: ", existing$id[1])
        } else {
          DBI::dbExecute(
            con,
            "INSERT INTO questionnaire_drafts (user_id, project_id, questionnaire_id, answers, status, last_saved) 
         VALUES ($1, $2, $3, $4, 'draft', NOW())",
            params = list(current_user_id, project_id, questionnaire_id, answers_json)
          )
          message("✓ New draft created")
        }
        
        showNotification(
          tags$div(
            style = "color: #28a745; font-weight: bold;",
            icon("check-circle"),
            " Fremdbewertung gespeichert!"
          ),
          type = "message",
          duration = 3
        )
        
      }, error = function(e) {
        message("ERROR: ", e$message)
        showNotification(paste("Fehler:", e$message), type = "error", duration = 10)
      })
      
      message("=================================\n")
    }
    
    # 3. LOAD FREMDBEWERTUNG DRAFT (called when fragebogen_ui renders)
    resume_fremdbewertung_draft <- function() {
      req(user_logged_in())
      req(user_role() %in% c("leading_peer", "co_peer"))
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      user_id_val <- user_id()
      proj_id_val <- current_project_id()
      qid_val <- "peer_fremdbewertung"
      
      message("\n========== LOAD PEER DRAFT ==========")
      message("User: ", user_id_val)
      message("Project: ", proj_id_val)
      
      if (is.null(proj_id_val) || proj_id_val == "" || is.na(proj_id_val)) {
        message("No project selected")
        return(list())
      }
      
      # Fetch draft
      draft <- DBI::dbGetQuery(
        con,
        "SELECT answers FROM questionnaire_drafts
     WHERE user_id = $1 AND project_id = $2 AND questionnaire_id = $3 AND status = 'draft'
     ORDER BY last_saved DESC LIMIT 1",
        params = list(user_id_val, proj_id_val, qid_val)
      )
      
      if (nrow(draft) == 0) {
        message("No draft found")
        return(list())
      }
      
      answers <- jsonlite::fromJSON(draft$answers[1])
      
      if (is.null(answers) || length(answers) == 0) {
        message("Draft is empty")
        return(list())
      }
      
      message("Loaded ", length(answers), " keys")
      message("✓ Restored ", sum(grepl("^FremdbewertungNum_", names(answers))), " inputs")
      message("=================================\n")
      
      return(answers)
    }
    
# -------------------------ADMIN: View Users & Projects ------------------------
    output$admin_users_table <- DT::renderDataTable({
      req(user_role() == "admin")
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      users <- DBI::dbGetQuery(con, "
    SELECT id, username, email, role, project_id, created_at 
    FROM users 
    ORDER BY id DESC
  ")
      
      DT::datatable(users, 
                    options = list(pageLength = 25),
                    rownames = FALSE
      )
    })
    
    # ===== ADMIN: View All Projects (you already have this) =====
    output$admin_projects_table <- DT::renderDataTable({
      req(user_role() == "admin")
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      projects <- DBI::dbGetQuery(con, "
    SELECT p.id, p.title as name, p.status, p.created_at,
           u.username as created_by_user
    FROM projects p
    LEFT JOIN users u ON p.owner_id = u.id
    ORDER BY p.id DESC
  ")
      
      DT::datatable(projects, 
                    options = list(pageLength = 25),
                    rownames = FALSE
      )
    })
    
    # ✅ NEW: ADMIN - View All Drafts
    # ✅ ADMIN - View All Drafts (Fixed with casting)
    output$admin_drafts_table <- DT::renderDataTable({
      req(user_role() == "admin")
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      drafts <- DBI::dbGetQuery(con, "
    SELECT 
      d.id,
      u.username,
      u.role,
      p.title as project,
      d.questionnaire_id,
      d.status,
      d.last_saved,
      (SELECT COUNT(*) FROM jsonb_object_keys(d.answers)) as num_fields
    FROM questionnaire_drafts d
    LEFT JOIN users u ON d.user_id::integer = u.id
    LEFT JOIN projects p ON d.project_id::integer = p.id
    ORDER BY d.last_saved DESC
  ")
      
      DT::datatable(drafts, 
                    options = list(
                      pageLength = 10,
                      order = list(list(6, 'desc'))  # Sort by last_saved descending
                    ),
                    rownames = FALSE,
                    selection = 'single',
                    colnames = c(
                      'ID', 'Benutzer', 'Rolle', 'Projekt', 
                      'Fragebogen', 'Status', 'Zuletzt gespeichert', 'Anzahl Felder'
                    )
      )
    })
    
    # ✅ NEW: Show Draft Details When Row is Selected
    observeEvent(input$admin_drafts_table_rows_selected, {
      req(user_role() == "admin")
      req(length(input$admin_drafts_table_rows_selected) > 0)
      
      row_selected <- input$admin_drafts_table_rows_selected
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      # Get all drafts (same order as displayed)
      drafts <- DBI::dbGetQuery(con, "
    SELECT id, user_id, project_id, answers, last_saved
    FROM questionnaire_drafts 
    ORDER BY last_saved DESC
  ")
      
      if (row_selected <= nrow(drafts)) {
        draft_id <- drafts$id[row_selected]
        answers_json <- drafts$answers[row_selected]
        
        # Parse JSON
        answers <- jsonlite::fromJSON(answers_json)
        
        # Remove metadata fields
        answers <- answers[!grepl("^_", names(answers))]
        
        # Count non-empty fields
        non_empty <- sum(sapply(answers, function(x) !is.null(x) && x != "" && x != "N/A"))
        
        # Create formatted view
        output$draft_details_view <- renderUI({
          tagList(
            h3(paste("Entwurf ID:", draft_id)),
            h4(paste("Ausgefüllte Felder:", non_empty, "von", length(answers))),
            p(strong("Zuletzt gespeichert:"), format(as.POSIXct(drafts$last_saved[row_selected]), "%d.%m.%Y %H:%M:%S")),
            
            hr(),
            
            h4("Ausgefüllte Antworten:"),
            tags$div(
              style = "max-height: 600px; overflow-y: auto;",
              lapply(names(answers), function(field_name) {
                value <- answers[[field_name]]
                
                # Only show non-empty values
                if (!is.null(value) && value != "" && value != "N/A") {
                  tags$div(
                    style = "padding: 10px; margin-bottom: 10px; background: #f8f9fa; border-left: 3px solid #007bff; border-radius: 4px;",
                    tags$strong(field_name, ":"),
                    tags$br(),
                    tags$span(style = "color: #495057;", as.character(value))
                  )
                }
              })
            ),
            
            hr(),
            
            h4("Rohdaten (JSON):"),
            tags$pre(
              style = "background: #272822; color: #f8f8f2; padding: 15px; border-radius: 5px; max-height: 400px; overflow-y: auto; font-family: 'Courier New', monospace; font-size: 12px;",
              jsonlite::toJSON(answers, pretty = TRUE, auto_unbox = TRUE)
            ),
            
            # Download button
            downloadButton("download_selected_draft", "JSON herunterladen", 
                           style = "margin-top: 10px;")
          )
        })
        
        # Store selected draft ID for download
        session$userData$selected_draft_id <- draft_id
      }
    })
    
    # ✅ NEW: Download Selected Draft as JSON
    output$download_selected_draft <- downloadHandler(
      filename = function() {
        paste0("entwurf_", session$userData$selected_draft_id, "_", Sys.Date(), ".json")
      },
      content = function(file) {
        req(user_role() == "admin")
        req(!is.null(session$userData$selected_draft_id))
        
        con <- get_db_con()
        on.exit(DBI::dbDisconnect(con))
        
        draft <- DBI::dbGetQuery(con,
                                 "SELECT answers FROM questionnaire_drafts WHERE id = $1",
                                 params = list(session$userData$selected_draft_id)
        )
        
        if (nrow(draft) > 0) {
          # Pretty print the JSON
          answers <- jsonlite::fromJSON(draft$answers[1])
          writeLines(jsonlite::toJSON(answers, pretty = TRUE, auto_unbox = TRUE), file)
        }
      }
    )
#-------------------------generating invitation logic----------------------------
    
    get_project_choices <- function(role, user_id) {
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      if (role == "admin") {
        projs <- DBI::dbGetQuery(con, "SELECT id, title FROM projects ORDER BY title")
      } else if (role == "laborleitung") {
        projs <- DBI::dbGetQuery(
          con,
          "SELECT id, title FROM projects WHERE owner_id = $1 ORDER BY title",
          params = list(user_id)
        )
      } else if (role == "leading_peer") {
        projs <- DBI::dbGetQuery(
          con,
          "SELECT id, title FROM projects WHERE id IN (SELECT project_id FROM users WHERE id = $1)",
          params = list(user_id)
        )
      } else {
        projs <- data.frame(id = character(0), title = character(0))
      }
      setNames(projs$id, projs$title)
    }
    
    output$invite_project_selector_ui_colleagues <- renderUI({
      role <- user_role()
      uid <- user_id()
      choices <- get_project_choices(role, uid)
      if (length(choices) == 0) {
        return(h5("Keine Peer Reviews für Ihren Account vorhanden."))
      }
      selectInput(
        "invite_project_selector_colleagues",
        "Peer Review für Einladung auswählen::",
        choices = choices,
        selected = choices[[1]]
      )
    })
    
    output$invite_project_selector_ui_leading_peer <- renderUI({
      role <- user_role()
      uid <- user_id()
      choices <- get_project_choices(role, uid)
      if (length(choices) == 0) {
        return(h5("Keine Peer Reviews für Ihren Account vorhanden."))
      }
      selectInput(
        "invite_project_selector_leading_peer",
        "Peer Review für Einladung auswählen::",
        choices = choices,
        selected = choices[[1]]
      )
    })
    

    # -------------------------------------------
    # ADD: Server-side dropdown for Antworten ansehen
    # -------------------------------------------

    # In your UI where the download button should be:
    output$antworten_project_selector_ui <- renderUI({
      role <- user_role()
      uid <- user_id()
      req(role, uid)
      
      choices <- get_project_choices(role, uid)
      if (length(choices) == 0) {
        return(h5("Keine Peer Reviews für Ihren Account vorhanden."))
      }
      
      tagList(
        selectInput(
          "antworten_project_selector",
          "Peer Review auswählen:",
          choices = choices,
          selected = choices[[1]]
        ),
        
        # ✅ Add download button HERE
        tags$hr(),
        tags$div(
          style = "margin-top: 15px; margin-bottom: 15px;",
          downloadButton(
            "downloadZusammenfassung",
            "Gesamtbericht herunterladen (Labinfo + Selbstbewertung)",
            icon = icon("file-pdf"),
            style = "background-color: #dd4b39; color: white; font-weight: bold; padding: 12px 24px; font-size: 16px;"
          ),
          tags$p(
            style = "margin-top: 10px; color: #666; font-size: 14px;",
            "Bitte wählen Sie zuerst ein Projekt aus der Dropdown-Liste aus."
          )
        )
      )
    })
    
    observeEvent(input$antworten_project_selector, {
      
      val <- input$antworten_project_selector
      
      message("DEBUG: antworten_project_selector changed to: ", val)
      
      
      if (is.null(val) || val == "") {
        current_project_id(NULL)
        current_project_name(NULL)
        return()
      }
      
      current_project_id(val)
      message("DEBUG: current_project_id set to: ", val)
      
      
      con <- get_db_con(); on.exit(DBI::dbDisconnect(con))
      trow <- DBI::dbGetQuery(con, "SELECT title FROM projects WHERE id = $1", params = list(val))
      if (nrow(trow) > 0 && !is.na(trow$title[1])) {
        current_project_name(trow$title[1])
      } else {
        current_project_name(NULL)
      }
    })
    
    
    leiter_codes_colleagues <- reactiveVal(data.frame())
    leiter_codes_leading_peer <- reactiveVal(data.frame())
    leiter_codes_co_peer <- reactiveVal(data.frame()) # For leading_peer role
    
    # ---- Laborleiter invites colleagues ----
    observeEvent(input$generate_leiter_codes_colleagues, {
      req(user_id(), user_role() == "laborleitung", !is.null(input$invite_project_selector_colleagues))
      num_codes <- input$num_leiter_codes_colleagues
      project_id <- as.integer(input$invite_project_selector_colleagues)
      if (num_codes > 0 && !is.na(project_id)) {
        con <- get_db_con(); on.exit(DBI::dbDisconnect(con))
        codes <- character(num_codes)
        for (i in seq_len(num_codes)) {
          code <- uuid::UUIDgenerate()
          DBI::dbExecute(
            con,
            "INSERT INTO invitation_codes (code, assessment_type, role, generated_by, project_id) VALUES ($1, $2, $3, $4, $5)",
            params = list(code, "selbstbewertung", "colleague", user_id(), project_id)
          )
          codes[i] <- code
        }
        df <- data.frame(
          Code = codes,
          Rolle = rep("colleague", num_codes),
          Projekt_ID = rep(project_id, num_codes),
          Status = rep("Unused", num_codes),
          stringsAsFactors = FALSE
        )
        leiter_codes_colleagues(df)
        showNotification(sprintf("%d Einladungscodes für Kollegen (Projekt %s) generiert.", num_codes, project_id), type = "message")
      }
    })
    
    output$leiter_codes_table_colleagues <- DT::renderDataTable({
      df <- leiter_codes_colleagues()
      if (nrow(df) == 0) return(NULL)
      df$Copy <- sprintf(
        '<button class="btn btn-primary btn-sm btn-copy" data-code="%s">Copy</button>',
        df$Code
      )
      df
    }, escape = FALSE, selection = "none", options = list(dom = 't', paging = FALSE))
    
    # ---- Laborleiter invites leading peer ----
    observeEvent(input$generate_leiter_codes_leading_peer, {
      req(user_id(), user_role() == "laborleitung", !is.null(input$invite_project_selector_leading_peer))
      num_codes <- input$num_leiter_codes_leading_peer
      project_id <- as.integer(input$invite_project_selector_leading_peer)
      if (num_codes > 0 && !is.na(project_id)) {
        con <- get_db_con(); on.exit(DBI::dbDisconnect(con))
        codes <- character(num_codes)
        for (i in seq_len(num_codes)) {
          code <- uuid::UUIDgenerate()
          DBI::dbExecute(
            con,
            "INSERT INTO invitation_codes (code, assessment_type, role, generated_by, project_id) VALUES ($1, $2, $3, $4, $5)",
            params = list(code, "fremdbewertung", "leading_peer", user_id(), project_id)
          )
          codes[i] <- code
        }
        df <- data.frame(
          Code = codes,
          Rolle = rep("leading_peer", num_codes),
          Projekt_ID = rep(project_id, num_codes),
          Status = rep("Unused", num_codes),
          stringsAsFactors = FALSE
        )
        leiter_codes_leading_peer(df)
        showNotification(sprintf("%d Einladungscodes für Leading Peer (Projekt %s) generiert.", num_codes, project_id), type = "message")
      }
    })
    
    output$leiter_codes_table_leading_peer <- DT::renderDataTable({
      df <- leiter_codes_leading_peer()
      if (nrow(df) == 0) return(NULL)
      df$Copy <- sprintf(
        '<button class="btn btn-primary btn-sm btn-copy" data-code="%s">Copy</button>',
        df$Code
      )
      df
    }, escape = FALSE, selection = "none", options = list(dom = 't', paging = FALSE))
    
    # ---- Leading peer invites co-peer ----
    
    output$invite_project_selector_ui_leading_peer_co_peer <- renderUI({
      role <- user_role()
      uid <- user_id()
      choices <- get_project_choices(role, uid)
      if (length(choices) == 0) {
        return(h5("Keine Peer Reviews für Ihren Account vorhanden."))
      }
      selectInput(
        "invite_project_selector",  # This ID must match what your observeEvent expects!
        "Peer Review für Einladung auswählen::",
        choices = choices,
        selected = choices[[1]]
      )
    })
    
    observeEvent(input$generate_leading_peer_codes_co_peer, {
      req(user_id(), user_role() == "leading_peer", !is.null(input$invite_project_selector))
      num_codes <- input$num_leading_peer_codes_co_peer
      project_id <- as.integer(input$invite_project_selector)
      if (num_codes > 0 && !is.na(project_id)) {
        con <- get_db_con(); on.exit(DBI::dbDisconnect(con))
        codes <- character(num_codes)
        for (i in seq_len(num_codes)) {
          code <- uuid::UUIDgenerate()
          DBI::dbExecute(
            con,
            "INSERT INTO invitation_codes (code, assessment_type, role, generated_by, project_id) VALUES ($1, $2, $3, $4, $5)",
            params = list(code, "fremdbewertung", "co_peer", user_id(), project_id)
          )
          codes[i] <- code
        }
        df <- data.frame(
          Code = codes,
          Rolle = rep("co_peer", num_codes),
          Projekt_ID = rep(project_id, num_codes),
          Status = rep("Unused", num_codes),
          stringsAsFactors = FALSE
        )
        leiter_codes_co_peer(df)
        showNotification(sprintf("%d Einladungscodes für Co-Peers (Projekt %s) generiert.", num_codes, project_id), type = "message")
      } else {
        showNotification("Kein Projekt zugewiesen – bitte wenden Sie sich an den Laborleiter.", type = "error")
      }
    })
    
    output$leading_peer_codes_table_co_peer <- DT::renderDataTable({
      df <- leiter_codes_co_peer()
      print("DEBUG Table render:"); print(df)
      if (is.null(df) || nrow(df) == 0) return(data.frame(Hinweis = "Noch keine Codes generiert"))
      df$Copy <- sprintf(
        '<button class="btn btn-primary btn-sm btn-copy" data-code="%s">Copy</button>',
        df$Code
      )
      df
    }, escape = FALSE, selection = "none", options = list(dom = 't', paging = FALSE))
#-------------------------------------------------------------------------------  
    
#--------------------------code redemption logic------------------------------
    
    observeEvent(input$redeem_code_button, {
      req(user_logged_in())
      code_input <- trimws(input$invitation_code_input)
      
      if (nchar(code_input) == 0) {
        output$code_redemption_status <- renderUI(span("Bitte geben Sie einen Einladungscode ein.", style = "color:red;"))
        return()
      }
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      # Check if code exists and is not used, and fetch project_id if available
      code_data <- DBI::dbGetQuery(
        con,
        "SELECT id, code, assessment_type, used_by_user_id, used_at, project_id FROM invitation_codes WHERE code = $1",
        params = list(code_input)
      )
      
      if (nrow(code_data) == 0) {
        output$code_redemption_status <- renderUI(span("Code ungültig oder nicht gefunden.", style = "color:red;"))
        return()
      }
      
      if (!is.na(code_data$used_by_user_id[1])) {
        output$code_redemption_status <- renderUI(span(
          paste0(
            "Dieser Code wurde bereits am ",
            format(as.POSIXct(code_data$used_at[1]), "%Y-%m-%d %H:%M"),
            " verwendet."
          ),
          style = "color:orange;"
        ))
        return()
      }
      
      # --- Check that a valid project_id is present ---
      project_id_val <- code_data$project_id[1]
      if (is.null(project_id_val) || is.na(project_id_val) || as.character(project_id_val) == "" || project_id_val == 0) {
        output$code_redemption_status <- renderUI(span(
          "Fehler: Dem Einladungscode ist kein Projekt zugeordnet. Bitte wenden Sie sich an den Laborleiter.",
          style = "color:red;"
        ))
        return()
      }
      
      # Set current_project_id and name
      current_project_id(as.integer(project_id_val))
      project_row <- DBI::dbGetQuery(
        con,
        "SELECT id, title FROM projects WHERE id = $1",
        params = list(project_id_val)
      )
      if (nrow(project_row) > 0) {
        current_project_name(project_row$title[1])
        print(paste("Set current_project_name to", project_row$title[1]))
      } else {
        current_project_name(NULL)
        print("Project not found for given project_id (but project_id set)")
      }
      
      # Code is valid and unused - mark as used
      tryCatch({
        DBI::dbExecute(
          con,
          "UPDATE invitation_codes SET used_by_user_id = $1, used_at = NOW() WHERE id = $2",
          params = list(user_id(), code_data$id[1])
        )
        
        # Store information about the redeemed code (including project_id)
        redeemed_code_info(
          list(
            code = code_input,
            assessment_type = code_data$assessment_type[1],
            project_id = project_id_val
          )
        )
        
        output$code_redemption_status <- renderUI(span(
          paste0(
            "Code erfolgreich eingelöst! Bewertungstyp: ",
            code_data$assessment_type[1]
          ),
          style = "color:green;"
        ))
        
        # Clear the input field
        updateTextInput(session, "invitation_code_input", value = "")
        
        # Trigger an update to the project selector so it shows projects based on the redeemed code
        invitation_data_changed(invitation_data_changed() + 1)
        
      }, error = function(e) {
        output$code_redemption_status <- renderUI(span(
          paste0("Fehler beim Einlösen des Codes: ", e$message),
          style = "color:red;"
        ))
        warning("Error redeeming code: ", e$message)
      })
    })

# ---------------------------------- COPY BUTTON EVENTS ------------------------
    observeEvent(input$copied_code, {
      showNotification(sprintf("Code kopiert: %s", input$copied_code), type = "message")
    })
    
    # This one is not triggered by the .btn-copy, so it remains for other potential copy mechanisms
    observeEvent(input$direct_copy_code, {
      if (!is.null(input$direct_copy_code) &&
          input$direct_copy_code != "") {
        showNotification(sprintf("Code kopiert: %s", input$direct_copy_code),
                         type = "message")
      }
    })
    
    
    # -------------- ADMIN: Populate Projects for SelectInput --------------
    observe({
      req(user_role() %in% c("laborleitung", "leading_peer"),
          user_id()) # Ensure user_id is available
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      projs <- DBI::dbGetQuery(con, "SELECT id, title FROM projects ORDER BY title")
      updateSelectInput(session, "admin_project", choices = setNames(projs$id, projs$title))
    })
    
    # -------------- LABORLEITER/LEADING_PEER: Populate Projects for SelectInput --------------
    observe({
      req(user_role() %in% c("laborleitung", "leading_peer"),
          user_id()) # Ensure user_id is available
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      # Fetch only projects owned by the current user for selection/editing
      projs <- DBI::dbGetQuery(
        con,
        "SELECT id, title FROM projects WHERE owner_id = $1 ORDER BY created_at DESC",
        params = list(user_id())
      ) # MODIFIED
      updateSelectInput(session,
                        "leiter_peer_project",
                        choices = setNames(projs$id, projs$title))
    })
    
    # Observe changes in project selection for labor_leiter/leading_peer and set current_project_id
    observeEvent(input$leiter_peer_project, {
      req(input$leiter_peer_project,
          input$leiter_peer_project != "")
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      selected_proj_id <- input$leiter_peer_project
      
      # Fetch the title of the selected project
      selected_proj_info <- DBI::dbGetQuery(con,
                                            "SELECT id, title FROM projects WHERE id = $1",
                                            params = list(selected_proj_id))
      
      if (nrow(selected_proj_info) > 0) {
        current_project_id(selected_proj_info$id[1])
        current_project_name(selected_proj_info$title[1])
        showNotification(paste("Projekt geladen:", selected_proj_info$title[1]),
                         type = "message")
        # Optionally switch to the lab_ui tab directly after selection
        # updateTabItems(session, "sidebar_menu", selected = "fill_labinfo")
      } else {
        current_project_id(NULL)
        current_project_name(NULL)
        showNotification("Ausgewähltes Projekt nicht gefunden.", type = "warning")
      }
    })
    # Reactive value to store projects owned by labor_leiter/leading_peer
    leiter_peer_projects <- reactiveVal(data.frame())
    
    # Load projects for the current labor_leiter/leading_peer
    load_leiter_peer_projects <- function() {
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      projs <- DBI::dbGetQuery(con,
                               "SELECT * FROM projects WHERE owner_id = $1",
                               params = list(user_id()))
      leiter_peer_projects(projs)
    }
    
    # Initial load and refresh
    observeEvent(user_role(), {
      if (user_role() %in% c("laborleitung", "leading_peer"))
        load_leiter_peer_projects()
    })
    observeEvent(input$refresh_leiter_peer_projects, {
      load_leiter_peer_projects()
    })
    
    # Selector UI
    output$leiter_peer_project_selector <- renderUI({
      projs <- leiter_peer_projects()
      if (nrow(projs) == 0)
        return(h5("Keine eigenen Peer Reviews vorhanden"))
      selectInput(
        "selected_leiter_peer_project",
        "Peer Review auswählen:",
        choices = setNames(projs$id, projs$title)
      )
    })
    
    # Set current project when selected
    observeEvent(input$selected_leiter_peer_project, {
      req(input$selected_leiter_peer_project)
      current_project_id(as.character(input$selected_leiter_peer_project))
      print(paste("Current project set to", current_project_id()))
    })
    
    # Table UI
    output$leiter_peer_projects_table <- DT::renderDataTable({
      projs <- leiter_peer_projects()
      if (nrow(projs) == 0)
        return(NULL)
      DT::datatable(projs, selection = "single", rownames = FALSE)
    })
    
    # Management buttons UI
    output$leiter_peer_management_buttons <- renderUI({
      projs <- leiter_peer_projects()
      if (is.null(input$selected_leiter_peer_project) ||
          nrow(projs) == 0)
        return(NULL)
      sel_proj <- projs[projs$id == input$selected_leiter_peer_project, ]
      if (nrow(sel_proj) == 0)
        return(NULL)
      tagList(
        # actionButton("edit_leiter_peer_project", "Projekt bearbeiten"),
        actionButton("delete_leiter_peer_project", "Projekt löschen", style = "color: red;")
      )
    })
    
    
    
# ----------------- Deletion logic (with modal confirmation) -----------------
    observeEvent(input$delete_leiter_peer_project, {
      projs <- leiter_peer_projects()
      sel_proj <- projs[projs$id == input$selected_leiter_peer_project, ]
      showModal(modalDialog(
        title = "Löschen bestätigen",
        paste("Projekt wirklich löschen:", sel_proj$title, "?"),
        footer = tagList(
          actionButton(
            "confirm_delete_leiter_peer_project",
            "Ja, löschen",
            style = "color: red;"
          ),
          modalButton("Abbrechen")
        )
      ))
    })
    
    observeEvent(input$confirm_delete_leiter_peer_project, {
      removeModal()
      projs <- leiter_peer_projects()
      sel_proj <- projs[projs$id == input$selected_leiter_peer_project, ]
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      DBI::dbExecute(con,
                     "DELETE FROM projects WHERE id = $1",
                     params = list(sel_proj$id))
      showNotification("Projekt gelöscht.", type = "message")
      load_leiter_peer_projects()
    })
    

    #--------------------Expand Responses for Statistics-------------------------
    
    expand_responses_by_group <- function(project_id, question_ids) {
      message("    === expand_responses_by_group START ===")
      message("    project_id: ", project_id)
      message("    question_ids: ", paste(question_ids, collapse = ", "))
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      sql <- sprintf(
        "SELECT user_id, question_id, response_value, text_value, group_size
     FROM responses
     WHERE project_id = $1
       AND question_id IN (%s)
       AND assessment_type = 'selbstbewertung'
       AND response_value IS NOT NULL",
        paste0("'", question_ids, "'", collapse = ", ")
      )
      
      message("    SQL: ", sql)
      
      responses <- DBI::dbGetQuery(con, sql, params = list(project_id))
      message("    Raw responses: ", nrow(responses), " rows")
      
      if (nrow(responses) == 0) {
        message("    No responses found - returning empty data frame")
        return(data.frame(
          user_id = character(),
          question_id = character(),
          response_value = numeric(),
          text_value = character()
        ))
      }
      
      # Expand by group_size
      expanded_list <- lapply(1:nrow(responses), function(i) {
        row <- responses[i, ]
        group_size <- if (is.na(row$group_size) || row$group_size < 1) 1 else row$group_size
        
        if (group_size == 1) {
          return(row[, c("user_id", "question_id", "response_value", "text_value")])
        } else {
          return(row[rep(1, group_size), c("user_id", "question_id", "response_value", "text_value")])
        }
      })
      
      expanded <- do.call(rbind, expanded_list)
      message("    Expanded to: ", nrow(expanded), " rows")
      message("    === expand_responses_by_group END ===")
      
      return(expanded)
    }
    #--------------Helper Functions----------------------------------------------
    
    # ✅ Define questionnaires list
    questionnaires <- list(
      fuehrung = fuehrung_questions,
      mitarbeitende = mit_questions,
      patienten = pat_questions,
      einsender = ein_questions,
      qualitaet = qual_questions
    )
    
    # ✅ Helper to get question label
    get_question_label <- function(question_list, qid) {
      for (q in question_list) {
        if (q$id == qid) {
          return(paste0(q$number, ". ", q$label))
        }
      }
      return(qid)  # Fallback
    }
    
    #--------------Statistics Function-------------------------------------------
    
    get_topic_weighted_stats <- function(project_id, topic_name) {
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      
      # Map topic names to question prefixes
      topic_prefix <- switch(topic_name,
                             "fuehrung" = "F",
                             "mitarbeitende" = "M",
                             "patienten" = "P",
                             "einsender" = "E",
                             "qualitaet" = "Q",
                             NULL
      )
      
      if (is.null(topic_prefix)) {
        return(data.frame(Statistik = "Fehler", Wert = "Unbekanntes Thema"))
      }
      
      # ✅ Count users AND sum group_size
      result <- DBI::dbGetQuery(con, "
    WITH user_stats AS (
      SELECT 
        r.user_id,
        MAX(COALESCE(r.group_size, 1)) as group_size,
        AVG(CAST(r.response_value AS NUMERIC)) as user_avg
      FROM responses r
      WHERE r.project_id = $1
        AND r.question_id LIKE $2
        AND r.response_value ~ '^[0-9]+$'
        AND r.assessment_type = 'selbstbewertung'
      GROUP BY r.user_id
    )
    SELECT 
      COUNT(DISTINCT user_id) as n_users,
      SUM(group_size) as total_people,
      AVG(user_avg) as mean_score,
      STDDEV(user_avg) as sd_score,
      MIN(user_avg) as min_score,
      MAX(user_avg) as max_score
    FROM user_stats
  ", params = list(as.character(project_id), paste0(topic_prefix, "%")))
      
      if (nrow(result) == 0 || is.na(result$n_users[1]) || result$n_users[1] == 0) {
        return(data.frame(
          Statistik = c("Anzahl Teilnehmer", "Gesamtzahl Personen", "Durchschnitt", "Standardabweichung", "Minimum", "Maximum"),
          Wert = c("0", "0", "-", "-", "-", "-")
        ))
      }
      
      data.frame(
        Statistik = c("Anzahl Teilnehmer", "Gesamtzahl Personen", "Durchschnitt", "Standardabweichung", "Minimum", "Maximum"),
        Wert = c(
          as.character(result$n_users[1]),
          as.character(result$total_people[1]),
          sprintf("%.2f", result$mean_score[1]),
          sprintf("%.2f", ifelse(is.na(result$sd_score[1]), 0, result$sd_score[1])),
          sprintf("%.2f", result$min_score[1]),
          sprintf("%.2f", result$max_score[1])
        )
      )
    }
    
    #--------------Spider Plot Data----------------------------------------------
    
    get_weighted_spider_data <- function(project_id, topic_name, binary_qnums = NULL) {
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      
      # Map topic names to question prefixes
      topic_prefix <- switch(topic_name,
                             "fuehrung" = "F",
                             "mitarbeitende" = "M",
                             "patienten" = "P",
                             "einsender" = "E",
                             "qualitaet" = "Q",
                             NULL
      )
      
      if (is.null(topic_prefix)) return(NULL)
      
      # ✅ Use question_id LIKE instead of tag =
      df <- DBI::dbGetQuery(con, "
    SELECT 
      r.question_id,
      r.user_id,
      r.response_value,
      qd.answers::json->>'Berufsgruppe' as berufsgruppe
    FROM responses r
    LEFT JOIN questionnaire_drafts qd 
      ON qd.user_id::text = r.user_id::text 
      AND qd.project_id::text = r.project_id::text
      AND qd.questionnaire_id = 'peer_review'
    WHERE r.project_id = $1
      AND r.question_id LIKE $2
      AND r.response_value ~ '^[0-9]+$'
  ", params = list(project_id, paste0(topic_prefix, "%")))
      
      if (nrow(df) == 0) return(NULL)
      
      df$response_value <- as.numeric(df$response_value)
      df$berufsgruppe[is.na(df$berufsgruppe)] <- "Unbekannt"
      
      # Get question numbers for labeling
      df$qnum <- as.integer(gsub("^[A-Z]+", "", df$question_id))
      
      # Calculate weighted averages per question
      weighted_avgs <- df %>%
        group_by(question_id, qnum, berufsgruppe) %>%
        summarise(group_mean = mean(response_value, na.rm = TRUE), .groups = "drop") %>%
        group_by(question_id, qnum) %>%
        summarise(weighted_avg = mean(group_mean, na.rm = TRUE), .groups = "drop") %>%
        arrange(qnum)
      
      if (nrow(weighted_avgs) == 0) return(NULL)
      
      list(
        values = weighted_avgs$weighted_avg,
        labels = paste0("Q", weighted_avgs$qnum)
      )
    }
    
    #--------------Individual Responses------------------------------------------
    
    get_selbstbewertung_topic_responses <- function(project_id, topic_name) {
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      
      # Map topic names to question prefixes
      topic_prefix <- switch(topic_name,
                             "fuehrung" = "F",
                             "mitarbeitende" = "M",
                             "patienten" = "P",
                             "einsender" = "E",
                             "qualitaet" = "Q",
                             NULL
      )
      
      if (is.null(topic_prefix)) {
        return(data.frame(
          Frage = character(0),
          Berufsgruppe = character(0),
          Bewertung = character(0),
          Begründung = character(0)
        ))
      }
      
      # ✅ Use question_id LIKE instead of tag =
      result <- DBI::dbGetQuery(con, "
    SELECT 
      r.question_id,
      COALESCE(qd.answers::json->>'Berufsgruppe', 'Unbekannt') as berufsgruppe,
      r.response_value,
      r.text_value
    FROM responses r
    LEFT JOIN questionnaire_drafts qd 
      ON qd.user_id::text = r.user_id::text 
      AND qd.project_id::text = r.project_id::text
      AND qd.questionnaire_id = 'peer_review'
    WHERE r.project_id = $1
      AND r.question_id LIKE $2
      AND r.assessment_type = 'selbstbewertung'
    ORDER BY r.question_id, berufsgruppe
  ", params = list(project_id, paste0(topic_prefix, "%")))
      
      if (nrow(result) == 0) {
        return(data.frame(
          Frage = character(0),
          Berufsgruppe = character(0),
          Bewertung = character(0),
          Begründung = character(0)
        ))
      }
      
      data.frame(
        Frage = result$question_id,
        Berufsgruppe = result$berufsgruppe,
        Bewertung = result$response_value,
        Begründung = substr(result$text_value, 1, 200)
      )
    }
    
    #-------------------- Project Summary ----------------------------------------
    
    output$project_summary <- renderUI({
      req(current_project_id())
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      
      # ✅ Count total people (sum of group sizes)
      total_people <- DBI::dbGetQuery(
        con,
        "SELECT COALESCE(SUM(group_size), 0) as total
     FROM (
       SELECT DISTINCT user_id, MAX(group_size) as group_size
       FROM responses
       WHERE project_id = $1 AND assessment_type = 'selbstbewertung'
       GROUP BY user_id
     ) as user_groups",
        params = list(current_project_id())
      )$total
      
      # Count unique submissions
      unique_submissions <- DBI::dbGetQuery(
        con,
        "SELECT COUNT(DISTINCT user_id) as count
     FROM responses
     WHERE project_id = $1 AND assessment_type = 'selbstbewertung'",
        params = list(current_project_id())
      )$count
      
      tagList(
        h4("Projektzusammenfassung"),
        p(strong("Anzahl Einreichungen: "), unique_submissions),
        p(strong("Gesamtzahl Personen: "), total_people),
        p(em("(Gruppenantworten werden als mehrere Personen gezählt)"))
      )
    })
    
#--------------Project Selector for All Roles--------------------------------
    
    output$labor_leiter_summary_project_selector_ui <- renderUI({
      role <- user_role()
      uid <- user_id()
      if (is.null(role) || is.null(uid) || uid == "") return(NULL)
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      
      # Query projects based on role
      if (role == "laborleitung") {
        projs <- DBI::dbGetQuery(
          con,
          "SELECT id, title FROM projects WHERE owner_id = $1 ORDER BY created_at DESC",
          params = list(uid)
        )
      } else if (role == "leading_peer") {
        projs <- DBI::dbGetQuery(
          con,
          "SELECT id, title FROM projects WHERE id = (SELECT project_id FROM users WHERE id = $1)",
          params = list(uid)
        )
      } else if (role == "admin") {
        projs <- DBI::dbGetQuery(
          con,
          "SELECT id, title FROM projects ORDER BY created_at DESC"
        )
      } else {
        projs <- data.frame(id=character(), title=character())
      }
      
      # If no projects found, show a placeholder
      choices <- if (nrow(projs) > 0) setNames(projs$id, projs$title) else c("Keine Peer Reviews verfügbar" = "")
      selected <- if (nrow(projs) > 0) projs$id[1] else ""
      
      selectInput(
        "labor_leiter_summary_project_selector",
        "Peer Review auswählen:",
        choices = choices,
        selected = selected
      )
    })
    
    observeEvent(input$labor_leiter_summary_project_selector, {
      val <- input$labor_leiter_summary_project_selector
      if (is.null(val) || val == "") {
        current_project_id(NULL)
        current_project_name(NULL)
        return(NULL)
      }
      current_project_id(val)
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      project_name_data <- DBI::dbGetQuery(
        con,
        "SELECT title FROM projects WHERE id = $1",
        params = list(val)
      )
      if (nrow(project_name_data) > 0) {
        current_project_name(project_name_data$title[1])
      } else {
        current_project_name(NULL)
      }
    })
    
    #--------------Helper Function: Get Berufsgruppe Breakdown-------------------
    
    get_berufsgruppe_breakdown <- function(project_id) {
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      
      project_id_str <- as.character(project_id)
      
      query <- "
    SELECT 
      qd.answers::json->>'Berufsgruppe' as berufsgruppe,
      COUNT(DISTINCT u.id) as n_users,
      SUM(CAST(qd.answers::json->>'group_size' AS INTEGER)) as n_people
    FROM users u
    LEFT JOIN questionnaire_drafts qd 
      ON u.id::text = qd.user_id::text 
      AND u.project_id::text = qd.project_id::text
      AND qd.questionnaire_id = 'peer_review'
    WHERE u.project_id::text = $1
      AND u.role IN ('laborleitung', 'colleague')
      AND qd.answers IS NOT NULL
      AND qd.answers::json->>'Berufsgruppe' IS NOT NULL
    GROUP BY qd.answers::json->>'Berufsgruppe'
    ORDER BY berufsgruppe
  "
      
      breakdown <- DBI::dbGetQuery(con, query, params = list(project_id_str))
      
      # 🔍 DEBUG
      cat("\n=== get_berufsgruppe_breakdown result ===\n")
      print(breakdown)
      print(paste("Columns:", paste(colnames(breakdown), collapse=", ")))
      cat("===\n")
      
      return(breakdown)
    }
    
    #--------------Wire Up Statistics Outputs------------------------------------
    
    # Führung
    output$fuehrung_stats_table <- renderTable({
      req(current_project_id())
      stats <- get_topic_weighted_stats(current_project_id(), "fuehrung")
      
      # Add Berufsgruppe breakdown
      berufsgruppe_data <- get_berufsgruppe_breakdown(current_project_id())
      
      if (nrow(berufsgruppe_data) > 0) {
        # Create breakdown text
        breakdown_text <- paste(
          berufsgruppe_data$Berufsgruppe, 
          " (n=", berufsgruppe_data$Anzahl, ")", 
          sep = "", 
          collapse = ", "
        )
        
        # Add to stats
        stats <- rbind(
          stats,
          data.frame(
            Statistik = "Berufsgruppen",
            Wert = breakdown_text
          )
        )
      }
      
      stats
    }, digits = 2, sanitize.text.function = function(x) x)
    
    output$fuehrung_responses <- DT::renderDataTable({
      req(current_project_id())
      get_selbstbewertung_topic_responses(current_project_id(), "fuehrung")
    }, escape = FALSE, options = list(pageLength = 10))
    
    # Mitarbeitende
    output$mitarbeitende_stats_table <- renderTable({
      req(current_project_id())
      stats <- get_topic_weighted_stats(current_project_id(), "mitarbeitende")
      
      berufsgruppe_data <- get_berufsgruppe_breakdown(current_project_id())
      
      if (nrow(berufsgruppe_data) > 0) {
        breakdown_text <- paste(
          berufsgruppe_data$Berufsgruppe, 
          " (n=", berufsgruppe_data$Anzahl, ")", 
          sep = "", 
          collapse = ", "
        )
        
        stats <- rbind(
          stats,
          data.frame(
            Statistik = "Berufsgruppen",
            Wert = breakdown_text
          )
        )
      }
      
      stats
    }, digits = 2, sanitize.text.function = function(x) x)
    
    output$mitarbeitende_responses <- DT::renderDataTable({
      req(current_project_id())
      get_selbstbewertung_topic_responses(current_project_id(), "mitarbeitende")
    }, escape = FALSE, options = list(pageLength = 10))
    
    # Patienten
    output$pat_stats_table <- renderTable({
      req(current_project_id())
      stats <- get_topic_weighted_stats(current_project_id(), "patienten")
      
      berufsgruppe_data <- get_berufsgruppe_breakdown(current_project_id())
      
      if (nrow(berufsgruppe_data) > 0) {
        breakdown_text <- paste(
          berufsgruppe_data$Berufsgruppe, 
          " (n=", berufsgruppe_data$Anzahl, ")", 
          sep = "", 
          collapse = ", "
        )
        
        stats <- rbind(
          stats,
          data.frame(
            Statistik = "Berufsgruppen",
            Wert = breakdown_text
          )
        )
      }
      
      stats
    }, digits = 2, sanitize.text.function = function(x) x)
    
    output$pat_responses <- DT::renderDataTable({
      req(current_project_id())
      get_selbstbewertung_topic_responses(current_project_id(), "patienten")
    }, escape = FALSE, options = list(pageLength = 10))
    
    # Einsender
    output$ein_stats_table <- renderTable({
      req(current_project_id())
      stats <- get_topic_weighted_stats(current_project_id(), "einsender")
      
      berufsgruppe_data <- get_berufsgruppe_breakdown(current_project_id())
      
      if (nrow(berufsgruppe_data) > 0) {
        breakdown_text <- paste(
          berufsgruppe_data$Berufsgruppe, 
          " (n=", berufsgruppe_data$Anzahl, ")", 
          sep = "", 
          collapse = ", "
        )
        
        stats <- rbind(
          stats,
          data.frame(
            Statistik = "Berufsgruppen",
            Wert = breakdown_text
          )
        )
      }
      
      stats
    }, digits = 2, sanitize.text.function = function(x) x)
    
    output$ein_responses <- DT::renderDataTable({
      req(current_project_id())
      get_selbstbewertung_topic_responses(current_project_id(), "einsender")
    }, escape = FALSE, options = list(pageLength = 10))
    
    # Qualität
    output$qual_stats_table <- renderTable({
      req(current_project_id())
      stats <- get_topic_weighted_stats(current_project_id(), "qualitaet")
      
      berufsgruppe_data <- get_berufsgruppe_breakdown(current_project_id())
      
      if (nrow(berufsgruppe_data) > 0) {
        breakdown_text <- paste(
          berufsgruppe_data$Berufsgruppe, 
          " (n=", berufsgruppe_data$Anzahl, ")", 
          sep = "", 
          collapse = ", "
        )
        
        stats <- rbind(
          stats,
          data.frame(
            Statistik = "Berufsgruppen",
            Wert = breakdown_text
          )
        )
      }
      
      stats
    }, digits = 2, sanitize.text.function = function(x) x)
    
    output$qual_responses <- DT::renderDataTable({
      req(current_project_id())
      get_selbstbewertung_topic_responses(current_project_id(), "qualitaet")
    }, escape = FALSE, options = list(pageLength = 10))
    
    #--------------Spider Plots for All Topics (with legend at bottom)-----------
    
    output$fuehrung_spider <- renderPlotly({
      message("=== START: Rendering fuehrung_spider ===")
      
      pid <- current_project_id()
      message("DEBUG: Project ID = ", ifelse(is.null(pid), "NULL", as.character(pid)))
      
      req(pid)
      
      message("DEBUG: Calling get_weighted_spider_data for fuehrung")
      data <- get_weighted_spider_data(pid, "fuehrung")
      
      message("DEBUG: Data returned is NULL? ", is.null(data))
      
      if (is.null(data)) {
        message("DEBUG: Returning 'no data' plot")
        return(plotly::plot_ly() %>% layout(title = "Keine Daten vorhanden"))
      }
      
      message("DEBUG: data$values = ", paste(data$values, collapse = ", "))
      message("DEBUG: data$labels = ", paste(data$labels, collapse = ", "))
      
      plot <- plotly::plot_ly(
        type = 'scatterpolar',
        mode = 'lines+markers',
        r = data$values,
        theta = data$labels,
        fill = 'toself',
        name = "Durchschnitt",
        fillcolor = "rgba(1, 115, 178, 0.3)",
        line = list(color = 'rgb(1, 115, 178)', width = 2)
      ) %>%
        layout(
            title = "Führung - Durchschnitt (gewichtet nach Personenzahl)",
          polar = list(radialaxis = list(visible = TRUE, range = c(0, 5))),
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.2,
            yanchor = "top"
          )
        )
      
      message("=== END: fuehrung_spider completed ===")
      
      return(plot)
    })
    
    output$mitarbeitende_spider <- renderPlotly({
      req(current_project_id())
      data <- get_weighted_spider_data(current_project_id(), "mitarbeitende", binary_qnums = c(10, 20, 24))
      
      if (is.null(data)) {
        return(plotly::plot_ly() %>% layout(title = "Keine Daten vorhanden"))
      }
      
      plotly::plot_ly(
        type = 'scatterpolar',
        mode = 'lines+markers',
        r = data$values,
        theta = data$labels,
        fill = 'toself',
        name = "Durchschnitt",
        fillcolor = "rgba(222, 143, 5, 0.3)",
        line = list(color = 'rgb(222, 143, 5)', width = 2)
      ) %>%
        layout(
          list(
            title = "Mitarbeitende - Durchschnitt (gewichtet nach Personenzahl)",
            font = list(size = 14)
          ),
          polar = list(radialaxis = list(visible = TRUE, range = c(0, 5))),
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.2,
            yanchor = "top"
          )
        )
    })
    
    output$pat_spider <- renderPlotly({
      req(current_project_id())
      data <- get_weighted_spider_data(current_project_id(), "patienten")
      
      if (is.null(data)) {
        return(plotly::plot_ly() %>% layout(title = "Keine Daten vorhanden"))
      }
      
      plotly::plot_ly(
        type = 'scatterpolar',
        mode = 'lines+markers',
        r = data$values,
        theta = data$labels,
        fill = 'toself',
        name = "Durchschnitt",
        fillcolor = "rgba(2, 158, 115, 0.3)",
        line = list(color = 'rgb(2, 158, 115)', width = 2)
      ) %>%
        layout(
          title = "Patienten & Angehörige - Durchschnitt",
          polar = list(radialaxis = list(visible = TRUE, range = c(0, 5))),
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.2,
            yanchor = "top"
          )
        )
    })
    
    output$ein_spider <- renderPlotly({
      req(current_project_id())
      data <- get_weighted_spider_data(current_project_id(), "einsender")
      
      if (is.null(data)) {
        return(plotly::plot_ly() %>% layout(title = "Keine Daten vorhanden"))
      }
      
      plotly::plot_ly(
        type = 'scatterpolar',
        mode = 'lines+markers',
        r = data$values,
        theta = data$labels,
        fill = 'toself',
        name = "Durchschnitt",
        fillcolor = "rgba(204, 120, 188, 0.3)",
        line = list(color = 'rgb(204, 120, 188)', width = 2)
      ) %>%
        layout(
          title = "Einsender & Kooperationspartner - Durchschnitt",
          polar = list(radialaxis = list(visible = TRUE, range = c(0, 5))),
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.2,
            yanchor = "top"
          )
        )
    })
    
    output$qual_spider <- renderPlotly({
      req(current_project_id())
      data <- get_weighted_spider_data(current_project_id(), "qualitaet")
      
      if (is.null(data)) {
        return(plotly::plot_ly() %>% layout(title = "Keine Daten vorhanden"))
      }
      
      plotly::plot_ly(
        type = 'scatterpolar',
        mode = 'lines+markers',
        r = data$values,
        theta = data$labels,
        fill = 'toself',
        name = "Durchschnitt",
        fillcolor = "rgba(202, 145, 97, 0.3)",
        line = list(color = 'rgb(202, 145, 97)', width = 2)
      ) %>%
        layout(
          title = "Qualitätsindikatoren - Durchschnitt",
          polar = list(radialaxis = list(visible = TRUE, range = c(0, 5))),
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.2,
            yanchor = "top"
          )
        )
    })
    
#--------------Summary for labor leiter + leading peer-----------------------

    
    output$labor_leiter_summary_project_selector_ui <- renderUI({
      role <- user_role()
      uid <- user_id()
      if (is.null(role) || is.null(uid) || uid == "") return(NULL)
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      
      # Query projects based on role
      if (role == "laborleitung") {
        projs <- DBI::dbGetQuery(
          con,
          "SELECT id, title FROM projects WHERE owner_id = $1 ORDER BY created_at DESC",
          params = list(uid)
        )
      } else if (role == "leading_peer") {
        projs <- DBI::dbGetQuery(
          con,
          "SELECT id, title FROM projects WHERE id = (SELECT project_id FROM users WHERE id = $1)",
          params = list(uid)
        )
      } else {
        projs <- data.frame(id=character(), title=character())
      }
      
      # If no projects found, show a placeholder
      choices <- if (nrow(projs) > 0) setNames(projs$id, projs$title) else c("Keine Peer Reviews verfügbar" = "")
      selected <- if (nrow(projs) > 0) projs$id[1] else ""
      
      selectInput(
        "labor_leiter_summary_project_selector",
        "Peer Review auswählen:",
        choices = choices,
        selected = selected
      )
    })
    
    observeEvent(input$labor_leiter_summary_project_selector, {
      val <- input$labor_leiter_summary_project_selector
      if (is.null(val) || val == "") {
        current_project_id(NULL)
        current_project_name(NULL)
        return(NULL)
      }
      current_project_id(val)
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      project_name_data <- DBI::dbGetQuery(
        con,
        "SELECT title FROM projects WHERE id = $1",
        params = list(val)
      )
      if (nrow(project_name_data) > 0) {
        current_project_name(project_name_data$title[1])
      } else {
        current_project_name(NULL)
      }
    })

    
        
#-----------------------------Project finalisation------------------------------
    
    output$project_finalization_project_selector_ui <- renderUI({
      req(user_role() == "laborleitung")
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      projs <- DBI::dbGetQuery(con,
                               "SELECT id, title FROM projects WHERE owner_id = $1",
                               params = list(user_id()))
      selectInput(
        "project_finalization_project_selector",
        "Peer Review auswählen:",
        choices = setNames(projs$id, projs$title),
        selected = if (nrow(projs) > 0)
          projs$id[1]
        else
          NULL
      )
    })
    
    observeEvent(input$project_finalization_project_selector, {
      current_project_id(input$project_finalization_project_selector)
      # Optionally update current_project_name as well
    })
    
    
    output$finalization_lab_info_ui <- renderUI({
      req(current_project_id())
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      labinfo <- DBI::dbGetQuery(con,
                                 "SELECT * FROM lab_info WHERE project_id = $1",
                                 params = list(current_project_id()))
      if (nrow(labinfo) == 0)
        return(h5("Keine Labordaten für dieses Projekt hinterlegt."))
      
      tags$dl(
        tags$dt("Name und Anschrift:"),
        tags$dd(labinfo$name),
        tags$dt("Internetadresse:"),
        tags$dd(labinfo$internetadresse),
        tags$dt("Trägerschaft:"),
        tags$dd(labinfo$trag),
        tags$dt("Zugehörigkeit zu Unternehmensverbund:"),
        tags$dd(labinfo$zug),
        tags$dt("Ansprechpartner (mit Titel und Qualifikation):"),
        tags$dd(labinfo$ansprechpartner),
        tags$dt("Email Ansprechpartner:"),
        tags$dd(labinfo$email),
        tags$dt("Telefonnummer Ansprechpartner:"),
        tags$dd(labinfo$telefonnummer),
        tags$dt("Öffnungzeiten des Labors:"),
        tags$dd(labinfo$oeffnungzeiten),
        tags$dt("Versorgung / Einsender:"),
        tags$dd(labinfo$versorgung),
        tags$dt("Sonstiges Versorgung:"),
        tags$dd(labinfo$sonstiges_versorgung),
        tags$dt("Laborbereiche:"),
        tags$dd(labinfo$laborbereiche),
        tags$dt("Sonstiges Laborbereiche:"),
        tags$dd(labinfo$sonstiges_laborbereiche),
        tags$dt("Leistungsspektrum / Methoden:"),
        tags$dd(labinfo$leistungsspektrum),
        tags$dt("Sonstiges Leistungsspektrum:"),
        tags$dd(labinfo$sonstiges_leistungsspektrum),
        tags$dt("Ausstattung:"),
        tags$dd(labinfo$ausstattung),
        tags$dt("Hauptlieferanten:"),
        tags$dd(labinfo$hauptlieferanten),
        tags$dt("Farbcodesysteme genutzt:"),
        tags$dd(labinfo$question_binary_1),
        tags$dt("Welche Farbcodesysteme werden verwendet:"),
        tags$dd(labinfo$farbcodesystem),
        tags$dt("Labor nimmt an Studien teil:"),
        tags$dd(labinfo$question_binary_2),
        tags$dt("Verfahren zur Auftragsgenerierung:"),
        tags$dd(labinfo$auftragsgenerierung),
        tags$dt("Hinweise zur Präanalytik:"),
        tags$dd(labinfo$praeanalytik),
        tags$dt("Befundübermittlung:"),
        tags$dd(labinfo$befunduebermittlung),
        tags$dt("Beratungsleistungen:"),
        tags$dd(labinfo$beratungsleistungen),
        tags$dt("Hinweise zur DRG Gruppierung ausgegeben:"),
        tags$dd(labinfo$question_binary_3),
        tags$dt("Organigramm-Dateiname:"),
        tags$dd(labinfo$organigramm_filename),
        tags$dt("Ergänzung:"),
        tags$dd(labinfo$ergaenzung),
        tags$dt("Anzahl Mitarbeitende gesamt:"),
        tags$dd(labinfo$anzahl_total),
        tags$dt("Ärztliches Personal (gesamt):"),
        tags$dd(labinfo$anzahl_mit),
        tags$dt("davon Fachärzte:"),
        tags$dd(labinfo$davon_fachaerzte),
        tags$dt("davon in Weiterbildung:"),
        tags$dd(labinfo$davon_weiterbildung),
        tags$dt("Anzahl ärztliche Planstellen:"),
        tags$dd(labinfo$anzahl_planstellen),
        tags$dt("davon unbesetzt (ärztlich):"),
        tags$dd(labinfo$davon_unbesetzt),
        tags$dt("Technisches Personal (gesamt):"),
        tags$dd(labinfo$anzahl_tech),
        tags$dt("Anzahl technische Planstellen:"),
        tags$dd(labinfo$anzahl_tplanstellen),
        tags$dt("davon unbesetzt (technisch):"),
        tags$dd(labinfo$davon_tunbesetzt),
        tags$dt("Naturwissenschaftliches Personal (gesamt):"),
        tags$dd(labinfo$anzahl_natur),
        tags$dt("Anzahl naturwiss. Planstellen:"),
        tags$dd(labinfo$anzahl_nplanstellen),
        tags$dt("davon unbesetzt (naturwiss.):"),
        tags$dd(labinfo$davon_nunbesetzt),
        tags$dt("IT-Mitarbeitende (gesamt):"),
        tags$dd(labinfo$anzahl_it),
        tags$dt("Anzahl IT-Planstellen:"),
        tags$dd(labinfo$anzahl_iplanstellen),
        tags$dt("davon unbesetzt (IT):"),
        tags$dd(labinfo$davon_iunbesetzt),
        tags$dt("Beschreibung IT-Unterstützung:"),
        tags$dd(labinfo$beschreibung_it),
        tags$dt("Weitere Informationen zu Personal und Stellenbesetzungen:"),
        tags$dd(labinfo$weitereinfo_personal),
        tags$dt("Anbieter Laborinformationssystem:"),
        tags$dd(labinfo$anbieterinfo),
        tags$dt("Anbieter Order-Entry:"),
        tags$dd(labinfo$anbieterorder),
        tags$dt("Anbieter Middleware:"),
        tags$dd(labinfo$anbietermiddleware),
        tags$dt("Weitere IT-Systeme:"),
        tags$dd(labinfo$weitereit),
        tags$dt("Öffnungs- und Betriebszeiten:"),
        tags$dd(labinfo$angaben),
        tags$dt("Kennzahlen laufendes Jahr:"),
        tags$dd(labinfo$laufendenjahres),
        tags$dt("Kennzahlen Vorjahr:"),
        tags$dd(labinfo$vorjahres),
        tags$dt("Kompetenzschwerpunkte:"),
        tags$dd(labinfo$kompetenzschwerpunkte),
        tags$dt("Peer-Review Anmerkungen:"),
        tags$dd(labinfo$anmerkungen)
        # Add further fields as needed for completeness.
      )
    })
    
    
    output$finalization_all_responses_table <- DT::renderDataTable({
      req(current_project_id())
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      responses <- DBI::dbGetQuery(
        con,
        "SELECT r.user_id, u.username, r.question_id, r.response_value, r.text_value
     FROM responses r
     LEFT JOIN users u ON r.user_id = u.id
     WHERE r.project_id = $1
     ORDER BY r.user_id, r.question_id",
        params = list(current_project_id())
      )
      # Remove user_id and username columns
      responses <- responses[, !(names(responses) %in% c("user_id", "username"))]
      DT::datatable(responses, options = list(pageLength = 25, scrollX = TRUE))
    })
    
    
#------------------------Dashboard for each user--------------------------------

    # 1. Get current user's project ID
    colleague_project_id <- reactive({
      req(user_id())
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      row <- DBI::dbGetQuery(con, "SELECT project_id FROM users WHERE id = $1", params = list(user_id()))
      if (nrow(row) > 0) row$project_id[1] else NULL
    })
    
   
    # 3. Get current user's responses for their project
    colleague_responses <- reactive({
      req(user_id(), colleague_project_id())
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      DBI::dbGetQuery(con, 
                      "SELECT * FROM responses WHERE user_id = $1 AND project_id = $2", 
                      params = list(user_id(), colleague_project_id()))
    })
    
    # 4. Helper to display questions + responses for a topic
    render_colleague_topic_responses <- function(topic, questions, responses) {
      qids <- sapply(questions, `[[`, "id")
      labels <- sapply(questions, `[[`, "label")
      tagList(lapply(seq_along(qids), function(i) {
        qid <- qids[[i]]
        label <- labels[[i]]
        response_row <- responses[responses$question_id == qid, ]
        score <- if (nrow(response_row) > 0) as.character(response_row$response_value[1]) else "—"
        text  <- if (nrow(response_row) > 0 && !is.na(response_row$text_value[1])) as.character(response_row$text_value[1]) else ""
        
        div(
          style = "margin-bottom: 22px; padding: 14px; background: #f7fbff; border-radius: 6px; border: 1px solid #c6dbef;",
          tags$span(style="font-weight: bold;", label),
          br(),
          tags$span(style = "color: #003B73;", paste("Antwort:", score)),
          if (nzchar(text)) tags$div(style="margin-top:8px;", text) else NULL
        )
      }))
    }
    
    # 5. UI for dashboard: grouped by topic, with plots and tables
    output$colleague_dashboard_questions_ui <- renderUI({
      req(colleague_responses())
      res <- colleague_responses()
      lapply(names(topic_questions), function(topic) {
        questions <- topic_questions[[topic]]
        box(
          title = topic_labels[[topic]],
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          style = "margin-bottom: 24px;",
          render_colleague_topic_responses(topic, questions, res)
        )
      })
    })
    
    # 6. Plots and tables per topic
    for (topic in names(topic_questions)) {
      local({
        topic_now <- topic
        output[[paste0("colleague_plot_", topic_now)]] <- plotly::renderPlotly({
          questions <- topic_questions[[topic_now]]
          df <- colleague_responses()
          req(nrow(df) > 0)
          labels <- sapply(questions, `[[`, "label")
          qids <- sapply(questions, `[[`, "id")
          answers <- sapply(qids, function(qid) {
            val <- df$response_value[df$question_id == qid]
            if (length(val) == 1) as.numeric(val) else NA
          })
          plotly::plot_ly(
            type = 'scatterpolar',
            mode = 'lines+markers',
            r = answers,
            theta = labels,
            fill = 'toself',
            fillcolor = paste0(topic_colors[topic_now], "66"),
            line = list(color = topic_colors[topic_now], width = 3)
          ) %>%
            layout(
              title = list(text = topic_labels[topic_now], y = 0.92, font = list(size = 16)),
              polar = list(radialaxis = list(visible = TRUE, range = c(0, 5))),
              showlegend = FALSE,
              margin = list(t=60)
            )
        })
        output[[paste0("colleague_table_", topic_now)]] <- DT::renderDataTable({
          questions <- topic_questions[[topic_now]]
          df <- colleague_responses()
          req(nrow(df) > 0)
          qids <- sapply(questions, `[[`, "id")
          labels <- sapply(questions, `[[`, "label")
          df_topic <- df[df$question_id %in% qids, ]
          df_topic$Frage <- labels[match(df_topic$question_id, qids)]
          df_show <- df_topic[, c("Frage", "response_value", "text_value")]
          colnames(df_show) <- c("Frage", "Antwort", "Begründung")
          DT::datatable(df_show, rownames = FALSE, options = list(dom = 't', pageLength = 50))
        })
      })
    }
    
    
    #--------------------------- Dashboard Statistics -----------------------------
    #--------------------------- For Admin and Labor_leiter(-in)-------------------
    
    
    # 1. Define reactiveVals for project selection (top of server)
    current_project_id <- reactiveVal(NULL)
    current_project_name <- reactiveVal(NULL)
    
    # 2. UI for project selector (for admin)
    output$admin_project_selector_ui <- renderUI({
      req(user_logged_in())
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      # ---- KEY LOGIC ----
      role <- user_role()
      if (!is.null(role) && length(role) == 1 && role == "admin") {
        projs <- DBI::dbGetQuery(con, "SELECT id, title FROM projects ORDER BY title")
      } else {
        projs <- data.frame(id = integer(0), title = character(0)) # fallback for other roles
      }
      if (nrow(projs) == 0)
        return(NULL)
      selectInput(
        "admin_selected_project",
        "Peer Review auswählen:",
        choices = setNames(projs$id, projs$title)
      )
    })
    
    # 3. Update current_project_id and current_project_name on selection
    # ---------------- Dashboard project selection (Admin) ----------------
    observeEvent(input$dashboard_project_selector, {
      req(input$dashboard_project_selector)
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      proj <- DBI::dbGetQuery(
        con,
        "SELECT id, title FROM projects WHERE id = $1",
        params = list(input$dashboard_project_selector)
      )
      
      if (nrow(proj) > 0) {
        current_project_id(proj$id[1])
        current_project_name(proj$title[1])
      }
    })
    
    
    # 4. Display project name in a styled box
    output$project_title_box <- renderUI({
      req(current_project_name())
      box(
        title = NULL,
        solidHeader = TRUE,
        status = "info",
        width = 12,
        style = "margin-bottom: 15px; background-color: #e3f2fd; border-left: 5px solid #003B73;",
        h4(
          icon("folder-open"),
          "Aktuelles Projekt: ",
          tags$span(style = "color: #003B73; font-weight: bold;", current_project_name())
        )
      )
    })
    
    # 5. Reactive for all submissions for the selected project
    admin_submissions <- reactive({
      req(user_logged_in(), user_role() == "admin")
      req(current_project_id())
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      DBI::dbGetQuery(con,
                      "SELECT * FROM responses WHERE project_id = $1",
                      params = list(current_project_id()))
    })
    
    
    # 6. stats boxes
    output$totalUsersBox <- renderValueBox({
      req(user_logged_in(),
          user_role() %in% c("admin", "laborleitung"))
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      user_data <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS total FROM users")
      user_count <- user_data$total
      valueBox(
        value = tags$span(user_count, style = "color: white;"),
        subtitle = "Registrierte Benutzer",
        icon = icon("users"),
        color = "aqua"
      )
    })
    
    output$totalProjectsBox <- renderValueBox({
      req(user_logged_in(),
          user_role() %in% c("admin", "laborleitung"))
      project_data_changed() # Depend on this to update
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      count_row <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS count FROM projects")
      count <- if (nrow(count_row) > 0)
        count_row$count[1]
      else
        0
      valueBox(
        value = count,
        subtitle = "Erstellte Peer Reviews",
        icon = icon("folder"),
        color = "green"
      )
    })
    
    output$totalSubmissionsBox <- renderValueBox({
      req(user_logged_in(),
          user_role() %in% c("admin", "laborleitung"))
      responses_data_changed() # Depend on this to update
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      count_row <- DBI::dbGetQuery(con, "SELECT COUNT(DISTINCT user_id) AS count FROM responses")
      count <- if (nrow(count_row) > 0)
        count_row$count[1]
      else
        0
      valueBox(
        value = count,
        subtitle = "Abgeschlossene Fragebögen",
        icon = icon("check-square"),
        color = "yellow"
      )
    })
    
    
    output$user_registration_plot <- renderPlotly({
      req(user_logged_in(),
          user_role() %in% c("admin", "labor_leiter"))
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      user_data <- DBI::dbGetQuery(
        con,
        "SELECT registered_at::date AS registration_date, COUNT(*) as count FROM users GROUP BY 1 ORDER BY 1"
      )
      plot_ly(
        user_data,
        x = ~ registration_date,
        y = ~ count,
        type = 'scatter',
        mode = 'lines+markers'
      ) %>%
        layout(
          title = "Benutzerregistrierungen über die Zeit",
          xaxis = list(title = "Datum"),
          yaxis = list(
            title = "Anzahl der Registrierungen",
            dtick = 1,
            tickformat = ',d'
          )
        )
    })
    
    output$project_status_pie <- renderPlotly({
      req(user_logged_in(), user_role() == "admin")
      project_data_changed() # Depend on this to update
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      project_status <- DBI::dbGetQuery(con,
                                        "SELECT status, COUNT(*) as count FROM projects GROUP BY status")
      plot_ly(
        project_status,
        labels = ~ status,
        values = ~ count,
        type = 'pie'
      ) %>%
        layout(title = "Projektstatus-Verteilung")
    })
    
    output$dashboard_project_name <- renderUI({
      req(current_project_name())
      h3(paste("Aktuelles Projekt:", current_project_name()),
         style = "color:#003B73;")
    })
    
    #------------------------- Submissions Table -------------------------------
    output$admin_submissions_table <- DT::renderDataTable({
      req(admin_submissions())
      admin_submissions()
    })
    
    #--------------------- Project Creation (Admin, labor_leiter, leading_peer) ------------------------
    # observeEvent(input$create_project_button, {
    #   req(user_logged_in(),
    #       user_role() %in% c("admin", "laborleitung", "leading_peer"))
    #   req(input$project_name,
    #       input$project_description,
    #       input$project_dates)
    #   
    #   project_name <- trimws(input$project_name)
    #   project_description <- trimws(input$project_description)
    #   start_date <- input$project_dates[1]
    #   end_date <- input$project_dates[2]
    #   
    #   if (nchar(project_name) < 3) {
    #     output$project_creation_status <- renderUI(span("Projektname muss mindestens 3 Zeichen lang sein.", style = "color:red;"))
    #     return()
    #   }
    #   if (end_date < start_date) {
    #     output$project_creation_status <- renderUI(span("Enddatum darf nicht vor Startdatum liegen.", style = "color:red;"))
    #     return()
    #   }
    #   
    #   con <- get_db_con()
    #   on.exit(DBI::dbDisconnect(con))
    #   tryCatch({
    #     DBI::dbExecute(
    #       con,
    #       "INSERT INTO projects (title, description, start_date, end_date, status, created_at, owner_id) VALUES ($1, $2, $3, $4, $5, NOW(), $6)",
    #       params = list(
    #         project_name,
    #         project_description,
    #         start_date,
    #         end_date,
    #         "active",
    #         user_id()
    #       )
    #     )
    # 
    #     output$project_creation_status <- renderUI(span("Projekt erfolgreich erstellt!", style = "color:green;"))
    #     updateTextInput(session, "project_name", value = "")
    #     updateTextAreaInput(session, "project_description", value = "")
    #     updateDateRangeInput(session,
    #                          "project_dates",
    #                          start = Sys.Date(),
    #                          end = Sys.Date() + 30)
    #     project_data_changed(project_data_changed() + 1) # trigger project list refresh
    #     
    #     # Optionally set current project to newly created
    #     new_proj <- DBI::dbGetQuery(con,
    #                                 "SELECT id, title FROM projects ORDER BY created_at DESC LIMIT 1")
    #     if (nrow(new_proj) > 0) {
    #       current_project_id(new_proj$id[1])
    #       current_project_name(new_proj$title[1])
    #     }
    #   }, error = function(e) {
    #     output$project_creation_status <- renderUI(span(
    #       paste0("Fehler beim Erstellen des Projekts: ", e$message),
    #       style = "color:red;"
    #     ))
    #   })
    # })
    # 
    
    #--------------------- Project Creation (Admin, labor_leiter, leading_peer) ------------------------
    observeEvent(input$create_project_button, {
      req(user_logged_in(),
          user_role() %in% c("admin", "laborleitung", "leading_peer"))
      req(input$project_name,
          input$project_description)
      
      project_name <- trimws(input$project_name)
      project_description <- trimws(input$project_description)
      
      if (nchar(project_name) < 3) {
        output$project_creation_status <- renderUI(
          span("Projektname muss mindestens 3 Zeichen lang sein.", style = "color:red;")
        )
        return()
      }
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      tryCatch({
        # Only these fields: title, description, status, created_at, owner_id
        DBI::dbExecute(
          con,
          "INSERT INTO projects (title, description, status, created_at, owner_id) VALUES ($1, $2, $3, NOW(), $4)",
          params = list(
            project_name,
            project_description,
            "active",
            user_id()
          )
        )
        
        output$project_creation_status <- renderUI(
          span("Peer Review erfolgreich erstellt!", style = "color:green;")
        )
        updateTextInput(session, "project_name", value = "")
        updateTextAreaInput(session, "project_description", value = "")
        # Optionally remove/updateDateRangeInput here if you are not using it
        # updateDateRangeInput(session, "project_dates", start = Sys.Date(), end = Sys.Date() + 30)
        project_data_changed(project_data_changed() + 1) # trigger project list refresh
        
        # Optionally set current project to newly created
        new_proj <- DBI::dbGetQuery(con,
                                    "SELECT id, title FROM projects ORDER BY created_at DESC LIMIT 1")
        if (nrow(new_proj) > 0) {
          current_project_id(new_proj$id[1])
          current_project_name(new_proj$title[1])
        }
      }, error = function(e) {
        output$project_creation_status <- renderUI(
          span(paste0("Fehler beim Erstellen des Projekts: ", e$message), style = "color:red;")
        )
      })
    })
    
    
    
    #------------------ Projects Table (Admin only) ---------------------------
    projects_data <- reactive({
      req(user_logged_in(),
          user_role() %in% c("admin", "laborleitung"))
      project_data_changed() # Depend on this to refresh
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      data <- DBI::dbGetQuery(
        con,
        "SELECT id, title, description, start_date, end_date, status, created_at FROM projects ORDER BY created_at DESC"
      )
      data$start_date <- as.Date(data$start_date)
      data$end_date <- as.Date(data$end_date)
      data$created_at <- format(as.POSIXct(data$created_at, tz = "UTC"),
                                "%Y-%m-%d %H:%M:%S")
      colnames(data) <- c("ID",
                          "Titel",
                          "Beschreibung",
                          "Startdatum",
                          "Enddatum",
                          "Status",
                          "Erstellt am")
      data
    })
    
    output$projects_table <- DT::renderDT({
      req(user_logged_in(),
          user_role() %in% c("admin", "laborleitung")) # MODIFIED
      projects_data()
    }, options = list(pageLength = 10, autoWidth = TRUE), selection = 'single', rownames = FALSE)
    
    #------------------ Invitation Code Table (Admin only) ---------------------------
    all_invitation_codes <- reactive({
      invitation_data_changed()
      req(user_logged_in(), user_role() == "admin")
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      codes <- DBI::dbGetQuery(
        con,
        "SELECT
    ic.code,
    ic.role,
    ic.assessment_type,
    ic.generated_at,
    ic.expires_at,
    CASE
      WHEN ic.used_at IS NOT NULL THEN 'Used'
      WHEN ic.expires_at <= NOW() THEN 'Expired'
      ELSE 'Unused'
    END AS status,
    ic.used_at,
    p.title AS project_name
  FROM invitation_codes ic
  LEFT JOIN projects p ON ic.project_id = p.id
  ORDER BY ic.generated_at DESC"
      )
      
      codes
    })
    
    output$current_invitations_table <- DT::renderDT({
      req(user_logged_in(), user_role() == "admin")
      all_invitation_codes()
    }, options = list(pageLength = 10, autoWidth = TRUE), selection = 'none', rownames = FALSE)
    
    #------------------ Invitation Code Generation (Admin only) ------------------------
    observeEvent(input$generate_codes, {
      req(user_logged_in(), user_role() == "admin")
      
      num_codes <- input$num_codes
      code_type <- input$code_type
      invite_role <- input$invite_role
      project_id <- as.integer(input$dashboard_project_selector_for_codes)
      req(project_id)
      
      
      req(project_id)  # make sure a project is selected
      
      if (num_codes <= 0) {
        output$code_generation_status <- renderUI(
          span("Bitte geben Sie eine positive Anzahl ein.", style = "color:red;")
        )
        return()
      }
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      generated_codes_list <- c()
      
      for (i in seq_len(num_codes)) {
        code <- uuid::UUIDgenerate()
        tryCatch({
          DBI::dbExecute(
            con,
            "INSERT INTO invitation_codes (code, role, assessment_type, generated_at, project_id)
         VALUES ($1, $2, $3, NOW(), $4)",
            params = list(code, invite_role, code_type, project_id)
          )
          generated_codes_list <- c(generated_codes_list, code)
        }, error = function(e) {
          warning(sprintf("Failed to insert code: %s; %s", code, e$message))
        })
      }
      
      output$code_generation_status <- renderUI(
        span(paste0("Erfolgreich ", length(generated_codes_list), " Codes generiert."), style = "color:green;")
      )
      
      output$generated_codes_table <- renderDataTable({
        data.frame(
          Einladungscodes = generated_codes_list,
          Rolle = invite_role,
          Typ = code_type,
          Projekt_ID = project_id,
          stringsAsFactors = FALSE
        )
      })
    })
  
    
    
    #Wire the invitations tab selector to current_project_id()
    # ---------------- Admin invitations project selector ----------------
    observe({
      req(user_logged_in(), user_role() == "admin")
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      projs <- DBI::dbGetQuery(con, "SELECT id, title FROM projects ORDER BY title")
      choices <- setNames(projs$id, projs$title)
      
      updateSelectInput(session, "dashboard_project_selector_for_codes", choices = choices)
    })
    
    observeEvent(input$dashboard_project_selector_for_codes, {
      req(input$dashboard_project_selector_for_codes)
      current_project_id(input$dashboard_project_selector_for_codes)
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      trow <- DBI::dbGetQuery(con, "SELECT title FROM projects WHERE id = $1", params = list(input$dashboard_project_selector_for_codes))
      if (nrow(trow) > 0) current_project_name(trow$title[1])
    })
    
    
    
    # ---------------- User Admin (Admin only) ----------------
    
    users_data_changed <- reactiveVal(0)
    
    admin_users_data <- reactive({
      req(user_logged_in(), user_role() == "admin")
      users_data_changed()
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      DBI::dbGetQuery(con, "
    SELECT
      u.id,
      u.username,
      u.role,
      u.assessment_type,
      u.project_id,
      p.title AS project_title,
      u.activated,
      u.registered_at
    FROM users u
    LEFT JOIN projects p ON p.id = u.project_id
    ORDER BY u.registered_at DESC
  ")
    })
    
    output$user_admin_content <- renderUI({
      req(user_logged_in(), user_role() == "admin")
      fluidRow(
        box(
          title = tags$span("Benutzerliste", style = "color:white;"),
          status = "primary",
          solidHeader = TRUE,
          width = 8,
          DT::dataTableOutput("admin_users_table")
        ),
        box(
          title = tags$span("Benutzer bearbeiten", style = "color:white;"),
          status = "warning",
          solidHeader = TRUE,
          width = 4,
          uiOutput("selected_user_ui"),
          uiOutput("selected_user_edit_ui"),
          actionButton("admin_update_user_btn", "Änderungen speichern", icon = icon("save")),
          hr(),
          actionButton("admin_toggle_activation_btn", "Aktivierung umschalten", icon = icon("user-slash")),
          uiOutput("admin_user_edit_status")
        )
      )
    })
    
    output$admin_users_table <- DT::renderDT({
      df <- admin_users_data()
      DT::datatable(
        df,
        rownames = FALSE,
        selection = "single",
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
    
    selected_user_row <- reactive({
      req(input$admin_users_table_rows_selected)
      df <- admin_users_data()
      df[input$admin_users_table_rows_selected, , drop = FALSE]
    })
    
    output$selected_user_ui <- renderUI({
      u <- selected_user_row()
      tagList(
        tags$b("Ausgewählt: "),
        tags$span(u$username),
        tags$br(),
        tags$small(paste("User ID:", u$id))
      )
    })
    
    output$selected_user_edit_ui <- renderUI({
      u <- selected_user_row()
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      projs <- DBI::dbGetQuery(con, "SELECT id, title FROM projects ORDER BY title")
      proj_choices <- setNames(projs$id, projs$title)
      
      tagList(
        selectInput(
          "admin_edit_role",
          "Rolle",
          choices = c("admin", "laborleitung", "colleague", "leading_peer", "co_peer"),
          selected = u$role
        ),
        selectInput(
          "admin_edit_assessment_type",
          "Assessment-Typ",
          choices = c("selbstbewertung", "fremdbewertung"),
          selected = ifelse(is.na(u$assessment_type) || u$assessment_type == "", "selbstbewertung", u$assessment_type)
        ),
        selectInput(
          "admin_edit_project_id",
          "Projekt",
          choices = c("— kein Projekt —" = "", proj_choices),
          selected = ifelse(is.na(u$project_id), "", as.character(u$project_id))
        )
      )
    })
    
    
    # user_admin_content (Admin: users + roles + project assignment)
    observeEvent(input$admin_update_user_btn, {
      req(user_logged_in(), user_role() == "admin")
      u <- selected_user_row()
      
      new_role <- input$admin_edit_role
      new_assessment <- input$admin_edit_assessment_type
      new_project <- input$admin_edit_project_id
      if (is.null(new_project) || new_project == "") new_project <- NA
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      tryCatch({
        DBI::dbExecute(
          con,
          "UPDATE users
       SET role = $1, assessment_type = $2, project_id = $3
       WHERE id = $4",
          params = list(new_role, new_assessment, new_project, u$id[1])
        )
        users_data_changed(users_data_changed() + 1)
        output$admin_user_edit_status <- renderUI(span("Gespeichert.", style = "color:green;"))
      }, error = function(e) {
        output$admin_user_edit_status <- renderUI(span(paste("Fehler:", e$message), style = "color:red;"))
      })
    })
    
    observeEvent(input$admin_toggle_activation_btn, {
      req(user_logged_in(), user_role() == "admin")
      u <- selected_user_row()
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      tryCatch({
        DBI::dbExecute(
          con,
          "UPDATE users
       SET activated = NOT activated
       WHERE id = $1",
          params = list(u$id[1])
        )
        users_data_changed(users_data_changed() + 1)
        output$admin_user_edit_status <- renderUI(span("Aktivierung geändert.", style = "color:green;"))
      }, error = function(e) {
        output$admin_user_edit_status <- renderUI(span(paste("Fehler:", e$message), style = "color:red;"))
      })
    })
    
    
 #--------------summary server code for laborleitung and admin -----------------  
    output$summary_page_title <- renderText({
      role <- user_role()
      
      # Safety check
      if (is.null(role) || length(role) == 0 || role == "") {
        return("Zusammenfassung")
      }
      
      if (role == "admin") {
        return("Admin - Fortschrittsüberwachung & Zusammenfassung")
      } else if (role == "leading_peer") {
        return("Peer Review Übersicht (Leading Peer)")
      } else if (role == "laborleitung") {
        return("Zusammenfassung der Antworten der Kollegen")
      } else {
        return("Zusammenfassung")
      }
    })
    
    
    # Update the project selector to handle all roles
    output$antworten_project_selector_ui <- renderUI({
      role <- user_role()
      uid <- user_id()
      if (is.null(role) || is.null(uid) || uid == "") return(NULL)
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      # Query projects based on role
      if (role == "laborleitung") {
        projs <- DBI::dbGetQuery(
          con,
          "SELECT id, title FROM projects WHERE owner_id = $1 ORDER BY created_at DESC",
          params = list(uid)
        )
      } else if (role == "leading_peer") {
        projs <- DBI::dbGetQuery(
          con,
          "SELECT id, title FROM projects WHERE id = (SELECT project_id FROM users WHERE id = $1)",
          params = list(uid)
        )
      } else if (role == "admin") {  # ✅ Admin sees ALL projects
        projs <- DBI::dbGetQuery(
          con,
          "SELECT id, title FROM projects ORDER BY created_at DESC"
        )
      } else {
        projs <- data.frame(id=character(), title=character())
      }
      
      # If no projects found, show a placeholder
      choices <- if (nrow(projs) > 0) setNames(projs$id, projs$title) else c("Keine Peer Reviews verfügbar" = "")
      selected <- if (nrow(projs) > 0) projs$id[1] else ""
      
      selectInput(
        "antworten_project_selector",
        "Peer Review auswählen:",
        choices = choices,
        selected = selected
      )
    })
    
    #monitor_progress
    # ---------------- Monitor Progress (Admin) ----------------
    
    output$monitor_project_selector_ui <- renderUI({
      req(user_logged_in(), user_role() == "admin")
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      projs <- DBI::dbGetQuery(con, "SELECT id, title FROM projects ORDER BY title")
      if (nrow(projs) == 0) return(h5("Keine Projekte vorhanden."))
      selectInput(
        "monitor_selected_project",
        "Peer Review auswählen:",
        choices = setNames(projs$id, projs$title)
      )
    })
    
    monitor_progress_data <- reactive({
      req(user_logged_in(), user_role() == "admin", input$monitor_selected_project)
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      # users assigned to project + whether they submitted any responses
      DBI::dbGetQuery(con, "
    SELECT
      u.username,
      u.role,
      u.assessment_type,
      u.activated,
      CASE WHEN r.user_id IS NULL THEN 'Nein' ELSE 'Ja' END AS hat_abgegeben,
      MIN(r.created_at) AS erste_abgabe,
      MAX(r.created_at) AS letzte_abgabe
    FROM users u
    LEFT JOIN responses r
      ON r.user_id = u.id AND r.project_id = $1
    WHERE u.project_id = $1
    GROUP BY u.username, u.role, u.assessment_type, u.activated, r.user_id
    ORDER BY u.role, u.username
  ", params = list(input$monitor_selected_project))
    })
    
    output$monitor_progress_table <- DT::renderDataTable({
      req(input$monitor_project_selector)
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      # ✅ Proper type casting
      progress <- DBI::dbGetQuery(
        con,
        "SELECT 
      u.username AS \"Nutzer\",
      u.email AS \"Email\",
      u.role AS \"Rolle\",
      u.berufsgruppe AS \"Berufsgruppe\",
      COUNT(DISTINCT r.question_id) AS \"Beantwortete Fragen\",
      COUNT(DISTINCT r.question_id) FILTER (WHERE r.response_value IS NOT NULL) AS \"Mit Bewertung\",
      MAX(r.created_at)::date AS \"Letzte Aktivität\"
     FROM users u
     LEFT JOIN responses r ON u.id = r.user_id 
       AND r.project_id = $1::uuid 
       AND r.assessment_type = 'selbstbewertung'
     WHERE u.project_id = $1::uuid
     GROUP BY u.id, u.username, u.email, u.role, u.berufsgruppe
     ORDER BY u.username",
        params = list(as.character(input$monitor_project_selector))
      )
      
      progress
    }, options = list(
      pageLength = 20,
      language = list(
        search = "Suchen:",
        lengthMenu = "Zeige _MENU_ Einträge",
        info = "Zeige _START_ bis _END_ von _TOTAL_ Einträgen"
      )
    ))
    
    
    # --- Helper: check if user has already submitted ---
    user_has_submitted <- function(user_id, tag_value, con) {
      n <- DBI::dbGetQuery(
        con,
        "
    SELECT COUNT(*) as n FROM responses WHERE user_id = $1 AND tag = $2
  ",
        params = list(user_id, tag_value)
      )$n
      n > 0
    }
    
#-------------------------------lab_info---------------------------------------
    
    output$lab_ui <- renderUI({
      req(user_logged_in())
      role <- user_role()
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      # Project filtering logic
      projects <- switch(role,
                         "laborleitung" = DBI::dbGetQuery(
                           con,
                           "SELECT id, title FROM projects WHERE owner_id = $1 ORDER BY created_at DESC",
                           params = list(user_id())
                         ),
                         "colleague" = {
                           user_row <- DBI::dbGetQuery(con, "SELECT project_id FROM users WHERE id = $1", params = list(user_id()))
                           if (!is.null(user_row$project_id) && !is.na(user_row$project_id)) {
                             DBI::dbGetQuery(con, "SELECT id, title FROM projects WHERE id = $1", params = list(user_row$project_id))
                           } else {
                             data.frame(id = integer(), title = character())
                           }
                         },
                         "leading_peer" = {
                           user_row <- DBI::dbGetQuery(con, "SELECT project_id FROM users WHERE id = $1", params = list(user_id()))
                           if (!is.null(user_row$project_id) && !is.na(user_row$project_id)) {
                             DBI::dbGetQuery(con, "SELECT id, title FROM projects WHERE id = $1", params = list(user_row$project_id))
                           } else {
                             data.frame(id = integer(), title = character())
                           }
                         },
                         "co_peer" = {
                           user_row <- DBI::dbGetQuery(con, "SELECT project_id FROM users WHERE id = $1", params = list(user_id()))
                           if (!is.null(user_row$project_id) && !is.na(user_row$project_id)) {
                             DBI::dbGetQuery(con, "SELECT id, title FROM projects WHERE id = $1", params = list(user_row$project_id))
                           } else {
                             data.frame(id = integer(), title = character())
                           }
                         },
                         data.frame(id = integer(), title = character()) # fallback
      )
      
      project_choices <- setNames(projects$id, projects$title)
      
      tagList(
        selectInput(
          "selected_labinfo_project",
          "Peer Review auswählen",
          choices = project_choices,
          selected = isolate(current_project_id()),
          # Optionally disable for roles with only one project
          # disabled = (role %in% c("leading_peer", "co_peer", "colleague"))
        ),
        uiOutput("labinfo_form_ui")
      )
    })
    
    output$labinfo_form_ui <- renderUI({
      pid <- current_project_id()
      req(pid)
      role <- user_role()
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      labinfo <- DBI::dbGetQuery(con, "SELECT * FROM lab_info WHERE project_id = $1", params = list(pid))
      getval <- function(field) if (nrow(labinfo) > 0) labinfo[[field]][1] else ""
      
      if (role %in% c("laborleitung", "admin")) {
        box(
          title = tags$span(style = "color:white;"),
          status = "success",
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          
          div(
            style = "width:100%; text-align:center; margin-bottom: 15px;",
            actionButton("submit_labinfo", "Labordaten speichern", icon = icon("save"),
                         style = "background-color:#003B73;color:white;border:none;"),
            uiOutput("labinfo_submit_status")
          ),
          
          tabsetPanel(
            id = "labinfo_tabs",
            
            # ---------------TAB 1: Grundlagen und Organisation ---------------
            tabPanel(
              "Grundlagen und Organisation",
              value = "tab_grundlagen",
              
              div(
                class = "container-fluid",
                style = "max-width: 800px; margin: auto;",
                
                div(
                  class = "section-container",
                  style = "margin-bottom: 30px; background-color: #c6dbef; padding: 20px; border-radius: 8px;",
                  
                  h4("Laborinformation", style = "margin-bottom: 20px; color: #2c3e50;"),
                  h5("Bitte füllen Sie alle Pflichtfelder aus.", style = "margin-bottom: 25px; color: #2c3e50;"),
                  
                  div(
                    style = "display: flex; flex-direction: column; gap: 15px;",
                    
                    
                    textInput("name", labelMandatory("Name und Anschrift der Einrichtung:"), "", width = "100%"),
                    textInput("Internetadresse", labelMandatory("Internetadresse der Einrichtung:"), "", width = "100%"),
                    textInput("Trag", labelMandatory("Trägerschaft mit Erläuterung ob öffentlich oder privat:"), "", width = "100%"),
                    textInput("Zug", "Zugehörigkeit zu einem Unternehmensverbund:", "", width = "100%"),
                    textInput("Ansprechpartner", labelMandatory("Ansprechpartner mit Titel und Qualifikation:"), "", width = "100%"),
                    textInput("Email", labelMandatory("Email-Adresse Ansprechpartner:"), "", width = "100%"),
                    textInput("Telefonnummer", "Telefonnummer Ansprechpartner:", "", width = "100%"),
                    textInput("Öffnungzeiten", "Öffnungzeiten des Labors:", "", width = "100%"),
                    
                    checkboxGroupInput(
                      "versorgung",
                      labelMandatory("Versorgung / Einsender (bitte auswählen):"),
                      choices = c(
                        "Zentrallabor Krankenhauslabor" = "zentrallabor",
                        "Fachabteilung" = "fachabteilung",
                        "Niedergelassenes Labor" = "niedergelassen",
                        "Sonstiges (bitte benennen)" = "sonstiges"
                      ),
                      width = "100%"
                    ),
                    
                    conditionalPanel(
                      condition = 'input.versorgung && input.versorgung.includes("sonstiges")',
                      textInput("sonstiges_versorgung", "Bitte benennen:", "", width = "100%")
                    ),
                    
                    checkboxGroupInput(
                      "laborbereiche",
                      "Laborbereiche (bitte auswählen):",
                      choices = c(
                        "Klinische Chemie", "Hämostaseologie", "Hämatologie", "Immunologie",
                        "Immunhämatologie", "Mikrobiologie", "Infektionsserologie", "QM des POCT",
                        "Autoantikärperdiagnostik", "Allergologie", "Aminosäureanalytik",
                        "Toxikologie/TDM", "Sonstiges (bitte benennen)"
                      ),
                      width = "100%"
                    ),
                    
                    conditionalPanel(
                      condition = 'input.laborbereiche && input.laborbereiche.includes("Sonstiges (bitte benennen)")',
                      textInput("sonstiges_laborbereiche", "Bitte benennen:", "", width = "100%")
                    ),
                    
                    checkboxGroupInput(
                      "leistungsspektrum",
                      "Leistungsspektrum / Methoden (bitte auswählen):",
                      choices = c(
                        "Absorptionsspektrometrie (AAS)", "Aggregometrie", "Coulometrie",
                        "Durchflußzytometrie (FACS)", "Elektrophorese", "Flammenphotometrie",
                        "Fluoreszenzmikroskopie", "HPLC", "Immunfixation", "Immunoassays",
                        "ISE", "Koagulometrie", "Lichtmikroskopie", "MS", "Nephelometrie",
                        "PCR / molekularbiol. Methoden", "RIA", "Turbidimetrie",
                        "Sonstiges (bitte benennen)"
                      ),
                      width = "100%"
                    ),
                    
                    conditionalPanel(
                      condition = 'input.leistungsspektrum.includes("Sonstiges (bitte benennen)")',
                      textInput("sonstiges_leistungsspektrum", "Bitte benennen:", "", width = "100%")
                    ),
                    
                    textInput("ausstattung", "Ausstattung", "", width = "100%"),
                    textInput("hauptlieferanten", "Benennen Sie Ihre Hauptlieferanten", "", width = "100%"),
                    
                    radioButtons(
                      "question_binary_1",
                      label = "Werden Farbcodesysteme einheitlich genutzt?",
                      choices = list("Ja" = "Ja", "Nein" = "Nein"),
                      selected = character(0),
                      inline = TRUE
                    ),
                    
                    textInput("farbcodesystem", "Welche Farbcodesysteme werden aktuell bei Probenröhrchen verwendet?", width = "100%"),
                    
                    radioButtons(
                      "question_binary_2",
                      label = "Nimmt Ihr Labor an Studien teil?",
                      choices = list("Ja" = "Ja", "Nein" = "Nein"),
                      selected = character(0),
                      inline = TRUE
                    ),
                    
                    textInput("auftragsgenerierung", "Welche Verfahren zur Auftragsgenerierung bietet das Labor dem Einsender an (z. B. order entry etc.)?", width = "100%"),
                    textInput("präanalytik", "Welche Hinweise zur Präanalytik und in welcher Form stellt das Labor den Einsendern bei der Auftragsgenerierung zur Verfügung?", width = "100%"),
                    textInput("befundübermittlung", "Wie erfolgt die Befundübermittlung?", width = "100%"),
                    textInput("beratungsleistungen", "Welche Service- und Beratungsleistungen erbringen Sie?", width = "100%"),
                    
                    radioButtons(
                      "question_binary_3",
                      label = "Werden Hinweise zur DRG Gruppierung ausgegeben?",
                      choices = list("Ja" = "Ja", "Nein" = "Nein"),
                      selected = character(0),
                      inline = TRUE
                    )
                  )
                ),
                
                # ✅ Navigation: Only "Weiter" button (first tab)
                div(
                  style = "margin-top: 30px; text-align: right;",
                  actionButton(
                    "btn_grundlagen_to_personal",
                    "Weiter",
                    icon = icon("arrow-right"),
                    style = "background-color: #3c8dbc; color: white; border: none; padding: 10px 20px; font-weight: 500;"
                  )
                )
              )
            ),
            
            # ----------------------------- TAB 2: Personal --------------------
            tabPanel(
              "Personal",
              value = "tab_personal",
              
              div(
                class = "container-fluid",
                style = "max-width: 800px; margin: auto;",
                
                # ... (all your existing personal input fields) ...
                div(
                  class = "section-container",
                  style = "margin-bottom: 30px; background-color: #c6dbef; padding: 20px; border-radius: 8px;",
                  numericInput("Anzahl_total", "Anzahl Mitarbeitende (gesamt)", value = 0, min = 0, max = 500, width = "100%")
                ),
                
                div(
                  class = "staff-section",
                  style = "margin-bottom: 30px; background-color: #c6dbef; padding: 20px; border-radius: 8px;",
                  h4("Ärztliches Personal", style = "margin-bottom: 20px; color: #2c3e50;"),
                  numericInput("Anzahl_mit", "Anzahl (auch Teilzeit)", value = 0, min = 0, max = 1000, width = "100%"),
                  conditionalPanel(
                    condition = "input.Anzahl_mit >= 1",
                    div(
                      style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px; margin-top: 15px;",
                      numericInput("davon_Fachaerzte", "davon Fachärzte", value = 0, min = 0, max = 1000, width = "100%"),
                      numericInput("davon_weiterbildung", "davon in Weiterbildung", value = 0, min = 0, max = 1000, width = "100%"),
                      numericInput("Anzahl_Planstellen", "Anzahl Planstellen", value = 0, min = 0, max = 1000, width = "100%"),
                      numericInput("davon_unbesetzt", "davon unbesetzt", value = 0, min = 0, max = 1000, width = "100%")
                    )
                  )
                ),
                
                div(
                  class = "staff-section",
                  style = "margin-bottom: 30px; background-color: #c6dbef; padding: 20px; border-radius: 8px;",
                  h4("Technisches Personal", style = "margin-bottom: 20px; color: #2c3e50;"),
                  numericInput("Anzahl_tech", "Anzahl (auch Teilzeit)", value = 0, min = 0, max = 1000, width = "100%"),
                  conditionalPanel(
                    condition = "input.Anzahl_tech >= 1",
                    div(
                      style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px; margin-top: 15px;",
                      numericInput("Anzahl_TPlanstellen", "Anzahl Planstellen", value = 0, min = 0, max = 1000, width = "100%"),
                      numericInput("davon_Tunbesetzt", "davon unbesetzt", value = 0, min = 0, max = 1000, width = "100%")
                    )
                  )
                ),
                
                div(
                  class = "staff-section",
                  style = "margin-bottom: 30px; background-color: #c6dbef; padding: 20px; border-radius: 8px;",
                  h4("Naturwissenschaftliches Personal", style = "margin-bottom: 20px; color: #2c3e50;"),
                  numericInput("Anzahl_natur", "Anzahl (auch Teilzeit)", value = 0, min = 0, max = 1000, width = "100%"),
                  conditionalPanel(
                    condition = "input.Anzahl_natur >= 1",
                    div(
                      style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px; margin-top: 15px;",
                      numericInput("Anzahl_NPlanstellen", "Anzahl Planstellen", value = 0, min = 0, max = 1000, width = "100%"),
                      numericInput("davon_Nunbesetzt", "davon unbesetzt", value = 0, min = 0, max = 1000, width = "100%")
                    )
                  )
                ),
                
                div(
                  class = "staff-section",
                  style = "margin-bottom: 30px; background-color: #c6dbef; padding: 20px; border-radius: 8px;",
                  h4("IT-Mitarbeitende", style = "margin-bottom: 20px; color: #2c3e50;"),
                  numericInput("Anzahl_IT", "Anzahl (auch Teilzeit) direkt dem Labor zugeordnet", value = 0, min = 0, max = 1000, width = "100%"),
                  conditionalPanel(
                    condition = "input.Anzahl_IT >= 1",
                    div(
                      style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px; margin-top: 15px;",
                      numericInput("Anzahl_IPlanstellen", "Anzahl Planstellen", value = 0, min = 0, max = 1000, width = "100%"),
                      numericInput("davon_Iunbesetzt", "davon unbesetzt", value = 0, min = 0, max = 1000, width = "100%")
                    )
                  )
                ),
                
                div(
                  class = "section-container",
                  style = "margin-bottom: 30px; background-color: #c6dbef; padding: 20px; border-radius: 8px;",
                  textAreaInput("Beschreibung", "Beschreibung der IT-Unterstützung, die nicht direkt dem Laborpersonal zugeordnet ist", width = "100%", height = "100px"),
                  textAreaInput("WeitereInfo", "Weitere Informationen zu Personal und Stellenbesetzungen (bitte benennen):", width = "100%", height = "100px")
                ),
                
                # ✅ Navigation: Zurück and Weiter
                div(
                  style = "margin-top: 30px; display: flex; justify-content: space-between; gap: 20px;",
                  actionButton(
                    "btn_personal_back",
                    "Zurück",
                    icon = icon("arrow-left"),
                    style = "background-color: #6c757d; color: white; border: none; padding: 10px 20px; font-weight: 500;"
                  ),
                  actionButton(
                    "btn_personal_to_edv",
                    "Weiter",
                    icon = icon("arrow-right"),
                    style = "background-color: #3c8dbc; color: white; border: none; padding: 10px 20px; font-weight: 500;"
                  )
                )
              )
            ),
            
            # -------------------- TAB 3: EDV und Kennzahlen -------------------
            tabPanel(
              "EDV und Kennzahlen",
              value = "tab_edv",
              
              div(
                class = "container-fluid",
                style = "max-width: 800px; margin: auto;",
                
                # ... (all your existing EDV input fields) ...
                div(
                  class = "section-container",
                  style = "margin-bottom: 30px; background-color: #c6dbef; padding: 20px; border-radius: 8px;",
                  h4("System Informationen", style = "margin-bottom: 20px; color: #2c3e50;"),
                  div(
                    style = "display: flex; flex-direction: column; gap: 15px;",
                    textInput("AnbieterInfo", "Anbieter Laborinformationssystem", width = "100%"),
                    textInput("AnbieterOrder", "Anbieter Order-Entry", width = "100%"),
                    textInput("AnbieterMiddleware", "Anbieter Middleware, falls mehrere bitte alle eintragen", width = "100%"),
                    textInput("WeitereIT", "Weitere für das Labor wichtige IT-Systeme", width = "100%"),
                    textInput("Angaben", "Öffnungs- und Betriebszeiten, z.B. 24/7", width = "100%")
                  )
                ),
                
                div(
                  class = "section-container",
                  style = "margin-bottom: 30px; background-color: #c6dbef; padding: 20px; border-radius: 8px;",
                  h4("Kennzahlen", style = "margin-bottom: 20px; color: #2c3e50;"),
                  div(
                    style = "display: flex; flex-direction: column; gap: 15px;",
                    textInput("laufendenJahres", "Analysenanzahlen, GOÄ-Punkte oder andere Mengenkennzahlen des laufenden Jahres", width = "100%"),
                    textInput("Vorjahres", "Analysenanzahlen, GOÄ-Punkte oder andere Mengenkennzahlen des Vorjahres", width = "100%"),
                    textInput("Kompetenzschwerpunkte", "Kompetenzschwerpunkte und Alleinstellungsmerkmale", width = "100%")
                  )
                ),
                
                div(
                  class = "section-container",
                  style = "margin-bottom: 30px; background-color: #c6dbef; padding: 20px; border-radius: 8px;",
                  h4("Dokumente und Ergänzungen", style = "margin-bottom: 20px; color: #3c8dbc;"),
                  div(
                    style = "display: flex; flex-direction: column; gap: 15px;",
                    fileInput("Organigramm", "Organigramm (Dateiname, Bezeichnung der Anlage - bitte beifügen)"),
                    textInput("Ergänzung", "[Es fehlt ein wichtiger Aspekt? Hier ist Raum für Ihre Ergänzung.]", width = "100%")
                  )
                ),
                
                # ✅ Navigation: Zurück and Weiter
                div(
                  style = "margin-top: 30px; display: flex; justify-content: space-between; gap: 20px;",
                  actionButton(
                    "btn_edv_back",
                    "Zurück",
                    icon = icon("arrow-left"),
                    style = "background-color: #6c757d; color: white; border: none; padding: 10px 20px; font-weight: 500;"
                  ),
                  actionButton(
                    "btn_edv_to_input",
                    "Weiter",
                    icon = icon("arrow-right"),
                    style = "background-color: #3c8dbc; color: white; border: none; padding: 10px 20px; font-weight: 500;"
                  )
                )
              )
            ),
            
            # -------------- TAB 4: Input durch Peer Review --------------------
            tabPanel(
              "Input durch Peer Review",
              value = "tab_input",
              
              div(
                class = "container-fluid",
                style = "max-width: 800px; margin: auto;",
                
                div(
                  class = "section-container",
                  style = "margin-bottom: 30px; background-color: #c6dbef; padding: 20px; border-radius: 8px;",
                  h4("Hauptthema / Anlass", style = "margin-bottom: 20px; color: #2c3e50;"),
                  div(
                    style = "display: flex; gap: 20px;",
                    div(
                      style = "flex: 1;",
                      uiOutput("anmerkungenInputs"),
                      actionButton(
                        "addAnmerkung",
                        "Weitere hinzufügen",
                        class = "btn-primary",
                        style = "width: 50%; margin-top: 15px; font-weight: 500;"
                      )
                    )
                  )
                ),
                
                # ✅ Navigation: Zurück and Abschließen
                div(
                  style = "margin-top: 30px; display: flex; justify-content: space-between; gap: 20px;",
                  actionButton(
                    "btn_input_back",
                    "Zurück",
                    icon = icon("arrow-left"),
                    style = "background-color: #6c757d; color: white; border: none; padding: 10px 20px; font-weight: 500;"
                  ),
                  actionButton(
                    "btn_labinfo_complete",
                    "Abschließen",
                    icon = icon("check"),
                    style = "background-color: #00a65a; color: white; border: none; padding: 10px 20px; font-weight: 500;"
                  )
                )
              )
            )
          )
        )
      }
    })
    
#-----------------------Questionnaire/ Fragebogen UI----------------------------
    
    output$selbstbewertung_summary <- renderUI({
      df <- get_selbstbewertung_topic_responses(current_project_id(), "your_topic_key")
      #if (nrow(df) == 0) return(div("Keine Daten verfügbar."))
      
      tags$table(
        class = "table table-bordered",
        tags$thead(
          tags$tr(
            lapply(names(df), function(n) tags$th(n))
          )
        ),
        tags$tbody(
          lapply(1:nrow(df), function(i) {
            tags$tr(
              tags$td(df$Nutzer[i]),
              tags$td(df$Frage[i]),
              tags$td(df$Antwort[[i]]), # This contains the icons
              tags$td(df$Begründung[i]),
              tags$td(df$Zeitpunkt[i])
            )
          })
        )
      )
    })
    

    # Helper
    get_tag_value <- function() {
      val <- redeemed_code_info()$assessment_type
      if (is.null(val) || val == "") val <- "default"
      val
    }
    
    #old
    # get_submission_tag <- function() {
    #   # For labor_leiter: use current_project_id() or assessment_type
    #   # For others: use invitation_code (from session$userData)
    #   if (user_role() == "laborleitung") {
    #     current_project_id()
    #   } else {
    #     code <- session$userData$invitation_code
    #     if (is.null(code) || code == "") {
    #       # fallback, but this will block submission anyway
    #       "default"
    #     } else {
    #       code
    #     }
    #   }
    # }
    
    get_submission_tag <- function() {
      if (user_role() %in% c("laborleitung", "admin")) {
        # Admin/Laborleitung: use project_id as tag
        proj_id <- current_project_id()
        if (is.null(proj_id) || length(proj_id) != 1 || is.na(proj_id) || proj_id == "") {
          "default"
        } else {
          paste0("project_", proj_id)
        }
        
      } else if (user_role() == "colleague") {
        # Colleague: unique tag per user per project
        proj_id <- current_project_id()
        if (is.null(proj_id) || length(proj_id) != 1 || is.na(proj_id) || proj_id == "") {
          "default"
        } else {
          paste0("colleague_", user_id(), "_project_", proj_id)
        }
        
      } else if (user_role() %in% c("leading_peer", "co_peer")) {
        # Peers: use invitation code if available
        code <- session$userData$invitation_code
        if (is.null(code) || code == "" || length(code) != 1 || is.na(code)) {
          proj_id <- current_project_id()
          paste0(user_role(), "_", user_id(), "_project_", proj_id)
        } else {
          as.character(code)
        }
        
      } else {
        "default"
      }
    }
    
  #----------------------------questionnaire_ui---------------------------------  
    output$questionnaire_ui <- renderUI({
      
      req(user_logged_in())
      user <- user_id()
      con <- get_db_con(); on.exit(DBI::dbDisconnect(con))
      tag_value <- get_submission_tag()
      
      print(current_project_id())
      
      # -- Show current project for laborleitung --
      ui_list <- list()
      if (user_role() == "laborleitung") {
        current_proj_name <- current_project_name()
        if (!is.null(current_proj_name) &&
            nzchar(current_proj_name)) {
          ui_list <- c(ui_list, list(
            div(
              style = "font-weight:bold; color:#003B73; margin-bottom:15px;",
              paste("Aktuelles Projekt:", current_proj_name)
            )
          ))
        }
      }
      
      
      if (user_role() %in% c("leading_peer", "co_peer", "colleague")) {
        row <- DBI::dbGetQuery(con, "SELECT p.title FROM projects p JOIN users u ON p.id = u.project_id WHERE u.id = $1", params = list(user_id()))
        if (nrow(row) > 0 && nzchar(row$title[1])) {
          ui_list <- c(ui_list, list(
            div(
              style = "font-weight:bold; color:#003B73; margin-bottom:15px;",
              paste("Projekt:", row$title[1])
            )
          ))
        }
      }
      
      # -- If already submitted, show message, otherwise, show questionnaire tabs --
      # if (user_role() != "admin" && user_has_submitted(user, tag_value, con)) {
      #   ui_list <- c(ui_list, list(
      #     div(
      #       style = "color:red; font-weight:bold; margin: 40px; font-size: 18px;",
      #       "Sie haben den Fragebogen bereits abgeschickt. Mehrfacheinreichungen sind nicht möglich. Vielen Dank!"
      #     )
      #   ))
      # } else {
      
      # next button
      next_button_ui <- function(id = "next_tab_btn") {
        div(
          style = "text-align:right; margin-top:30px;",
          actionButton(
            id,
            "Nächste →",
            icon = icon("arrow-right"),
            style = "background-color:#003B73;color:white;"
          )
        )
      }
      

        ui_list <- c(ui_list, list(
          tabsetPanel(
            id = "question_info",
            
            tabPanel(
              "Berufsgruppe",
              
              div(
                class = "section-container",
                style = "margin-bottom: 30px; background-color: #c6dbef; padding: 20px; border-radius: 8px;",
                
                radioButtons(
                  "Berufsgruppe",
                  labelMandatory("Berufsgruppe:"),
                  choices = c(
                    "MTL",
                    "Wissenschaftler(-in)",
                    "Ärzte",
                    "IT",
                    "nicht anwendbar"
                  ),
                  selected = "nicht anwendbar",
                  inline = TRUE
                ),
                
                
                shinyjs::useShinyjs(),
                div(
                  class = "section-container",
                  style = "margin-bottom: 30px; background-color: #c6dbef; padding: 20px; border-radius: 8px;",
                  tags$h4("Füllen Sie den Fragebogen alleine oder in einer Gruppe aus?"),
                  radioButtons(
                    "alleine_oder_gruppe",
                    label = NULL,
                    choices = c("Alleine" = "alleine", "Gruppe" = "gruppe"),
                    selected = character(0),
                    inline = TRUE
                  ),
                  conditionalPanel(
                    condition = "input.alleine_oder_gruppe == 'gruppe'",
                    selectInput("gruppen_groesse", "Wie viele Personen?", choices = as.character(1:10), selected = NULL)
                  ),
                  actionButton("go_to_fuehrung", "Weiter", icon = icon("arrow-right"), disabled = TRUE, class = 'btn-primary')
                )
                
              )
            ),
            # --- Section: Führung ---
            tabPanel("Führung",
                     fluidRow(
                       column(width = 12, align = "center",
                              div(
                                tags$h4("Ausgewählte Fragestellungen und Aspekte"),
                                tags$h5("Selbsteinschätzung Reifegrad (N/A = keine Antwort, 1 = sehr niedrig bis 5 =sehr hoch)")
                              )
                       )
                     ),
                     fluidRow(
                       column(width = 8,
                              div(
                                style = "border: 1px solid #dddddd; padding: 10px; margin-right: 15px; background-color: #c6dbef",
                                fuehrung_ui
                              )
                       ),
                       column(width = 4, plotlyOutput("plot_fuehrung", height = "550px"))
                     )
            ),
            
            # --- Section: Mitarbeitende ---
            tabPanel("Mitarbeitende",
                     fluidRow(
                       column(width = 12,
                              div(
                                style = "text-align: center;",
                                tags$h4("Ausgewählte Fragestellungen und Aspekte"),
                                tags$h5("Selbsteinschätzung Reifegrad (N/A = keine Antwort, 1 = sehr niedrig bis 5 =sehr hoch)")
                              )
                       )
                     ),
                     fluidRow(
                       column(width = 8,
                              div(
                                style = "border: 1px solid #dddddd; padding: 10px; margin-right: 15px; background-color: #c6dbef",
                                do.call(tagList, lapply(seq_along(mit_questions), function(qnum) {
                                  q <- mit_questions[[qnum]]
                                  qid <- q$id
                                  numeric_qnum <- as.numeric(sub("^[A-Za-z]+", "", qid))
                                  is_binary <- numeric_qnum %in% mit_binary_global_qnums
                                  info_id <- paste0("info_", qid)
                                  div(
                                    style = "margin-bottom: 20px;",
                                    tags$h5(
                                      style = "text-align: left;",
                                      # ✅ ADD THE NUMBER HERE!
                                      strong(paste0(q$number, ". ", q$label)),
                                      bsButton(info_id, label = "", icon = icon("info-circle"),
                                               style = "info", size = "extra-small", class = "btn-xs info-button",
                                               `data-style` = "margin-left: 5px; padding: 2px 6px;"
                                      )
                                    ),
                                    bsPopover(
                                      id = info_id,
                                      title = paste("Info zu Frage", q$number),  # ✅ Use q$number
                                      content = q$info,
                                      placement = "right",
                                      trigger = "hover",
                                      options = list(container = "body")
                                    ),
                                    if (is_binary) {
                                      radioButtons(
                                        inputId = paste0("Freitext", qid),
                                        label = NULL,
                                        choices = list("Ja" = "Ja", "Nein" = "Nein"),
                                        selected = character(0),
                                        inline = TRUE
                                      )
                                    } else {
                                      radioButtons(
                                        inputId = paste0("Freitext", qid),
                                        label = NULL,
                                        choices = c("N/A", 1:5),
                                        selected = "N/A",
                                        inline = TRUE
                                      )
                                    },
                                    textAreaInput(
                                      inputId = paste0("Freitext_text", qid),
                                      label = NULL,
                                      placeholder = "Text eingeben...",
                                      height = "100px",
                                      width = "100%"
                                    )
                                  )
                                }))
                              )
                       ),
                       column(width = 4,
                              div(style = "position: sticky; top: 20px;", plotlyOutput("plot_mit", height = "550px"))
                       )
                     )
            ),
            
            # --- Section: Patient und Angehörige ---
            tabPanel("Patient und Angehörige",
                     fluidRow(
                       column(width = 12, align = "center",
                              div(
                                tags$h4("Ausgewählte Fragestellungen und Aspekte"),
                                tags$h5("Selbsteinschätzung Reifegrad (N/A = keine Antwort, 1 = sehr niedrig bis 5 =sehr hoch)")
                              )
                       )
                     ),
                     fluidRow(
                       column(width = 8,
                              div(
                                style = "border: 1px solid #dddddd; padding: 10px; margin-right: 15px; background-color: #c6dbef",
                                do.call(tagList, lapply(seq_along(pat_questions), function(i) {
                                  q <- pat_questions[[i]]
                                  qid <- q$id
                                  info_id <- paste0("info_", qid)
                                  heading <- switch(
                                    as.character(i),
                                    "1" = "Kriterien : Patientenorientierung / Patientenbeziehungen",
                                    "2" = "Kriterien : Patienteninformation",
                                    "3" = "Kriterien : Beteiligung Angehörige",
                                    "4" = "Kriterien : Patientenrechte",
                                    NULL
                                  )
                                  items <- list()
                                  if (!is.null(heading)) {
                                    items <- c(items, list(tags$h4(style = "text-align: left;", heading)))
                                  }
                                  items <- c(items, list(
                                    div(
                                      style = "margin-bottom: 20px;",
                                      tags$h5(
                                        style = "text-align: left;",
                                        # ✅ ADD NUMBER HERE
                                        strong(paste0(q$number, ". ", q$label)),
                                        bsButton(info_id, label = "", icon = icon("info-circle"),
                                                 style = "info", size = "extra-small", class = "btn-xs info-button",
                                                 `data-style` = "margin-left: 5px; padding: 2px 6px;"
                                        )
                                      ),
                                      bsPopover(
                                        id = info_id,
                                        title = paste("Info zu Frage", q$number),  # ✅ Use q$number
                                        content = q$info,
                                        placement = "right",
                                        trigger = "hover",
                                        options = list(container = "body")
                                      ),
                                      radioButtons(
                                        paste0("Freitext", qid),
                                        label = NULL,
                                        choices = c("N/A", 1:5),
                                        selected = "N/A",
                                        inline = TRUE
                                      ),
                                      textAreaInput(
                                        paste0("Freitext_text", qid),
                                        label = NULL,
                                        placeholder = "Text eingeben...",
                                        height = "100px",
                                        width = "100%"
                                      )
                                    )
                                  ))
                                  tagList(items)
                                }))
                              )
                       ),
                       column(width = 4,
                              div(style = "position: sticky; top: 20px;", plotlyOutput("plot_pat", height = "550px"))
                       )
                     )
            ),
            
            # --- Section: Einsender und Kooperationspartner ---
            tabPanel("Einsender und Kooperationspartner",
                     fluidRow(
                       column(width = 12, align = "center",
                              div(
                                tags$h4("Ausgewählte Fragestellungen und Aspekte"),
                                tags$h5("Selbsteinschätzung Reifegrad (N/A = keine Antwort, 1 = sehr niedrig bis 5 =sehr hoch)")
                              )
                       )
                     ),
                     fluidRow(
                       column(width = 8,
                              div(
                                style = "border: 1px solid #dddddd; padding: 10px; margin-right: 15px; background-color: #c6dbef",
                                do.call(tagList, lapply(seq_along(ein_questions), function(i) {
                                  q <- ein_questions[[i]]
                                  qid <- q$id
                                  info_id <- paste0("info_", qid)
                                  items <- list(
                                    div(
                                      style = "margin-bottom: 20px;",
                                      tags$h5(
                                        style = "text-align: left;",
                                        # ✅ ADD NUMBER HERE
                                        strong(paste0(q$number, ". ", q$label)),
                                        bsButton(info_id, label = "", icon = icon("info-circle"),
                                                 style = "info", size = "extra-small", class = "btn-xs info-button",
                                                 `data-style` = "margin-left: 5px; padding: 2px 6px;"
                                        )
                                      ),
                                      bsPopover(
                                        id = info_id,
                                        title = paste("Info zu Frage", q$number),  # ✅ Use q$number
                                        content = q$info,
                                        placement = "right",
                                        trigger = "hover",
                                        options = list(container = "body")
                                      ),
                                      radioButtons(
                                        paste0("Freitext", qid),
                                        label = NULL,
                                        choices = c("N/A", 1:5),
                                        selected = "N/A",
                                        inline = TRUE
                                      ),
                                      textAreaInput(
                                        paste0("Freitext_text", qid),
                                        label = NULL,
                                        placeholder = "Text eingeben...",
                                        height = "100px",
                                        width = "100%"
                                      )
                                    )
                                  )
                                  tagList(items)
                                }))
                              )
                       ),
                       column(width = 4,
                              div(style = "position: sticky; top: 20px;", plotlyOutput("einsender_plot", height = "550px"))
                       )
                     )
            ),
            
            # --- Section: Qualitätsindikatoren ---
            tabPanel("Qualitätsindikatoren und Teschnishe und Medizinische Validation",
                     fluidRow(
                       column(width = 12, align = "center",
                              div(
                                tags$h4("Ausgewählte Fragestellungen und Aspekte"),
                                tags$h5("Selbsteinschätzung Reifegrad (N/A = keine Antwort, 1 = sehr niedrig bis 5 =sehr hoch)")
                              )
                       )
                     ),
                     fluidRow(
                       column(width = 8,
                              div(
                                style = "border: 1px solid #dddddd; padding: 10px; margin-right: 15px; background-color: #c6dbef",
                                do.call(tagList, lapply(seq_along(qual_questions), function(i) {
                                  q <- qual_questions[[i]]
                                  qid <- q$id
                                  info_id <- paste0("info_", qid)
                                  heading <- switch(
                                    i,
                                    "1" = "Kriterien : Auftragsgenerierung",
                                    "7" = "Kriterien : Präanalytik im Labor",
                                    "8" = "Kriterien : Analytik",
                                    "11" = "Kriterien : TAT gesamt (von Anforderung bis Befundregistrierung)",
                                    "12" = "Kriterien : Befundung",
                                    "13" = "Kriterien : Probenarchivierung",
                                    "14" = "Kriterien : Klinische Scores auf Basis/mit Anteil laboratoriumsmedizinischer Untersuchungenk",
                                    "15" = "Kriterien : Umgang mit interner Qualitätssicherung",
                                    NULL
                                  )
                                  items <- list()
                                  if (!is.null(heading)) {
                                    items <- c(items, list(tags$h4(style = "text-align: left;", heading)))
                                  }
                                  items <- c(items, list(
                                    div(
                                      style = "margin-bottom: 20px;",
                                      tags$h5(
                                        style = "text-align: left;",
                                        # ✅ ADD NUMBER HERE
                                        strong(paste0(q$number, ". ", q$label)),
                                        bsButton(info_id, label = "", icon = icon("info-circle"),
                                                 style = "info", size = "extra-small", class = "btn-xs info-button",
                                                 `data-style` = "margin-left: 5px; padding: 2px 6px;"
                                        )
                                      ),
                                      bsPopover(
                                        id = info_id,
                                        title = paste("Info zu Frage", q$number),  # ✅ Use q$number
                                        content = q$info,
                                        placement = "right",
                                        trigger = "hover",
                                        options = list(container = "body")
                                      ),
                                      radioButtons(
                                        paste0("Freitext", qid),
                                        label = NULL,
                                        choices = c("N/A", 1:5),
                                        selected = "N/A",
                                        inline = TRUE
                                      ),
                                      textAreaInput(
                                        paste0("Freitext_text", qid),
                                        label = NULL,
                                        placeholder = "Text eingeben...",
                                        height = "100px",
                                        width = "100%"
                                      )
                                    )
                                  ))
                                  tagList(items)
                                }))
                              )
                       ),
                       column(width = 4,
                              div(style = "position: sticky; top: 20px;", plotlyOutput("qual_plot", height = "550px"))
                       )
                     )
            ),
            # Abschicken
            tabPanel(
              "Abschicken und Fragebogen herunterladen",
              
              div(
                tags$h4(
                  "Bitte überprüfen Sie Ihre Eingaben: Sind alle Angaben vollständig? Falls Sie noch Anpassungen vornehmen möchten, können Sie dies jetzt tun.",
                  style = "font-size: 1.5em; text-align: center; margin: 0; color: #003B73;"
                ),
                style = "border: 1px solid #dddddd; padding: 20px; margin-right: 15px; background-color: #c6dbef;"
              ),
              
              
              div(
                style = "margin-top: 20px; text-align: center;",
                downloadButton(
                  "downloadPersonalReport_reports",
                  "Mein Selbstbewerbung herunterladen",
                  style = "background-color:#003B73;color:white;border:none;"
                )
              ),
              
              div(
                style = "margin-top: 40px; text-align: center;",
                actionButton(
                  "submit_questionnaire",
                  "Fragebogen absenden",
                  icon = icon("paper-plane"),
                  style = "background-color:#003B73;color:white;border:none;"
                ),
                uiOutput("questionnaire_submit_status")
              )
            )

            
          )
          
        ))

      
    })
    
    
    # logic to bring the user to Führung when they select Alleine oder Gruppe
    observe({
      enable_btn <- FALSE
      if (!is.null(input$alleine_oder_gruppe)) {
        if (input$alleine_oder_gruppe == "alleine") {
          enable_btn <- TRUE
        } else if (input$alleine_oder_gruppe == "gruppe" &&
                   !is.null(input$gruppen_groesse) &&
                   input$gruppen_groesse != "") {
          enable_btn <- TRUE
        }
      }
      if (enable_btn) {
        shinyjs::enable("go_to_fuehrung")
      } else {
        shinyjs::disable("go_to_fuehrung")
      }
    })
    observeEvent(input$go_to_fuehrung, {
      updateTabsetPanel(session, "question_info", selected = "Führung")
    })

#-------------------PEER FREMDBEWERTUNG NAVIGATION BUTTONS---------------------- 
    
    # ✅ PEER BUTTONS
    peer_tabs <- c("Führung", "Mitarbeitende", "Patient und Angehörige",
                   "Einsender und Kooperationspartner",
                   "Qualitätsindikatoren und Technische und Medizinische Validation")
    
    # Save buttons - all use same ID
    lapply(c("fuehrung", "mit", "pat", "ein", "qual"), function(tab) {
      output[[paste0("peer_save_button_", tab)]] <- renderUI({
        actionButton("save_peer_draft_btn", "Speichern", 
                     icon = icon("save"), class = "btn-fragebogen")
      })
    })
    
    # Next buttons
    lapply(c("fuehrung", "mit", "pat", "ein"), function(tab) {
      output[[paste0("peer_next_button_", tab)]] <- renderUI({
        actionButton("peer_next_tab_btn", "Weiter \u2192", 
                     icon = icon("arrow-right"), class = "btn-fragebogen")
      })
    })
    
    # Last tab: Submit
    output$peer_next_button_qual <- renderUI({
      actionButton("submit_peer_fremdbewertung_all", "Fragebogen abschicken",
                   icon = icon("paper-plane"), class = "btn-fragebogen")
    })
    
    # Next tab navigation
    observeEvent(input$peer_next_tab_btn, {
      req(input$fragebogen_tabs_peer)
      idx <- match(input$fragebogen_tabs_peer, peer_tabs)
      if (!is.na(idx) && idx < length(peer_tabs)) {
        updateTabsetPanel(session, "fragebogen_tabs_peer", selected = peer_tabs[idx + 1])
      }
    }, ignoreInit = TRUE)
    
    # Helper to create next/submit button
    create_peer_next_button <- function(tab_name, is_last = FALSE) {
      renderUI({
        req(input$fragebogen_tabs_peer)
        if (input$fragebogen_tabs_peer != tab_name) return(NULL)
        
        if (is_last) {
          actionButton(
            paste0("submit_peer_fremdbewertung_", gsub(" ", "_", tolower(tab_name))),
            "Fragebogen abschicken",
            icon = icon("paper-plane"),
            class = "btn-fragebogen"
          )
        } else {
          actionButton(
            "peer_next_tab_btn",
            "Weiter",
            icon = icon("arrow-right"),
            class = "btn-fragebogen"
          )
        }
      })
    }
    

    # ✅ PEER SAVE BUTTONS
    output$peer_save_button_fuehrung <- renderUI({
      actionButton("save_peer_draft_btn_fuehrung", "Speichern", 
                   icon = icon("save"), class = "btn-fragebogen")
    })
    
    output$peer_save_button_mit <- renderUI({
      actionButton("save_peer_draft_btn_mit", "Speichern", 
                   icon = icon("save"), class = "btn-fragebogen")
    })
    
    output$peer_save_button_pat <- renderUI({
      actionButton("save_peer_draft_btn_pat", "Speichern", 
                   icon = icon("save"), class = "btn-fragebogen")
    })
    
    output$peer_save_button_ein <- renderUI({
      actionButton("save_peer_draft_btn_ein", "Speichern", 
                   icon = icon("save"), class = "btn-fragebogen")
    })
    
    output$peer_save_button_qual <- renderUI({
      actionButton("save_peer_draft_btn_qual", "Speichern", 
                   icon = icon("save"), class = "btn-fragebogen")
    })
    
    # PEER NEXT BUTTONS
    # ✅ Check your renderUI outputs match these names:
    output$peer_next_button_fuehrung <- renderUI({
      actionButton("peer_next_tab_btn_fuehrung", "Weiter →", 
                   icon = icon("arrow-right"), class = "btn-fragebogen")
    })
    
    output$peer_next_button_mit <- renderUI({
      actionButton("peer_next_tab_btn_mit", "Weiter →", 
                   icon = icon("arrow-right"), class = "btn-fragebogen")
    })
    
    output$peer_next_button_pat <- renderUI({
      actionButton("peer_next_tab_btn_pat", "Weiter →", 
                   icon = icon("arrow-right"), class = "btn-fragebogen")
    })
    
    output$peer_next_button_ein <- renderUI({
      actionButton("peer_next_tab_btn_ein", "Weiter →", 
                   icon = icon("arrow-right"), class = "btn-fragebogen")
    })
    
    # ✅ LAST TAB: Submit button
    output$peer_next_button_qual <- renderUI({
      actionButton("submit_peer_fremdbewertung_all", "Fragebogen abschicken",
                   icon = icon("paper-plane"), class = "btn-fragebogen")
    })
    
    
    # ✅ NEXT TAB HANDLER - listens to ALL next buttons
    peer_tabs <- c("Führung", "Mitarbeitende", "Patient und Angehörige",
                   "Einsender und Kooperationspartner",
                   "Qualitätsindikatoren und Technische und Medizinische Validation")
    
    
    
    # Next button observer
    observeEvent(input$peer_next_tab_btn, {
      req(input$fragebogen_tabs_peer)
      idx <- match(input$fragebogen_tabs_peer, peer_tabs)
      req(!is.na(idx))
      
      if (idx < length(peer_tabs)) {
        updateTabsetPanel(session, "fragebogen_tabs_peer", selected = peer_tabs[idx + 1])
      }
    }, ignoreInit = TRUE)
    
    #--------------------- SAVE PEER FREMDBEWERTUNG DRAFT-----------------------

    observeEvent(input$save_peer_draft_btn, {
      message("\n========== SAVE PEER DRAFT ==========")
      
      req(user_logged_in())
      req(user_role() %in% c("leading_peer", "co_peer"))
      
      current_user_id <- user_id()
      project_id <- current_project_id()
      questionnaire_id <- "peer_fremdbewertung"  # Different from selbstbewertung
      
      message("User ID: ", current_user_id)
      message("Role: ", user_role())
      message("Project ID: ", project_id)
      
      if (is.null(project_id) || project_id == "" || is.na(project_id)) {
        showNotification("Kein Projekt ausgewählt!", type = "error")
        return()
      }
      
      # Collect all fremdbewertung answers
      all_answers <- list()
      
      # Save current tab position
      if (!is.null(input$fragebogen_tabs_peer)) {
        all_answers[["current_tab"]] <- input$fragebogen_tabs_peer
        message("  Current tab: ", input$fragebogen_tabs_peer)
      }
      
      # All question lists
      all_question_lists <- list(
        fuehrung_questions,
        mit_questions,
        pat_questions,
        ein_questions,
        qual_questions
      )
      
      # Collect fremdbewertung responses
      for (qlist in all_question_lists) {
        for (q in qlist) {
          qid <- q$id
          score_id <- paste0("FremdbewertungNum_", qid)
          text_id <- paste0("FremdbewertungText_", qid)
          
          # Save rating
          if (!is.null(input[[score_id]]) && input[[score_id]] != "") {
            all_answers[[score_id]] <- input[[score_id]]
          }
          
          # Save text
          if (!is.null(input[[text_id]]) && input[[text_id]] != "") {
            all_answers[[text_id]] <- input[[text_id]]
          }
        }
      }
      
      message("Total answers: ", length(all_answers))
      
      if (length(all_answers) <= 1) {  # Only tab position saved
        showNotification("Keine Daten zum Speichern!", type = "warning")
        return()
      }
      
      answers_json <- jsonlite::toJSON(all_answers, auto_unbox = TRUE)
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      
      tryCatch({
        # Check if draft exists
        existing <- DBI::dbGetQuery(
          con,
          "SELECT id FROM questionnaire_drafts 
       WHERE user_id = $1 AND project_id = $2 AND questionnaire_id = $3",
          params = list(current_user_id, project_id, questionnaire_id)
        )
        
        if (nrow(existing) > 0) {
          # Update existing
          DBI::dbExecute(
            con,
            "UPDATE questionnaire_drafts 
         SET answers = $1, last_saved = NOW(), status = 'draft'
         WHERE id = $2",
            params = list(answers_json, existing$id[1])
          )
          message("✓ Draft updated - ID: ", existing$id[1])
        } else {
          # Insert new
          DBI::dbExecute(
            con,
            "INSERT INTO questionnaire_drafts (user_id, project_id, questionnaire_id, answers, status, last_saved) 
         VALUES ($1, $2, $3, $4, 'draft', NOW())",
            params = list(current_user_id, project_id, questionnaire_id, answers_json)
          )
          message("✓ New draft created")
        }
        
        showNotification("Entwurf gespeichert!", type = "message", duration = 3)
        
      }, error = function(e) {
        message("ERROR: ", e$message)
        showNotification(paste("Fehler:", e$message), type = "error", duration = 10)
      })
      
      message("=================================\n")
    })
    
#----------------- LOAD PEER FREMDBEWERTUNG DRAFT (on login/page load)----------
    load_peer_fremdbewertung_draft <- function() {
      req(user_logged_in())
      req(user_role() %in% c("leading_peer", "co_peer"))
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      user_id_val <- user_id()
      proj_id_val <- current_project_id()
      qid_val <- "peer_fremdbewertung"
      
      message("\n========== LOAD PEER DRAFT ==========")
      message("User: ", user_id_val)
      message("Project: ", proj_id_val)
      
      if (is.null(proj_id_val) || proj_id_val == "" || is.na(proj_id_val)) {
        message("No project_id")
        return()
      }
      
      # Fetch draft
      draft <- DBI::dbGetQuery(
        con,
        "SELECT answers FROM questionnaire_drafts
     WHERE user_id = $1 AND project_id = $2 AND questionnaire_id = $3 AND status = 'draft'
     ORDER BY last_saved DESC LIMIT 1",
        params = list(user_id_val, proj_id_val, qid_val)
      )
      
      if (nrow(draft) == 0) {
        message("No draft found")
        return()
      }
      
      answers <- jsonlite::fromJSON(draft$answers[1])
      
      if (is.null(answers) || length(answers) == 0) {
        message("Draft is empty")
        return()
      }
      
      message("Loaded ", length(answers), " keys")
      
      # Restore tab position
      if ("current_tab" %in% names(answers)) {
        shinyjs::delay(200, {
          updateTabsetPanel(session, "fragebogen_tabs_peer", selected = answers$current_tab)
          message("✓ Restored tab: ", answers$current_tab)
        })
      }
      
      # Restore all fremdbewertung inputs
      restored_count <- 0
      
      for (id in names(answers)) {
        if (id == "current_tab") next
        
        val <- answers[[id]]
        if (is.null(val) || (is.character(val) && val == "")) next
        
        # Radio buttons (FremdbewertungNum_)
        if (grepl("^FremdbewertungNum_", id)) {
          shinyjs::delay(300, {
            updateRadioButtons(session, id, selected = val)
          })
          restored_count <- restored_count + 1
        }
        # Text areas (FremdbewertungText_)
        else if (grepl("^FremdbewertungText_", id)) {
          shinyjs::delay(300, {
            updateTextAreaInput(session, id, value = val)
          })
          restored_count <- restored_count + 1
        }
      }
      
      message("✓ Restored ", restored_count, " inputs")
      message("=================================\n")
      
      showNotification("Entwurf geladen!", type = "message", duration = 3)
    }
    
    # Auto-load draft when peer navigates to fremdbewertung tab
    observeEvent(input$sidebar_menu, {
      if (input$sidebar_menu == "fill_fragebogen" && user_role() %in% c("leading_peer", "co_peer")) {
        shinyjs::delay(500, {
          load_peer_fremdbewertung_draft()
        })
      }
    }) 

    
    #------------------Fragebogen_ui for leading peer and the co peer------------

    output$fragebogen_ui <- renderUI({
      req(user_logged_in())
      if (!user_role() %in% c("leading_peer", "co_peer")) return(div("Nicht verfügbar"))
      
      project_id <- current_project_id()
      req(project_id)
      con <- get_db_con(); on.exit(DBI::dbDisconnect(con))
      
      # ✅ LOAD SAVED FREMDBEWERTUNG DRAFT
      saved_fremd <- resume_fremdbewertung_draft()
      
      create_table_rows <- function(df, questions_list) {
        rows <- lapply(questions_list, function(q) {
          qid <- q$id
          q_num <- q$number  
          selb_rows <- df[df$question_id == qid & df$response_value != "", ]
          
          # 🔍 DEBUG - Add this
          if (qid == "F1") {
            cat("\n=== FRAGEBOGEN DEBUG F1 ===\n")
            print("Columns in selb_rows:")
            print(colnames(selb_rows))
            print("Data:")
            print(selb_rows[, c("response_value", "group_size")])
          }
          
          selb_display <- if (nrow(selb_rows) > 0) {
            
            if (!"group_size" %in% colnames(selb_rows)) {
              cat("ERROR: group_size missing in fragebogen for", qid, "\n")
              selb_rows$group_size <- 1
            }
            selb_rows$group_size[is.na(selb_rows$group_size)] <- 1
            
            total_people <- sum(selb_rows$group_size, na.rm = TRUE)
            
            rating_weighted <- aggregate(group_size ~ response_value, data = selb_rows, FUN = sum)
            rating_counts_weighted <- setNames(rating_weighted$group_size, rating_weighted$response_value)
            
            # 🔍 DEBUG for F1
            if (qid == "F1") {
              cat("rating_weighted:\n")
              print(rating_weighted)
              cat("rating_counts_weighted:\n")
              print(rating_counts_weighted)
            }
            
            rating_display <- tags$div(
              class = "rating-inline",
              lapply(c("N/A", "1", "2", "3", "4", "5"), function(j) {
                count <- rating_counts_weighted[as.character(j)]
                count <- if (is.na(count)) 0 else as.integer(count)
                
                if (count > 0) {
                  if (count == 1) {
                    tags$span(class = "rating-active", j)
                  } else {
                    tags$span(
                      class = "rating-active rating-with-count",
                      j,
                      tags$sup(class = "rating-count", paste0("\u00d7", count))
                    )
                  }
                } else {
                  tags$span(class = "rating-inactive", j)
                }
              })
            )
            respondent_info <- if (total_people > 1) {
              tags$div(
                class = "respondent-info",
                icon("users"),
                paste0(" ", total_people, " Teilnehmer")
              )
            } else {
              NULL
            }
            
            # Weight text counts
            texts_with_group <- selb_rows[selb_rows$text_value != "" & !is.na(selb_rows$text_value), 
                                          c("text_value", "group_size")]
            
            text_display <- if (nrow(texts_with_group) > 0) {
              text_weighted <- aggregate(group_size ~ text_value, data = texts_with_group, FUN = sum)
              
              tags$div(
                class = "selbst-text-container",
                lapply(1:nrow(text_weighted), function(i) {
                  txt <- text_weighted$text_value[i]
                  count <- text_weighted$group_size[i]
                  tags$div(
                    class = "selbst-text-item",
                    icon("comment-o"),
                    txt,
                    tags$span(class = "text-count-badge", paste0("\u00d7", count))
                  )
                })
              )
            } else {
              NULL
            }
            
            tags$div(rating_display, respondent_info, text_display)
          } else {
            tags$span(class = "no-data", "\u2014")
          }
            
          # ✅ Load saved fremdbewertung values from DRAFT
          score_key <- paste0("FremdbewertungNum_", qid)
          text_key <- paste0("FremdbewertungText_", qid)
          
          saved_rating <- if (score_key %in% names(saved_fremd) && saved_fremd[[score_key]] != "") {
            as.character(saved_fremd[[score_key]])
          } else {
            "N/A"
          }
          
          saved_text <- if (text_key %in% names(saved_fremd) && !is.na(saved_fremd[[text_key]]) && saved_fremd[[text_key]] != "") {
            saved_fremd[[text_key]]
          } else {
            ""
          }
          
          # ✅ Peer assessment inputs - with saved values
          fremd_radio <- radioButtons(
            inputId = paste0("FremdbewertungNum_", qid), 
            label = NULL, 
            choices = c("N/A", "1", "2", "3", "4", "5"),
            selected = saved_rating,
            inline = TRUE
          )
          
          fremd_text <- textAreaInput(
            inputId = paste0("FremdbewertungText_", qid), 
            label = NULL, 
            value = saved_text,
            placeholder = "Ihre Bewertung...", 
            width = "100%",
            rows = 2
          )
          
          tags$tr(
            class = "question-row",
            tags$td(class = "col-question", 
                    tags$strong(paste0(q_num, ". ")), q$label),
            tags$td(class = "col-selbst-display", selb_display),
            tags$td(class = "col-fremd-rating", fremd_radio),
            tags$td(class = "col-fremd-text", fremd_text)
          )
        })
        do.call(tagList, rows)
      }
      
      # Fetch data for all topics
      fuehrung_df <- DBI::dbGetQuery(con, "
  SELECT 
    u.username, 
    u.id as user_id,
    r.question_id, 
    r.response_value, 
    r.text_value,
    CAST(qd.answers::json->>'group_size' AS INTEGER) as group_size
  FROM responses r
  JOIN users u ON u.id = r.user_id
  LEFT JOIN questionnaire_drafts qd 
    ON u.id::text = qd.user_id::text 
    AND qd.project_id::text = r.project_id::text
    AND qd.questionnaire_id = 'peer_review'
  WHERE r.project_id::text = $1::text 
    AND r.assessment_type = 'selbstbewertung'
    AND r.question_id LIKE 'F%'
  ORDER BY r.question_id, u.username
", params = list(as.character(project_id)))
      
      
      mit_df <- DBI::dbGetQuery(con, "
  SELECT 
    u.username, 
    u.id as user_id,
    r.question_id, 
    r.response_value, 
    r.text_value,
    CAST(qd.answers::json->>'group_size' AS INTEGER) as group_size
  FROM responses r
  JOIN users u ON u.id = r.user_id
  LEFT JOIN questionnaire_drafts qd 
    ON u.id::text = qd.user_id::text 
    AND qd.project_id::text = r.project_id::text
    AND qd.questionnaire_id = 'peer_review'
  WHERE r.project_id::text = $1::text 
    AND r.assessment_type = 'selbstbewertung'
    AND r.question_id LIKE 'M%'
  ORDER BY r.question_id, u.username
", params = list(as.character(project_id)))
      
      
      pat_df <- DBI::dbGetQuery(con, "
  SELECT 
    u.username, 
    u.id as user_id,
    r.question_id, 
    r.response_value, 
    r.text_value,
    CAST(qd.answers::json->>'group_size' AS INTEGER) as group_size
  FROM responses r
  JOIN users u ON u.id = r.user_id
  LEFT JOIN questionnaire_drafts qd 
    ON u.id::text = qd.user_id::text 
    AND qd.project_id::text = r.project_id::text
    AND qd.questionnaire_id = 'peer_review'
  WHERE r.project_id::text = $1::text 
    AND r.assessment_type = 'selbstbewertung'
    AND r.question_id LIKE 'P%'
  ORDER BY r.question_id, u.username
", params = list(as.character(project_id)))
      
      ein_df <- DBI::dbGetQuery(con, "
  SELECT 
    u.username, 
    u.id as user_id,
    r.question_id, 
    r.response_value, 
    r.text_value,
    CAST(qd.answers::json->>'group_size' AS INTEGER) as group_size
  FROM responses r
  JOIN users u ON u.id = r.user_id
  LEFT JOIN questionnaire_drafts qd 
    ON u.id::text = qd.user_id::text 
    AND qd.project_id::text = r.project_id::text
    AND qd.questionnaire_id = 'peer_review'
  WHERE r.project_id::text = $1::text 
    AND r.assessment_type = 'selbstbewertung'
    AND r.question_id LIKE 'E%'
  ORDER BY r.question_id, u.username
", params = list(as.character(project_id)))
      
      
      qual_df <- DBI::dbGetQuery(con, "
  SELECT 
    u.username, 
    u.id as user_id,
    r.question_id, 
    r.response_value, 
    r.text_value,
    CAST(qd.answers::json->>'group_size' AS INTEGER) as group_size
  FROM responses r
  JOIN users u ON u.id = r.user_id
  LEFT JOIN questionnaire_drafts qd 
    ON u.id::text = qd.user_id::text 
    AND qd.project_id::text = r.project_id::text
    AND qd.questionnaire_id = 'peer_review'
  WHERE r.project_id::text = $1::text 
    AND r.assessment_type = 'selbstbewertung'
    AND r.question_id LIKE 'Q%'
  ORDER BY r.question_id, u.username
", params = list(as.character(project_id)))
      
      # Create tables
      create_topic_table <- function(df, questions_list) {
        tags$div(
          class = "fragebogen-wrapper",
          tags$table(
            class = "fragebogen-table-compact",
            tags$thead(
              tags$tr(
                tags$th(class = "col-question", "Fragen"),
                tags$th(class = "col-selbst-display", "Selbstbewertung"),
                tags$th(class = "col-fremd-rating", "Fremdbewertung"),
                tags$th(class = "col-fremd-text", "Fremdbewertung Freitext")
              )
            ),
            tags$tbody(create_table_rows(df, questions_list))
          )
        )
      }
      
      fuehrung_table <- create_topic_table(fuehrung_df, fuehrung_questions)
      mit_table <- create_topic_table(mit_df, mit_questions)
      pat_table <- create_topic_table(pat_df, pat_questions)
      ein_table <- create_topic_table(ein_df, ein_questions)
      qual_table <- create_topic_table(qual_df, qual_questions)
      
      # Modern CSS
      custom_css <- tags$head(tags$style(HTML("
    body, .tab-content { background-color: #f8f9fa; }
    .fragebogen-wrapper { background: white; border-radius: 6px; box-shadow: 0 1px 3px rgba(0,59,115,0.1); padding: 16px; margin: 16px 0; overflow-x: auto; }
    .fragebogen-table-compact { width: 100%; border-collapse: collapse; font-size: 13px; }
    .fragebogen-table-compact thead { background: #003B73; color: white; position: sticky; top: 0; z-index: 10; }
    .fragebogen-table-compact th { padding: 10px 8px; font-weight: 600; text-align: left; font-size: 13px; border: 1px solid #0056a6; color: white; }
    .col-question { width: 22%; min-width: 180px; }
    .col-selbst-display { width: 32%; min-width: 220px; }
    .col-fremd-rating { width: 20%; text-align: center; min-width: 150px; }
    .col-fremd-text { width: 26%; min-width: 180px; }
    .question-row { border-bottom: 1px solid #e9ecef; transition: background-color 0.15s; }
    .question-row:hover { background-color: #f8f9fa; }
    .fragebogen-table-compact td { padding: 10px 8px; vertical-align: top; border: 1px solid #dee2e6; }
    .col-question { font-size: 13px; line-height: 1.4; color: #212529; }
    .col-question strong { color: #003B73; font-size: 13px; }
    .rating-inline { display: inline-flex; gap: 4px; align-items: center; flex-wrap: wrap; }
    .rating-active { background: #003B73; color: white; font-weight: 600; padding: 4px 7px; border-radius: 4px; font-size: 11px; display: inline-block; min-width: 28px; text-align: center; position: relative; }
    .rating-with-count { padding-right: 16px; }
    .text-count-badge { display: inline-block; margin-left: 6px; padding: 1px 6px; background: #c6dbef; color: #003B73; font-size: 10px; font-weight: 700; border-radius: 8px; vertical-align: middle; }
    .rating-count { position: absolute; top: -6px; right: -2px; font-size: 9px; color: #003B73; font-weight: 700; background: #c6dbef; padding: 1px 3px; border-radius: 6px; line-height: 1; }
    .rating-inactive { background: #e9ecef; color: #adb5bd; padding: 4px 7px; border-radius: 4px; font-size: 10px; display: inline-block; min-width: 28px; text-align: center; }
    .respondent-info { display: inline-block; margin-top: 6px; padding: 2px 8px; background: #c6dbef; border-radius: 12px; font-size: 11px; color: #003B73; font-weight: 600; }
    .respondent-info .fa { font-size: 10px; color: #003B73; }
    .selbst-text-container { margin-top: 8px; }
    .selbst-text-item { padding: 6px 10px; margin-bottom: 6px; background: #f8f9fa; border-left: 3px solid #003B73; border-radius: 0 4px 4px 0; font-size: 12px; line-height: 1.5; color: #495057; word-wrap: break-word; }
    .selbst-text-item .fa { color: #003B73; margin-right: 6px; font-size: 10px; }
    .no-data { color: #adb5bd; font-style: italic; font-size: 12px; }
    .col-fremd-rating .form-group { margin-bottom: 0 !important; }
    .col-fremd-rating .shiny-input-radiogroup { margin: 0 !important; }
    .col-fremd-rating .shiny-options-group { display: flex !important; gap: 4px !important; flex-wrap: wrap !important; justify-content: center !important; }
    .col-fremd-rating .radio { display: inline-block !important; margin: 0 !important; padding: 0 !important; }
    .col-fremd-rating .radio label { display: inline-block !important; margin: 0 !important; padding: 6px 9px !important; border-radius: 4px !important; font-size: 12px !important; cursor: pointer !important; transition: all 0.15s !important; background: #e9ecef !important; border: 2px solid #dee2e6 !important; min-width: 32px !important; text-align: center !important; color: #495057 !important; font-weight: 500 !important; line-height: 1.2 !important; }
    .col-fremd-rating .radio label:hover { background: #dde4ea !important; border-color: #003B73 !important; }
    .col-fremd-rating .radio label input[type='radio'] { display: none !important; }
    .col-fremd-rating .radio.fremd-selected label { background: #003B73 !important; color: white !important; font-weight: 700 !important; border-color: #003B73 !important; box-shadow: 0 2px 4px rgba(0,59,115,0.3) !important; }
    .col-fremd-text textarea { border: 1px solid #ced4da; border-radius: 4px; font-size: 12px; padding: 6px 8px; transition: border-color 0.15s; resize: vertical; min-height: 50px; width: 100%; }
    .col-fremd-text textarea:focus { border-color: #003B73; outline: none; box-shadow: 0 0 0 2px rgba(0,59,115,0.1); }
    .nav-tabs { border-bottom: 2px solid #dee2e6; margin-bottom: 0; }
    .nav-tabs > li > a { color: #495057; font-weight: 500; border: none; border-bottom: 3px solid transparent; padding: 10px 20px; font-size: 14px; transition: all 0.15s; }
    .nav-tabs > li > a:hover { background-color: #f8f9fa; border-bottom-color: #003B73; }
    .nav-tabs > li.active > a { color: #003B73; border-bottom-color: #003B73; background-color: white; font-weight: 600; }
    .button-container { display: flex; justify-content: flex-start; gap: 12px; margin: 16px 0; padding: 12px 16px; background: white; border-radius: 6px; box-shadow: 0 1px 3px rgba(0,59,115,0.1); }
    .btn-fragebogen, #save_peer_draft_btn, #peer_next_tab_btn, #submit_peer_fremdbewertung_all { background-color: #003B73 !important; color: white !important; border: none !important; padding: 8px 16px; font-size: 14px; font-weight: 500; border-radius: 4px; transition: all 0.15s; box-shadow: 0 2px 4px rgba(0,59,115,0.2); cursor: pointer; }
    .btn-fragebogen:hover, #save_peer_draft_btn:hover, #peer_next_tab_btn:hover, #submit_peer_fremdbewertung_all:hover { background-color: #00508f !important; transform: translateY(-1px); box-shadow: 0 3px 6px rgba(0,59,115,0.3); }
    @media (max-width: 1400px) { .col-question { width: 20%; min-width: 160px; } .col-selbst-display { width: 30%; min-width: 200px; } }
  ")))
      
      tagList(
        custom_css,
        
        tags$script(HTML("
      $(document).ready(function() {
        function highlightSelectedRadios() {
          $('.col-fremd-rating .radio').each(function() {
            var $radio = $(this).find('input[type=\"radio\"]');
            if ($radio.is(':checked')) {
              $(this).addClass('fremd-selected');
            } else {
              $(this).removeClass('fremd-selected');
            }
          });
        }
        
        $(document).on('click', '.col-fremd-rating .radio label', function(e) {
          var $radioDiv = $(this).closest('.radio');
          var $group = $radioDiv.closest('.shiny-input-radiogroup');
          $group.find('.radio').removeClass('fremd-selected');
          $radioDiv.addClass('fremd-selected');
        });
        
        $(document).on('change', '.col-fremd-rating input[type=\"radio\"]', function() {
          var $group = $(this).closest('.shiny-input-radiogroup');
          $group.find('.radio').removeClass('fremd-selected');
          $(this).closest('.radio').addClass('fremd-selected');
        });
        
        setTimeout(highlightSelectedRadios, 500);
        setTimeout(highlightSelectedRadios, 1000);
        setTimeout(highlightSelectedRadios, 2000);
        
        $(document).on('shown.bs.tab', function() {
          setTimeout(highlightSelectedRadios, 300);
        });
        
        $(document).on('shiny:value', function() {
          setTimeout(highlightSelectedRadios, 300);
        });
      });
    ")),
        
        tabsetPanel(
          id = "fragebogen_tabs_peer",
          
          tabPanel(
            "Führung",
            fuehrung_table,
            div(
              class = "button-container",
              uiOutput("peer_save_button_fuehrung"),
              uiOutput("peer_next_button_fuehrung")
            ),
            uiOutput("peer_fremdbewertung_submit_status_fuehrung")
          ),
          
          tabPanel(
            "Mitarbeitende",
            mit_table,
            div(
              class = "button-container",
              uiOutput("peer_save_button_mit"),
              uiOutput("peer_next_button_mit")
            ),
            uiOutput("peer_fremdbewertung_submit_status_mitarbeitende")
          ),
          
          tabPanel(
            "Patient und Angehörige",
            pat_table,
            div(
              class = "button-container",
              uiOutput("peer_save_button_pat"),
              uiOutput("peer_next_button_pat")
            ),
            uiOutput("peer_fremdbewertung_submit_status_pat")
          ),
          
          tabPanel(
            "Einsender und Kooperationspartner",
            ein_table,
            div(
              class = "button-container",
              uiOutput("peer_save_button_ein"),
              uiOutput("peer_next_button_ein")
            ),
            uiOutput("peer_fremdbewertung_submit_status_ein")
          ),
          
          tabPanel(
            "Qualitätsindikatoren und Technische und Medizinische Validation",
            qual_table,
            div(
              class = "button-container",
              uiOutput("peer_save_button_qual"),
              uiOutput("peer_next_button_qual")
            ),
            uiOutput("peer_fremdbewertung_submit_status_qual")
          )
        )
      )
    })
    
    
    #----------SAVE FREMDBEWERTUNG DRAFT - saves all fremdbewertung answers to DB
    observeEvent(input$save_peer_draft_btn, {
      req(user_logged_in())
      req(user_role() %in% c("leading_peer", "co_peer"))
      
      project_id <- current_project_id()
      req(project_id)
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      uid <- user_id()
      saved_count <- 0
      
      tryCatch({
        # Loop through all question topics
        for (section_key in names(topic_questions)) {
          questions <- topic_questions[[section_key]]
          
          for (q in questions) {
            qid <- q$id
            score <- input[[paste0("FremdbewertungNum_", qid)]]
            text_val <- input[[paste0("FremdbewertungText_", qid)]]
            
            if (is.null(score)) next
            
            score_val <- as.character(score)
            text_val <- if (!is.null(text_val)) as.character(text_val) else ""
            
            # Check if response already exists
            existing <- DBI::dbGetQuery(con, "
          SELECT id FROM responses 
          WHERE project_id = $1 AND user_id = $2 
            AND question_id = $3 AND assessment_type = 'fremdbewertung'
        ", params = list(project_id, uid, qid))
            
            if (nrow(existing) > 0) {
              # Update
              DBI::dbExecute(con, "
            UPDATE responses 
            SET response_value = $1, text_value = $2, updated_at = NOW()
            WHERE project_id = $3 AND user_id = $4 
              AND question_id = $5 AND assessment_type = 'fremdbewertung'
          ", params = list(score_val, text_val, project_id, uid, qid))
            } else {
              # Insert
              DBI::dbExecute(con, "
            INSERT INTO responses (project_id, user_id, question_id, response_value, text_value, assessment_type, created_at, updated_at)
            VALUES ($1, $2, $3, $4, $5, 'fremdbewertung', NOW(), NOW())
          ", params = list(project_id, uid, qid, score_val, text_val))
            }
            
            saved_count <- saved_count + 1
          }
        }
        
        showNotification(
          paste0("Fremdbewertung gespeichert! (", saved_count, " Antworten)"),
          type = "message",
          duration = 5
        )
        
      }, error = function(e) {
        message("ERROR saving fremdbewertung: ", e$message)
        showNotification(
          paste("Fehler beim Speichern:", e$message),
          type = "error",
          duration = 10
        )
      })
    }, ignoreInit = TRUE)
    
    
    #----------------------Submit Fremdbewertung--------------------------------
    
    submit_all_fremdbewertung <- function() {
      req(user_logged_in())
      req(user_role() %in% c("leading_peer", "co_peer"))
      
      current_user_id <- user_id()
      project_id <- current_project_id()
      submission_id <- uuid::UUIDgenerate()
      
      message("\n=== SUBMIT ALL FREMDBEWERTUNG ===")
      message("User: ", current_user_id, " (", user_role(), ")")
      message("Project: ", project_id)
      
      if (is.null(project_id) || project_id == "" || is.na(project_id)) {
        showNotification("Fehler: Kein Projekt ausgewählt!", type = "error")
        return()
      }
      
      tag_value <- get_submission_tag()
      
      # Collect all responses from all topics
      responses_list <- list()
      
      all_question_lists <- list(
        fuehrung_questions,
        mit_questions,
        pat_questions,
        ein_questions,
        qual_questions
      )
      
      for (qlist in all_question_lists) {
        for (q in qlist) {
          qid <- q$id
          score_id <- paste0("FremdbewertungNum_", qid)
          text_id <- paste0("FremdbewertungText_", qid)
          
          score <- input[[score_id]]
          text <- input[[text_id]]
          
          if (!is.null(score) && score != "") {
            responses_list[[length(responses_list) + 1]] <- list(
              user_id = current_user_id,
              question_id = qid,
              response_value = as.character(score),
              text_value = if (!is.null(text) && text != "") text else NA_character_,
              tag = tag_value,
              assessment_type = "fremdbewertung",
              submission_id = submission_id,
              project_id = project_id,
              group_size = 1L
            )
          }
        }
      }
      
      if (length(responses_list) == 0) {
        showNotification("Keine Antworten zum Speichern gefunden.", type = "warning")
        return()
      }
      
      message("Collected ", length(responses_list), " responses")
      
      # Save to database
      responses_df <- dplyr::bind_rows(responses_list)
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      
      tryCatch({
        sql <- "
      INSERT INTO responses (
        user_id, question_id, response_value, text_value, 
        tag, assessment_type, submission_id, project_id, group_size
      ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
    "
        
        for (i in 1:nrow(responses_df)) {
          row_data <- unname(as.list(responses_df[i, c(
            "user_id", "question_id", "response_value", "text_value",
            "tag", "assessment_type", "submission_id", "project_id", "group_size"
          )]))
          
          DBI::dbExecute(con, sql, params = row_data)
        }
        
        message("✓ Saved ", nrow(responses_df), " responses")
        message("✓ Submission ID: ", submission_id)
        
        # Delete draft after successful submission
        DBI::dbExecute(
          con,
          "DELETE FROM questionnaire_drafts 
       WHERE user_id = $1 AND project_id = $2 AND questionnaire_id = 'peer_fremdbewertung'",
          params = list(current_user_id, project_id)
        )
        message("✓ Draft deleted")
        message("=== SUCCESS ===\n")
        
        showNotification(
          "Fremdbewertung erfolgreich abgesendet!",
          type = "message",
          duration = 5
        )
        
        # Show success message
        showCenteredSuccess("Fremdbewertung erfolgreich abgesendet!")
        
      }, error = function(e) {
        message("ERROR: ", e$message)
        showNotification(
          paste("Fehler beim Speichern:", e$message),
          type = "error",
          duration = 10
        )
      })
    }
    
#-----------------------Database queries: Responses tables per section----------
    #-Führung
    output$fuehrung_responses_leading_peer <- DT::renderDataTable({
      
      print(user_role())
      
      req(current_project_id())
      con <- get_db_con(); on.exit(DBI::dbDisconnect(con))
      df <- DBI::dbGetQuery(con, "
    SELECT u.username, r.question_id, r.response_value, r.text_value
    FROM responses r
    JOIN users u ON u.id = r.user_id
    WHERE r.assessment_type = 'selbstbewertung'
      AND r.project_id = $1
      AND r.question_id LIKE 'F%'
    ORDER BY r.question_id, u.username
  ", params = list(current_project_id()))
      if (nrow(df) > 0) {
        df$question <- sapply(df$question_id, function(qid) get_question_label(fuehrung_questions, qid))
        df <- df[, c("username", "question", "response_value", "text_value")]
      }
      df
    })
    
    #Mitarbeitende
    output$mitarbeitende_responses_leading_peer <- DT::renderDataTable({
      req(current_project_id())
      con <- get_db_con(); on.exit(DBI::dbDisconnect(con))
      df <- DBI::dbGetQuery(con, "
    SELECT u.username, r.question_id, r.response_value, r.text_value
    FROM responses r
    JOIN users u ON u.id = r.user_id
    WHERE r.assessment_type = 'selbstbewertung'
      AND r.project_id = $1
      AND r.question_id LIKE 'M%'
    ORDER BY r.question_id, u.username
  ", params = list(current_project_id()))
      if (nrow(df) > 0) {
        df$question <- sapply(df$question_id, function(qid) get_question_label(mit_questions, qid))
        df <- df[, c("username", "question", "response_value", "text_value")]
      }
      df
    })
    
    # Patienten & Angehörige
    output$pat_responses_leading_peer <- DT::renderDataTable({
      req(current_project_id())
      con <- get_db_con(); on.exit(DBI::dbDisconnect(con))
      df <- DBI::dbGetQuery(con, "
    SELECT u.username, r.question_id, r.response_value, r.text_value
    FROM responses r
    JOIN users u ON u.id = r.user_id
    WHERE r.assessment_type = 'selbstbewertung'
      AND r.project_id = $1
      AND r.question_id LIKE 'P%'
    ORDER BY r.question_id, u.username
  ", params = list(current_project_id()))
      if (nrow(df) > 0) {
        df$question <- sapply(df$question_id, function(qid) get_question_label(pat_questions, qid))
        df <- df[, c("username", "question", "response_value", "text_value")]
      }
      df
    })
    
    
    # Einsender & Kooperationspartner
    output$ein_responses_leading_peer <- DT::renderDataTable({
      req(current_project_id())
      con <- get_db_con(); on.exit(DBI::dbDisconnect(con))
      df <- DBI::dbGetQuery(con, "
    SELECT u.username, r.question_id, r.response_value, r.text_value
    FROM responses r
    JOIN users u ON u.id = r.user_id
    WHERE r.assessment_type = 'selbstbewertung'
      AND r.project_id = $1
      AND r.question_id LIKE 'E%'
    ORDER BY r.question_id, u.username
  ", params = list(current_project_id()))
      if (nrow(df) > 0) {
        df$question <- sapply(df$question_id, function(qid) get_question_label(ein_questions, qid))
        df <- df[, c("username", "question", "response_value", "text_value")]
      }
      df
    })
    
    
    # Qualitätsindikatoren
    output$qual_responses_leading_peer <- DT::renderDataTable({
      req(current_project_id())
      con <- get_db_con(); on.exit(DBI::dbDisconnect(con))
      df <- DBI::dbGetQuery(con, "
    SELECT u.username, r.question_id, r.response_value, r.text_value
    FROM responses r
    JOIN users u ON u.id = r.user_id
    WHERE r.assessment_type = 'selbstbewertung'
      AND r.project_id = $1
      AND r.question_id LIKE 'Q%'
    ORDER BY r.question_id, u.username
  ", params = list(current_project_id()))
      if (nrow(df) > 0) {
        df$question <- sapply(df$question_id, function(qid) get_question_label(qual_questions, qid))
        df <- df[, c("username", "question", "response_value", "text_value")]
      }
      df
    })
    
    #----------------- Questionnaire Logic ----------------------------------------
    # Update project selector for questionnaire AND control visibility
    
    # Debug: Print Selbstbewertung inputs for testing
    observe({
      print(sapply(1:6, function(i)
        input[[paste0("Selbstbewertung", i)]]))
    })
    
    observe({
      req(user_logged_in())
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      # Only show project selector if admin or code redeemed
      if (user_role() == "admin" ||
          !is.null(redeemed_code_info())) {
        shinyjs::show("project_selector_qn_label")
        shinyjs::show("project_selector_qn")
        shinyjs::show("start_qn")
        
        projects <- DBI::dbGetQuery(con, "SELECT id, title FROM projects ORDER BY title")
        project_choices <- setNames(projects$id, projects$title)
        
        # For non-admins: pre-select project from redeemed_code_info, if available
        selected_project <- NULL
        if (!is.null(redeemed_code_info()) &&
            !is.null(redeemed_code_info()$project_id)) {
          selected_project <- redeemed_code_info()$project_id
        }
        updateSelectInput(session,
                          "project_selector_qn",
                          choices = project_choices,
                          selected = selected_project)
      } else {
        # User is logged in but no code redeemed, hide project selection
        shinyjs::hide("project_selector_qn_label")
        shinyjs::hide("project_selector_qn")
        shinyjs::hide("start_qn")
        updateSelectInput(session,
                          "project_selector_qn",
                          choices = c("Bitte Code einlösen" = ""))
      }
    })
    
    #--------------------------personal------------------------------------------
    all_present_and_valid <- function(...) {
      vals <- list(...)
      all(sapply(vals, function(x)
        ! is.null(x) && !is.na(x)))
    }
    
    numeric_inputs <- c(
      "Anzahl_total",
      "Anzahl_mit",
      "davon_Fachaerzte",
      "davon_weiterbildung",
      "Anzahl_Planstellen",
      "davon_unbesetzt",
      "Anzahl_tech",
      "Anzahl_TPlanstellen",
      "davon_Tunbesetzt",
      "Anzahl_natur",
      "Anzahl_NPlanstellen",
      "davon_Nunbesetzt",
      "Anzahl_IT",
      "Anzahl_IPlanstellen",
      "davon_Iunbesetzt"
    )
    
    # Create observers for each numeric input
    lapply(numeric_inputs, function(input_id) {
      observeEvent(input[[input_id]], {
        val <- input[[input_id]]
        if (!is.null(val) && !is.na(val) && val > 1000) {
          updateNumericInput(session, input_id, value = 1000)
          showNotification("Die maximale Anzahl beträgt 1000.", type = "warning")
        }
      }, ignoreInit = TRUE)
    })
    
    observe({
      # Validate Fachärzte + Weiterbildung <= Anzahl_mit
      if (all_present_and_valid(input$davon_Fachaerzte,
                                input$davon_weiterbildung,
                                input$Anzahl_mit)) {
        total <- input$davon_Fachaerzte + input$davon_weiterbildung
        if (total > input$Anzahl_mit) {
          updateNumericInput(session, "davon_Fachaerzte", value = 0)
          updateNumericInput(session, "davon_weiterbildung", value = 0)
          showNotification(
            "Die Summe von Fachärzten und Weiterbildung kann nicht größer sein als die Gesamtanzahl.",
            type = "warning"
          )
        }
      }
      
      # Validate unbesetzte <= Planstellen for each category
      validate_positions <- function(planstellen_id, unbesetzt_id) {
        planstellen <- input[[planstellen_id]]
        unbesetzt   <- input[[unbesetzt_id]]
        if (all_present_and_valid(planstellen, unbesetzt)) {
          if (unbesetzt > planstellen) {
            updateNumericInput(session, unbesetzt_id, value = planstellen)
            showNotification(
              "Unbesetzte Stellen können nicht größer sein als die Gesamtanzahl der Planstellen.",
              type = "warning"
            )
          }
        }
      }
      
      validate_positions("Anzahl_Planstellen", "davon_unbesetzt")
      validate_positions("Anzahl_TPlanstellen", "davon_Tunbesetzt")
      validate_positions("Anzahl_NPlanstellen", "davon_Nunbesetzt")
      validate_positions("Anzahl_IPlanstellen", "davon_Iunbesetzt")
    })
    
    # "Input durch Peer Review" part
    # it adds two text boxes "Beschreibung" and "Anmerkungen des Peers" when user press "Add More" button
    
    anmerkungen <- reactiveValues(counter = 1)
    
    observeEvent(input$addAnmerkung, {
      anmerkungen$counter <- anmerkungen$counter + 1
    })
    
    output$anmerkungenInputs <- renderUI({
      anmerkungenInputs <- lapply(1:anmerkungen$counter, function(i) {
        tagList(
          textAreaInput(
            paste0("Beschreibung", i),
            "Beschreibung",
            value = isolate(input[[paste0("Beschreibung", i)]]),
            rows = 3,
            width = "100%"
          ),
          textInput(
            paste0("Anmerkung", i),
            "Anmerkungen des Peers",
            value = isolate(input[[paste0("Anmerkung", i)]]),
            width = "100%"
          )
        )
      })
      tagList(anmerkungenInputs)
    })
    
    output$auth_output <- renderPrint({
      reactiveValuesToList(res_auth)
    })
    
    
    observe({
      # Print relevant environment information
      message("Working directory: ", getwd())
      message("Temp directory: ", tempdir())
      message("R version: ", R.version.string)
      message("rmarkdown version: ", packageVersion("rmarkdown"))
    })
    
    input_prefixes <- list(
      fuehrung      = list(score = "Freitext", text = "Freitext_text"),
      mitarbeitende = list(score = "Freitext", text = "Freitext_text"),
      patienten     = list(score = "Freitext", text = "Freitext_text"),
      einsender     = list(score = "Freitext", text = "Freitext_text"),
      qualitaet     = list(score = "Freitext", text = "Freitext_text")
    )
    
    # Compute numeric values for plot inside a reactive expression
    score_prefix <- "Freitext"
    numeric_values_fuehrung <- reactive({
      sapply(fuehrung_questions, function(q) {
        valu <- input[[paste0(score_prefix, q$id)]]
        if (is.null(valu) ||
            is.na(valu) || valu == "N/A")
          NA_real_
        else
          as.numeric(valu)
      })
    })
    
    
#------------------------- interactive Plotly plot from user-based response-----------------------------
    
    fuehrung_plot <- reactive({
      plotly::plot_ly(
        source = "offline",
        mode = 'markers+lines',
        type = 'scatterpolar',
        fill = 'toself',
        fillcolor = "rgba(1, 115, 178, 0.5)",
        line = list(color = "rgb(1, 115, 178)"),
        marker = list(
          symbol = 100,
          size = 6,
          color = 'rgb(0, 0, 0)',
          line = list(color = 'rgb(0, 0, 0)', width = 1)
        ),
        r = numeric_values_fuehrung(),
        # Use the reactive numeric_values()
        theta = c(
          "<b>1</b>",
          "<b>2</b>",
          "<b>3</b>",
          "<b>4</b>",
          "<b>5</b>",
          "<b>6</b>"
        )
      ) |>
        layout(
          title = list(text = "Führung", y = 0.99),
          polar = list(radialaxis = list(
            visible = TRUE, range = c(0, 5)
          )),
          angularaxis = list(rotation = 90, direction = "clockwise"),
          showlegend = FALSE,
          margin = list(
            l = 30,
            r = 30,
            b = 30,
            t = 30
          ),
          font = list(size = 9, color = 'black'),
          paper_bgcolor = "#c6dbef",
          plot_bgcolor = "#003B73",
          width = 400,
          height = 300
        )
    })

    output$plot_fuehrung <- renderPlotly({
      fuehrung_plot()
    })
    
# Function to prepare `ggradar` data
prepare_fuehrung_radar_data <- function(responses, questions) {
  # Aggregate responses by question_id to calculate mean scores
  aggregated_data <- responses %>%
    group_by(question_id) %>%
    summarise(response_value = mean(as.numeric(response_value), na.rm = TRUE)) %>%
    ungroup()
  
  # Convert question IDs to labels
  aggregated_data <- aggregated_data %>%
    mutate(axis_labels = sapply(question_id, function(qid) get_question_label(questions, qid)))  # Helper assumed
  
  # Transform into wide format suitable for ggradar
  radar_data <- aggregated_data %>%
    select(axis_labels, response_value) %>%
    pivot_wider(names_from = axis_labels, values_from = response_value) %>%
    mutate(group = "Average")  # Add group column required for ggradar
  
  return(as.data.frame(radar_data))
}

#--------------------------------- Mitarbeitende plot---------------------------

mit_binary_global_qnums <- c(10, 20, 24)

# ✅ Get only NON-binary question values for the plot
mit_values <- reactive({
  
  all_values <- sapply(seq_along(mit_questions), function(idx) {
    q <- mit_questions[[idx]]
    qid <- q$id
    numeric_qnum <- as.numeric(sub("^[A-Za-z]+", "", qid))
    val <- input[[paste0("Freitext", qid)]]
    
    # ✅ Skip binary questions - return NULL
    if (numeric_qnum %in% mit_binary_global_qnums) {
      return(NULL)
    }
    
    # For numeric questions
    if (is.null(val) || val == "N/A" || val == "" || is.na(val)) {
      return(NA_real_)
    }
    
    return(as.numeric(val))
  })
  
  # ✅ Remove NULL entries (binary questions)
  all_values[!sapply(all_values, is.null)]
})

# ✅ Get theta labels (question numbers) - only non-binary
mit_theta_labels <- reactive({
  non_binary_questions <- Filter(function(q) {
    numeric_qnum <- as.numeric(sub("^[A-Za-z]+", "", q$id))
    !(numeric_qnum %in% mit_binary_global_qnums)
  }, mit_questions)
  
  sapply(non_binary_questions, function(q) {
    num <- as.numeric(sub("^[A-Za-z]+", "", q$id))
    sprintf("<b>%d</b>", num)
  })
})

output$plot_mit <- renderPlotly({
  plotly::plot_ly(
    mode = 'markers+lines',
    type = 'scatterpolar',
    fill = 'toself',
    fillcolor = "rgba(222, 143, 5, 0.5)",
    line = list(color = "rgb(222, 143, 5)"),
    marker = list(
      symbol = 100,
      size = 6,
      color = 'rgb(0, 0, 0)',
      line = list(color = 'rgb(0, 0, 0)', width = 1)
    ),
    r = mit_values(),  # ✅ Only non-binary values
    theta = mit_theta_labels()  # ✅ Only non-binary question numbers
  ) %>%
    layout(
      title = list(text = "Mitarbeitende", y = 0.99),
      polar = list(radialaxis = list(
        visible = TRUE, range = c(0, 5)
      )),
      angularaxis = list(rotation = 90, direction = "clockwise"),
      showlegend = FALSE,
      margin = list(
        l = 30,
        r = 30,
        b = 30,
        t = 30
      ),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#c6dbef",
      plot_bgcolor = "#136377",
      width = 400,
      height = 300
    )
})
    
    
#--------------------------------- Patient und Angehörige-----------------------

    score_prefix_pat <- "Freitext"
    numeric_values_pat <- reactive({
      vapply(pat_questions, function(q) {
        val <- input[[paste0(score_prefix_pat, q$id)]]
        if (is.null(val) ||
            is.na(val) || val == "N/A")
          NA_real_
        else
          as.numeric(val)
      }, numeric(1))
    })
    
    
    
    output$plot_pat <- renderPlotly({
      pat_nums <- sapply(pat_questions, function(q)
        as.numeric(sub("^[A-Za-z]+", "", q$id)))
      vals <- numeric_values_pat()
      print(vals)  # Debug
      print(pat_nums) # Debug
      print(length(vals))
      print(length(pat_nums)) # Debug
      plotly::plot_ly(
        mode = 'markers+lines',
        type = 'scatterpolar',
        fill = 'toself',
        fillcolor = "rgba(2, 158, 115, 0.5)",
        line = list(color = "rgb(2, 158, 115)"),
        marker = list(
          symbol = 100,
          size = 6,
          color = 'rgb(0, 0, 0)',
          line = list(color = 'rgb(0, 0, 0)', width = 1)
        ),
        r = vals,
        theta = sprintf("<b>%d</b>", pat_nums)
      ) %>%
        layout(
          title = list(text = "Patient und Angehörige", y = 0.99),
          polar = list(radialaxis = list(
            visible = TRUE, range = c(0, 5)
          )),
          angularaxis = list(rotation = 90, direction = "clockwise"),
          showlegend = FALSE,
          margin = list(
            l = 30,
            r = 30,
            b = 30,
            t = 30
          ),
          font = list(size = 9, color = 'black'),
          paper_bgcolor = "#c6dbef",
          plot_bgcolor = "#b23a48",
          width = 400,
          height = 300
        )
    })

    
    #------------------------------------- Einsender-------------------------------
    
    # cat("ein_questions IDs:\n")
    # print(sapply(ein_questions, function(q) q$id))
    #
    numeric_values_ein <- reactive({
      vapply(ein_questions, function(q) {
        id <- paste0("Freitext", q$id)
        val <- input[[id]]
        if (is.null(val) ||
            is.na(val) || val == "N/A")
          NA_real_
        else
          as.numeric(val)
      }, numeric(1))
    })

    
    
    output$einsender_plot <- renderPlotly({
      vals <- numeric_values_ein()
      ein_nums <- sapply(ein_questions, function(q)
        as.numeric(sub("^[A-Za-z]+", "", q$id)))
      plotly::plot_ly(
        mode = 'markers+lines',
        type = 'scatterpolar',
        fill = 'toself',
        fillcolor = "rgba(204, 120, 188, 0.5)",
        line = list(color = "rgb(204, 120, 188)"),
        marker = list(
          symbol = 100,
          size = 6,
          color = 'rgb(0, 0, 0)',
          line = list(color = 'rgb(0, 0, 0)', width = 1)
        ),
        r = vals,
        theta = sprintf("<b>%d</b>", ein_nums)
      ) %>%
        layout(
          title = list(text = "Einsender und Kooperationspartner", y = 0.99),
          polar = list(radialaxis = list(
            visible = TRUE, range = c(0, 5)
          )),
          angularaxis = list(rotation = 90, direction = "clockwise"),
          showlegend = FALSE,
          margin = list(
            l = 30,
            r = 30,
            b = 30,
            t = 30
          ),
          font = list(size = 9, color = 'black'),
          paper_bgcolor = "#c6dbef",
          plot_bgcolor = "#ff6384",
          width = 400,
          height = 300
        )
    })

    #-------------------------------- Qualitätsicherung-----------------------------
    numeric_values_qual <- reactive({
      sapply(qual_questions, function(q) {
        valu <- input[[paste0("Freitext", q$id)]]
        if (is.null(valu) ||
            is.na(valu) || valu == "N/A")
          NA_real_
        else
          as.numeric(valu)
      })
    })
    
    plot_qual <- reactive({
      # Dynamically extract numbers from qual_questions IDs, e.g. "Q45" -> 45
      qual_nums <- sapply(qual_questions, function(q)
        as.numeric(sub("^[A-Za-z]+", "", q$id)))
      
      plotly::plot_ly(
        source = "offline",
        mode = 'markers+lines',
        type = 'scatterpolar',
        fill = 'toself',
        fillcolor = "rgba(202, 145, 97, 0.5)",  # ✅ Updated to match color scheme
        line = list(color = 'rgb(202, 145, 97)'),  # ✅ Updated
        marker = list(
          symbol = 100,
          size = 6,
          color = 'rgb(202, 145, 97)',  # ✅ Fixed: removed fillcolor, added color
          line = list(color = 'rgb(202, 145, 97)', width = 2)  # ✅ Removed trailing comma
        ),
        r = numeric_values_qual(),
        theta = sprintf("<b>%d</b>", qual_nums)
      ) |>
        layout(
          title = list(
            text = "Qualitätsindikatoren <br>Technische & Medizinische Validierung",
            y = 0.90,
            font = list(size = 12)
          ),
          polar = list(
            radialaxis = list(visible = TRUE, range = c(0, 5))
          ),
          angularaxis = list(rotation = 90, direction = "clockwise"),
          showlegend = FALSE,
          margin = list(
            l = 30,
            r = 30,
            b = 30,
            t = 90
          ),
          font = list(size = 9, color = 'black'),
          paper_bgcolor = "#c6dbef",
          plot_bgcolor = "#CA9161",  # ✅ Updated to match hex color
          width = 450,
          height = 400
        )
    })
    
    output$qual_plot <- renderPlotly({
      plot_qual()
    })
    #-------------------------------------------------------------------------------
    # Helper to never pass NULL (or length-not-1) to your DB
    get_input_safe <- function(id) {
      val <- input[[id]]
      if (is.null(val) ||
          (is.character(val) && nchar(trimws(val)) == 0)) {
        return(NA_character_)
      }
      as.character(val)
    }
    
    # ------- User responses visible as a table to the admin ------------
    
    # Führung
    fuehrung_responses <- reactive({
      Numeric_Response <- sapply(fuehrung_questions, function(q) {
        val <- input[[paste0("Freitext", q$id)]]
        if (is.null(val) || val == "N/A")
          "N/A"
        else
          val
      })
      Text_Response <- sapply(fuehrung_questions, function(q) {
        val <- input[[paste0("Freitext_text", q$id)]]
        if (is.null(val))
          ""
        else
          val
      })
      data.frame(
        Frage = sapply(fuehrung_questions, function(q)
          q$label),
        Antwort = Numeric_Response,
        Begründung = Text_Response,
        stringsAsFactors = FALSE
      )
    })
    
    # Mitarbeitende (with binary logic)
    mitarbeitende_responses <- reactive({
      Numeric_Response <- sapply(mit_questions, function(q) {
        qid <- q$id
        if (!is.null(q$is_binary) && q$is_binary) {
          val <- input[[paste0("Freitext", qid)]]
          if (is.null(val))
            "N/A"
          else
            val
        } else {
          val <- input[[paste0("Freitext", qid)]]
          if (is.null(val) || val == "N/A")
            "N/A"
          else
            val
        }
      })
      Text_Response <- sapply(mit_questions, function(q) {
        val <- input[[paste0("Freitext_text", q$id)]]
        if (is.null(val))
          ""
        else
          val
      })
      data.frame(
        Frage = sapply(mit_questions, function(q)
          q$label),
        Antwort = Numeric_Response,
        Begründung = Text_Response,
        stringsAsFactors = FALSE
      )
    })
    
    # Patient und Angehörige
    pat_responses <- reactive({
      Numeric_Response <- sapply(pat_questions, function(q) {
        val <- input[[paste0("Freitext", q$id)]]
        if (is.null(val) || val == "N/A")
          "N/A"
        else
          val
      })
      Text_Response <- sapply(pat_questions, function(q) {
        val <- input[[paste0("Freitext_text", q$id)]]
        if (is.null(val))
          ""
        else
          val
      })
      data.frame(
        Frage = sapply(pat_questions, function(q)
          q$label),
        Antwort = Numeric_Response,
        Begründung = Text_Response,
        stringsAsFactors = FALSE
      )
    })
    
    # Einsender und Kooperationspartner
    ein_responses <- reactive({
      Numeric_Response <- sapply(ein_questions, function(q) {
        val <- input[[paste0("Freitext", q$id)]]
        if (is.null(val) || val == "N/A")
          "N/A"
        else
          val
      })
      Text_Response <- sapply(ein_questions, function(q) {
        val <- input[[paste0("Freitext_text", q$id)]]
        if (is.null(val))
          ""
        else
          val
      })
      data.frame(
        Frage = sapply(ein_questions, function(q)
          q$label),
        Antwort = Numeric_Response,
        Begründung = Text_Response,
        stringsAsFactors = FALSE
      )
    })
    
    # Qualitätsindikatoren und Technische und Medizinische Validation
    qual_responses <- reactive({
      Numeric_Response <- sapply(qual_questions, function(q) {
        val <- input[[paste0("Freitext", q$id)]]
        if (is.null(val) || val == "N/A")
          "N/A"
        else
          val
      })
      Text_Response <- sapply(qual_questions, function(q) {
        val <- input[[paste0("Freitext_text", q$id)]]
        if (is.null(val))
          ""
        else
          val
      })
      data.frame(
        Frage = sapply(qual_questions, function(q)
          q$label),
        Antwort = Numeric_Response,
        Begründung = Text_Response,
        stringsAsFactors = FALSE
      )
    })
    
    #--------------------- Admin View: Combine all responses
    all_responses <- reactive({
      df_list <- list(
        fuehrung_responses(),
        mitarbeitende_responses(),
        pat_responses(),
        ein_responses(),
        qual_responses()
      )
      df <- dplyr::bind_rows(df_list)
      df
    })
    
    # Helper to get all responses in wide format for admin table
    get_all_responses_wide <- function(con) {
      DBI::dbGetQuery(
        con,
        "
    SELECT r.user_id, r.question_id, r.response_value
    FROM responses r
    ORDER BY r.user_id
  "
      ) %>%
        tidyr::pivot_wider(id_cols = user_id,
                           names_from = question_id,
                           values_from = response_value)
    }
    
    # Admin responses table
    output$admin_responses_table <- DT::renderDataTable({
      req(user_role() == "admin")
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      wide <- get_all_responses_wide(con)
      DT::datatable(
        wide,
        options = list(pageLength = 25, scrollX = TRUE),
        rownames = FALSE,
        filter = "top"
      )
    })
    
#-----------------User View: Only Their Own Responses-------------------------
    
    # Helper function: safely get input values (unchanged)
    get_input_safe <- function(id) {
      val <- input[[id]]
      if (is.null(val) || length(val) == 0)
        return(NA_character_)
      if (is.logical(val) &&
          length(val) == 1 && !is.na(val))
        return(as.character(val))
      if (is.character(val) && length(val) == 1)
        return(val)
      if (is.numeric(val) &&
          length(val) == 1)
        return(as.character(val))
      return(NA_character_)
    }
    
    # Helper to get all responses (wide format) for a specific user
    get_user_responses_wide <- function(con, user_id) {
      DBI::dbGetQuery(
        con,
        sprintf(
          "
    SELECT r.question_id, r.response_value, r.text_value
    FROM responses r
    WHERE r.user_id = '%s'
    ORDER BY r.question_id
  ",
          user_id
        )
      ) %>%
        tidyr::pivot_wider(
          id_cols = question_id,
          names_from = question_id,
          values_from = c(response_value, text_value)
        )
    }
    
    # Helper to get all responses (long format) for a specific user
    get_user_responses_long <- function(con, user_id) {
      DBI::dbGetQuery(
        con,
        sprintf(
          "
    SELECT r.question_id, r.response_value, r.text_value
    FROM responses r
    WHERE r.user_id = '%s'
    ORDER BY r.question_id
  ",
          user_id
        )
      )
    }
    
    output$user_responses_table <- DT::renderDataTable({
      req(user_role() != "admin")  # Only for non-admin users
      user_id <- current_user()    # Replace this with however you get the current user
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      
      user_long <- DBI::dbGetQuery(
        con,
        sprintf(
          "
    SELECT r.question_id, r.response_value, r.text_value
    FROM responses r
    WHERE r.user_id = '%s'
    ORDER BY r.question_id
  ",
          user_id
        )
      )
      
      # Map question_id to question label for display
      question_labels <- c(
        sapply(fuehrung_questions, function(q)
          setNames(q$label, q$id)),
        sapply(mit_questions, function(q)
          setNames(q$label, q$id)),
        sapply(pat_questions, function(q)
          setNames(q$label, q$id)),
        sapply(ein_questions, function(q)
          setNames(q$label, q$id)),
        sapply(qual_questions, function(q)
          setNames(q$label, q$id))
      )
      user_long$Frage <- question_labels[user_long$question_id]
      
      DT::datatable(
        user_long[, c("Frage", "response_value", "text_value")],
        colnames = c("Frage", "Antwort", "Begründung"),
        options = list(pageLength = 25, scrollX = TRUE),
        rownames = FALSE
      )
    })
    
    #------------------ response saving logic on database for users---------------
    
    observeEvent(input$submit_labinfo, {
      pid <- current_project_id()
      if (is.null(pid) || pid == "") {
        output$labinfo_submit_status <- renderUI(span("Bitte wählen Sie ein Projekt!", style="color:red;"))
        return()
      }
      
      # Helper for safe input retrieval/NA for missing
      safe_input <- function(x, default = NA_character_) {
        if (is.null(x) || length(x) == 0) default else x
      }
      safe_input_collapse <- function(x, default = NA_character_) {
        if (is.null(x) || length(x) == 0) default else paste(x, collapse = ", ")
      }
      
      # Build anmerkungen JSON
      anmerkung_list <- list()
      for (i in seq_len(anmerkungen$counter)) {
        val <- input[[paste0("Anmerkung", i)]]
        if (!is.null(val) && nzchar(trimws(val))) {
          anmerkung_list[[length(anmerkung_list) + 1]] <- val
        }
      }
      anmerkung_json <- jsonlite::toJSON(anmerkung_list, auto_unbox = TRUE)
      
      # Prepare data
      info <- list(
        project_id                   = pid,
        created_by                   = user_id(),
        name                         = safe_input(input$name),
        internetadresse              = safe_input(input$Internetadresse),
        trag                         = safe_input(input$Trag),
        zug                          = safe_input(input$Zug),
        ansprechpartner              = safe_input(input$Ansprechpartner),
        email                        = safe_input(input$Email),
        telefonnummer                = safe_input(input$Telefonnummer),
        oeffnungzeiten               = safe_input(input$Öffnungzeiten),
        versorgung                   = safe_input_collapse(input$versorgung),
        sonstiges_versorgung         = safe_input(input$sonstiges_versorgung),
        laborbereiche                = safe_input_collapse(input$laborbereiche),
        sonstiges_laborbereiche      = safe_input(input$sonstiges_laborbereiche),
        leistungsspektrum            = safe_input_collapse(input$leistungsspektrum),
        sonstiges_leistungsspektrum  = safe_input(input$sonstiges_leistungsspektrum),
        ausstattung                  = safe_input(input$ausstattung),
        hauptlieferanten             = safe_input(input$hauptlieferanten),
        question_binary_1            = safe_input(input$question_binary_1),
        farbcodesystem               = safe_input(input$farbcodesystem),
        question_binary_2            = safe_input(input$question_binary_2),
        auftragsgenerierung          = safe_input(input$auftragsgenerierung),
        praeanalytik                 = safe_input(input$präanalytik),
        befunduebermittlung          = safe_input(input$befundübermittlung),
        beratungsleistungen          = safe_input(input$beratungsleistungen),
        question_binary_3            = safe_input(input$question_binary_3),
        organigramm_filename         = if (!is.null(input$Organigramm)) input$Organigramm$name else NA,
        ergaenzung                   = safe_input(input$Ergänzung),
        anzahl_total                 = safe_input(input$Anzahl_total, NA_integer_),
        anzahl_mit                   = safe_input(input$Anzahl_mit, NA_integer_),
        davon_fachaerzte             = safe_input(input$davon_Fachaerzte, NA_integer_),
        davon_weiterbildung          = safe_input(input$davon_weiterbildung, NA_integer_),
        anzahl_planstellen           = safe_input(input$Anzahl_Planstellen, NA_integer_),
        davon_unbesetzt              = safe_input(input$davon_unbesetzt, NA_integer_),
        anzahl_tech                  = safe_input(input$Anzahl_tech, NA_integer_),
        anzahl_tplanstellen          = safe_input(input$Anzahl_TPlanstellen, NA_integer_),
        davon_tunbesetzt             = safe_input(input$davon_Tunbesetzt, NA_integer_),
        anzahl_natur                 = safe_input(input$Anzahl_natur, NA_integer_),
        anzahl_nplanstellen          = safe_input(input$Anzahl_NPlanstellen, NA_integer_),
        davon_nunbesetzt             = safe_input(input$davon_Nunbesetzt, NA_integer_),
        anzahl_it                    = safe_input(input$Anzahl_IT, NA_integer_),
        anzahl_iplanstellen          = safe_input(input$Anzahl_IPlanstellen, NA_integer_),
        davon_iunbesetzt             = safe_input(input$davon_Iunbesetzt, NA_integer_),
        beschreibung_it              = safe_input(input$Beschreibung),
        weitereinfo_personal         = safe_input(input$WeitereInfo),
        anbieterinfo                 = safe_input(input$AnbieterInfo),
        anbieterorder                = safe_input(input$AnbieterOrder),
        anbietermiddleware           = safe_input(input$AnbieterMiddleware),
        weitereit                    = safe_input(input$WeitereIT),
        angaben                      = safe_input(input$Angaben),
        laufendenjahres              = safe_input(input$laufendenJahres),
        vorjahres                    = safe_input(input$Vorjahres),
        kompetenzschwerpunkte        = safe_input(input$Kompetenzschwerpunkte),
        anmerkungen                  = anmerkung_json  # ✅ This is the 53rd parameter
      )
      
      message("Total parameters: ", length(info))  # Should be 53
      
      sql <- "
  INSERT INTO lab_info (
    project_id, created_by, name, internetadresse, trag, zug, ansprechpartner, email, telefonnummer, oeffnungzeiten,
    versorgung, sonstiges_versorgung, laborbereiche, sonstiges_laborbereiche, leistungsspektrum, sonstiges_leistungsspektrum,
    ausstattung, hauptlieferanten, question_binary_1, farbcodesystem, question_binary_2, auftragsgenerierung, praeanalytik,
    befunduebermittlung, beratungsleistungen, question_binary_3, organigramm_filename, ergaenzung,
    anzahl_total, anzahl_mit, davon_fachaerzte, davon_weiterbildung, anzahl_planstellen, davon_unbesetzt, anzahl_tech,
    anzahl_tplanstellen, davon_tunbesetzt, anzahl_natur, anzahl_nplanstellen, davon_nunbesetzt, anzahl_it, anzahl_iplanstellen,
    davon_iunbesetzt, beschreibung_it, weitereinfo_personal, anbieterinfo, anbieterorder, anbietermiddleware, weitereit,
    angaben, laufendenjahres, vorjahres, kompetenzschwerpunkte, anmerkungen
  )
  VALUES (
    $1, $2, $3, $4, $5, $6, $7, $8, $9, $10,
    $11, $12, $13, $14, $15, $16, $17, $18, $19, $20,
    $21, $22, $23, $24, $25, $26, $27, $28, $29, $30,
    $31, $32, $33, $34, $35, $36, $37, $38, $39, $40,
    $41, $42, $43, $44, $45, $46, $47, $48, $49, $50,
    $51, $52, $53, $54
  )
  ON CONFLICT (project_id) DO UPDATE SET
    name = EXCLUDED.name,
    internetadresse = EXCLUDED.internetadresse,
    trag = EXCLUDED.trag,
    zug = EXCLUDED.zug,
    ansprechpartner = EXCLUDED.ansprechpartner,
    email = EXCLUDED.email,
    telefonnummer = EXCLUDED.telefonnummer,
    oeffnungzeiten = EXCLUDED.oeffnungzeiten,
    versorgung = EXCLUDED.versorgung,
    sonstiges_versorgung = EXCLUDED.sonstiges_versorgung,
    laborbereiche = EXCLUDED.laborbereiche,
    sonstiges_laborbereiche = EXCLUDED.sonstiges_laborbereiche,
    leistungsspektrum = EXCLUDED.leistungsspektrum,
    sonstiges_leistungsspektrum = EXCLUDED.sonstiges_leistungsspektrum,
    ausstattung = EXCLUDED.ausstattung,
    hauptlieferanten = EXCLUDED.hauptlieferanten,
    question_binary_1 = EXCLUDED.question_binary_1,
    farbcodesystem = EXCLUDED.farbcodesystem,
    question_binary_2 = EXCLUDED.question_binary_2,
    auftragsgenerierung = EXCLUDED.auftragsgenerierung,
    praeanalytik = EXCLUDED.praeanalytik,
    befunduebermittlung = EXCLUDED.befunduebermittlung,
    beratungsleistungen = EXCLUDED.beratungsleistungen,
    question_binary_3 = EXCLUDED.question_binary_3,
    organigramm_filename = EXCLUDED.organigramm_filename,
    ergaenzung = EXCLUDED.ergaenzung,
    anzahl_total = EXCLUDED.anzahl_total,
    anzahl_mit = EXCLUDED.anzahl_mit,
    davon_fachaerzte = EXCLUDED.davon_fachaerzte,
    davon_weiterbildung = EXCLUDED.davon_weiterbildung,
    anzahl_planstellen = EXCLUDED.anzahl_planstellen,
    davon_unbesetzt = EXCLUDED.davon_unbesetzt,
    anzahl_tech = EXCLUDED.anzahl_tech,
    anzahl_tplanstellen = EXCLUDED.anzahl_tplanstellen,
    davon_tunbesetzt = EXCLUDED.davon_tunbesetzt,
    anzahl_natur = EXCLUDED.anzahl_natur,
    anzahl_nplanstellen = EXCLUDED.anzahl_nplanstellen,
    davon_nunbesetzt = EXCLUDED.davon_nunbesetzt,
    anzahl_it = EXCLUDED.anzahl_it,
    anzahl_iplanstellen = EXCLUDED.anzahl_iplanstellen,
    davon_iunbesetzt = EXCLUDED.davon_iunbesetzt,
    beschreibung_it = EXCLUDED.beschreibung_it,
    weitereinfo_personal = EXCLUDED.weitereinfo_personal,
    anbieterinfo = EXCLUDED.anbieterinfo,
    anbieterorder = EXCLUDED.anbieterorder,
    anbietermiddleware = EXCLUDED.anbietermiddleware,
    weitereit = EXCLUDED.weitereit,
    angaben = EXCLUDED.angaben,
    laufendenjahres = EXCLUDED.laufendenjahres,
    vorjahres = EXCLUDED.vorjahres,
    kompetenzschwerpunkte = EXCLUDED.kompetenzschwerpunkte,
    anmerkungen = EXCLUDED.anmerkungen
"
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con))
      
      tryCatch({
        DBI::dbExecute(con, sql, params = unname(info))
        output$labinfo_submit_status <- renderUI(
          span("✓ Labordaten gespeichert!", style="color:green; font-weight:bold;")
        )
      }, error = function(e) {
        message("Lab info save error: ", e$message)
        output$labinfo_submit_status <- renderUI(
          span(paste0("Fehler beim Speichern: ", e$message), style="color:red;")
        )
      })
    })

#------------------ read-only for other users------------------------------------
    
    
# helper    
    get_labinfo_project_id <- function() {
      role <- user_role()
      if (role %in% c("admin", "laborleitung")) {
        get_selected_project_id() # your normal project selector function
      } else {
        # For leading_peer, co_peer, colleague: use their assigned project
        con <- get_db_con()
        on.exit(DBI::dbDisconnect(con))
        row <- DBI::dbGetQuery(con, "SELECT project_id FROM users WHERE id = $1", params = list(user_id()))
        if (nrow(row) > 0) row$project_id[1] else NULL
      }
    }

# Helper function to check if user is not a power user or admin
is_lab_viewer <- reactive({
  user_role() %in% c("colleague", "leading_peer", "co_peer")
})

# Lab info panel: show read-only for regular users
output$labinfo_panel <- renderUI({
  req(user_id())
  project_id <- get_labinfo_project_id()
  req(!is.null(project_id) && project_id != "")
  con <- get_db_con()
  on.exit(DBI::dbDisconnect(con))
  labinfo <- DBI::dbGetQuery(con, "SELECT * FROM lab_info WHERE project_id = $1", params = list(project_id))
  if (nrow(labinfo) == 0) {
    return(h5("Keine Labordaten für dieses Peer Review hinterlegt."))
  }
  
  tagList(
    h4("Labordaten (nur lesbar)"),
    tags$dl(
      tags$dt("Name und Anschrift:"),
      tags$dd(labinfo$name),
      tags$dt("Internetadresse:"),
      tags$dd(labinfo$internetadresse),
      tags$dt("Trägerschaft:"),
      tags$dd(labinfo$trag),
      tags$dt("Zugang:"),
      tags$dd(labinfo$zug),
      tags$dt("Ansprechpartner:"),
      tags$dd(labinfo$ansprechpartner),
      tags$dt("Email:"),
      tags$dd(labinfo$email),
      tags$dt("Telefonnummer:"),
      tags$dd(labinfo$telefonnummer),
      tags$dt("Öffnungszeiten:"),
      tags$dd(labinfo$oeffnungzeiten),
      tags$dt("Versorgung:"),
      tags$dd(labinfo$versorgung),
      tags$dt("Laborbereiche:"),
      tags$dd(labinfo$laborbereiche),
      tags$dt("Leistungsspektrum:"),
      tags$dd(labinfo$leistungsspektrum),
      tags$dt("Ausstattung:"),
      tags$dd(labinfo$ausstattung),
      tags$dt("Hauptlieferanten:"),
      tags$dd(labinfo$hauptlieferanten),
      tags$dt("Werden Farbcodesysteme einheitlich genutzt?"),
      tags$dd(labinfo$question_binary_1),
      tags$dt("Welche Farbcodesysteme werden aktuell bei Probenröhrchen verwendet?"),
      tags$dd(labinfo$farbcodesystem),
      tags$dt("Nimmt Ihr Labor an Studien teil?"),
      tags$dd(labinfo$question_binary_2),
      tags$dt("Welche Verfahren zur Auftragsgenerierung bietet das Labor dem Einsender an (z. B. order entry etc.)?"),
      tags$dd(labinfo$auftragsgenerierung),
      tags$dt("Welche Hinweise zur Präanalytik und in welcher Form stellt das Labor den Einsendern bei der Auftragsgenerierung zur Verfügung?"),
      tags$dd(labinfo$praeanalytik),
      tags$dt("Wie erfolgt die Befundübermittlung?"),
      tags$dd(labinfo$befunduebermittlung),
      tags$dt("Welche Service- und Beratungsleistungen erbringen Sie?"),
      tags$dd(labinfo$beratungsleistungen),
      tags$dt("Werden Hinweise zur DRG Gruppierung ausgegeben?"),
      tags$dd(labinfo$question_binary_3),
      
      tags$dt("Personal gesamt:"),
      tags$dd(labinfo$anzahl_total),
      tags$dt("Davon mit medizinischer Ausbildung:"),
      tags$dd(labinfo$anzahl_mit),
      tags$dt("Davon Fachärzte:"),
      tags$dd(labinfo$davon_fachaerzte),
      tags$dt("Davon in Weiterbildung:"),
      tags$dd(labinfo$davon_weiterbildung),
      tags$dt("Planstellen gesamt:"),
      tags$dd(labinfo$anzahl_planstellen),
      tags$dt("Davon unbesetzt:"),
      tags$dd(labinfo$davon_unbesetzt),
      tags$dt("Technisches Personal:"),
      tags$dd(labinfo$anzahl_tech),
      tags$dt("Technische Planstellen:"),
      tags$dd(labinfo$anzahl_tplanstellen),
      tags$dt("Davon unbesetzt (technisch):"),
      tags$dd(labinfo$davon_tunbesetzt),
      tags$dt("Naturwissenschaftliches Personal:"),
      tags$dd(labinfo$anzahl_natur),
      tags$dt("Naturwissenschaftliche Planstellen:"),
      tags$dd(labinfo$anzahl_nplanstellen),
      tags$dt("Davon unbesetzt (naturw.):"),
      tags$dd(labinfo$davon_nunbesetzt),
      tags$dt("IT-Personal:"),
      tags$dd(labinfo$anzahl_it),
      tags$dt("IT-Planstellen:"),
      tags$dd(labinfo$anzahl_iplanstellen),
      tags$dt("Davon unbesetzt (IT):"),
      tags$dd(labinfo$davon_iunbesetzt),
      tags$dt("Beschreibung IT:"),
      tags$dd(labinfo$beschreibung_it),
      tags$dt("Weitere Informationen Personal:"),
      tags$dd(labinfo$weitereinfo_personal),
      
      tags$dt("Anbieter LIS:"),
      tags$dd(labinfo$anbieterinfo),
      tags$dt("Anbieter Order Entry:"),
      tags$dd(labinfo$anbieterorder),
      tags$dt("Anbieter Middleware:"),
      tags$dd(labinfo$anbietermiddleware),
      tags$dt("Weitere IT-Systeme:"),
      tags$dd(labinfo$weitereit),
      
      tags$dt("Angaben:"),
      tags$dd(labinfo$angaben),
      tags$dt("Statistik laufendes Jahr:"),
      tags$dd(labinfo$laufendenjahres),
      tags$dt("Statistik Vorjahr:"),
      tags$dd(labinfo$vorjahres),
      tags$dt("Kompetenzschwerpunkte:"),
      tags$dd(labinfo$kompetenzschwerpunkte),
      tags$dt("Organigramm:"),
      tags$dd(labinfo$organigramm_filename),
      tags$dt("Ergänzungen:"),
      tags$dd(labinfo$ergaenzung),
      tags$dt("PeerReview Beschreibungen:"),
      tags$dd(NA),
      tags$dt("PeerReview Anmerkungen:"),
      tags$dd(NA)
    )
  )
})

# List of question objects for each questionnaire
questionnaires <- list(
  fuehrung     = fuehrung_questions,
  mitarbeitende = mit_questions,
  patienten    = pat_questions,
  einsender    = ein_questions,
  qualitaet    = qual_questions
)

safe_input <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0) default else x
}
safe_input_collapse <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0) default else paste(x, collapse = ", ")
}

# Helper to get the tag from the invitation code
get_tag_from_invitation <- function() {
  tag <- redeemed_code_info()$assessment_type
  if (is.null(tag) || tag == "")
    tag <- "selbstbewertung" # fallback
  tag
}

get_project_id_from_invitation_code <- function(invitation_code) {
  con <- get_db_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  res <- DBI::dbGetQuery(
    con,
    "SELECT project_id FROM invitation_codes WHERE code = $1",
    params = list(invitation_code)
  )
  if (nrow(res) > 0 && !is.na(res$project_id[1])) {
    return(res$project_id[1])
  } else {
    return(NA_integer_)
  }
}

#-------------------------------------------------------------------------------

# ---- Helper: Get assessment_type from invitation code ----
get_assessment_type_from_invitation_code <- function(code) {
  con <- get_db_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  res <- DBI::dbGetQuery(con,
                         "SELECT assessment_type FROM invitation_codes WHERE code = $1",
                         params = list(code))
  if (nrow(res) == 1)
    return(res$assessment_type[[1]])
  return(NA_character_)
}


get_submission_tag <- function() {
  if (user_role() %in% c("laborleitung", "admin")) {
    proj_id <- current_project_id()
    if (is.null(proj_id) || length(proj_id) != 1 || is.na(proj_id) || proj_id == "") {
      "default"
    } else {
      paste0("project_", proj_id)
    }
    
  } else if (user_role() == "colleague") {
    proj_id <- current_project_id()
    if (is.null(proj_id) || length(proj_id) != 1 || is.na(proj_id) || proj_id == "") {
      "default"
    } else {
      paste0("colleague_", user_id(), "_project_", proj_id)
    }
    
  } else if (user_role() %in% c("leading_peer", "co_peer")) {
    # ✅ Use invitation code if available, otherwise create unique tag
    code <- session$userData$invitation_code
    if (!is.null(code) && code != "" && !is.na(code)) {
      as.character(code)
    } else {
      proj_id <- current_project_id()
      paste0(user_role(), "_", user_id(), "_project_", proj_id)
    }
    
  } else {
    "default"
  }
}

showCenteredSuccess <- function(msg, duration = 5000) {
  session$sendCustomMessage("centeredSuccess", list(message = msg, duration = duration))
}


#-----------------------------submit response-----------------------------------

observeEvent(input$submit_questionnaire, {
  req(user_logged_in())
  
  current_user_id <- user_id()
  submission_id <- uuid::UUIDgenerate()
  
  # ✅ Get project_id (everyone uses current_project_id)
  project_id <- current_project_id()
  
  if (is.null(project_id) || project_id == "" || is.na(project_id)) {
    showNotification("Fehler: Kein Projekt ausgewählt!", type = "error", duration = 10)
    return()
  }
  
  # ✅ Get tag (unique per user/role but same project_id)
  tag_value <- get_submission_tag()
  
  # ✅ Assessment type based on role
  assessment_type <- if (user_role() %in% c("leading_peer", "co_peer")) {
    "fremdbewertung"
  } else {
    "selbstbewertung"
  }
  
  # ✅ Group size with safe NULL/NA checks
  group_size <- 1L
  if (!is.null(input$alleine_oder_gruppe) && 
      !is.na(input$alleine_oder_gruppe) && 
      input$alleine_oder_gruppe == "gruppe" &&
      !is.null(input$gruppen_groesse) && 
      !is.na(input$gruppen_groesse)) {
    group_size <- as.integer(input$gruppen_groesse)
  }
  
  message("\n=== SUBMIT ===")
  message("User: ", current_user_id, " (", user_role(), ")")
  message("Project ID: ", project_id)
  message("Assessment: ", assessment_type)
  message("Tag: ", tag_value)
  message("Group size: ", group_size)
  
  # ✅ Collect responses
  relevant_questionnaires <- list(
    fuehrung = fuehrung_questions,
    mitarbeitende = mit_questions,
    patienten = pat_questions,
    einsender = ein_questions,
    qualitaet = qual_questions
  )
  
  responses_list <- list()
  for (qn_key in names(relevant_questionnaires)) {
    question_list <- relevant_questionnaires[[qn_key]]
    for (q in question_list) {
      qid <- q$id
      score_id <- paste0("Freitext", qid)
      comment_id <- paste0("Freitext_text", qid)
      
      score <- if (is.null(input[[score_id]]) || input[[score_id]] == "")
        NA_character_
      else
        input[[score_id]]
      
      comment <- if (is.null(input[[comment_id]]) || input[[comment_id]] == "")
        NA_character_
      else
        input[[comment_id]]
      
      if (!is.na(score) || !is.na(comment)) {
        responses_list[[length(responses_list) + 1]] <- list(
          user_id = current_user_id,
          question_id = qid,
          response_value = score,
          text_value = comment,
          tag = tag_value,
          assessment_type = assessment_type,
          submission_id = submission_id,
          project_id = project_id,  # ✅ SAME FOR EVERYONE IN THIS PROJECT
          group_size = group_size
        )
      }
    }
  }
  
  if (length(responses_list) == 0) {
    showNotification("Keine Antworten zum Speichern gefunden.", type = "warning")
    return()
  }
  
  # ✅ Save to database
  responses_df <- dplyr::bind_rows(responses_list)
  
  con <- get_db_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  tryCatch({
    message("Saving ", nrow(responses_df), " responses...")
    
    sql <- "
      INSERT INTO responses (
        user_id, 
        question_id, 
        response_value, 
        text_value, 
        tag, 
        assessment_type,
        submission_id,
        project_id,
        group_size
      ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
    "
    
    for (i in 1:nrow(responses_df)) {
      row_data <- unname(as.list(responses_df[i, c(
        "user_id", "question_id", "response_value", "text_value", 
        "tag", "assessment_type", "submission_id", "project_id", "group_size"
      )]))
      
      DBI::dbExecute(con, sql, params = row_data)
    }
    
    message("✓ Saved ", nrow(responses_df), " responses")
    message("✓ Project ID: ", project_id)
    message("✓ Submission ID: ", submission_id)
    message("=== SUCCESS ===\n")
    
    showCenteredSuccess("Fragebogen erfolgreich abgesendet!")
    
  }, error = function(e) {
    message("ERROR: ", e$message)
    showNotification(
      paste("Fehler beim Speichern:", e$message),
      type = "error",
      duration = 10
    )
  })
})

# ---- Helper: get question label from a question list and question_id ----
get_question_label <- function(question_list, question_id) {
  idx <- which(sapply(question_list, function(q)
    q$id) == question_id)
  if (length(idx) == 1)
    question_list[[idx]]$label
  else
    NA_character_
}

# ---- Helper to get responses for any questionnaire ----
get_responses <- function(question_list, tag_value) {
  con <- get_db_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  question_ids <- sapply(question_list, function(q)
    q$id)
  sql <- sprintf(
    "SELECT user_id, question_id, response_value, text_value, tag, created_at
     FROM responses
     WHERE question_id IN (%s) AND tag = $1
     ORDER BY user_id, question_id",
    paste0("'", question_ids, "'", collapse = ", ")
  )
  res <- DBI::dbGetQuery(con, sql, params = list(tag_value))
  if (nrow(res) == 0)
    return(res)
  res$question_label <- sapply(res$question_id, function(qid)
    get_question_label(question_list, qid))
  res[, c(
    "user_id",
    "question_id",
    "question_label",
    "response_value",
    "text_value",
    "tag",
    "created_at"
  )]
}

# ---- Response accessors for each questionnaire ----
fuehrung_responses <- function() {
  tag_value <- get_tag_from_invitation()
  get_responses(fuehrung_questions, tag_value)
}

mitarbeitende_responses <- function() {
  tag_value <- get_tag_from_invitation()
  get_responses(mit_questions, tag_value)
}

pat_responses <- function() {
  tag_value <- get_tag_from_invitation()
  get_responses(pat_questions, tag_value)
}

ein_responses <- function() {
  tag_value <- get_tag_from_invitation()
  get_responses(ein_questions, tag_value)
}

qual_responses <- function() {
  tag_value <- get_tag_from_invitation()
  get_responses(qual_questions, tag_value)
}

#--------------------dashboard display for admin, laborleiter, and leading peer----------------

# Helper function to fetch all projects
get_all_projects <- function(con) {
  res <- DBI::dbGetQuery(con, "SELECT id, title FROM projects ORDER BY title")
  return(res)
}


# Reactive to fetch project choices for all dashboard selectors
dashboard_project_choices <- reactive({
  con <- get_db_con()
  on.exit(DBI::dbDisconnect(con))
  projects <- DBI::dbGetQuery(con,
                              "SELECT id, title FROM projects WHERE status = 'active' ORDER BY title")
  if (nrow(projects) > 0) {
    setNames(projects$id, projects$title)
  } else {
    character(0) # Return empty character vector if no projects
  }
})


# Add this temporarily for debugging purposes
output$debug_laborleiter_project_id <- renderText({
  paste(
    "Current laborleiter_dashboard_project_selector value:",
    input$laborleiter_dashboard_project_selector
  )
})

# Observer to update the Labor Leiter's project selection dropdown
observeEvent(dashboard_project_choices(), {
  choices <- dashboard_project_choices()
  
  selected_value <- NULL # Default to NULL
  current_selection_from_input <- isolate(input$laborleiter_dashboard_project_selector)
  
  if (length(choices) > 0) {
    if (!is.null(current_selection_from_input) &&
        length(current_selection_from_input) > 0 &&
        # Added check for length zero
        current_selection_from_input %in% choices) {
      selected_value <- current_selection_from_input
    } else {
      selected_value <- choices[1] # Select the first project if no valid previous selection
    }
    
    updateSelectInput(
      session,
      "laborleiter_dashboard_project_selector",
      choices = choices,
      selected = selected_value
    )
  } else {
    # If no projects are available, set choices to a placeholder
    updateSelectInput(
      session,
      "laborleiter_dashboard_project_selector",
      choices = c("No projects available" = ""),
      selected = ""
    )
  }
}, ignoreInit = FALSE) # Run on app startup and whenever choices change


# Ensure project selection is updated when user role changes (e.g., after login)
observeEvent(user_role(), {
  role <- user_role()
  if (role %in% c("admin", "laborleitung")) {
    # Trigger dashboard_project_choices to update when role becomes admin/laborleitung
    project_data_changed(project_data_changed() + 1)
  }
}, ignoreNULL = TRUE, ignoreInit = TRUE) # ignoreInit since initial update is handled by the above observers

# Modify the laborleitung_projects reactive to also return all projects for consistency
# This reactive is used in output$laborleitung_project_selector (under 'Fragebogen bearbeiten')
labor_leiter_projects <- reactive({
  req(user_role() == "laborleitung") # Only execute if the user is a labor leiter
  con <- get_db_con()
  on.exit(DBI::dbDisconnect(con))
  # As per the user's request, labor_leiter should see all projects for selection and overview
  projects <- get_all_projects(con)
  return(projects)
})

# Observer to update current_project_id and current_project_name when admin selects a project
observeEvent(input$dashboard_project_selector,
             {
               req(user_role() == "admin", input$dashboard_project_selector)
               selected_id <- input$dashboard_project_selector
               current_project_id(selected_id)
               
               con <- get_db_con()
               on.exit(DBI::dbDisconnect(con))
               project_name_data <- DBI::dbGetQuery(con,
                                                    "SELECT title FROM projects WHERE id = $1",
                                                    params = list(selected_id))
               
               if (nrow(project_name_data) > 0) {
                 current_project_name(project_name_data$title[1])
               } else {
                 current_project_name(NULL)
               }
             },
             ignoreNULL = TRUE,
             ignoreInit = TRUE) # ignoreInit to prevent immediate trigger on startup if NULL

# Observer to update current_project_id and current_project_name when laborleitung selects a project
observeEvent(
  input$laborleiter_dashboard_project_selector,
  {
    req(user_role() == "laborleitung",
        input$laborleiter_dashboard_project_selector)
    selected_id <- input$laborleiter_dashboard_project_selector
    current_project_id(selected_id)
    
    con <- get_db_con()
    on.exit(DBI::dbDisconnect(con))
    project_name_data <- DBI::dbGetQuery(con,
                                         "SELECT title FROM projects WHERE id = $1",
                                         params = list(selected_id))
    
    if (nrow(project_name_data) > 0) {
      current_project_name(project_name_data$title[1])
    } else {
      current_project_name(NULL)
    }
  },
  ignoreNULL = TRUE,
  ignoreInit = TRUE
) # ignoreInit to prevent immediate trigger on startup if NULL

# Reactive value to store selected project data
selected_project_data <- reactiveVal(NULL)

# Helper: Check if user is admin, laborleiter, or leading peer
is_dashboard_user <- reactive({
  role <- user_role()
  ! is.null(role) &&
    role %in% c("admin", "laborleitung") ||
    is_leading_peer() # Corrected 'laborleiter' to 'labor_leiter'
})

# Display selected project title
output$selected_project_title <- renderText({
  project_data <- selected_project_data()
  if (!is.null(project_data) && nrow(project_data) > 0) {
    paste("Ausgewähltes Projekt:", project_data$title[1])
  } else {
    ""
  }
})
# At top of server:
project_title_val <- reactiveVal("")

# Update whenever selected_project_data() changes
observe({
  project_data <- selected_project_data()
  if (!is.null(project_data) && nrow(project_data) > 0) {
    project_title_val(project_data$title[1])
  } else {
    project_title_val("")
  }
})

# Update submissions table based on selected project
output$admin_submissions_table <- DT::renderDataTable({
  req(input$dashboard_project_selector)
  con <- get_db_con()
  on.exit(DBI::dbDisconnect(con))
  submissions <- DBI::dbGetQuery(
    con,
    "SELECT r.user_id, u.username, r.assessment_type, r.created_at, r.question_id, r.response_value, r.text_value
   FROM responses r
   JOIN users u ON r.user_id = u.id
   WHERE r.project_id = $1
   ORDER BY r.created_at DESC",
    params = list(input$dashboard_project_selector)
  )
  if (nrow(submissions) == 0) {
    return(data.frame(Message = "Keine Einreichungen für dieses Projekt gefunden."))
  }
  submissions$created_at <- as.POSIXct(submissions$created_at)
  return(submissions)
}, options = list(
  pageLength = 10,
  scrollX = TRUE,
  dom = 'Bfrtip'
))

# Function to get project-specific data for plots/tables
get_project_data <- function(project_id, question_ids = NULL) {
  con <- get_db_con()
  on.exit(DBI::dbDisconnect(con))
  query <- "SELECT r.*, u.username
          FROM responses r
          JOIN users u ON r.user_id = u.id
          WHERE r.project_id = $1"
  params <- list(project_id)
  if (!is.null(question_ids)) {
    in_clause <- paste(sprintf("'%s'", question_ids), collapse = ",")
    query <- paste0(query, " AND r.question_id IN (", in_clause, ")")
  }
  query <- paste(query, "ORDER BY r.question_id")
  data <- DBI::dbGetQuery(con, query, params = params)
  return(data)
}


# 1. Helper: Get currently selected project id by role
# Define a reactive expression to get the currently selected project ID
# Make sure your UI has a selectInput with the ID relevant to each role
get_selected_project_id <- reactive({
  if (user_role() == "laborleitung") {
    # Assuming 'laborleiter_dashboard_project_selector' is the ID of the dropdown for Labor Leiter
    return(input$laborleiter_dashboard_project_selector)
  } else if (user_role() == "admin") {
    # Assuming 'dashboard_project_selector' is the ID of the dropdown for Admin
    return(input$dashboard_project_selector)
  }
  return(NULL) # Return NULL if no role or selector found
})


#----------------------DRY for laborleitung-------------------------------------

make_spider_plot <- function(r,
                             theta_labels,
                             title,
                             fillcolor,
                             linecolor,
                             hover_text = NULL) {
  plotly::plot_ly(
    mode = 'markers+lines',
    type = 'scatterpolar',
    fill = 'toself',
    fillcolor = fillcolor,
    line = list(color = linecolor, width = 3),
    marker = list(
      symbol = "circle",
      size = 10,
      color = linecolor,
      line = list(color = 'white', width = 2)
    ),
    r = r,
    theta = theta_labels,
    text = hover_text,
    # <-- Enable custom hover text
    hoverinfo = if (!is.null(hover_text))
      "text"
    else
      "all"
  ) %>%
    layout(
      title = list(
        text = title,
        y = 0.95,
        font = list(size = 22, family = "Arial Black")
      ),
      polar = list(
        radialaxis = list(
          visible = TRUE,
          range = c(0, 5),
          color = "#666",
          tickfont = list(size = 16)
        ),
        angularaxis = list(
          tickfont = list(size = 16),
          color = "#666",
          rotation = 90,
          direction = "clockwise"
        )
      ),
      showlegend = FALSE,
      margin = list(
        l = 60,
        r = 60,
        b = 60,
        t = 100
      ),
      font = list(size = 16, color = '#222'),
      paper_bgcolor = "#f8f9fa",
      plot_bgcolor = "#f8f9fa",
      height = 600
    )
}


# Helper: Generate all outputs for a questionnaire topic
add_topic_outputs <- function(topic, questions, output_prefix = NULL) {
  output_name <- function(x) paste0(if (is.null(output_prefix)) topic else output_prefix, "_", x)
  
  # Compute friendly display label once; closures below can use it
  friendly_topic <- get_topic_label(topic)
  
  # 1. Mapping: question IDs to text
  question_texts <- setNames(
    sapply(questions, function(q) q$label),
    sapply(questions, function(q) q$id)
  )
  
  # 2. Stats reactive (DB-driven, project-filtered)
  stats_reactive <- reactive({
    project_data_changed()
    project_id <- current_project_id()
    req(project_id)
    
    con <- get_db_con()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    
    df <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT user_id, response_value, text_value, question_id, assessment_type
         FROM responses
         WHERE question_id IN (%s) AND project_id = $1",
        paste(sprintf("'%s'", names(question_texts)), collapse = ",")
      ),
      params = list(project_id)
    )
    
    if (nrow(df) == 0) return(NULL)
    
    df$question_text   <- question_texts[df$question_id]
    df$response_value  <- suppressWarnings(as.numeric(df$response_value))
    df                 <- df[!is.na(df$question_text), ]
    if (nrow(df) == 0) return(NULL)
    
    means <- tapply(df$response_value, df$question_text, mean, na.rm = TRUE)
    mins  <- tapply(df$response_value, df$question_text, min,  na.rm = TRUE)
    maxs  <- tapply(df$response_value, df$question_text, max,  na.rm = TRUE)
    texts <- tapply(
      df$text_value[!is.na(df$text_value) & df$text_value != ""],
      df$question_text[!is.na(df$text_value) & df$text_value != ""],
      function(x) paste(x, collapse = "\n---\n")
    )
    
    list(df = df, means = means, mins = mins, maxs = maxs, texts = texts, labels = names(means))
  })
  assign(paste0(topic, "_stats"), stats_reactive, envir = .GlobalEnv)
  
  # 3. Spider plot (radar)
  output[[output_name("spider")]] <- renderPlotly({
    stats <- stats_reactive()
    if (is.null(stats) || length(stats$means) == 0) {
      p <- plotly::plotly_empty()
      return(p %>% layout(
        annotations = list(
          text = "Keine Daten vorhanden",
          x = 0.5, y = 0.5, showarrow = FALSE,
          font = list(size = 24)
        )
      ))
    } else {
      theta_labels <- names(stats$means)  # these are question texts given how 'means' is built
      hover_text   <- paste0(theta_labels, "<br>Mittelwert: ", sprintf("%.2f", as.numeric(stats$means)))
      make_spider_plot(
        r            = as.numeric(stats$means),
        theta_labels = theta_labels,
        title        = paste0(friendly_topic, ": Durchschnitt"),
        fillcolor    = "rgba(255, 158, 74, 0.4)",
        linecolor    = "#ff9e4a",
        hover_text   = hover_text
      )
    }
  })
  
  # 4. Kürzel (Q1, ...) and question text table
  output[[output_name("table")]] <- renderTable({
    if (length(question_texts) == 0) return(data.frame(Hinweis = "Keine Daten vorhanden"))
    data.frame(Kürzel = paste0("Q", seq_along(question_texts)), Frage = unname(question_texts))
  })
  
  # 5. Stats (min, mean, max) table
  output[[output_name("stats_table")]] <- renderTable({
    stats <- stats_reactive()
    if (is.null(stats)) return(data.frame(Hinweis = "Keine Daten vorhanden"))
    all_labels <- sapply(questions, function(q) q$label)
    means <- stats$means[all_labels]
    mins  <- stats$mins[all_labels]
    maxs  <- stats$maxs[all_labels]
    data.frame(
      Frage  = all_labels,
      Min    = mins,
      Mittel = ifelse(is.na(means), NA, sprintf("%.2f", means)),
      Max    = maxs
    )
  }, rownames = FALSE)
  
  # 6. All free-text answers per question
  output[[output_name("text_table")]] <- renderTable({
    stats <- stats_reactive()
    if (is.null(stats) || length(stats$texts) == 0) return(data.frame(Hinweis = "Keine Daten vorhanden"))
    expand.grid(
      Frage   = names(stats$texts),
      Antwort = unlist(strsplit(stats$texts, "\n---\n"))
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  # 7. All responses table
  output[[output_name("responses")]] <- DT::renderDataTable({
    stats <- stats_reactive()
    if (is.null(stats) || nrow(stats$df) == 0) return(data.frame(Hinweis = "Keine Daten vorhanden"))
    df <- stats$df
    df$Antwort     <- df$response_value
    df$Begründung  <- df$text_value
    df <- df[, c("question_text", "Antwort", "Begründung")]
    names(df)[1] <- "Frage"
    df
  })
  
  # 8. Vergleich Table
  output[[output_name("vergleich_table")]] <- DT::renderDataTable({
    project_id <- current_project_id()
    req(project_id)
    con <- get_db_con()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    frage_ids <- sapply(questions, function(q) q$id)
    frage_labels <- setNames(sapply(questions, function(q) q$label), frage_ids)
    
    # Fetch all selbstbewertung responses for this project and these questions
    df <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT question_id, response_value, text_value
         FROM responses
         WHERE question_id IN (%s)
           AND assessment_type = 'selbstbewertung'
           AND project_id = $1
         ORDER BY question_id",
        paste(sprintf("'%s'", frage_ids), collapse = ",")
      ),
      params = list(project_id)
    )
    
    # If no rows, return a placeholder
    if (nrow(df) == 0) {
      return(DT::datatable(
        data.frame(Hinweis = "Keine Antworten vorhanden"),
        rownames = FALSE,
        options = list(dom = 't', paging = FALSE)
      ))
    }
    
    # For each question, get the first response (or NA)
    rows <- lapply(frage_ids, function(qid) {
      label <- frage_labels[[qid]]
      subdf <- df[df$question_id == qid, ]
      val <- if (nrow(subdf) > 0) subdf$response_value[1] else NA
      txt <- if (nrow(subdf) > 0) subdf$text_value[1] else ""
      # Radio button icons
      radio_icons <- tagList(
        lapply(1:5, function(j) {
          if (!is.na(val) && as.character(j) == as.character(val)) {
            tags$span(style="font-weight:bold;font-size:1.2em;", icon("dot-circle"), j)
          } else {
            tags$span(style="font-size:1.1em;", icon("circle"), j)
          }
        })
      )
      list(
        Frage = label,
        Selbsteinschätzung = as.character(radio_icons),
        Selbstbewertung_Freitext = if (nzchar(txt)) txt else "—"
      )
    })
    
    vergleich_df <- do.call(rbind, lapply(rows, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
    
    DT::datatable(
      vergleich_df,
      rownames = FALSE,
      escape = FALSE,
      colnames = c("Frage", "Selbsteinschätzung", "Selbstbewertung Freitext"),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        language = list(
          zeroRecords = "Keine Antworten vorhanden",
          search = "Suche:",
          lengthMenu = "Zeige _MENU_ Einträge"
        )
      )
    )
  })
}
# ---- Usage Example (in your server function) ----


add_topic_outputs("fuehrung", fuehrung_questions)
add_topic_outputs("mitarbeitende", mit_questions)
add_topic_outputs("pat", pat_questions)
add_topic_outputs("ein", ein_questions)
add_topic_outputs("qual", qual_questions)

# -------------------------------- DRY: for leading peer -----------------------
add_leading_peer_topic_outputs <- function(topic, questions) {
  output_name <- function(x) paste0("leading_peer_", topic, "_", x)
  
  # Compute the friendly title once; captured by closures below
  friendly_topic <- get_topic_label(topic)
  
  # 1. Mapping: question IDs to text
  question_texts <- setNames(
    sapply(questions, function(q) q$label),
    sapply(questions, function(q) q$id)
  )
  
  # 2. Stats reactive (DB-driven, project-filtered)
  stats_reactive <- reactive({
    project_data_changed()
    project_id <- current_project_id()
    req(project_id)
    con <- get_db_con()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    df <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT user_id, response_value, text_value, question_id, assessment_type
         FROM responses
         WHERE question_id IN (%s) AND project_id = $1",
        paste(sprintf("'%s'", names(question_texts)), collapse = ",")
      ),
      params = list(project_id)
    )
    if (nrow(df) == 0) return(NULL)
    df$question_text <- question_texts[df$question_id]
    df$response_value <- suppressWarnings(as.numeric(df$response_value))
    df <- df[!is.na(df$question_text), ]
    if (nrow(df) == 0) return(NULL)
    means <- tapply(df$response_value, df$question_text, mean, na.rm = TRUE)
    mins  <- tapply(df$response_value, df$question_text, min, na.rm = TRUE)
    maxs  <- tapply(df$response_value, df$question_text, max, na.rm = TRUE)
    texts <- tapply(
      df$text_value[!is.na(df$text_value) & df$text_value != ""],
      df$question_text[!is.na(df$text_value) & df$text_value != ""],
      function(x) paste(x, collapse = "\n---\n")
    )
    list(df = df, means = means, mins = mins, maxs = maxs, texts = texts, labels = names(means))
  })
  assign(paste0("leading_peer_", topic, "_stats"), stats_reactive, envir = .GlobalEnv)
  
  # 3. Spider plot (radar)
  output[[output_name("spider")]] <- renderPlotly({
    stats <- stats_reactive()
    if (is.null(stats) || length(stats$means) == 0) {
      p <- plotly::plotly_empty()
      p <- p %>% layout(
        annotations = list(
          text = "Keine Daten vorhanden",
          x = 0.5, y = 0.5, showarrow = FALSE,
          font = list(size = 24)
        )
      )
      return(p)
    } else {
      theta_labels <- names(stats$means)  # question_texts (labels)
      hover_text <- paste0(theta_labels, "<br>Mittelwert: ", sprintf("%.2f", as.numeric(stats$means)))
      make_spider_plot(
        r = as.numeric(stats$means),
        theta_labels = theta_labels,
        title = paste0(friendly_topic, ": Durchschnitt"),
        fillcolor = "rgba(85, 205, 252, 0.4)",
        linecolor = "#55cdfc",
        hover_text = hover_text
      )
    }
  })
  
  # 4. Stats (min, mean, max) table
  output[[output_name("stats_table")]] <- renderTable({
    stats <- stats_reactive()
    if (is.null(stats)) return(data.frame(Hinweis = "Keine Daten vorhanden"))
    all_labels <- sapply(questions, function(q) q$label)
    means <- stats$means[all_labels]
    mins  <- stats$mins[all_labels]
    maxs  <- stats$maxs[all_labels]
    data.frame(
      Frage  = all_labels,
      Min    = mins,
      Mittel = ifelse(is.na(means), NA, sprintf("%.2f", means)),
      Max    = maxs
    )
  }, rownames = FALSE)
  
  # 5. All responses table
  output[[output_name("responses")]] <- DT::renderDataTable({
    stats <- stats_reactive()
    if (is.null(stats) || nrow(stats$df) == 0)
      return(data.frame(Hinweis = "Keine Daten vorhanden"))
    df <- stats$df
    df$Antwort <- df$response_value
    df$Begründung <- df$text_value
    df <- df[, c("question_text", "Antwort", "Begründung")]
    names(df)[1] <- "Frage"
    df
  })
  
  # 6. Vergleich table
  output[[output_name("vergleich_table")]] <- DT::renderDataTable({
    project_id <- current_project_id()
    req(project_id)
    con <- get_db_con()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    frage_ids <- sapply(questions, function(q) q$id)
    frage_labels <- setNames(sapply(questions, function(q) q$label), frage_ids)
    
    # Fetch all selbstbewertung responses for this project and these questions
    df <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT question_id, response_value, text_value
         FROM responses
         WHERE question_id IN (%s)
           AND assessment_type = 'selbstbewertung'
           AND project_id = $1
         ORDER BY question_id",
        paste(sprintf("'%s'", frage_ids), collapse = ",")
      ),
      params = list(project_id)
    )
    
    # If no rows, return a placeholder
    if (nrow(df) == 0) {
      return(DT::datatable(
        data.frame(Hinweis = "Keine Antworten vorhanden"),
        rownames = FALSE,
        options = list(dom = 't', paging = FALSE)
      ))
    }
    
    # For each question, get the first response (or NA)
    rows <- lapply(frage_ids, function(qid) {
      label <- frage_labels[[qid]]
      subdf <- df[df$question_id == qid, ]
      val <- if (nrow(subdf) > 0) subdf$response_value[1] else NA
      txt <- if (nrow(subdf) > 0) subdf$text_value[1] else ""
      # Radio button icons
      radio_icons <- tagList(
        lapply(1:5, function(j) {
          if (!is.na(val) && as.character(j) == as.character(val)) {
            tags$span(style="font-weight:bold;font-size:1.2em;", icon("dot-circle"), j)
          } else {
            tags$span(style="font-size:1.1em;", icon("circle"), j)
          }
        })
      )
      list(
        Frage = label,
        Selbsteinschätzung = as.character(radio_icons),
        Selbstbewertung_Freitext = if (nzchar(txt)) txt else "—"
      )
    })
    
    vergleich_df <- do.call(rbind, lapply(rows, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
    
    DT::datatable(
      vergleich_df,
      rownames = FALSE,
      escape = FALSE,
      colnames = c("Frage", "Selbsteinschätzung", "Selbstbewertung Freitext"),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        language = list(
          zeroRecords = "Keine Antworten vorhanden",
          search = "Suche:",
          lengthMenu = "Zeige _MENU_ Einträge"
        )
      )
    )
  })
}

# Calls
add_leading_peer_topic_outputs("fuehrung", fuehrung_questions)
add_leading_peer_topic_outputs("mitarbeitende", mit_questions)
add_leading_peer_topic_outputs("pat", pat_questions)
add_leading_peer_topic_outputs("ein", ein_questions)
add_leading_peer_topic_outputs("qual", qual_questions)




# 1. UI for the report project selector
output$report_project_selector_ui <- renderUI({
  role <- user_role()
  uid <- user_id()
  if (is.null(role) || length(role) == 0) {
    return(NULL)
  }
  choices <- get_project_choices(role, uid)
  if (length(choices) == 0) {
    return(tagList(
      p("Keine Projekte verfügbar."),
      actionButton("refresh_report_projects", "Projekte aktualisieren", icon = icon("refresh"))
    ))
  }
  
  selectInput(
    "report_project_selector",
    "Projekt auswählen:",
    choices = choices,
    selected = if (length(choices) > 0) choices[[1]] else NULL
  )
})

# 2. Observer to keep the project selector up to date
observe({
  req(user_logged_in())
  role <- user_role()
  uid <- user_id()
  con <- get_db_con()
  on.exit(DBI::dbDisconnect(con))
  
  if (role == "admin") {
    projs <- DBI::dbGetQuery(con, "SELECT id, title FROM projects ORDER BY title")
  } else if (role == "laborleitung") {
    projs <- DBI::dbGetQuery(con,
                             "SELECT id, title FROM projects WHERE owner_id = $1 ORDER BY title",
                             params = list(uid)
    )
  } else {
    projs <- data.frame(id = character(0), title = character(0))
  }
  
  choices <- setNames(projs$id, projs$title)
  
  cur_proj_id <- input$report_project_selector
  sel <- if (!is.null(cur_proj_id) && cur_proj_id %in% projs$id) cur_proj_id
  else if (length(choices) > 0) choices[[1]] else NULL
  
  updateSelectInput(
    session, "report_project_selector",
    choices = choices,
    selected = sel
  )
})

# 3. Download handler for the main report
output$downloadReport <- downloadHandler(
  filename = function() {
    project_id <- input$report_project_selector
    
    if (is.null(project_id) || project_id == "" || is.na(as.integer(project_id))) {
      showNotification("Bitte wählen Sie ein gültiges Projekt aus.", type = "error")
      return(NULL)
    }
    
    con <- get_db_con()
    on.exit(DBI::dbDisconnect(con))
    proj <- DBI::dbGetQuery(con, "SELECT title FROM projects WHERE id = $1", params = list(as.integer(project_id)))
    project_name <- if (nrow(proj) > 0) proj$title[1] else "projekt"
    
    paste0("Projektabschlussbericht_", project_name, "_", current_username(), ".pdf")
  },
  content = function(file) {
    req(input$report_project_selector)
    project_id <- as.integer(input$report_project_selector)
    
    temp_env <- new.env()
    original_wd <- getwd()
    tmpdir <- tempdir()
    owd <- setwd(tmpdir)
    on.exit(setwd(owd), add = TRUE)
    file.copy(file.path(original_wd, "report.Rnw"), tmpdir, overwrite = TRUE)
    
    con <- NULL
    tryCatch({
      con <- get_db_con()
      
      labinfo <- DBI::dbGetQuery(con, "SELECT * FROM lab_info WHERE project_id = $1", params = list(project_id))
      if (nrow(labinfo) == 0) {
        showNotification("Keine Labordaten für dieses Projekt. Bericht kann nicht erstellt werden.", type = "error")
        file.create(file); cat("Keine Labordaten für dieses Projekt.", file = file); return(NULL)
      }
      
      responses <- DBI::dbGetQuery(
        con,
        "SELECT r.*, u.username, u.role FROM responses r JOIN users u ON r.user_id = u.id WHERE r.project_id = $1",
        params = list(project_id)
      )
      
      DBI::dbDisconnect(con)
      con <- NULL
      
      # Helper function to get responses for a section, robust to empty data
      get_section_df <- function(question_list, responses_df) {
        qids <- sapply(question_list, function(q) q$id)
        qlabels <- setNames(sapply(question_list, function(q) q$label), qids)
        
        sub <- responses_df[responses_df$question_id %in% qids, c("question_id", "response_value", "text_value"), drop = FALSE]
        
        if (nrow(sub) == 0) {
          return(data.frame(Frage = character(0), Antwort = character(0), Begründung = character(0), stringsAsFactors = FALSE))
        }
        
        data.frame(
          Frage = unname(qlabels[as.character(sub$question_id)]),
          Antwort = as.character(sub$response_value),
          Begründung = as.character(sub$text_value),
          stringsAsFactors = FALSE
        )
      }
      
      fuehrung_df <- get_section_df(fuehrung_questions, responses)
      mitarbeitende_df <- get_section_df(mit_questions, responses)
      pat_df <- get_section_df(pat_questions, responses)
      ein_df <- get_section_df(ein_questions, responses)
      qual_df <- get_section_df(qual_questions, responses)
      
      # Plot generation for the main report
      save_cumulative_spider_plot(fuehrung_questions, responses, file.path(tmpdir, "fuehrung_plot.png"), "Führung (Mittelwert)", "rgba(51, 160, 255, 0.4)", "#3390ff")
      save_cumulative_spider_plot(mit_questions, responses, file.path(tmpdir, "mit_plot.png"), "Mitarbeitende (Mittelwert)", "rgba(255, 158, 74, 0.4)", "#ff9e4a")
      save_cumulative_spider_plot(pat_questions, responses, file.path(tmpdir, "pat_plot.png"), "Patienten und Angehörige (Mittelwert)", "rgba(129, 199, 132, 0.4)", "#81c784")
      save_cumulative_spider_plot(ein_questions, responses, file.path(tmpdir, "einsender_plot.png"), "Einsender & Kooperationspartner (Mittelwert)", "rgba(255, 99, 132, 0.4)", "#ff6384")
      save_cumulative_spider_plot(qual_questions, responses, file.path(tmpdir, "plot_qual.png"), "Qualitätsindikatoren (Mittelwert)", "rgba(255, 206, 86, 0.4)", "#ffce56")
      
      assign("labinfo", labinfo, envir = temp_env)
      assign("fuehrung_responses", fuehrung_df, envir = temp_env)
      assign("mitarbeitende_responses", mitarbeitende_df, envir = temp_env)
      assign("pat_responses", pat_df, envir = temp_env)
      assign("ein_responses", ein_df, envir = temp_env)
      assign("qual_responses", qual_df, envir = temp_env)
      
      input_list <- as.list(labinfo[1, ])
      input_list$username <- current_username()
      proj <- DBI::dbGetQuery(get_db_con(), "SELECT title FROM projects WHERE id = $1", params = list(project_id))
      input_list$project_title <- if (nrow(proj) > 0) proj$title[1] else "Unbekanntes Projekt"
      input_list$Berufsgruppe <- input$Berufsgruppe
      
      assign("input", input_list, envir = temp_env)
      
      rmarkdown::render(
        "report.Rnw",
        output_file = file,
        output_dir = tmpdir,
        envir = temp_env
      )
      
    }, error = function(e) {
      showNotification(paste("Fehler beim Erstellen des Berichts:", e$message), type = "error", duration = NULL)
      warning("Error rendering report:", e$message)
      file.create(file)
      cat("Fehler beim Erstellen des Berichts:\n\n", e$message, file = file)
    }, finally = {
      if (!is.null(con) && DBI::dbIsValid(con)) {
        DBI::dbDisconnect(con)
      }
    })
  },
  contentType = "application/pdf"
)

# 4. Download handler for the personal report


# 1) Keep the project selector up to date
observe({
  req(user_logged_in())
  role <- user_role()
  uid <- user_id()
  con <- get_db_con()
  on.exit(DBI::dbDisconnect(con))

  if (role == "admin") {
    projs <- DBI::dbGetQuery(con, "SELECT id, title FROM projects ORDER BY title")
  } else if (role == "laborleitung") {
    projs <- DBI::dbGetQuery(con,
                             "SELECT id, title FROM projects WHERE owner_id = $1 ORDER BY title",
                             params = list(uid)
    )
  } else {
    code <- session$userData$invitation_code
    if (!is.null(code)) {
      projinfo <- DBI::dbGetQuery(con,
                                  "SELECT project_id FROM invitation_codes WHERE code = $1",
                                  params = list(code)
      )
      if (nrow(projinfo) > 0 && !is.na(projinfo$project_id[1])) {
        projs <- DBI::dbGetQuery(
          con,
          "SELECT id, title FROM projects WHERE id = $1",
          params = list(projinfo$project_id[1])
        )
      } else {
        projs <- data.frame(id = character(0), title = character(0))
      }
    } else {
      projs <- data.frame(id = character(0), title = character(0))
    }
  }

  cur_proj_id <- current_project_id()
  sel <- if (!is.null(cur_proj_id) && cur_proj_id %in% projs$id) cur_proj_id
  else if (nrow(projs) > 0) projs$id[1] else NULL

  updateSelectInput(
    session, "report_project_selector",
    choices = setNames(projs$id, projs$title),
    selected = sel
  )
})

# 2) Notify if no project is selected when pressing download
observeEvent(input$downloadReport, {
  if (is.null(input$report_project_selector) || input$report_project_selector == "") {
    showNotification("Bitte wählen Sie ein Projekt aus.", type = "error")
  }
})

# 3) Filename helper
make_report_filename <- function(type, project_id, user_name) {
  con <- get_db_con()
  on.exit(DBI::dbDisconnect(con))
  proj <- DBI::dbGetQuery(con, "SELECT title FROM projects WHERE id = $1", params = list(project_id))
  project_name <- if (nrow(proj) > 0) proj$title[1] else "projekt"
  paste0(type, "_", project_name, "_", user_name, ".pdf")
}

# 4) Build Q/A table safely
build_question_answer_table <- function(questions, responses_df) {
  question_labels <- sapply(questions, function(q) q$label)
  question_ids    <- sapply(questions, function(q) q$id)
  if (length(question_ids) == 0) {
    return(data.frame(Frage = character(0), Antwort = character(0), stringsAsFactors = FALSE))
  }
  rows <- lapply(seq_along(question_ids), function(i) {
    qid   <- question_ids[i]
    label <- question_labels[i]
    these <- responses_df[responses_df$question_id == qid, ]
    if (nrow(these) == 0) {
      return(data.frame(Frage = label, Antwort = "", stringsAsFactors = FALSE))
    }
    all_answers <- vapply(seq_len(nrow(these)), function(j) {
      rv  <- as.character(these$response_value[j])
      tv  <- as.character(these$text_value[j])
      ans <- if (!is.na(rv) && nzchar(rv) && rv != "NA") rv else ""
      txt <- if (!is.na(tv) && nzchar(tv) && tv != "NA") tv else ""
      if (nzchar(ans) && nzchar(txt)) paste0(ans, " (", txt, ")")
      else if (nzchar(ans)) ans
      else if (nzchar(txt)) txt
      else ""
    }, character(1))
    data.frame(Frage = label, Antwort = paste(all_answers, collapse = "; "), stringsAsFactors = FALSE)
  })
  ret <- do.call(rbind, rows)
  rownames(ret) <- NULL
  ret
}

# 5) Plot saving helpers (global)
save_plot_image <- function(p, file, width = 800, height = 600) {
  ok <- FALSE
  if ("save_image" %in% getNamespaceExports("plotly")) {
    try({
      plotly::save_image(p, file = file, width = width, height = height)
      ok <- TRUE
    }, silent = TRUE)
  }
  if (!ok && requireNamespace("webshot2", quietly = TRUE)) {
    tmp_html <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(p, tmp_html, selfcontained = TRUE)
    try({
      webshot2::webshot(tmp_html, file = file, vwidth = width, vheight = height)
      ok <- TRUE
    }, silent = TRUE)
  }
  if (!ok) {
    png(file, width = width, height = height)
    plot.new()
    text(0.5, 0.5, "Plot nicht verfügbar", cex = 1.2)
    dev.off()
  }
}


# Helper for extracting numbers from question labels
get_num <- function(q) as.numeric(gsub("\\D", "", q$label))

# Helper to extract all answers as data.frame (section, label, numeric, text)
gather_answers <- function() {
  all_responses <- list()
  
  for (section_key in names(topic_questions)) {
    questions <- topic_questions[[section_key]]
    
    for (q in questions) {
      qid <- q$id
      score_id <- paste0("Freitext", qid)
      text_id <- paste0("Freitext_text", qid)
      
      score <- input[[score_id]]
      text_val <- input[[text_id]]
      
      if ((is.null(score) || score == "N/A" || score == "") && 
          (is.null(text_val) || text_val == "")) {
        next
      }
      
      all_responses[[length(all_responses) + 1]] <- data.frame(
        Section = section_key,
        # ✅ Use GLOBAL number for report
        Frage = paste0(q$number, ". ", q$label),
        Antwort = ifelse(is.null(score) || score == "N/A", "", score),
        Begründung = ifelse(is.null(text_val), "", text_val),
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(all_responses) > 0) {
    dplyr::bind_rows(all_responses)
  } else {
    data.frame(
      Section = character(0),
      Frage = character(0),
      Antwort = character(0),
      Begründung = character(0),
      stringsAsFactors = FALSE
    )
  }
}

save_plot_image <- function(p, filename, width = 800, height = 600) {
  ok <- FALSE
  # Try kaleido save_image
  if (requireNamespace("plotly", quietly = TRUE)) {
    try({
      plotly::save_image(p, filename, width = width, height = height)
      ok <- file.exists(filename) && file.info(filename)$size > 0
    }, silent = TRUE)
  }
  if (!ok) {
    # Fallback placeholder
    png(filename, width = width, height = height)
    par(mar = c(0,0,0,0))
    plot.new()
    text(0.5, 0.5, "Bild konnte nicht erzeugt werden", cex = 1.2, col = "red")
    dev.off()
  }
}

# Function building a single-user spider (returns plotly object)
single_user_spider <- function(question_list, responses_df, title, fill_color, line_color) {
  qids   <- vapply(question_list, function(q) q$id, character(1))
  labels <- vapply(question_list, function(q) q$label, character(1))
  vals <- numeric(length(qids))
  for (i in seq_along(qids)) {
    idx <- which(responses_df$question_id == qids[i])
    if (length(idx) == 1) {
      v <- suppressWarnings(as.numeric(responses_df$response_value[idx]))
      vals[i] <- ifelse(is.na(v), NA, v)
    } else {
      vals[i] <- NA
    }
  }
  all_na <- all(is.na(vals))
  plotly::plot_ly(
    type = "scatterpolar",
    mode = "lines+markers",
    r = vals,
    theta = labels,
    fill = "toself",
    fillcolor = fill_color,
    line = list(color = line_color, width = 3),
    marker = list(symbol = "circle", size = 8, color = line_color,
                  line = list(color = "white", width = 2))
  ) %>%
    plotly::layout(
      title = list(
        text = if (all_na) paste0(title, " (Keine Antworten)") else title,
        y = 0.95,
        font = list(size = 16, family = "Arial Black")
      ),
      polar = list(
        radialaxis = list(visible = TRUE, range = c(0, 5)),
        angularaxis = list(rotation = 90)
      ),
      showlegend = FALSE
    )
}

# Helper to extract a section table for the current user
user_section_table <- function(question_list, responses_df) {
  qids <- vapply(question_list, function(q) q$id, character(1))
  labs <- vapply(question_list, function(q) q$label, character(1))
  sub <- responses_df[responses_df$question_id %in% qids, , drop = FALSE]
  if (nrow(sub) == 0)
    return(data.frame(Frage = labs, Antwort = NA, Begründung = NA))
  # Map label
  sub$Frage <- labs[match(sub$question_id, qids)]
  sub$Antwort <- sub$response_value
  sub$Begründung <- sub$text_value
  out <- sub[, c("Frage", "Antwort", "Begründung")]
  # Ensure all questions appear (even unanswered)
  missing <- setdiff(labs, out$Frage)
  if (length(missing) > 0) {
    out <- rbind(out,
                 data.frame(Frage = missing, Antwort = NA, Begründung = NA,
                            stringsAsFactors = FALSE))
  }
  out[match(labs, out$Frage), ]
}

#-----------------------------Helper Functions----------------------------------

# get_num <- function(q) as.numeric(gsub("\\D", "", q$label))
# 
# section_plot <- function(questions, prefix, fillcolor, paper_bg, plot_bg, width=450, height=350, title="Radarplot") {
#   vals <- sapply(questions, function(q) {
#     val <- input[[paste0("Freitext", q$id)]]
#     if (is.null(val) || val == "N/A" || is.na(val)) NA_real_ else as.numeric(val)
#   })
#   
#   theta_vec <- sapply(questions, function(q) sprintf("<b>%d</b>", get_num(q)))
#   
#   plotly::plot_ly(
#     mode = 'markers+lines', type = 'scatterpolar', fill = 'toself',
#     r = vals, theta = theta_vec, width=width, height=height,
#     fillcolor = fillcolor,
#     line = list(color = 'rgb(0,0,0)'),
#     marker = list(symbol = 100, size = 6, color = 'rgb(0,0,0)', line = list(color = 'rgb(0,0,0)', width = 1))
#   ) %>%
#     layout(
#       title = list(text = title, y = 5,font = list(size = 15, color = 'black', family = 'Arial')),
#       polar = list(radialaxis = list(visible = TRUE, range = c(0, 5), tickfont = list(size = 15, color = 'black')),
#                    angularaxis = list(
#                      tickfont = list(size = 12, color = 'black')
#                    )),
#       showlegend = FALSE,
#       margin = list(l = 30, r = 30, b = 30, t = 50),
#       font = list(size = 15, color = 'black'),
#       paper_bgcolor = paper_bg,
#       plot_bgcolor = plot_bg
#     )
# }
# 
# gather_answers <- reactive({
#   sections <- list(
#     fuehrung      = list(qs = fuehrung_questions),
#     mitarbeitende = list(qs = mit_questions),
#     patienten     = list(qs = pat_questions),
#     einsender     = list(qs = ein_questions),
#     qualitaet     = list(qs = qual_questions)
#   )
#   out <- list()
#   for (sec in names(sections)) {
#     qs <- sections[[sec]]$qs
#     for (q in qs) {
#       val <- input[[paste0("Freitext", q$id)]]
#       txt <- input[[paste0("Freitext_text", q$id)]]
#       val <- if (is.null(val) || length(val) == 0) NA_character_ else as.character(val)[1]
#       txt <- if (is.null(txt) || length(txt) == 0) NA_character_ else as.character(txt)[1]
#       out[[length(out)+1]] <- data.frame(
#         Section = sec,
#         Frage = q$label,
#         Antwort = val,
#         Begründung = txt,
#         stringsAsFactors = FALSE
#       )
#     }
#   }
#   do.call(rbind, out)
# })
# 
# output$plot_fuehrung <- renderPlot({
#   section_plot(fuehrung_questions, "F", "rgba(112, 159, 167, 0.4)", "#e3f2fd", "#f7fff7", 400, 300, "Führung")
# })
# output$plot_mit <- renderPlot({
#   section_plot(mit_questions, "M", "rgba(80, 170, 255, 0.4)", "#e3f2fd", "#e3f2fd", 400, 300, "Mitarbeitende")
# })
# output$plot_pat <- renderPlot({
#   section_plot(pat_questions, "P", "rgba(255, 175, 189, 0.4)", "#e3f2fd", "#fce4ec", 400, 300, "Patienten & Angehörige")
# })
# output$einsender_plot <- renderPlot({
#   section_plot(ein_questions, "E", "rgba(186, 189, 255, 0.5)", "#e3f2fd", "#e8eaf6", 400, 300, "Einsender & Kooperationspartner")
# })
# output$qual_plot <- renderPlot({
#   section_plot(qual_questions, "Q", "rgba(255, 231, 150, 0.4)", "#e3f2fd", "#fffbea", 400, 400, "Qualitätsindikatoren & Validierung")
# })
# 
# save_plot_image <- function(p, file, width = 800, height = 600) {
#   ok <- FALSE
#   # 1) Try Plotly API
#   if (requireNamespace("plotly", quietly = TRUE)) {
#     tryCatch({
#       
#       # PLOTLY_USERNAME = "studienlabor"
#       # PLOTLY_API_KEY = "fz7FqJQu4br1KHJXoRtg"
#       
#       plotly::save_image(p, format = "png", out_file = file, width = width, height = height)
#       ok <- TRUE
#     }, error = function(e) {
#       warning("Error using plotly_IMAGE:", e$message)
#     })
#   }
#   # 2) Fallback: webshot2
#   if (!ok && requireNamespace("webshot2", quietly = TRUE)) {
#     tmp_html <- tempfile(fileext = ".html")
#     htmlwidgets::saveWidget(p, tmp_html, selfcontained = TRUE)
#     tryCatch({
#       webshot2::webshot(tmp_html, file = file, vwidth = width, vheight = height)
#       ok <- TRUE
#     }, error = function(e) {
#       warning("Error using webshot2:", e$message)
#     })
#   }
#   # 3) Last resort: placeholder PNG
#   if (!ok) {
#     png(file, width = width, height = height)
#     plot.new()
#     text(0.5, 0.5, "Graphic could not be created.", col = "red", cex = 1.5)
#     dev.off()
#   }
# }

# ------------------FMSB Radar Chart Functions----------------------------------

make_radar_df_from_inputs_fmsb <- function(input, questions, group_name = "Aktueller Stand",
                                           binary_qnums = integer()) {
  
  # ✅ Filter out binary questions using GLOBAL number
  non_binary_questions <- if (length(binary_qnums) > 0) {
    Filter(function(q) !(q$number %in% binary_qnums), questions)
  } else {
    questions
  }
  
  # Get values
  vals <- sapply(non_binary_questions, function(q) {
    qid <- q$id
    v <- input[[paste0("Freitext", qid)]]
    
    if (is.null(v) || v == "" || v == "N/A" || is.na(v)) {
      return(0)
    }
    
    val <- suppressWarnings(as.numeric(v))
    return(ifelse(is.na(val), 0, val))
  })
  
  # ✅ Get GLOBAL question numbers for axis labels
  axis_names <- sapply(non_binary_questions, function(q) as.character(q$number))
  
  # Debug
  message("  Question IDs: ", paste(sapply(non_binary_questions, function(q) q$id), collapse = ", "))
  message("  Global numbers: ", paste(sapply(non_binary_questions, function(q) q$number), collapse = ", "))
  message("  Local numbers (axis): ", paste(axis_names, collapse = ", "))
  message("  Values: ", paste(vals, collapse = ", "))
  
  # Ensure we have data
  if (length(vals) == 0) {
    return(data.frame(
      `1` = c(5, 0, 0),
      check.names = FALSE,
      row.names = c("Max", "Min", group_name)
    ))
  }
  
  # Create max/min
  max_min <- data.frame(
    matrix(c(5, 0), nrow = 2, ncol = length(vals)),
    check.names = FALSE
  )
  colnames(max_min) <- axis_names
  rownames(max_min) <- c("Max", "Min")
  
  # Create data row
  data_row <- data.frame(
    matrix(vals, nrow = 1),
    check.names = FALSE
  )
  colnames(data_row) <- axis_names
  rownames(data_row) <- group_name
  
  # Combine
  df <- rbind(max_min, data_row)
  
  return(df)
}


make_fmsb_radar_png <- function(radar_df, title, colour = "#003B73", 
                                filename = "radar_plot.png",
                                width = 800, height = 600) {
  
  # Open PNG device
  png(filename, width = width, height = height, res = 120)
  
  # Set margins
  op <- par(mar = c(1, 1, 2, 1))
  
  tryCatch({
    
    # Check data validity
    if (is.null(radar_df) || nrow(radar_df) < 3) {
      plot.new()
      text(0.5, 0.5, "Keine Daten vorhanden", cex = 1.5)
    } else {
      
      # Create radar chart
      radarchart(
        radar_df, 
        axistype = 1,
        
        # Customize the polygon
        pcol = colour,                              # Line color
        pfcol = scales::alpha(colour, 0.5),        # Fill color
        plwd = 2,                                   # Line width
        plty = 1,                                   # Line type
        
        # Customize the grid
        cglcol = "grey",                           # Grid color
        cglty = 1,                                 # Grid line type
        cglwd = 0.8,                               # Grid line width
        
        # Customize the axis
        axislabcol = "grey",                       # Axis label color
        
        # Variable labels
        vlcex = 1.2,                               # Label size
        vlabels = colnames(radar_df),
        
        # Axis labels
        caxislabels = c("0", "1", "2", "3", "4", "5"),
        
        # Title
        title = title
      )
    }
    
  }, error = function(e) {
    plot.new()
    text(0.5, 0.5, paste("Fehler:", e$message), cex = 1.0)
  })
  
  par(op)
  dev.off()
  
  return(filename)
}


#--------------------- OVERLAY RADAR CHART BY BERUFSGRUPPE----------------------


# Function to get all responses by Berufsgruppe for a topic
get_berufsgruppe_radar_data <- function(project_id, questions, binary_qnums = integer()) {
  con <- get_db_con()
  on.exit(DBI::dbDisconnect(con))
  
  # Filter out binary questions using GLOBAL number
  non_binary_questions <- if (length(binary_qnums) > 0) {
    Filter(function(q) !(q$number %in% binary_qnums), questions)
  } else {
    questions
  }
  
  if (length(non_binary_questions) == 0) {
    return(NULL)
  }
  
  # Get question IDs
  question_ids <- sapply(non_binary_questions, function(q) q$id)
  axis_names <- sapply(non_binary_questions, function(q) as.character(q$number))
  
  # ✅ FIXED: Use assessment_type instead of tag
  query <- "
    SELECT 
      r.question_id,
      r.response_value,
      d.answers::json->>'Berufsgruppe' as berufsgruppe
    FROM responses r
    JOIN users u ON r.user_id::integer = u.id
    LEFT JOIN questionnaire_drafts d ON d.user_id::integer = u.id 
      AND d.project_id::integer = r.project_id::integer
      AND d.questionnaire_id = 'peer_review'
    WHERE r.project_id::integer = $1 
      AND r.assessment_type = 'selbstbewertung'
      AND u.role IN ('laborleitung', 'colleague')
      AND r.response_value ~ '^[0-9]+$'
  "
  
  df <- DBI::dbGetQuery(con, query, params = list(as.integer(project_id)))
  
  if (nrow(df) == 0) return(NULL)
  
  # Filter to valid Berufsgruppen and current questions
  valid_groups <- c("MTL", "Wissenschaftler(-in)", "Ärzte", "IT")
  df <- df[df$berufsgruppe %in% valid_groups & df$question_id %in% question_ids, ]
  
  if (nrow(df) == 0) return(NULL)
  
  # Convert to numeric
  df$response_value <- as.numeric(df$response_value)
  
  # Calculate mean per question per Berufsgruppe
  result <- df %>%
    group_by(berufsgruppe, question_id) %>%
    summarise(mean_score = mean(response_value, na.rm = TRUE), .groups = "drop")
  
  # Pivot wider
  wide_data <- result %>%
    tidyr::pivot_wider(names_from = question_id, values_from = mean_score)
  
  # Ensure all questions are present (fill with 0 if missing)
  for (qid in question_ids) {
    if (!(qid %in% colnames(wide_data))) {
      wide_data[[qid]] <- 0
    }
  }
  
  # Reorder columns to match question order and rename to global numbers
  wide_data <- wide_data[, c("berufsgruppe", question_ids)]
  colnames(wide_data) <- c("berufsgruppe", axis_names)
  
  return(as.data.frame(wide_data))
}



# Function to get Berufsgruppe statistics
get_berufsgruppe_stats <- function(project_id) {
  con <- get_db_con()
  on.exit(DBI::dbDisconnect(con))
  
  # ✅ FIXED: Only count laborleitung and colleagues
  query <- "
    SELECT 
      d.answers::json->>'Berufsgruppe' as berufsgruppe,
      COUNT(DISTINCT r.user_id) as anzahl_teilnehmer,
      COUNT(DISTINCT r.question_id) as anzahl_antworten
    FROM responses r
    JOIN users u ON r.user_id = u.id
    LEFT JOIN questionnaire_drafts d ON d.user_id::integer = u.id 
      AND d.project_id::integer = r.project_id::integer
      AND d.questionnaire_id = 'peer_review'
    WHERE r.project_id::integer = $1 
      AND r.assessment_type = 'selbstbewertung'
      AND u.role IN ('laborleitung', 'colleague')
    GROUP BY d.answers::json->>'Berufsgruppe'
    ORDER BY anzahl_teilnehmer DESC
  "
  
  result <- DBI::dbGetQuery(con, query, params = list(as.integer(project_id)))
  
  return(result)
}

#------------------------ Create overlaid radar chart for multiple Berufsgruppen
make_fmsb_overlay_radar <- function(project_id, questions, title, 
                                    binary_qnums = integer(),
                                    filename = "overlay_radar.png",
                                    width = 800, height = 600) {
  
  # Get weighted data by Berufsgruppe
  data <- get_berufsgruppe_radar_data_weighted(project_id, questions, binary_qnums)
  
  # 🔍 DEBUG: Show what data we're plotting
  cat("\n=== RADAR PLOT DATA DEBUG ===\n")
  cat("Title:", title, "\n")
  print("Data for radar plot:")
  print(data)
  cat("===\n\n")
  
  if (is.null(data) || nrow(data) == 0) {
    # Create empty plot
    png(filename, width = width, height = height, res = 120)
    plot.new()
    text(0.5, 0.5, "Keine Daten verfügbar", cex = 1.5, col = "#666666")
    dev.off()
    return(filename)
  }
  
  # Separate Berufsgruppe column
  berufsgruppen <- data$berufsgruppe
  numeric_data <- data[, -1, drop = FALSE]
  
  # ✅ FIX: Extract question numbers (remove prefix like "F", "M", etc.)
  question_labels <- sapply(questions, function(q) as.character(q$number))
  colnames(numeric_data) <- question_labels
  
  # Create max/min rows
  num_questions <- ncol(numeric_data)
  max_row <- rep(5, num_questions)
  min_row <- rep(0, num_questions)
  
  # Combine: max, min, then all Berufsgruppen
  radar_df <- rbind(max_row, min_row, numeric_data)
  colnames(radar_df) <- question_labels  # ✅ Use just numbers
  rownames(radar_df) <- c("Max", "Min", berufsgruppen)
  
  # Color map
  # color_map <- c(
  #   "MTL"                   = "#0072B2",  # Blue
  #   "Wissenschaftler(-in)"  = "#D55E00",  # Vermillion/Orange
  #   "Ärzte"                 = "#009E73",  # Bluish Green
  #   "IT"                    = "#CC79A7"   # Reddish Purple
  # )
  # 
  color_map <- c(
    "MTL"                  = "#1F77B4",  # Strong blue
    "Wissenschaftler(-in)" = "#FF7F0E",  # Orange
    "Ärzte"                = "#2CA02C",  # Green
    "IT"                   = "#9467BD"   # Purple
  )
  
  # color_map <- c(
  #   "MTL"                  = "#2E86AB",
  #   "Wissenschaftler(-in)" = "#F18F01",
  #   "Ärzte"                = "#00A676",
  #   "IT"                   = "#B8336A"
  # )
  
  colors <- color_map[berufsgruppen]
  fill_colors <- scales::alpha(colors, 0.18)
  
  # Open PNG device
  png(filename, width = width, height = height, res = 120)
  
  # ✅ FIX: Increase bottom margin for legend
  op <- par(mar = c(4, 1, 3, 1))  # Increased bottom margin from 1 to 4
  
  tryCatch({
    radarchart(
      radar_df, 
      axistype = 1,
      
      # Polygon styling
      pcol = colors,
      pfcol = fill_colors,
      plwd = 3,
      plty = 1,
      
      # Grid styling
      cglcol = "#CCCCCC",
      cglty = 1,
      cglwd = 1.2,
      
      # Axis styling
      axislabcol = "#666666",
      
      # ✅ Variable labels (now just numbers: 1, 2, 3...)
      vlcex = 1.2,
      vlabels = colnames(radar_df),
      
      # Axis labels
      caxislabels = c("0", "1", "2", "3", "4", "5"),
      calcex = 1.0,
      
      # No default title
      title = ""
    )
    
    # Add custom title
    title(
      main = paste0(title, " (Gewichtet)"),
      cex.main = 0.9,
      font.main = 2,
      col.main = "#333333"
    )
    
    # ✅ FIX: Legend at bottom with proper positioning
    legend(
      x = "bottom",              # Position at bottom
      legend = berufsgruppen,
      col = colors,
      lty = 1,
      lwd = 4,
      bty = "n",                 # No box
      cex = 1.1,
      horiz = TRUE,              # Horizontal layout
      title = "Berufsgruppe",
      title.col = "#333333",
      text.col = "#333333",
      xpd = TRUE,                # Allow drawing outside plot area
      inset = c(0, -0.15)        # Move down slightly
    )
    
  }, error = function(e) {
    plot.new()
    text(0.5, 0.5, paste("Fehler:", e$message), cex = 1.0, col = "#D32F2F")
  })
  
  par(op)
  dev.off()
  
  return(filename)
}
#============================================================================
# ✅ ALTERNATIVE: Using unnest for safe array handling
#============================================================================
#============================================================================
# Get Weighted Radar Data by Berufsgruppe
#============================================================================
get_berufsgruppe_radar_data_weighted <- function(project_id, questions, binary_qnums = integer()) {
  con <- get_db_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  qids <- sapply(questions, function(q) q$id)
  qids_quoted <- paste0("'", qids, "'", collapse = ", ")
  
  query <- paste0("
    SELECT 
      qd.answers::json->>'Berufsgruppe' as berufsgruppe,
      r.question_id,
      -- Weighted average: sum(score * group_size) / sum(group_size)
      SUM(
        CASE 
          WHEN r.response_value ~ '^[0-9]+$' 
          THEN CAST(r.response_value AS NUMERIC) * CAST(qd.answers::json->>'group_size' AS INTEGER)
          ELSE 0 
        END
      ) / NULLIF(SUM(CAST(qd.answers::json->>'group_size' AS INTEGER)), 0) as weighted_mean
    FROM responses r
    JOIN users u ON u.id = r.user_id
    LEFT JOIN questionnaire_drafts qd 
      ON u.id::text = qd.user_id::text 
      AND qd.project_id::text = r.project_id::text
      AND qd.questionnaire_id = 'peer_review'
    WHERE r.project_id::text = '", project_id, "'
      AND r.assessment_type = 'selbstbewertung'
      AND r.question_id IN (", qids_quoted, ")
      AND r.response_value != ''
      AND r.response_value != 'N/A'
      AND qd.answers IS NOT NULL
    GROUP BY qd.answers::json->>'Berufsgruppe', r.question_id
    ORDER BY berufsgruppe, r.question_id
  ")
  
  result <- DBI::dbGetQuery(con, query)
  
  if (nrow(result) == 0) {
    return(NULL)
  }
  
  # Pivot to wide format (Berufsgruppe as rows, questions as columns)
  wide_data <- reshape2::dcast(result, berufsgruppe ~ question_id, value.var = "weighted_mean")
  
  # 🔍 DEBUG
  cat("\n=== get_berufsgruppe_radar_data_weighted DEBUG ===\n")
  print("Query result (long format):")
  print(head(result))
  print("\nWide format for radar:")
  print(wide_data)
  cat("===\n\n")
  
  return(wide_data)
}
#------------- OVERLAY RADAR PLOTS FOR LEADING PEER DASHBOARD-------------------

output$overlay_radar_fuehrung_leading <- renderPlot({
  req(current_project_id())
  
  filename <- tempfile(fileext = ".png")
  make_fmsb_overlay_radar(
    project_id = current_project_id(),
    questions = fuehrung_questions,
    title = "Führung - nach Berufsgruppe",
    binary_qnums = integer(),
    filename = filename,
    width = 800,
    height = 600
  )
  
  img <- png::readPNG(filename)
  grid::grid.raster(img)
})

output$overlay_radar_mitarbeitende_leading <- renderPlot({
  req(current_project_id())
  
  filename <- tempfile(fileext = ".png")
  make_fmsb_overlay_radar(
    project_id = current_project_id(),
    questions = mit_questions,
    title = "Mitarbeitende - nach Berufsgruppe",
    binary_qnums = mit_binary_global_qnums,
    filename = filename
  )
  
  img <- png::readPNG(filename)
  grid::grid.raster(img)
})

output$overlay_radar_patienten_leading <- renderPlot({
  req(current_project_id())
  
  filename <- tempfile(fileext = ".png")
  make_fmsb_overlay_radar(
    project_id = current_project_id(),
    questions = pat_questions,
    title = "Patienten & Angehörige - nach Berufsgruppe",
    binary_qnums = integer(),
    filename = filename
  )
  
  img <- png::readPNG(filename)
  grid::grid.raster(img)
})

output$overlay_radar_einsender_leading <- renderPlot({
  req(current_project_id())
  
  filename <- tempfile(fileext = ".png")
  make_fmsb_overlay_radar(
    project_id = current_project_id(),
    questions = ein_questions,
    title = "Einsender & Kooperationspartner - nach Berufsgruppe",
    binary_qnums = integer(),
    filename = filename
  )
  
  img <- png::readPNG(filename)
  grid::grid.raster(img)
})

output$overlay_radar_qualitaet_leading <- renderPlot({
  req(current_project_id())
  
  filename <- tempfile(fileext = ".png")
  make_fmsb_overlay_radar(
    project_id = current_project_id(),
    questions = qual_questions,
    title = "Qualitätsindikatoren & Validierung - nach Berufsgruppe",
    binary_qnums = integer(),
    filename = filename
  )
  
  img <- png::readPNG(filename)
  grid::grid.raster(img)
})

#------------------- Download Handler (Selbstbewertung Bericht)-----------------

output$downloadPersonalReport_reports <- downloadHandler(
  filename = function() paste0("selbstbewertung_", format(Sys.Date(), "%Y%m%d"), ".pdf"),
  content  = function(file) {
    
    message("=== Download started ===")
    
    # Use .Rmd file
    template_file <- "selbstbewertung_report.Rmd"
    
    if (!file.exists(template_file)) {
      showNotification("Template nicht gefunden.", type = "error", duration = 10)
      writeLines("Template nicht verfügbar.", file)
      return(NULL)
    }
    
    tryCatch({
      
      message("Step 1: Getting template path")
      template_path <- normalizePath(template_file, mustWork = TRUE)
      
      message("Step 2: Changing to temp directory")
      tmpdir <- tempdir()
      owd <- setwd(tmpdir)
      on.exit(setwd(owd), add = TRUE)
      
      message("Step 3: Copying template to temp dir")
      file.copy(template_path, file.path(tmpdir, template_file), overwrite = TRUE)
      
      message("Step 4: Gathering user responses")
      user_responses <- tryCatch({
        gather_answers()
      }, error = function(e) {
        message("ERROR in gather_answers: ", e$message)
        data.frame(
          Section = character(0),
          Frage = character(0),
          Antwort = character(0),
          Begründung = character(0),
          stringsAsFactors = FALSE
        )
      })
      
      # Debug output
      message("Gathered ", nrow(user_responses), " responses")
      if (nrow(user_responses) > 0) {
        message("Columns: ", paste(names(user_responses), collapse = ", "))
        message("Sections: ", paste(unique(user_responses$Section), collapse = ", "))
      } else {
        message("WARNING: No responses gathered!")
      }
      
      # Save user responses to CSV in temp dir
      message("Step 5: Saving user responses to CSV")
      write.csv(
        user_responses,
        file = file.path(tmpdir, "user_responses.csv"),
        row.names = FALSE,
        fileEncoding = "UTF-8"
      )
      
      message("Step 6: Creating FMSB radar plots in temp dir")
      mit_binary_global_qnums <- c(10, 20, 24)
      
      # ✅ Updated plot configs for FMSB
      plot_configs <- list(
        plot_fuehrung = list(
          data = make_radar_df_from_inputs_fmsb(input, fuehrung_questions),
          title = "Führung",
          colour = "#003B73"
        ),
        plot_mit = list(
          data = make_radar_df_from_inputs_fmsb(input, mit_questions, binary_qnums = mit_binary_global_qnums),
          title = "Mitarbeitende",
          colour = "#136377"
        ),
        plot_pat = list(
          data = make_radar_df_from_inputs_fmsb(input, pat_questions),
          title = "Patienten & Angehörige",
          colour = "#b23a48"
        ),
        plot_ein = list(
          data = make_radar_df_from_inputs_fmsb(input, ein_questions),
          title = "Einsender & Kooperationspartner",
          colour = "#4b61d1"
        ),
        plot_qual = list(
          data = make_radar_df_from_inputs_fmsb(input, qual_questions),
          title = "Qualitätsindikatoren & Validierung",
          colour = "#c58f00"
        )
      )
      
      plot_files <- c(
        "plot_fuehrung.png",
        "plot_mit.png",
        "plot_pat.png",
        "plot_ein.png",
        "plot_qual.png"
      )
      
      # ✅ Create FMSB plots
      for (i in seq_along(plot_configs)) {
        config <- plot_configs[[i]]
        plot_file <- file.path(tmpdir, plot_files[i])
        
        message(paste("  Creating FMSB plot:", config$title))
        
        tryCatch({
          
          # ✅ Use fmsb radar chart function
          make_fmsb_radar_png(
            radar_df = config$data,
            title = config$title,
            colour = config$colour,
            filename = plot_file,
            width = 800,
            height = 600
          )
          
          message(paste("    Plot saved:", plot_file))
          
        }, error = function(e) {
          message(paste("    ERROR:", e$message))
          
          # ✅ Create empty/placeholder plot on error
          png(plot_file, width = 800, height = 600)
          plot.new()
          text(0.5, 0.5, paste("Fehler beim Erstellen des Plots:\n", config$title), cex = 1.5)
          dev.off()
        })
      }
      
      # Get user's Berufsgruppe and alleine_oder_gruppe for the report
      berufsgruppe <- if (!is.null(input$Berufsgruppe) && input$Berufsgruppe != "") {
        as.character(input$Berufsgruppe)
      } else {
        "Nicht angegeben"
      }
      
      alleine_oder_gruppe <- if (!is.null(input$alleine_oder_gruppe) && input$alleine_oder_gruppe != "") {
        as.character(input$alleine_oder_gruppe)
      } else {
        "Nicht angegeben"
      }
      
      # ✅ Get group size
      gruppen_groesse <- if (!is.null(input$alleine_oder_gruppe) && 
                             input$alleine_oder_gruppe == "gruppe" && 
                             !is.null(input$gruppen_groesse)) {
        as.character(input$gruppen_groesse)
      } else {
        "1"  # Default to 1 if filling alone
      }
      
      
      message("Step 7: Rendering PDF")
      message("  Berufsgruppe: ", berufsgruppe)
      message("  Alleine oder Gruppe: ", alleine_oder_gruppe)
      
      # Render PDF using rmarkdown
      output_file <- rmarkdown::render(
        input = file.path(tmpdir, template_file),
        output_format = "pdf_document",
        output_file = "report.pdf",
        output_dir = tmpdir,
        params = list(
          berufsgruppe = berufsgruppe,
          alleine_oder_gruppe = alleine_oder_gruppe
        ),
        envir = new.env(),
        quiet = FALSE
      )
      
      message("Step 8: Moving PDF to destination")
      file.copy(output_file, file, overwrite = TRUE)
      
      message("=== Download completed successfully ===")
      
      showNotification(
        "Bericht erfolgreich erstellt!",
        type = "message",
        duration = 5
      )
      
    }, error = function(e) {
      message("=== ERROR ===")
      message(paste("  Message:", e$message))
      message(paste("  Traceback:", paste(sys.calls(), collapse = "\n")))
      
      showNotification(
        paste("Fehler beim Erstellen des Berichts:", e$message),
        type = "error",
        duration = 15
      )
      
      writeLines(paste("Fehler:", e$message), file)
    })
  }
)
#---------------------------------download handler------------------------------
#--------------------Zusammenfassung Selbstbewertung + labinfo------------------

output$downloadZusammenfassung <- downloadHandler(
  filename = function() {
    paste0("Zusammenfassung_", format(Sys.Date(), "%Y%m%d"), ".pdf")
  },
  
  content = function(file) {
    
    message("\n###############################################")
    message("### ZUSAMMENFASSUNG PDF GENERATION ###")
    message("###############################################\n")
    
    # Get project ID
    project_id <- isolate(current_project_id())
    
    # Validate
    if (is.null(project_id) || project_id == "" || is.na(project_id)) {
      showNotification(
        "Bitte wählen Sie ein Projekt aus der Dropdown-Liste aus.",
        type = "error",
        duration = 10
      )
      writeLines("FEHLER: Kein Projekt ausgewählt.", file)
      return(NULL)
    }
    
    message("✓ Project ID: ", project_id)
    
    # Check for template
    template_file <- "zusammenfassung_report.Rmd"
    
    if (!file.exists(template_file)) {
      message("ERROR: Template not found: ", template_file)
      message("Working directory: ", getwd())
      message("Files: ", paste(list.files(pattern = "\\.Rmd$"), collapse = ", "))
      
      showNotification(
        paste0("Template nicht gefunden: ", template_file),
        type = "error",
        duration = 10
      )
      writeLines("FEHLER: Template nicht gefunden.", file)
      return(NULL)
    }
    
    message("✓ Template found")
    
    # Show progress
    showNotification(
      "PDF wird erstellt... Bitte warten.",
      type = "message",
      duration = NULL,
      id = "pdf_progress"
    )
    
    tryCatch({
      
      # Setup temp directory
      message("\nStep 1: Setup temp directory")
      tmpdir <- tempdir()
      original_wd <- getwd()
      setwd(tmpdir)
      on.exit({
        setwd(original_wd)
      }, add = TRUE)
      
      message("  Working dir: ", getwd())
      
      # Copy template
      message("\nStep 2: Copy template")
      template_path <- file.path(original_wd, template_file)
      file.copy(template_path, file.path(tmpdir, template_file), overwrite = TRUE)
      message("  ✓ Template copied")
      
      # Database connection
      message("\nStep 3: Database connection")
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      
      # Get project info
      project_query <- DBI::dbGetQuery(
        con,
        "SELECT title FROM projects WHERE id = $1",
        params = list(project_id)
      )
      project_title <- if (nrow(project_query) > 0) project_query$title[1] else "Projekt"
      message("  ✓ Project: ", project_title)
      
      # ===== LAB INFO =====
      message("\nStep 4: Lab info")
      labinfo_raw <- DBI::dbGetQuery(
        con,
        "SELECT * FROM lab_info WHERE project_id = $1",
        params = list(project_id)
      )
      
      if (nrow(labinfo_raw) == 0) {
        message("  ⚠ No lab info - placeholder")
        labinfo <- data.frame(
          lab_name = "Nicht verfügbar",
          street = "", plz = "", city = "", country = "",
          phone = "", email = "", website = "",
          lab_director = "", deputy = "",
          num_employees = "", num_doctors = "", num_mtl = "",
          accreditation = "", certification = "",
          stringsAsFactors = FALSE
        )
      } else {
        # Helper for safe extraction
        safe_char <- function(x) if (is.null(x) || is.na(x)) "" else as.character(x)
        
        labinfo <- data.frame(
          lab_name = safe_char(labinfo_raw$name[1]),
          street = safe_char(labinfo_raw$trag[1]),
          plz = "",
          city = "",
          country = "",
          phone = safe_char(labinfo_raw$telefonnummer[1]),
          email = safe_char(labinfo_raw$email[1]),
          website = safe_char(labinfo_raw$internetadresse[1]),
          lab_director = safe_char(labinfo_raw$ansprechpartner[1]),
          deputy = "",
          num_employees = safe_char(labinfo_raw$anzahl_total[1]),
          num_doctors = safe_char(labinfo_raw$anzahl_mit[1]),
          num_mtl = safe_char(labinfo_raw$anzahl_tech[1]),
          accreditation = "",
          certification = "",
          stringsAsFactors = FALSE
        )
      }
      
      write.csv(labinfo, "labinfo.csv", row.names = FALSE, fileEncoding = "UTF-8")
      message("  ✓ labinfo.csv")
      
      # ===== RESPONSES =====
      message("\nStep 5: Responses")
      
      get_topic_responses <- function(prefix, questions, topic_name) {
        df <- DBI::dbGetQuery(con, sprintf(
          "SELECT u.username, r.question_id, r.response_value, r.text_value
           FROM responses r
           JOIN users u ON u.id = r.user_id
           WHERE r.assessment_type = 'selbstbewertung'
             AND r.project_id = $1 AND r.question_id LIKE '%s%%'
           ORDER BY r.question_id, u.username", prefix
        ), params = list(project_id))
        
        message("  ", topic_name, ": ", nrow(df), " rows")
        
        if (nrow(df) > 0) {
          df$question <- sapply(df$question_id, function(qid) {
            q <- Find(function(x) x$id == qid, questions)
            if (!is.null(q)) q$label else qid
          })
          df <- df[, c("username", "question", "response_value", "text_value")]
          colnames(df) <- c("Nutzer", "Frage", "Bewertung", "Begründung")
        } else {
          df <- data.frame(
            Nutzer = character(0), Frage = character(0),
            Bewertung = character(0), Begründung = character(0),
            stringsAsFactors = FALSE
          )
        }
        df
      }
      
      write.csv(get_topic_responses("F", fuehrung_questions, "Führung"), 
                "fuehrung_responses.csv", row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(get_topic_responses("M", mit_questions, "Mitarbeitende"), 
                "mitarbeitende_responses.csv", row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(get_topic_responses("P", pat_questions, "Patienten"), 
                "pat_responses.csv", row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(get_topic_responses("E", ein_questions, "Einsender"), 
                "ein_responses.csv", row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(get_topic_responses("Q", qual_questions, "Qualität"), 
                "qual_responses.csv", row.names = FALSE, fileEncoding = "UTF-8")
      
      message("  ✓ All response CSVs")
      
      # ===== STATISTICS =====
      message("\nStep 6: Statistics")
      
      get_stats <- function(topic_key) {
        stats <- get_topic_weighted_stats(project_id, topic_key)
        if (!is.null(stats) && nrow(stats) > 0) {
          # Ensure we have the right columns
          if (ncol(stats) >= 4) {
            stats <- stats[, 1:4]
            names(stats) <- c("Frage", "Min", "Mittelwert", "Max")
          }
        } else {
          stats <- data.frame(
            Frage = character(0), Min = numeric(0),
            Mittelwert = numeric(0), Max = numeric(0),
            stringsAsFactors = FALSE
          )
        }
        stats
      }
      
      write.csv(get_stats("fuehrung"), "fuehrung_stats.csv", row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(get_stats("mitarbeitende"), "mitarbeitende_stats.csv", row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(get_stats("patienten"), "pat_stats.csv", row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(get_stats("einsender"), "ein_stats.csv", row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(get_stats("qualitaet"), "qual_stats.csv", row.names = FALSE, fileEncoding = "UTF-8")
      
      message("  ✓ All stats CSVs")
      
      # ===== PLOTS =====
      message("\nStep 7: Plots")
      
      mit_binary_global_qnums <- c(10, 20, 24)
      
      create_plot <- function(name, questions, title, colour, binary_qnums = integer(0)) {
        plot_file <- paste0("plot_", name, ".png")
        
        tryCatch({
          # Filter out binary questions
          questions_filtered <- if (length(binary_qnums) > 0) {
            Filter(function(q) !(q$number %in% binary_qnums), questions)
          } else {
            questions
          }
          
          question_ids <- sapply(questions_filtered, function(q) q$id)
          
          message("  Creating plot: ", title)
          message("    Questions: ", length(question_ids))
          
          if (length(question_ids) == 0) {
            message("    ⚠ No questions, creating empty plot")
            png(plot_file, width = 800, height = 600)
            plot.new()
            text(0.5, 0.5, "Keine Fragen", cex = 1.5)
            dev.off()
            return()
          }
          
          # Get responses from database
          responses <- DBI::dbGetQuery(con, sprintf(
            "SELECT question_id, AVG(response_value::numeric) as avg_value
       FROM responses
       WHERE project_id = $1 AND assessment_type = 'selbstbewertung'
         AND question_id IN (%s) 
         AND response_value IS NOT NULL 
         AND response_value != ''
       GROUP BY question_id",
            paste0("'", question_ids, "'", collapse = ", ")
          ), params = list(project_id))
          
          message("    Response rows: ", nrow(responses))
          
          if (nrow(responses) == 0) {
            message("    ⚠ No responses, creating empty plot")
            png(plot_file, width = 800, height = 600)
            plot.new()
            text(0.5, 0.5, "Keine Daten", cex = 1.5)
            dev.off()
            return()
          }
          
          # Create values vector - match order of questions_filtered
          values <- sapply(questions_filtered, function(q) {
            row <- responses[responses$question_id == q$id, ]
            if (nrow(row) > 0) row$avg_value else 0
          })
          
          message("    Values length: ", length(values))
          message("    Values: ", paste(round(values, 2), collapse = ", "))
          
          # ✅ FIX: Create radar data frame with correct dimensions
          radar_df <- as.data.frame(rbind(
            rep(5, length(values)),   # Max
            rep(0, length(values)),   # Min
            values                     # Actual values
          ))
          
          # ✅ FIX: Set column names to match number of columns
          colnames(radar_df) <- paste0("Q", seq_len(length(values)))
          
          message("    Radar df dimensions: ", nrow(radar_df), " x ", ncol(radar_df))
          message("    Column names: ", paste(colnames(radar_df), collapse = ", "))
          
          # Create plot
          make_fmsb_radar_png(radar_df, title, colour, plot_file, 800, 600)
          message("  ✓ ", title)
          
        }, error = function(e) {
          message("  ✗ ", title, " ERROR: ", e$message)
          message("  Stack trace:")
          print(sys.calls())
          
          # Create error plot
          png(plot_file, width = 800, height = 600)
          plot.new()
          text(0.5, 0.5, "Fehler", cex = 1.5)
          dev.off()
        })
      }
      
      create_plot("fuehrung", fuehrung_questions, "Führung", "#003B73")
      create_plot("mit", mit_questions, "Mitarbeitende", "#136377", mit_binary_global_qnums)
      create_plot("pat", pat_questions, "Patienten", "#b23a48")
      create_plot("ein", ein_questions, "Einsender", "#4b61d1")
      create_plot("qual", qual_questions, "Qualität", "#c58f00")
      
      # ===== RENDER PDF =====
      message("\nStep 8: Render PDF")
      
      output_file <- rmarkdown::render(
        input = template_file,
        output_format = rmarkdown::pdf_document(
          toc = TRUE,
          toc_depth = 2,
          number_sections = FALSE,
          keep_tex = FALSE,
          latex_engine = "pdflatex"
        ),
        output_file = "report.pdf",
        output_dir = tmpdir,
        params = list(
          project_title = project_title,
          project_id = project_id
        ),
        envir = new.env(),
        quiet = FALSE,
        encoding = "UTF-8"
      )
      
      message("  ✓ PDF: ", file.size(output_file), " bytes")
      
      # Copy to destination
      message("\nStep 9: Copy PDF")
      file.copy(output_file, file, overwrite = TRUE)
      
      removeNotification("pdf_progress")
      showNotification(
        "PDF erfolgreich erstellt!",
        type = "message",
        duration = 5
      )
      
      message("\n### SUCCESS ###\n")
      
    }, error = function(e) {
      message("\n### ERROR ###")
      message("Message: ", e$message)
      message("\nCall stack:")
      print(sys.calls())
      
      removeNotification("pdf_progress")
      showNotification(
        paste("Fehler:", e$message),
        type = "error",
        duration = 15
      )
      
      writeLines(paste("Fehler beim Erstellen des Berichts:\n\n", e$message), file)
    })
  },
  
  contentType = "application/pdf"
)

#--------------Downloadhandler for leading peer---------------------------

# Helper function
get_fremdbewertung_section <- function(questions) {
  data.frame(
    Frage     = sapply(questions, function(q) q$label),
    Bewertung = sapply(questions, function(q) {
      val <- input[[paste0("FremdbewertungNum_", q$id)]]
      if (is.null(val) || length(val) == 0) "" else as.character(val)
    }),
    Kommentar = sapply(questions, function(q) {
      val <- input[[paste0("FremdbewertungText_", q$id)]]
      if (is.null(val) || length(val) == 0) "" else as.character(val)
    }),
    stringsAsFactors = FALSE
  )
}

# Download Handler for Fremdbewertung (single CSV, all topics)
output$downloadFremdbewertung <- downloadHandler(
  filename = function() {
    paste0("Mein_Fremdbewertung_", Sys.Date(), ".pdf")
  },
  content = function(file) {
    
    template_path <- normalizePath("fremdbewertung_report.Rnw")
    owd <- setwd(tempdir()); on.exit(setwd(owd))
    
    # 1. Collect ALL Fremdbewertung answers into one data frame
    gather_fremdbewertung_answers <- function() {
      sections <- list(
        fuehrung      = fuehrung_questions,
        mitarbeitende = mit_questions,
        patienten     = pat_questions,
        einsender     = ein_questions,
        qualitaet     = qual_questions
      )
      out <- list()
      for (sec_name in names(sections)) {
        questions <- sections[[sec_name]]
        for (q in questions) {
          num_ans <- input[[paste0("FremdbewertungNum_", q$id)]]
          txt_ans <- input[[paste0("FremdbewertungText_", q$id)]]
          out[[length(out) + 1]] <- data.frame(
            Section     = sec_name,
            Frage       = q$label,
            Antwort     = if (is.null(num_ans) || num_ans == "") NA else as.character(num_ans),
            Begründung  = if (is.null(txt_ans) || txt_ans == "") NA else as.character(txt_ans),
            stringsAsFactors = FALSE
          )
        }
      }
      do.call(rbind, out)
    }
    
    # 2. Save the answers as CSV (for the LaTeX template)
    fremd_answers <- gather_fremdbewertung_answers()
    write.csv(fremd_answers, file = "user_fremdbewertung.csv", row.names = FALSE)
    
  
    # 4. Copy the LaTeX template to tempdir
    if (!file.exists(template_path)) {
      showNotification("Template file 'fremdbewertung_report.Rnw' does not exist.", type = "error")
      file.create(file); cat("Template missing.", file = file); return()
    }
    file.copy(template_path, "fremdbewertung_report.Rmd", overwrite = TRUE)
    
    # 5. Knit to PDF
    out <- tryCatch(knitr::knit2pdf("fremdbewertung_report.Rmd", clean = TRUE),
                    error = function(e) {
                      showNotification(paste("PDF konnte nicht erstellt werden: ", e$message), type = "error")
                      file.create(file); cat("PDF-Fehler: ", e$message, file = file)
                      return(NULL)
                    }
    )
    if (!is.null(out)) file.rename(out, file)
  }
)


#------- FREMDBEWERTUNG SUBMISSION OBSERVERS (For Leading Peer & Co-Peer)-------


# Helper function to submit fremdbewertung for a topic
submit_fremdbewertung_topic <- function(topic_prefix, questions_list, topic_name, status_output) {
  req(user_logged_in())
  req(user_role() %in% c("leading_peer", "co_peer"))
  
  current_user_id <- user_id()
  project_id <- current_project_id()
  submission_id <- uuid::UUIDgenerate()
  
  message("\n=== SUBMIT FREMDBEWERTUNG: ", topic_name, " ===")
  message("User: ", current_user_id, " (", user_role(), ")")
  message("Project: ", project_id)
  
  if (is.null(project_id) || project_id == "" || is.na(project_id)) {
    showNotification("Fehler: Kein Projekt ausgewählt!", type = "error")
    return()
  }
  
  # Get tag
  tag_value <- get_submission_tag()
  
  # Collect responses
  responses_list <- list()
  
  for (q in questions_list) {
    qid <- q$id
    score_id <- paste0("FremdbewertungNum_", qid)
    text_id <- paste0("FremdbewertungText_", qid)
    
    score <- input[[score_id]]
    text <- input[[text_id]]
    
    if (!is.null(score) && score != "") {
      responses_list[[length(responses_list) + 1]] <- list(
        user_id = current_user_id,
        question_id = qid,
        response_value = as.character(score),
        text_value = if (!is.null(text) && text != "") text else NA_character_,
        tag = tag_value,
        assessment_type = "fremdbewertung",
        submission_id = submission_id,
        project_id = project_id,
        group_size = 1L
      )
    }
  }
  
  if (length(responses_list) == 0) {
    showNotification("Keine Antworten zum Speichern gefunden.", type = "warning")
    output[[status_output]] <- renderUI(
      div(style = "color:orange;margin-top:12px;", "⚠ Keine Antworten gefunden.")
    )
    return()
  }
  
  message("Collected ", length(responses_list), " responses")
  
  # Save to database
  responses_df <- dplyr::bind_rows(responses_list)
  
  con <- get_db_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  tryCatch({
    sql <- "
      INSERT INTO responses (
        user_id, question_id, response_value, text_value, 
        tag, assessment_type, submission_id, project_id, group_size
      ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
    "
    
    for (i in 1:nrow(responses_df)) {
      row_data <- unname(as.list(responses_df[i, c(
        "user_id", "question_id", "response_value", "text_value",
        "tag", "assessment_type", "submission_id", "project_id", "group_size"
      )]))
      
      DBI::dbExecute(con, sql, params = row_data)
    }
    
    message("✓ Saved ", nrow(responses_df), " responses")
    message("✓ Submission ID: ", submission_id)
    message("=== SUCCESS ===\n")
    
    showNotification(
      paste0(topic_name, " Fremdbewertung erfolgreich abgesendet!"),
      type = "message",
      duration = 5
    )
    
    output[[status_output]] <- renderUI(
      div(style = "color:green;margin-top:12px;font-weight:bold;",
          icon("check-circle"), " Erfolgreich abgesendet!")
    )
    
  }, error = function(e) {
    message("ERROR: ", e$message)
    showNotification(
      paste("Fehler beim Speichern:", e$message),
      type = "error",
      duration = 10
    )
    
    output[[status_output]] <- renderUI(
      div(style = "color:red;margin-top:12px;", "✗ Fehler beim Speichern!")
    )
  })
}

# Führung
observeEvent(input$submit_peer_fremdbewertung_fuehrung, {
  submit_fremdbewertung_topic(
    "F", 
    fuehrung_questions, 
    "Führung",
    "peer_fremdbewertung_submit_status_fuehrung"
  )
})

# Mitarbeitende
observeEvent(input$submit_peer_fremdbewertung_mitarbeitende, {
  submit_fremdbewertung_topic(
    "M", 
    mit_questions, 
    "Mitarbeitende",
    "peer_fremdbewertung_submit_status_mitarbeitende"
  )
})

# Patienten
observeEvent(input$submit_peer_fremdbewertung_pat, {
  submit_fremdbewertung_topic(
    "P", 
    pat_questions, 
    "Patient und Angehörige",
    "peer_fremdbewertung_submit_status_pat"
  )
})

# Einsender
observeEvent(input$submit_peer_fremdbewertung_ein, {
  submit_fremdbewertung_topic(
    "E", 
    ein_questions, 
    "Einsender und Kooperationspartner",
    "peer_fremdbewertung_submit_status_ein"
  )
})

# Qualität
observeEvent(input$submit_peer_fremdbewertung_qual, {
  submit_fremdbewertung_topic(
    "Q", 
    qual_questions, 
    "Qualitätsindikatoren",
    "peer_fremdbewertung_submit_status_qual"
  )
})


#---------------------fremdbewertung download handler---------------------------

output$downloadFremdbewertung <- downloadHandler(
  filename = function() paste0("fremdbewertung_", format(Sys.Date(), "%Y%m%d"), ".pdf"),
  content  = function(file) {
    
    message("=== Fremdbewertung Download started ===")
    
    # Use .Rmd file
    template_file <- "fremdbewertung_report.Rmd"
    
    if (!file.exists(template_file)) {
      showNotification("Template nicht gefunden.", type = "error", duration = 10)
      writeLines("Template nicht verfügbar.", file)
      return(NULL)
    }
    
    tryCatch({
      
      message("Step 1: Getting template path")
      template_path <- normalizePath(template_file, mustWork = TRUE)
      
      message("Step 2: Changing to temp directory")
      tmpdir <- tempdir()
      owd <- setwd(tmpdir)
      on.exit(setwd(owd), add = TRUE)
      
      message("Step 3: Copying template to temp dir")
      file.copy(template_path, file.path(tmpdir, template_file), overwrite = TRUE)
      
      message("Step 4: Getting project ID")
      project_id <- current_project_id()
      if (is.null(project_id)) {
        showNotification("Kein Projekt ausgewählt.", type = "error")
        return(NULL)
      }
      message("  Project ID: ", project_id)
      
      message("Step 5: Gathering Fremdbewertung responses")
      # Gather Fremdbewertung answers
      gather_fremdbewertung_answers <- function() {
        sections <- list(
          fuehrung      = fuehrung_questions,
          mitarbeitende = mit_questions,
          patienten     = pat_questions,
          einsender     = ein_questions,
          qualitaet     = qual_questions
        )
        out <- list()
        for (sec_name in names(sections)) {
          questions <- sections[[sec_name]]
          for (q in questions) {
            num_ans <- input[[paste0("FremdbewertungNum_", q$id)]]
            txt_ans <- input[[paste0("FremdbewertungText_", q$id)]]
            out[[length(out) + 1]] <- data.frame(
              Section     = sec_name,
              question_id = q$id,
              Frage       = q$label,
              Antwort     = if (is.null(num_ans) || num_ans == "" || num_ans == "N/A") "—" else as.character(num_ans),
              Begründung  = if (is.null(txt_ans) || txt_ans == "") "—" else as.character(txt_ans),
              stringsAsFactors = FALSE
            )
          }
        }
        do.call(rbind, out)
      }
      
      fremd_responses <- tryCatch({
        gather_fremdbewertung_answers()
      }, error = function(e) {
        message("ERROR in gather_fremdbewertung_answers: ", e$message)
        data.frame(
          Section = character(0),
          question_id = character(0),
          Frage = character(0),
          Antwort = character(0),
          Begründung = character(0),
          stringsAsFactors = FALSE
        )
      })
      
      message("  Gathered ", nrow(fremd_responses), " fremdbewertung responses")
      
      message("Step 6: Gathering Selbstbewertung statistics, group data, and text responses")
      
      selbst_statistics <- tryCatch({
        rbind(
          get_question_level_statistics(project_id, fuehrung_questions, "fuehrung"),
          get_question_level_statistics(project_id, mit_questions, "mitarbeitende"),
          get_question_level_statistics(project_id, pat_questions, "patienten"),
          get_question_level_statistics(project_id, ein_questions, "einsender"),
          get_question_level_statistics(project_id, qual_questions, "qualitaet")
        )
      }, error = function(e) {
        message("ERROR in get_question_level_statistics: ", e$message)
        data.frame(Section = character(0), question_id = character(0), 
                   Frage = character(0), Statistik = character(0), stringsAsFactors = FALSE)
      })
      
      selbst_groups <- tryCatch({
        rbind(
          get_berufsgruppe_averages(project_id, fuehrung_questions, "fuehrung"),
          get_berufsgruppe_averages(project_id, mit_questions, "mitarbeitende"),
          get_berufsgruppe_averages(project_id, pat_questions, "patienten"),
          get_berufsgruppe_averages(project_id, ein_questions, "einsender"),
          get_berufsgruppe_averages(project_id, qual_questions, "qualitaet")
        )
      }, error = function(e) {
        message("ERROR in get_berufsgruppe_averages: ", e$message)
        data.frame(Section = character(0), question_id = character(0), 
                   Frage = character(0), Berufsgruppen = character(0), stringsAsFactors = FALSE)
      })
      
      selbst_texts <- tryCatch({
        rbind(
          get_selbstbewertung_text_responses(project_id, fuehrung_questions, "fuehrung"),
          get_selbstbewertung_text_responses(project_id, mit_questions, "mitarbeitende"),
          get_selbstbewertung_text_responses(project_id, pat_questions, "patienten"),
          get_selbstbewertung_text_responses(project_id, ein_questions, "einsender"),
          get_selbstbewertung_text_responses(project_id, qual_questions, "qualitaet")
        )
      }, error = function(e) {
        message("ERROR in get_selbstbewertung_text_responses: ", e$message)
        data.frame(Section = character(0), question_id = character(0), 
                   Frage = character(0), Textantworten = character(0), stringsAsFactors = FALSE)
      })
      
      message("  Gathered ", nrow(selbst_statistics), " statistics rows")
      message("  Gathered ", nrow(selbst_groups), " group rows")
      message("  Gathered ", nrow(selbst_texts), " text rows")
      
      message("Step 7: Saving CSV files")
      write.csv(fremd_responses, file = file.path(tmpdir, "user_fremdbewertung.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(selbst_statistics, file = file.path(tmpdir, "selbst_statistics.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(selbst_groups, file = file.path(tmpdir, "selbst_groups.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(selbst_texts, file = file.path(tmpdir, "selbst_texts.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      message("  CSV files saved")
      
      message("Step 7: Saving CSV files")
      write.csv(fremd_responses, file = file.path(tmpdir, "user_fremdbewertung.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(selbst_statistics, file = file.path(tmpdir, "selbst_statistics.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(selbst_groups, file = file.path(tmpdir, "selbst_groups.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      message("  CSV files saved")
      
      message("Step 7.5: Checking CSV files exist")
      fremd_csv_exists <- file.exists(file.path(tmpdir, "user_fremdbewertung.csv"))
      selbst_stats_exists <- file.exists(file.path(tmpdir, "selbst_statistics.csv"))
      selbst_groups_exists <- file.exists(file.path(tmpdir, "selbst_groups.csv"))
      
      message("  user_fremdbewertung.csv exists: ", fremd_csv_exists)
      message("  selbst_statistics.csv exists: ", selbst_stats_exists)
      message("  selbst_groups.csv exists: ", selbst_groups_exists)
      
      if (fremd_csv_exists) {
        test_fremd <- read.csv(file.path(tmpdir, "user_fremdbewertung.csv"))
        message("  Fremdbewertung rows: ", nrow(test_fremd))
        message("  Fremdbewertung columns: ", paste(names(test_fremd), collapse = ", "))
      }
      
      if (selbst_stats_exists) {
        test_stats <- read.csv(file.path(tmpdir, "selbst_statistics.csv"))
        message("  Statistics rows: ", nrow(test_stats))
        message("  Statistics columns: ", paste(names(test_stats), collapse = ", "))
      }
      
      if (selbst_groups_exists) {
        test_groups <- read.csv(file.path(tmpdir, "selbst_groups.csv"))
        message("  Groups rows: ", nrow(test_groups))
        message("  Groups columns: ", paste(names(test_groups), collapse = ", "))
      }
      message("Step 8: Rendering PDF")
      
      # Render PDF using rmarkdown
      output_file <- rmarkdown::render(
        input = file.path(tmpdir, template_file),
        output_format = "pdf_document",
        output_file = "fremdbewertung_report.pdf",
        output_dir = tmpdir,
        envir = new.env(parent = globalenv()),  # ✅ Changed this line
        quiet = FALSE
      )
      
      message("Step 9: Moving PDF to destination")
      file.copy(output_file, file, overwrite = TRUE)
      
      message("=== Fremdbewertung Download completed successfully ===")
      
      showNotification(
        "Fremdbewertung Bericht erfolgreich erstellt!",
        type = "message",
        duration = 5
      )
      
    }, error = function(e) {
      message("=== ERROR ===")
      message(paste("  Message:", e$message))
      message(paste("  Traceback:", paste(sys.calls(), collapse = "\n")))
      
      showNotification(
        paste("Fehler beim Erstellen des Berichts:", e$message),
        type = "error",
        duration = 15
      )
      
      writeLines(paste("Fehler:", e$message), file)
    })
  }
)


# Helper function to get fremdbewertung for a specific user
get_fremdbewertung_for_user <- function(project_id, user_id) {
  con <- get_db_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  query <- "
    SELECT 
      r.question_id,
      r.response_value as antwort,
      r.text_value as begruendung
    FROM responses r
    WHERE r.project_id::text = $1::text
      AND r.user_id::text = $2::text
      AND r.assessment_type = 'fremdbewertung'
    ORDER BY r.question_id
  "
  
  result <- DBI::dbGetQuery(con, query, params = list(as.character(project_id), as.character(user_id)))
  
  if (nrow(result) == 0) {
    return(data.frame(
      Section = character(0),
      question_id = character(0),
      Frage = character(0),
      Antwort = character(0),
      Begründung = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Add section and question labels
  all_questions <- c(fuehrung_questions, mit_questions, pat_questions, ein_questions, qual_questions)
  
  result$Section <- sapply(result$question_id, function(qid) {
    if (grepl("^F", qid)) "fuehrung"
    else if (grepl("^M", qid)) "mitarbeitende"
    else if (grepl("^P", qid)) "patienten"
    else if (grepl("^E", qid)) "einsender"
    else if (grepl("^Q", qid)) "qualitaet"
    else NA
  })
  
  result$Frage <- sapply(result$question_id, function(qid) {
    q <- Find(function(x) x$id == qid, all_questions)
    if (!is.null(q)) q$label else qid
  })
  
  result <- result[, c("Section", "question_id", "Frage", "antwort", "begruendung")]
  names(result) <- c("Section", "question_id", "Frage", "Antwort", "Begründung")
  
  return(result)
}

# Helper function to save report fields
save_report_field <- function(field_name, field_value) {
  con <- get_db_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  project_id <- current_project_id()
  user_id_val <- user_id()
  
  DBI::dbExecute(con, "
    INSERT INTO report_data (project_id, user_id, field_name, field_value, updated_at)
    VALUES ($1, $2, $3, $4, NOW())
    ON CONFLICT (project_id, user_id, field_name)
    DO UPDATE SET field_value = EXCLUDED.field_value, updated_at = NOW()
  ", params = list(project_id, user_id_val, field_name, field_value))
}

# Helper function to load report fields
load_report_field <- function(field_name) {
  con <- get_db_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  project_id <- current_project_id()
  user_id_val <- user_id()
  
  result <- DBI::dbGetQuery(con, "
    SELECT field_value FROM report_data
    WHERE project_id = $1 AND user_id = $2 AND field_name = $3
  ", params = list(project_id, user_id_val, field_name))
  
  if (nrow(result) > 0) result$field_value[1] else ""
}

# Save report data inputs to database
observeEvent(input$report_teilnehmende, {
  req(current_project_id())
  save_report_field("teilnehmende", input$report_teilnehmende)
})

observeEvent(input$report_ort, {
  req(current_project_id())
  save_report_field("ort", input$report_ort)
})

observeEvent(input$report_datum, {
  req(current_project_id())
  save_report_field("datum", input$report_datum)
})

observeEvent(input$swot_strengths, {
  req(current_project_id())
  save_report_field("swot_strengths", input$swot_strengths)
})

observeEvent(input$swot_weaknesses, {
  req(current_project_id())
  save_report_field("swot_weaknesses", input$swot_weaknesses)
})

observeEvent(input$swot_opportunities, {
  req(current_project_id())
  save_report_field("swot_opportunities", input$swot_opportunities)
})

observeEvent(input$swot_threats, {
  req(current_project_id())
  save_report_field("swot_threats", input$swot_threats)
})

observeEvent(input$best_practices, {
  req(current_project_id())
  save_report_field("best_practices", input$best_practices)
})

observeEvent(input$final_remarks, {
  req(current_project_id())
  save_report_field("final_remarks", input$final_remarks)
})

# ------------------ gesamte bericht für Leading Peer -------------------------------------------
output$downloadLeadingPeerGesamtbericht <- downloadHandler(
  filename = function() {
    project_name <- current_project_name()
    if (is.null(project_name) || project_name == "") {
      project_name <- "Peer_Review"
    }
    # Sanitize filename
    project_name <- gsub("[^A-Za-z0-9_-]", "_", project_name)
    paste0("Gesamtbericht_", project_name, "_", format(Sys.Date(), "%Y%m%d"), ".pdf")
  },
  
  content = function(file) {
    message("=== Gesamtbericht Download started ===")
    
    # Use .Rmd file directly (same directory as app)
    template_file <- "gesamtbericht_template.Rmd"
    
    if (!file.exists(template_file)) {
      showNotification("Template nicht gefunden.", type = "error", duration = 10)
      writeLines("Template nicht verfügbar.", file)
      return(NULL)
    }
    
    tryCatch({
      
      message("Step 1: Getting template path")
      template_path <- normalizePath(template_file, mustWork = TRUE)
      message("  Template path: ", template_path)
      
      message("Step 2: Changing to temp directory")
      tmpdir <- tempdir()
      owd <- setwd(tmpdir)
      on.exit(setwd(owd), add = TRUE)
      message("  Working in: ", tmpdir)
      
      message("Step 3: Copying template to temp dir")
      file.copy(template_path, file.path(tmpdir, template_file), overwrite = TRUE)
      
      message("Step 4: Getting project ID and database connection")
      project_id <- current_project_id()
      if (is.null(project_id)) {
        showNotification("Kein Projekt ausgewählt.", type = "error")
        return(NULL)
      }
      message("  Project ID: ", project_id)
      
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      
      # Step 4.5: Get lab info
      message("Step 4.5: Getting lab info")
      labinfo <- DBI::dbGetQuery(con, "
        SELECT * FROM lab_info WHERE project_id = $1
      ", params = list(as.character(project_id)))
      
      if (nrow(labinfo) == 0) {
        # Create empty dataframe with all expected columns
        labinfo <- data.frame(
          lab_name = "", street = "", plz = "", city = "", country = "",
          phone = "", email = "", website = "", lab_director = "", deputy = "",
          num_employees = "", num_doctors = "", num_mtl = "",
          accreditation = "", certification = "",
          stringsAsFactors = FALSE
        )
      }
      message("  Lab info rows: ", nrow(labinfo))
      
      # Step 5: Gather Selbstbewertung data
      message("Step 5: Gathering Selbstbewertung data")
      selbst_statistics <- tryCatch({
        rbind(
          get_question_level_statistics(project_id, fuehrung_questions, "fuehrung"),
          get_question_level_statistics(project_id, mit_questions, "mitarbeitende"),
          get_question_level_statistics(project_id, pat_questions, "patienten"),
          get_question_level_statistics(project_id, ein_questions, "einsender"),
          get_question_level_statistics(project_id, qual_questions, "qualitaet")
        )
      }, error = function(e) {
        message("ERROR in selbst_statistics: ", e$message)
        data.frame(
          Section = character(0),
          question_id = character(0),
          Frage = character(0),
          Statistik = character(0),
          stringsAsFactors = FALSE
        )
      })
      
      selbst_groups <- tryCatch({
        rbind(
          get_berufsgruppe_averages(project_id, fuehrung_questions, "fuehrung"),
          get_berufsgruppe_averages(project_id, mit_questions, "mitarbeitende"),
          get_berufsgruppe_averages(project_id, pat_questions, "patienten"),
          get_berufsgruppe_averages(project_id, ein_questions, "einsender"),
          get_berufsgruppe_averages(project_id, qual_questions, "qualitaet")
        )
      }, error = function(e) {
        message("ERROR in selbst_groups: ", e$message)
        data.frame(
          Section = character(0),
          question_id = character(0),
          Frage = character(0),
          Berufsgruppen = character(0),
          stringsAsFactors = FALSE
        )
      })
      
      selbst_texts <- tryCatch({
        rbind(
          get_selbstbewertung_text_responses(project_id, fuehrung_questions, "fuehrung"),
          get_selbstbewertung_text_responses(project_id, mit_questions, "mitarbeitende"),
          get_selbstbewertung_text_responses(project_id, pat_questions, "patienten"),
          get_selbstbewertung_text_responses(project_id, ein_questions, "einsender"),
          get_selbstbewertung_text_responses(project_id, qual_questions, "qualitaet")
        )
      }, error = function(e) {
        message("ERROR in selbst_texts: ", e$message)
        data.frame(
          Section = character(0),
          question_id = character(0),
          Frage = character(0),
          Textantworten = character(0),
          stringsAsFactors = FALSE
        )
      })
      
      message("  Gathered ", nrow(selbst_statistics), " statistics rows")
      message("  Gathered ", nrow(selbst_groups), " group rows")
      message("  Gathered ", nrow(selbst_texts), " text rows")
      
      # Step 6: Gather ALL Fremdbewertung data (leading + co-peers)
      message("Step 6: Gathering ALL Fremdbewertung data")
      
      # Get all peer users for this project
      peer_users <- DBI::dbGetQuery(con, "
        SELECT id, username, role 
        FROM users 
        WHERE project_id = $1 
          AND role IN ('leading_peer', 'co_peer')
        ORDER BY role DESC, username
      ", params = list(as.character(project_id)))
      
      message("  Found ", nrow(peer_users), " peer reviewers")
      
      # Collect fremdbewertung from all peers
      all_fremd_responses <- list()
      
      if (nrow(peer_users) > 0) {
        for (i in 1:nrow(peer_users)) {
          peer_user_id <- peer_users$id[i]
          peer_username <- peer_users$username[i]
          peer_role <- peer_users$role[i]
          
          message("  Loading fremdbewertung from: ", peer_username, " (", peer_role, ")")
          
          fremd_data <- get_fremdbewertung_for_user(project_id, peer_user_id)
          
          if (nrow(fremd_data) > 0) {
            fremd_data$Reviewer <- peer_username
            fremd_data$Reviewer_Role <- peer_role
            all_fremd_responses[[i]] <- fremd_data
          }
        }
      }
      
      if (length(all_fremd_responses) > 0) {
        fremd_combined <- do.call(rbind, all_fremd_responses)
      } else {
        fremd_combined <- data.frame(
          Section = character(0),
          question_id = character(0),
          Frage = character(0),
          Antwort = character(0),
          Begründung = character(0),
          Reviewer = character(0),
          Reviewer_Role = character(0),
          stringsAsFactors = FALSE
        )
      }
      
      message("  Total fremdbewertung rows: ", nrow(fremd_combined))
      
      # Step 7: Load report metadata
      message("Step 7: Loading report metadata")
      
      report_meta <- data.frame(
        teilnehmende = load_report_field("teilnehmende"),
        ort = load_report_field("ort"),
        datum = load_report_field("datum"),
        stringsAsFactors = FALSE
      )
      
      swot_data <- data.frame(
        strengths = load_report_field("swot_strengths"),
        weaknesses = load_report_field("swot_weaknesses"),
        opportunities = load_report_field("swot_opportunities"),
        threats = load_report_field("swot_threats"),
        stringsAsFactors = FALSE
      )
      
      best_practices_text <- load_report_field("best_practices")
      final_remarks_text <- load_report_field("final_remarks")
      
      message("  Loaded report metadata")
      
      # Step 8: Get co-peer list
      message("Step 8: Getting co-peer list")
      co_peers <- DBI::dbGetQuery(con, "
        SELECT username, email, created_at
        FROM users
        WHERE project_id = $1 AND role = 'co_peer'
        ORDER BY created_at
      ", params = list(as.character(project_id)))
      
      message("  Found ", nrow(co_peers), " co-peers")
      
      # Step 9: Save all data as CSV
      message("Step 9: Saving CSV files")
      write.csv(labinfo, file = file.path(tmpdir, "labinfo.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(selbst_statistics, file = file.path(tmpdir, "selbst_statistics.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(selbst_groups, file = file.path(tmpdir, "selbst_groups.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(selbst_texts, file = file.path(tmpdir, "selbst_texts.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(fremd_combined, file = file.path(tmpdir, "fremd_all.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(report_meta, file = file.path(tmpdir, "report_meta.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(swot_data, file = file.path(tmpdir, "swot_data.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(co_peers, file = file.path(tmpdir, "co_peers.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      
      writeLines(best_practices_text, file.path(tmpdir, "best_practices.txt"))
      writeLines(final_remarks_text, file.path(tmpdir, "final_remarks.txt"))
      
      message("  CSV files saved")
      
      # Step 10: Render PDF
      message("Step 10: Rendering PDF")
      rmarkdown::render(
        input = file.path(tmpdir, template_file),
        output_file = file,
        quiet = FALSE
      )
      
      message("=== Gesamtbericht Download completed ===")
      
    }, error = function(e) {
      message("ERROR in download handler: ", e$message)
      showNotification(paste("Fehler beim Erstellen des PDFs:", e$message), type = "error", duration = 10)
      writeLines(paste("Fehler:", e$message), file)
    })
  }
)

#------------------comprehensive summary download-------------------------------

output$download_comprehensive_summary <- downloadHandler(
  filename = function() {
    project_name <- current_project_name()
    if (is.null(project_name) || project_name == "") project_name <- "Projekt"
    paste0("Gesamtbericht_", gsub("[^A-Za-z0-9_-]", "_", project_name), "_", format(Sys.Date(), "%Y%m%d"), ".pdf")
  },
  
  content = function(file) {
    
    message("=== Starting comprehensive summary PDF ===")
    
    project_id <- current_project_id()
    project_name <- current_project_name()
    
    req(project_id, project_name)
    
    showNotification("PDF wird erstellt... Bitte warten.", type = "message", duration = NULL, id = "pdf_progress")
    
    template_file <- "comprehensive_summary_report.Rmd"
    
    if (!file.exists(template_file)) {
      showNotification("Template nicht gefunden.", type = "error", duration = 10)
      removeNotification("pdf_progress")
      return(NULL)
    }
    
    tryCatch({
      
      # Set up temp directory
      tmpdir <- tempdir()
      owd <- setwd(tmpdir)
      on.exit(setwd(owd), add = TRUE)
      
      message("Step 1: Copying template")
      file.copy(normalizePath(template_file), file.path(tmpdir, template_file), overwrite = TRUE)
      
      # Get database connection
      con <- get_db_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      
      # Step 2: Get lab info
      message("Step 2: Getting lab info")
      labinfo <- DBI::dbGetQuery(
        con,
        "SELECT * FROM lab_info WHERE project_id = $1",
        params = list(project_id)
      )
      
      if (nrow(labinfo) > 0) {
        write.csv(labinfo, file.path(tmpdir, "labinfo.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      
      # Step 3: Get responses for all topics
      message("Step 3: Getting responses")
      
      fuehrung_responses <- get_topic_responses_summary(project_id, "F", fuehrung_questions)
      mitarbeitende_responses <- get_topic_responses_summary(project_id, "M", mit_questions)
      pat_responses <- get_topic_responses_summary(project_id, "P", pat_questions)
      ein_responses <- get_topic_responses_summary(project_id, "E", ein_questions)
      qual_responses <- get_topic_responses_summary(project_id, "Q", qual_questions)
      
      write.csv(fuehrung_responses, file.path(tmpdir, "fuehrung_responses.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(mitarbeitende_responses, file.path(tmpdir, "mitarbeitende_responses.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(pat_responses, file.path(tmpdir, "pat_responses.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(ein_responses, file.path(tmpdir, "ein_responses.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(qual_responses, file.path(tmpdir, "qual_responses.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      
      # Step 4: Get statistics
      message("Step 4: Getting statistics")
      
      fuehrung_stats <- get_topic_weighted_stats(project_id, "fuehrung")
      mitarbeitende_stats <- get_topic_weighted_stats(project_id, "mitarbeitende")
      pat_stats <- get_topic_weighted_stats(project_id, "patienten")
      ein_stats <- get_topic_weighted_stats(project_id, "einsender")
      qual_stats <- get_topic_weighted_stats(project_id, "qualitaet")
      
      write.csv(fuehrung_stats, file.path(tmpdir, "fuehrung_stats.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(mitarbeitende_stats, file.path(tmpdir, "mitarbeitende_stats.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(pat_stats, file.path(tmpdir, "pat_stats.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(ein_stats, file.path(tmpdir, "ein_stats.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(qual_stats, file.path(tmpdir, "qual_stats.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      
      # Step 5: Create ggradar plots
      message("Step 5: Creating radar plots")
      
      mit_binary_global_qnums <- c(10, 20, 24)
      
      plot_configs <- list(
        list(name = "fuehrung", questions = fuehrung_questions, title = "Führung", 
             colour = "#003B73", binary_qnums = integer(0)),
        list(name = "mit", questions = mit_questions, title = "Mitarbeitende", 
             colour = "#136377", binary_qnums = mit_binary_global_qnums),
        list(name = "pat", questions = pat_questions, title = "Patienten", 
             colour = "#b23a48", binary_qnums = integer(0)),
        list(name = "ein", questions = ein_questions, title = "Einsender", 
             colour = "#4b61d1", binary_qnums = integer(0)),
        list(name = "qual", questions = qual_questions, title = "Qualität", 
             colour = "#c58f00", binary_qnums = integer(0))
      )
      
      for (config in plot_configs) {
        plot_file <- file.path(tmpdir, paste0("plot_", config$name, ".png"))
        message(paste("  Creating plot:", config$title))
        
        tryCatch({
          # Get average responses for radar plot
          question_ids <- sapply(
            if (length(config$binary_qnums) > 0) {
              Filter(function(q) !(q$number %in% config$binary_qnums), config$questions)
            } else {
              config$questions
            },
            function(q) q$id
          )
          
          responses <- DBI::dbGetQuery(
            con,
            sprintf(
              "SELECT question_id, AVG(response_value::numeric) as avg_value
               FROM responses
               WHERE project_id = $1
                 AND assessment_type = 'selbstbewertung'
                 AND question_id IN (%s)
                 AND response_value IS NOT NULL
                 AND response_value != ''
               GROUP BY question_id",
              paste0("'", question_ids, "'", collapse = ", ")
            ),
            params = list(project_id)
          )
          
          if (nrow(responses) > 0) {
            # Create radar data frame for ggradar
            values <- sapply(question_ids, function(qid) {
              row <- responses[responses$question_id == qid, ]
              if (nrow(row) > 0) row$avg_value else 0
            })
            
            radar_df <- data.frame(
              group = config$title,
              t(values)
            )
            colnames(radar_df) <- c("group", paste0("Q", seq_along(values)))
            
            # Add min/max rows
            max_row <- radar_df
            max_row$group <- "Max"
            max_row[, -1] <- 5
            
            min_row <- radar_df
            min_row$group <- "Min"
            min_row[, -1] <- 0
            
            radar_df_full <- rbind(max_row, min_row, radar_df)
            
            # Create plot using fmsb-style function
            make_fmsb_radar_png(
              radar_df = radar_df_full[3, -1],  # Just the data row
              title = config$title,
              colour = config$colour,
              filename = plot_file,
              width = 800,
              height = 600
            )
          } else {
            # Create placeholder
            png(plot_file, width = 800, height = 600)
            plot.new()
            text(0.5, 0.5, "Keine Daten verfügbar", cex = 1.5)
            dev.off()
          }
        }, error = function(e) {
          message(paste("    ERROR:", e$message))
          png(plot_file, width = 800, height = 600)
          plot.new()
          text(0.5, 0.5, paste("Fehler:\n", config$title), cex = 1.5)
          dev.off()
        })
      }
      
      # Step 6: Render PDF
      message("Step 6: Rendering PDF")
      
      output_file <- rmarkdown::render(
        input = file.path(tmpdir, template_file),
        output_format = "pdf_document",
        output_file = "report.pdf",
        output_dir = tmpdir,
        params = list(
          project_title = project_name,
          project_id = project_id
        ),
        envir = new.env(),
        quiet = FALSE
      )
      
      # Step 7: Copy to destination
      message("Step 7: Copying PDF")
      file.copy(output_file, file, overwrite = TRUE)
      
      removeNotification("pdf_progress")
      showNotification("PDF erfolgreich erstellt!", type = "message", duration = 5)
      
      message("=== PDF completed successfully ===")
      
    }, error = function(e) {
      message("=== ERROR ===")
      message(paste("Message:", e$message))
      
      removeNotification("pdf_progress")
      showNotification(
        paste("Fehler beim Erstellen des PDFs:", e$message),
        type = "error",
        duration = 15
      )
      
      writeLines(paste("Fehler:", e$message), file)
    })
  }
)


  
}


shinyApp(ui = ui, server = server)


