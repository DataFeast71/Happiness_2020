# Import libraries
library(shiny)
library(shinythemes)
library(ggplot2)
library(ggrepel)
library(plotly)
library(dplyr)

####################################################
#  Data
####################################################
## Rotation data PCA
df_rotation <- read.csv("Happiness_years_PCA_rotation.csv", header = TRUE)
# Var_type        PC1        PC2 Year
# 1                      Life.Ladder -3.9345964  0.7313381 2006
# 2               Log.GDP.per.capita -3.5657915  2.7360036 2006
# 3                   Social.support -3.0859818  2.1070222 2006
# 4 Healthy.life.expectancy.at.birth -3.2483523  2.9325475 2006
# 5     Freedom.to.make.life.choices -3.2915115 -2.5851631 2006
# 6                       Generosity -0.8767783 -3.9914615 2006

## Results from PC1 and PC2 by year
df_pca <- read.csv("Happiness_years_PCA.csv", header = TRUE)

#############################################################
#           User Interface
#############################################################

ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(HTML("<title>Happiness</title>")),
  navbarPage(
    #
    title = div(img(
      src = "https://raw.githubusercontent.com/DataFeast71/COVID19_plots/main/img/Logo_W.jpeg",
      style = "margin-top: -14.5px; padding-right:10px;padding-bottom:10px", height = 60
    )),
    tabPanel(
      title = "PCA Happiness",
      # Input values
      sidebarPanel(
        HTML("<h3>Input parateters</h3>"),

        selectInput("year",
          label = "Year:",
          choices = unique(df_pca$Year),
          selected = "2017"
        ),

        actionButton("submitButton", "Submit", class = "btn btn-primary")
      ), # Side Bar Panel
      mainPanel(
        tags$label(h3("Status/Output")),
        verbatimTextOutput("contents"),
        tableOutput("tableData"),
        plotOutput(outputId = "ggPlot"),
        verbatimTextOutput("contentsPlotly"),
        plotlyOutput(outputId = "plotlyPlot")
      ) # Main Panel Tab 1
    ), # Tab panel 1
    tabPanel(
      title = "Progress",

      sidebarPanel(
        # Input values
        selectInput("variable",
          " Variable to show: ",
          choices = list(
            "Life Ladder" = "Life.Ladder",
            "GDP per Capita" = "Log.GDP.per.capita",
            "Social support" = "Social.support",
            "Healthy life expectancy" = "Healthy.life.expectancy.at.birth",
            "Freedom do make choices" = "Freedom.to.make.life.choices",
            "Generosity" = "Generosity",
            "Perceptions of corruption" = "Perceptions.of.corruption",
            "Positive affect" = "Positive.affect",
            "Negative affect" = "Negative.affect",
            "Confidence in national government" = "Confidence.in.national.government",
            "Democratic quality" = "Democratic.Quality",
            "Delivery quality" = "Delivery.Quality"
          ),
          selected = "Perceptions of corruption"
        ),

        selectInput("Country1",
          label = "Country 1:",
          choices = unique(df_pca$Country),
          selected = "Mexico"
        ),
        selectInput("Country2",
          label = "Country 2:",
          choices = unique(df_pca$Country),
          selected = "Finland"
        ),
        selectInput("Country3",
          label = "Country 3:",
          choices = unique(df_pca$Country),
          selected = "Ghana"
        ),
        selectInput("Country4",
          label = "Country 4:",
          choices = unique(df_pca$Country),
          selected = "Japan"
        ),
        selectInput("Country5",
          label = "Country 5:",
          choices = unique(df_pca$Country),
          selected = "Australia"
        ),

        # actionButton("SubmitButton2", "Submit", class = "btn btn-primary")
      ), # Side Bar Tab2
      mainPanel(
        tags$label(h3("Status/Output")),
        verbatimTextOutput("contents2"),
        tableOutput(outputId = "Table2"),
        plotOutput(outputId = "ggcountries")
      ) # Main Panel
    ) # Tab Panel 2
  ) # Nav Bar
) # Fluid Page

# ui <- fluidPage(theme = shinytheme("united"),
#
#                 # Page Header
#                 tags$head(HTML("<title>PCA Happiness</title>")),
#                 titlePanel(
#                   HTML("<h1><a href='https://www.facebook.com/An%C3%A1lisis-y-visualizaci%C3%B3n-de-datos-100602148375744'><img src='https://raw.githubusercontent.com/DataFeast71/COVID19_plots/main/img/Logo_W.jpeg' style='width:1in'></a>PCA Happiness</h1>")
#                   ),
#
#                 # Input values
#                 sidebarPanel(
#                   HTML("<h3>Input parateters</h3>"),
#
#                   selectInput("year",
#                               label = "Year:",
#                               choices = unique(df_pca$Year),
#                               selected = "2017"),
#
#                   actionButton("submitButton", "Submit", class = "btn btn-primary")
#                 ), # Side Bar Panel
#                 mainPanel(
#                   tags$label(h3("Status/Output")),
#                   verbatimTextOutput('contents'),
#                   tableOutput("tableData"),
#                   plotOutput(outputId = "ggPlot"),
#                   verbatimTextOutput("contentsPlotly"),
#                   plotlyOutput(outputId = "plotlyPlot")
#                 )
#                 ) # Fluid Page

######################################
#         SERVER
######################################

server <- function(input, output, session) {
  # Input data
  Data_year <- reactive({
    # Year data
    df_pca_year <- df_pca[which(df_pca$Year == input$year), ]
    df_pca_year
  })

  Rotation_year <- reactive({
    # Rotation
    df_rota_year <- df_rotation[which(df_rotation$Year == input$year), ]
    df_rota_year
  })

  # Status/output text box
  output$contents <- renderPrint({
    if (input$submitButton > 0) {
      isolate("Analysis complete.")
    } else {
      return("Server is ready show the PCA results")
    }
  })
  # Prediction results table
  output$tableData <- renderTable({
    if (input$submitButton > 0) {
      isolate(head(Data_year()[order(Data_year()$Life.Ladder, decreasing = TRUE), c(3,4,5)]))
    }
  })

  output$ggPlot <- renderPlot({
    if (input$submitButton > 0) {
      ggplot(Data_year(), aes(x = PC1, y = PC2)) +
        geom_hline(yintercept = 0, lty = 2) +
        geom_vline(xintercept = 0, lty = 2) +
        guides(color = guide_legend(title = "")) +
        labs(x = "PC1", y = "PC2") +
        geom_point(aes(color = Continent), alpha = 0.6, size = 2) +
        geom_label(
          data = Data_year()[which(Data_year()$Country == "Mexico"), ],
          aes(label = Country), color = "black", size = 3
        ) +
        geom_segment(data = Rotation_year(), aes(x = 0, xend = PC1, y = 0, yend = PC2), alpha = 0.3, color = "red", arrow = arrow(length = unit(0.3, "cm"))) +
        geom_text(data = Rotation_year(), aes(x = PC1, y = PC2, label = Var_type), alpha = 0.3) +
        geom_text_repel(data = Rotation_year(), aes(x = PC1, y = PC2, label = Var_type), alpha = 0.3) +
        theme_bw()
    }
  })

  # plotly box
  output$contentsPlotly <- renderPrint({
    if (input$submitButton > 0) {
      return("Interactive plot")
    }
  })

  output$plotlyPlot <- renderPlotly({
    if (input$submitButton > 0) {
      fig <- plot_ly(Data_year(),
        x = ~PC1, y = ~PC2, type = "scatter", mode = "markers", color = ~Continent,
        colors = "Set1", hoverinfo = "text", marker = list(size = 8),
        text = ~ paste(
          "</br> Country: ", Country,
          "</br> Life Ladder: ", round(Life.Ladder, 2),
          "</br> GDP: ", round(Log.GDP.per.capita, 2),
          "</br> Healthy life expectancy at birth: ", round(Healthy.life.expectancy.at.birth, 2),
          "</br> Freedom to make life choices: ", round(Freedom.to.make.life.choices, 2),
          "</br> Percepcions of corruption: ", round(Perceptions.of.corruption, 2),
          "</br> Confidence in government: ", round(Confidence.in.national.government, 2)
        )
      )
      fig <- fig %>% layout(
        title = list(
          text = paste0("PCA data from ", Data_year()$Year[1]),
          x = 0.0
        ),
        legend = list(x = 1.01, y = 0.5)
      )
      fig
    }
  })

  ## Tab2
  Data_countries <- reactive({
    # Countries vector
    countries_selected <- c(
      input$Country1,
      input$Country2,
      input$Country3,
      input$Country4,
      input$Country5
    )
    df_countries <- df_pca %>%
      mutate(
        Year = as.integer(as.character(Year)),
        SelectedCountry = if_else(Country %in% countries_selected, "TRUE", "FALSE"),
        label = if_else(Year == max(Year) & SelectedCountry == "TRUE", as.character(Country), NA_character_)
      )
    df_countries <- df_countries[, c("Year", "SelectedCountry", "label", "Country", input$variable)]
    df_countries
  })

  output$contents2 <- renderPrint({
    return("Showing the countries")
  })

  output$Table2 <- renderTable({
    Data_countries() %>% filter(!is.na(label)) %>% select(-label, -SelectedCountry)
  })

  output$ggcountries <- renderPlot({
    df <- Data_countries()
    ggplot() +
      geom_line(
        data = df %>% filter(SelectedCountry == "FALSE"),
        aes(x = Year, y = get(input$variable), group = Country), show.legend = FALSE, alpha = 0.1
      ) +
      geom_line(
        data = df %>% filter(SelectedCountry != "FALSE"),
        aes(x = Year, y = get(input$variable), group = Country, color = Country),
        show.legend = FALSE, alpha = 1.0, size = 1.2
      ) +
      scale_x_continuous(limits = c(2005, 2021), expand = c(0, 0.4), breaks = c(2005:2019)) +
      geom_label_repel(
        data = df %>% filter(SelectedCountry != "FALSE" & Year == max(Year)),
        aes(x = Year, y = get(input$variable), label = label),
        na.rm = TRUE, size = 3, nudge_x = 1
      ) +
      labs(x = "", y = input$variable) +
      theme(
        # Plot
        panel.background = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "lines"),
        # Grid
        panel.grid = element_blank(),
        # Axis X
        axis.line.x.bottom = element_line(size = 1, color = "black"),
        axis.text.x = element_text(size = 11, color = "black", angle = 90, hjust = 0.5, vjust = 0.5),
        # Axis Y
        axis.line.y.left = element_line(size = 1, color = "black"),
        axis.text.y = element_text(size = 11, color = "black"),
      )
  })
}

##########################################
# Create the shiny app
##########################################
shinyApp(ui = ui, server = server)
