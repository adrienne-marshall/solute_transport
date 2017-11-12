library(shiny)

# Define UI ----
ui <- navbarPage("Solute transport",
  
   #About page              
  tabPanel("About",
           fluidRow(
             includeHTML("solutes_text.html")
             #column(12, offset = 0, p("test_text"))
           ) # end of fluidRow
  ), #end of first tabPanel
                 
  # 3d transport page ----
  tabPanel("Solutes in space and time",
  
  # Expanatory text for first tab.
  fluidRow(
    column(12, offset = 0, p("This page shows the concentration of a solute (designated by color) as a function of distance from a source (x axis), and 
                             time since a pulse of the solute was initiated (y-axis). Try adjusting each of the parameters in turn - how do they affect solute concentrations?
                             If a parameter doesn't seem to alter concentrations, remember to pay attention to the axes and color scale. Does it matter what 
                             units are used to measure the solute pulse? If there was an acceptable maximum solute concentration and a given set of parameters, 
                             at what points in space and time would the acceptable maximum concentration be exceeded?")
    ) #end of column
  ), #end of fluid row
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      numericInput(inputId = "pulse_conc",
                  label = "Concentration of pulse (units)",
                  value = 30),
      
      numericInput(inputId = "pulse_time",
                   label = "Duration of pulse (sec)",
                   value = 5),
      
      numericInput(inputId = "dispersion_coefficient",
                   label = "Dispersion coefficient",
                   value = 0.5),
      
      numericInput(inputId = "velocity",
                   label = "Velocity (cm/sec)",
                   value = 2),
      
      numericInput(inputId = "R",
                   label = "Retardation factor: 1 if there are no interactions between soil and solute",
                   value = 1),
      
      radioButtons(inputId = "max_value_on_off",
                   label = "Delineate acceptable concentration?",
                   choices = list("Yes" = TRUE, "No" = FALSE),
                   selected = FALSE),
      
      numericInput(inputId = "max_value",
                   label = "Maximum acceptable concentration:",
                   value = 5),
      
      sliderInput(inputId = "time_range",
                  label = "Time range of interest (sec)",
                  min = 1,
                  max = 1000,
                  value = c(1,1000)),
      
      sliderInput(inputId = "length_range",
                  label = "Distance range of interest (cm)",
                  min = 1,
                  max = 1000,
                  value = c(1,1000))
      
    ), #end of sidebar panel
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "pulse3d")
      
    ) #end of main panel
  ) #end of sidebar layout
  ), #end of tabPanel
  
  ## Peclet number page
  tabPanel("Peclet numbers",
           
           #Header text
           fluidRow(
             column(12, offset = 0, p("Adjust the parameters to determine how different conditions affect solute transport. 
                                      First, try adjusting the dispersion coefficients (D). How does D affect the Peclet number?
                                      What effect does changing D have on the shape of the curve that describes solute concentration over time? 
                                      Try adjusting each of the other parameters. Are there any that don't seem to affect the graph?
                                      If so, why not? What seem to be the parameters that have the largest effect on solute transport in soils?")
                    ) #end of column
             ), #end of fluidRow
           sidebarLayout(
             
             #Set up sidebar panel options
             sidebarPanel(
               numericInput(inputId = "D1_peclet", 
                            label = "First dispersion coefficient, D",
                            value = 0.5),
               
               numericInput(inputId = "D2_peclet", 
                            label = "Second dispersion coefficient, D",
                            value = 1),
               
               numericInput(inputId = "D3_peclet", 
                            label = "Third dispersion coefficient, D",
                            value = 10),
               
               numericInput(inputId = "pulse_conc_peclet", 
                            label = "Pulse concentration (units)",
                            value = 0.5), #pulse concentration shouldn't matter.
               
               numericInput(inputId = "pulse_time_peclet", 
                            label = "Pulse duration (seconds)",
                            value = 5), 
               
               sliderInput(inputId = "time_range_peclet",
                           label = "Time range of interest (sec)",
                           min = 1,
                           max = 100,
                           value = c(1,100)),
               
               numericInput(inputId = "length_peclet", 
                            label = "Distance from source (cm)",
                            value = 100),
               
               numericInput(inputId = "velocity_peclet", 
                            label = "Velocity of transport (cm)",
                            value = 2),
               
               numericInput(inputId = "R_peclet", 
                            label = "Retardation factor: 1 if there are no interactions between soil and solute",
                            value = 1)

             ), #end of sidebarPanel
             
             mainPanel(
               plotOutput(outputId = "peclet_plot")
             ) #end of main panel
           ) #end of sidebarLayout
           ) #end of tabPanel
  
) #end of UI. 

# Server  ------------------------------------------------------------------
server <- function(input, output, session) {
  source("pulse3d.R")
  source("pulse2d.R")
  output$pulse3d <- renderPlot({
    
    pulse3d(pulse_conc = input$pulse_conc, 
            pulse_time = input$pulse_time, 
            max_val = input$max_value,  
            time_limits = input$time_range, 
            L_limits = input$length_range, 
            D = input$dispersion_coefficient,
            v = input$velocity, 
            R = input$R,
            value_limit = input$max_value_on_off)
  }) #end of renderPlot
  
  output$peclet_plot <- renderPlot({
    pulse2d(pulse_conc = input$pulse_conc_peclet, 
            pulse_time = input$pulse_time_peclet,
            time_limits = input$time_range_peclet, 
            L = input$length_peclet, 
            v = input$velocity_peclet, 
            R = input$R_peclet, 
            D1 = input$D1_peclet,
            D2 = input$D2_peclet,
            D3 = input$D3_peclet)
  }) #end of render plot
  
} #end of server call

shinyApp(ui = ui, server = server)
