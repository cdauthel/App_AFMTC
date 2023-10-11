# Define UI for application
shiny::shinyUI(
  # navbarPage (6)
  navbarPage("AFMTC",
             br(),
             theme = shinythemes::shinytheme("superhero"),
             
             # first page
             tabPanel("Implementation",
                      fluidRow(
                        
                        # left panel
                        column(width = 4,
                               wellPanel(
                                 h4("Methode"),
                                 hr(),
                                 radioButtons("tab_cont_afmtc", h5("Use contingency table to run AFMTC"),
                                              choices = list("Yes" = 1, "No" = 2),
                                              selected = 2),
                                 br(),
                                 
                                 h4("Data"),
                                 hr(),
                                 radioButtons("impute_MFA", h5("Imputation of Na values by an MFA model"),
                                              choices = list("Yes" = 1, "No" = 2),
                                              selected = 2),
                                 conditionalPanel("input.impute_MFA == 1",
                                                  sliderInput("slider_mfa", h4("Number of dimensions keep for the imputation"),
                                                              min = 1, max = 10, value = 5)
                                 ),
                                 br(),
                                 
                                 h4("Principales variables"),
                                 hr(),
                                 
                                 br(),
                                 
                                 h4("Supplementary variable(s)"),
                                 hr(),
                                 
                                 br(),
                                 
                                 h4("Visuals options"),
                                 hr(),
                                 conditionalPanel("input.tabset_afm == 'Individual factor map'", 
                                                  selectInput("habillage_ind", "Color by",
                                                              c(""))
                                                  
                                 ),
                                 
                                 h4("Results"),
                                 hr(),
                                 radioButtons("radio_results_MFA", h4("Show results"),
                                              choices = list("Yes" = 1, "No" = 2),
                                              selected = 2),
                                 conditionalPanel("input.tabset_afm == 'Individual factor map' && input.radio_results_MFA == 1",
                                                  selectInput("results_ind", "",
                                                              c("Eign values", "Inertia ratio", "Contribution", "Cos2", "Within inertia"))
                                 ),
                                 
                                 conditionalPanel("input.tabset_afm == 'Correlation circle' && input.radio_results_MFA == 1",
                                                  radioButtons("results_var", h3("Variable type"),
                                                               choices = list("Qualitative" = 1, "Quantitative" = 2),
                                                               selected = 1),
                                                  conditionalPanel(condition = "input.results_var == 1",
                                                                   selectInput("results_var_quali", "",
                                                                               c("Contribution", "Cos2", "Within inertia", "V Test"))
                                                  ),
                                                  conditionalPanel(condition = "input.results_var == 2",
                                                                   selectInput("results_var_qaunti", "",
                                                                               c("Correlation", "Contribution", "Cos2"))
                                                  )
                                 ),
                                 
                                 conditionalPanel("input.tabset_afm == 'Groups representation' && input.radio_results_MFA == 1",
                                                  selectInput("results_groups", "",
                                                              c("Lg", "RV", "Correlation", "Contribution", "Cos2"))
                                 ),
                                 
                                 conditionalPanel("input.tabset_afm == 'Partial axes' && input.radio_results_MFA == 1",
                                                  selectInput("results_partial_axes", "",
                                                              c("Correlation", "Contribution"))
                                 )
                                 
                               )
                        ),
                        
                        # right panel
                        column(width = 7,
                               tabsetPanel(id = "tabset_afm",
                                 
                                 tabPanel("Individual factor map",
                                          plotOutput("mfa_plot_ind"),
                                          br(),
                                          conditionalPanel("input.radio_results_MFA == 1",
                                                           verbatimTextOutput("results_MFA_ind")
                                          )
                                 ),
                                 
                                 tabPanel("Correlation circle",
                                          plotOutput("mfa_plot_cor"),
                                          br(),
                                          conditionalPanel("input.radio_results_MFA == 1",
                                                           verbatimTextOutput("results_MFA_cor")
                                          )
                                 ),
                                 
                                 tabPanel("Groups representation",
                                          plotOutput("mfa_plot_grp"),
                                          br(),
                                          conditionalPanel("input.radio_results_MFA == 1",
                                                           verbatimTextOutput("results_MFA_grp")
                                          )
                                 ),
                                 
                                 tabPanel("Partial axes",
                                          plotOutput("mfa_plot_paraxe"),
                                          br(),
                                          conditionalPanel("input.radio_results_MFA == 1",
                                                           verbatimTextOutput("results_MFA_paraxe")
                                          )
                                 ),
                                 
                                 tabPanel("Partial individuals",
                                          plotOutput("mfa_plot_parind"),
                                          br(),
                                          conditionalPanel("input.radio_results_MFA == 1",
                                                           verbatimTextOutput("results_MFA_parind")
                                          )
                                 )
                                 
                               )
                        )
                        
                      )
             )
             
  )
)

