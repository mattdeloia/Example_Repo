library(tidyverse)
library(corrplot)
library(rstatix)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(psych)
library(GPArotation)
library(ggdendro)

# getwd()
setwd("C:/Users/M49851/Documents/Projects/CEMA Project_DrGraves/MSLQ_App/MSLQ_ShinyApp/MSLQ_Dashboard")

#read data
scale_alphas <- read_rds("mslq_scale_alphas.rds")
df_itemcorr_spearman <- read_rds("item_correlations_spearman.rds")
df_itemcorr_pearson <- read_rds("item_correlations_pearson.rds")
df_item <- read_rds("df_item.rds") %>% 
  left_join(
    read_rds("InfitAve_MOTIV.rds") %>% 
      rbind(read_rds("InfitAve_LS.rds")))

df_scale <- read_rds("scale_results.rds")
#####


df_scalecorr <- read_rds("scale_correlations.rds")
df_distmatrix <- read_rds("scaledistance_matrix.rds")
#write.csv(df_scalecorr %>% as.data.frame() %>% mutate_if(is.numeric, ~round(.x, 2)), "scale_correlations.csv")
write.csv(df_item %>% mutate_if(is.numeric, ~round(.x, 3)), "factor_analysis_itemresults.csv")
write.csv(df_scale %>% mutate_if(is.numeric, ~round(.x, 3)), "factor_analysis_scaleresults.csv")
item_mean <- mean(df_item$mean)
item_sd <- mean(df_item$sd)
df_item %>% gather(mean:loading, key=measure, value=score) %>% 
  filter(measure =="kurtosis") %>% ggplot(aes(x=measure, y=score, label=item)) + geom_boxplot()  + ggrepel::geom_text_repel() +facet_wrap(measure~., scales = "free")
df_scale %>% gather(mean:alpha, key=measure2, value=score) %>% 
  filter(measure2 =="alpha") %>% ggplot(aes(x=measure2, y=score, label=measure)) + geom_boxplot()  + ggrepel::geom_text_repel() +facet_wrap(measure2~., scales = "free")




Motivation <- c("Intrinsic_Goal_Orientation", "Extrinsic_Goal_Orientation", "Task_Value", 'Control_Beliefs_about_Learning', 'Selfefficacy_for_Learning_and_Performance', "Test_Anxiety")
Learning_Strategy <- c('Rehearsal', 'Elaboration', 'Organization', 'Critical_Thinking', 'Metacognitive_Selfregulation', 'Time_Study_Environment', 'Effort_Regulation', 'Peer_Learning', 'Help_Seeking')
Value <- c("Intrinsic_Goal_Orientation", "Extrinsic_Goal_Orientation", "Task_Value")
Expectancy <- c('Control_Beliefs_about_Learning', 'Selfefficacy_for_Learning_and_Performance')
Affective <- "Test_Anxiety"
Cognitive  <- c('Rehearsal', 'Elaboration', 'Organization', 'Critical_Thinking', 'Metacognitive_Selfregulation')
Resource_Mgmt <- c('Time_Study_Environment', 'Effort_Regulation', 'Peer_Learning', 'Help_Seeking')
all <- c(Motivation, Learning_Strategy)

all_scales <-  Motivation %>% as.data.frame() %>% 
  mutate(category ="Motivation") %>% 
  bind_rows(Learning_Strategy %>% 
              as.data.frame() %>% 
              mutate(category ="Learning Strategy")) %>% 
  dplyr::rename(scale = 1) 
  
#############################

ui <- dashboardPage(
    
    # Application title
    dashboardHeader(title = "MSLQ Factor Analysis"),
    
    dashboardSidebar( width = 0
   
    ),
    
    dashboardBody( 
      
      tabsetPanel(
        
  
             
                    tabPanel(title = "Confirmatory Factor Analysis",
                             h5("Use the Tooltip to select scales and adjust the threshold of poor item loadings colored RED.  Cronbach alphas measure the internal consistency of scale items."),
                             h5("- Draw a Box - around items for more information. The BLUE dashed lines indicated overall mean values."),
                             dropdownButton(
                               tags$h3("List of Input"),
                        
                               sliderInput(inputId = 'load_thresh', label = 'Loading threshold:', value = .4, min = 0, max = 1, step =.1),
                               h6("*Loading values over .4 suggest ok fit, over .7 good fit"),
                               sliderInput(inputId = 'overfit_thresh', label = 'Overfit threshold:', value = .85, min = .75, max = 1, step=.05),
                               h6("*Ave. Infit values below .75 suggest Overfitting of all categories"),
                               
                               sliderInput(inputId = 'underfit_thresh', label = 'Underfit threshold:', value = 1.15, min = 1, max = 1.25, step =.05),
                               h6("*Ave. Infit values above 1.25 suggest Underfitting of all categories"),
                               radioButtons("scale_category", "Scale Category", choices = c("Motivation", "Learning Strategy"), selected = "Motivation"),
                               checkboxGroupInput("item_scales", "Scales to Review:", choices = NULL, selected = NULL),

                               circle = TRUE, status = "danger", icon = icon("cog"), width = "300px",
                               tooltip = tooltipOptions(title = "Click to see inputs!")
                             ),
                             fluidPage(
                             plotOutput("item_plot", brush = "plot_brush"),
                             tableOutput("data")
                             )
                             ),
                    
                    tabPanel("Scale Comparison",
                             
                             tabsetPanel(
                                tabPanel(title="Correlation Plot",
                                         sliderInput(inputId = 'clusters', label = '# of clusters or scale groupings to draw:', value = 8, min = 1, max = 15),
                                         plotOutput("corrplot", height = "600px")
                             ),
                             tabPanel(title="Hierarchical Clustering",
                                      sliderInput(inputId = "cutoff", label = "Dendrogram Cutoff Value:", value=20, min=10, max=25, step = .25),
                                      plotOutput("clusterplot", height="600px"))
                             )),
                    
                    tabPanel(title = "Exploratory Factor Analysis",
                             fluidRow(
                             box(width=12,
                                 radioButtons("corr_type", "Correlation Method for efa:", choices = c("Pearson", "Spearman"), selected = "Pearson"),
                               sliderInput(inputId = 'factors', label = '# of Factors for efa (scree plot analysis suggests 8 factors):', value = 8, min = 0, max = 20),
                               verbatimTextOutput("eval"),
                               br(),
                               h4("Table listing the TOTAL number of scale items aligned to each factor."),
                               dataTableOutput("efa_table")
                               )
                             ),
                             
                             fluidRow(
                               box(width = 12,
                                   h4("Table of scale items by principal components or factors."),
                               dataTableOutput("efa_table2")
                               )
                             )
                    )
      )
    )
)                       
    
server <- function(input, output, session) {
  
  observe(showNotification("Created by Peraton for Army Analysis", duration = 15))
  
  category_scales <- reactive({filter(all_scales, category ==input$scale_category)})
  
  observeEvent(category_scales(), {
    choices <- category_scales()$scale
    updateCheckboxGroupInput(inputId = "item_scales", choices = choices, selected = choices)
  })
  
  df_itemplot <- reactive({
    df_item %>% 
      mutate(loading2 = if_else (loading <= input$load_thresh, "low","normal")) %>%
      mutate(infit = if_else(infit_ave<=input$overfit_thresh, "overfitting",
                             if_else(infit_ave<=input$underfit_thresh, "good", "underfitting"))) %>% 
      filter (scale %in% input$item_scales) %>% 
      left_join(scale_alphas) %>% 
      mutate(scale = paste(scale, " (alpha=", round(alpha,2), ")", sep="")) %>% 
      dplyr::select(-alpha) %>% 
      arrange(loading)
  })
  
   output$item_plot <- renderPlot({
     df_itemplot() %>% 
        ggplot(aes(x=mean, y=sd, color=loading2, shape=infit)) +
        geom_point(size=3) +
        scale_color_manual(values=c("low" = "red", "normal"="darkgray")) +
       scale_shape_manual(values=c("underfitting" = 8, "good"=16, "overfitting"=8)) +
        geom_vline(xintercept = item_mean, linetype="dashed", color="blue") +
        geom_hline(yintercept = item_sd, linetype="dashed", color="blue") +
        theme(legend.position = "blank", strip.text = element_text(size=12) ) +
        facet_wrap(scale~., scales = "free")+
        xlim(2.5,7) + ylim (.5,2) +
        ylab("item score sd") + 
        xlab("item mean score") 
   })
    
    output$data <- renderTable ({
      
      brushedPoints(df_itemplot() , input$plot_brush) %>% select(-scale, -loading2) 
    })
    
    output$corrplot <- renderPlot({
      
      corrplot(df_scalecorr, method="color", order="hclust", type="full", addrect=input$clusters, cl.lim=c(-1,1), addCoef.col="black", rect.col="green", diag = FALSE, number.font=1, number.digits = 1, number.cex = .7, cl.cex = .5)
               
    })
    
    output$clusterplot <- renderPlot({
      hclust(df_distmatrix) %>% 
        ggdendrogram(rotate = TRUE, size=3) +
        ggtitle("Hierarchical Clustering of Scales")+
        geom_hline(yintercept = input$cutoff, linetype = "dashed", color="blue")
      
    })
    

    df_itemcorr <- reactive({
      if(input$corr_type=="Pearson"){
        df_itemcorr <- df_itemcorr_pearson}
      else {
        df_itemcorr <- df_itemcorr_spearman
      }
      
    })
    
  #factor analysis solution
  solution <- reactive ({
    fa(r=df_itemcorr(), 
                 nfactors = input$factors, rotat="oblimin", fm="minres")
  })
  
  #results data frame
  df_efa <- reactive({
    solution()[["loadings"]] %>% 
    as.matrix.data.frame() %>% 
    as.data.frame() %>% 
    cbind(solution()$weights %>% 
            as.data.frame() %>% 
            rownames_to_column("item") %>% 
            dplyr::select(item)) %>% 
    dplyr::select(item, everything()) %>% 
    gather(2:(input$factors+1), key=factor, value=factor_loading) %>% 
    dplyr::group_by(item) %>% 
    mutate(rank = rank(-1*abs(factor_loading))) %>% 
    filter(rank<=1) %>% 
    arrange(factor, -factor_loading) %>% 
      mutate(factor_loading = round(factor_loading,3)) %>%
      left_join(df_item) %>% 
      arrange(factor, -factor_loading) %>% 
      dplyr::select(factor, item, factor_loading, scale, content) 
      
  })
  
  output$efa_table <- renderDataTable({
    df_efa()  %>%
      mutate(component = if_else(scale %in% Value, "Value",
                         if_else(scale %in% Expectancy, "Expectancy",
                         if_else(scale %in% Affective, "Affective",
                         if_else(scale %in% Cognitive, "Cognitive",
                         if_else(scale %in% Resource_Mgmt, "Resource_Mgmt", "NA")))))) %>%
      mutate(category = if_else(scale %in% Motivation, "Motivation", "Learning_Strategy")) %>%
      dplyr::group_by(factor, scale, component, category) %>%
      dplyr::summarise(count = n()) %>%
      pivot_wider(names_from = "factor", values_from = "count") %>%
      mutate_if(is.numeric, ~replace_na(.x, "-"))

  })
    
  output$efa_table2 <- renderDataTable({df_efa()})
  #model fit metrics
  
   eval <- reactive({
     cbind(solution()$rms, solution()$fit) %>% as.data.frame() %>% 
    mutate_if(is.numeric, ~round(.x, 3)) %>%
    rename(RMS = 1, Fit =2) 
   })
   
  output$eval <- renderPrint({
    eval <- paste("RMS for ", input$factors, "factors = ", eval()$RMS, "; Model Fit = ", eval()$Fit )
    eval
    })
  
}
  
# Run the application 
shinyApp(ui = ui, server = server)
