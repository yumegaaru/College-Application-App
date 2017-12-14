library(shiny)
library(shinydashboard)
library(gridExtra)
library(ggplot2)
library(stringr)
library(tibble)
library(tidyr)
library(dplyr)
library(purrr)
library(readr)
load("cleaned_gradcafe.Rdata")
load("wiki.Rdata")

##########get function
get_data = function(this_school, this_degree, 
                    this_student = c("American", "International with US degree", 
                                     "International without US degree")) {
  result = cleaned_gradcafe %>%
    filter(standard == this_school) %>%
    filter(degree == this_degree) %>%
    filter(student %in% this_student) %>%
    filter(status != "Interview")%>%
    filter(status != "Other")
  
  
  return(result)
}

## List of colleges
college_list = cleaned_gradcafe %>% 
  dplyr::select(standard) %>% unique() %>%  
  arrange((standard)) %>% 
  unname() %>% unlist()

## List of student types
stu_type_list = cleaned_gradcafe %>% 
  dplyr::select(student) %>% unique() %>%  
  arrange((student)) %>% unname() %>% unlist() %>% {.[1:3]}

## Body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "databoard",
            ## Row1: info boxes
            h4("Enrollment Rates:"),
            fluidRow(
              infoBoxOutput("acceptRateBox"),
              infoBoxOutput("waitRateBox"),
              infoBoxOutput("totalBox")
            ),
            
            h4("GRE Subject Test:"),
            fluidRow(
              infoBoxOutput("participantsBox"),
              infoBoxOutput("avgScoreBox"),
              infoBoxOutput("rangeScoreBox")
            ),
            
            ## Row2: 2 tabboxes
            fluidRow(
              tabBox(
                title = tagList(shiny::icon("graduation-cap"), "GPA"),
                selected = "Boxplot",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", height = "320px",
                tabPanel("Boxplot",
                         uiOutput("GPA")),
                tabPanel("Lineplot",
                         uiOutput("GPA_l")),
                tabPanel("Table", 
                         tableOutput("GPA_summary"))
              ),
              tabBox(
                title = tagList(shiny::icon("language"), "GRE Verbal"),
                selected = "Boxplot",
                height = "320px",
                tabPanel("Boxplot",
                         uiOutput("GRE_V")),
                tabPanel("Lineplot", 
                         uiOutput("GRE_V_l")),
                tabPanel("Table",
                         tableOutput("GRE_V_summary"))
              )
            ),
            
            ## Row3, 2 tabboxes
            fluidRow(
              tabBox(
                title = tagList(shiny::icon("cubes"), "GRE Quantitative"),
                selected = "Boxplot",
                height = "320px",
                tabPanel("Boxplot",
                         uiOutput("GRE_Q")),
                tabPanel("Lineplot",
                         uiOutput("GRE_Q_l")),
                tabPanel("Table",
                         tableOutput("GRE_Q_summary"))
              ),
              tabBox(
                title = tagList(shiny::icon("envelope-open"), "GRE Writing"),
                selected = "Boxplot",
                height = "320px",
                tabPanel("Boxplot",
                         uiOutput("GRE_W")),
                tabPanel("Lineplot",
                         uiOutput("GRE_W_l")),
                tabPanel("Table",
                         tableOutput("GRE_W_summary"))
              )
            )
    ),
    tabItem(tabName = "university",
            h2("University Introduction"),
            br(),
            uiOutput('logo'),
            br(),
            htmlOutput('name'),
            br(),
            htmlOutput('text'),
            br(),
            uiOutput('seal')
    )
  )
)

ui <- dashboardPage(
  dashboardHeader(title = "College Application"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Databoard", tabName = "databoard", icon = icon("th")),
      menuItem("Introduction", icon = icon("envelope-o"), tabName = "university"),
      
      ## school input
      selectInput("school", label = h4("Universities"), 
                  choices = college_list, selected = "Duke University"),
      
      ## program input
      selectInput("program", label = ("Program"), 
                  choices = list("Masters", "PhD"), selected = "Masters"),
      
      ## scores input
      checkboxInput("scores","Provide Your Scores?", value = FALSE),
      conditionalPanel(
        "input.scores == true",
        sliderInput("gpa", "GPA", min=1, max=4, value=1,step=0.1),
        # numericInput("gpa", "GPA", value=0, min=1, max=4),
        sliderInput("grev", "GRE Verbal", min=130, max=170, value=130,step=1),
        # numericInput("grev", "GRE Verbal", value=0, min=130, max=170),
        sliderInput("greq", "GRE Math", min=130, max=170, value=130,step=1),
        # numericInput("greq", "GRE Math", value=0, min=130, max=170),
        sliderInput("grew", "GRE Writing", min=1, max=6, value=1,step=0.5)
        # numericInput("grew", "GRE Writing", value=0, min=1, max=6)
      ),
      
      ## stu_type input
      checkboxInput("is_type","Specify Student type?", value = FALSE),
      conditionalPanel(
        "input.is_type == true",
        selectInput("stu_type", label = ("Student type"), 
                    choices = stu_type_list, selected = "American")
      ),
      
      ## action buttion
      actionButton("find", "Show Magic")
      
    )
  ),
  body
)

server <- function(input, output) {
  output$participantsBox <- renderInfoBox({
    infoBox(
      "Participants:", "--%", 
      icon = icon("id-card-o"), color = "light-blue"
    )
  })
  
  output$avgScoreBox <- renderInfoBox({
    infoBox(
      "Average Score:", "--", 
      icon = icon("institution"), color = "light-blue"
    )
  })
  
  output$rangeScoreBox <- renderInfoBox({
    infoBox(
      "Score Range:", "-- to --", 
      icon = icon("database"), color = "light-blue"
    )
  })
  
  output$acceptRateBox <- renderInfoBox({
    infoBox(
      "Accept Rate:", "--%", 
      icon = icon("trophy"), color = "light-blue"
    )
  })
  
  output$waitRateBox <- renderInfoBox({
    infoBox(
      "Waitlist Rate:", "--%", 
      icon = icon("question"), color = "light-blue"
    )
  })
  
  output$totalBox <- renderInfoBox({
    infoBox(
      "Applicants Provided:", "--", 
      icon = icon("users"), color = "light-blue"
    )
  })
  
  
  observeEvent(input$find, {
    
    ## Get Dataset
    if(input$is_type){
      data = get_data(input$school, input$program, input$stu_type)
    }else{
      data = get_data(input$school, input$program)
    }
    
    
    ## Boxplots
    p1 = ggplot(data %>% filter(!is.na(GPA)), 
                aes(x = as.factor(status), y = GPA, fill = as.factor(status)))+
      geom_boxplot()+
      xlab("Status")+
      ylab("GPA")+
      scale_fill_manual(values=c("lightpink", "lightblue", "lightgrey"))+
      theme_bw()+
      guides(fill=guide_legend(title="Status"))
    
    p2 = ggplot(data %>% filter(!is.na(GRE_General_V)), 
                aes(x = as.factor(status), y = GRE_General_V, fill = as.factor(status)))+
      geom_boxplot()+
      xlab("Status")+
      ylab("GRE General Verbal")+
      scale_fill_manual(values=c("lightpink", "lightblue", "lightgrey"))+
      guides(fill=guide_legend(title=NULL))+
      theme_bw()
    
    p3 = ggplot(data %>% filter(!is.na(GRE_General_Q)), 
                aes(x = as.factor(status), y = GRE_General_Q, fill = as.factor(status)))+
      geom_boxplot()+
      xlab("Status")+
      ylab("GRE General Quantitative")+
      scale_fill_manual(values=c("lightpink", "lightblue", "lightgrey"))+
      guides(fill=guide_legend(title=NULL))+
      theme_bw()
    
    p4 = ggplot(data %>% filter(!is.na(GRE_General_W)), 
                aes(x = as.factor(status), y = GRE_General_W, fill = as.factor(status)))+
      geom_boxplot()+
      xlab("Status")+
      ylab("GRE General Writing")+
      scale_fill_manual(values=c("lightpink", "lightblue", "lightgrey"))+
      guides(fill=guide_legend(title=NULL))+
      theme_bw()
    
    mean.dis = data %>% group_by(semester = as.factor(semester), status = as.factor(status)) %>%
      summarise(avg.gpa = mean(GPA, na.rm = TRUE),
                avg.grev = mean(GRE_General_V, na.rm=TRUE),
                avg.greq = mean(GRE_General_Q, na.rm=TRUE),
                avg.grew = mean(GRE_General_W, na.rm=TRUE)
      ) 
    
    ## Lineplots
    p5 = ggplot(mean.dis %>% filter(!is.na(avg.gpa)), 
                aes(x = semester, y = avg.gpa, color = status, group = status))+
      geom_line()+
      scale_color_manual(values=c("lightpink", "lightblue", "lightgrey"))+
      xlab("Semester")+
      ylab("Average GPA")+
      theme_bw()+
      guides(fill=guide_legend(title="Status"))
    
    p6 = ggplot(mean.dis %>% filter(!is.na(avg.grev)), 
                aes(x = semester, y = avg.grev, color = status, group = status))+
      geom_line()+
      scale_color_manual(values=c("lightpink", "lightblue", "lightgrey"))+
      xlab("Semester")+
      ylab("Average GRE Verbal")+
      theme_bw()+
      guides(fill=guide_legend(title="Status"))
    
    p7 = ggplot(mean.dis %>% filter(!is.na(avg.greq)), 
                aes(x = semester, y = avg.greq, color = status, group = status))+
      geom_line()+
      scale_color_manual(values=c("lightpink", "lightblue", "lightgrey"))+
      xlab("Semester")+
      ylab("Average GRE Quantitative")+
      theme_bw()+
      guides(fill=guide_legend(title="Status"))
    
    p8 = ggplot(mean.dis %>% filter(!is.na(avg.grew)), 
                aes(x = semester, y = avg.grew, color = status, group = status))+
      geom_line()+
      scale_color_manual(values=c("lightpink", "lightblue", "lightgrey"))+
      xlab("Semester")+
      ylab("Average GRE Writing")+
      theme_bw()+
      guides(fill=guide_legend(title="Status"))
    
    ## Add lines for "your score"
    if(input$scores){
      p1 = p1 + geom_hline(aes(yintercept=input$gpa), colour="#990000")
      p2 = p2 + geom_hline(aes(yintercept=input$grev), colour="#990000")
      p3 = p3 + geom_hline(aes(yintercept=input$greq), colour="#990000")
      p4 = p4 + geom_hline(aes(yintercept=input$grew), colour="#990000")
      p5 = p5 + geom_hline(aes(yintercept=input$gpa), colour="#990000")
      p6 = p6 + geom_hline(aes(yintercept=input$grev), colour="#990000")
      p7 = p7 + geom_hline(aes(yintercept=input$greq), colour="#990000")
      p8 = p8 + geom_hline(aes(yintercept=input$grew), colour="#990000")
    }
    
    ## Plot outputs
    
    output$GPA <- renderUI({
      if(nrow(data %>% filter(!is.na(GPA)))>0){
        output$myPlot_GPA <- renderPlot({
          p1
        })
        plotOutput("myPlot_GPA", height = "250px")  
      }else{
        img(src = "https://www.anychart.com/products/anychart/history/images/no-data-label.svg", height = "250px", align = "center")
      }
    })
    
    output$GRE_V <- renderUI({
      if(nrow(data %>% filter(!is.na(GPA)))>0){
        output$myPlot_GRE_V <- renderPlot({
          p2
        })
        plotOutput("myPlot_GRE_V", height = "250px")  
      }else{
        img(src = "https://www.anychart.com/products/anychart/history/images/no-data-label.svg", height = "250px", align = "center")
      }
    })
    
    output$GRE_Q <- renderUI({
      if(nrow(data %>% filter(!is.na(GPA)))>0){
        output$myPlot_GRE_Q <- renderPlot({
          p3
        })
        plotOutput("myPlot_GRE_Q", height = "250px")  
      }else{
        img(src = "https://www.anychart.com/products/anychart/history/images/no-data-label.svg", height = "250px", align = "center")
      }
    })
    
    output$GRE_W <- renderUI({
      if(nrow(data %>% filter(!is.na(GPA)))>0){
        output$myPlot_GRE_W <- renderPlot({
          p4
        })
        plotOutput("myPlot_GRE_W", height = "250px")  
      }else{
        img(src = "https://www.anychart.com/products/anychart/history/images/no-data-label.svg", height = "250px", align = "center")
      }
    })
    
    output$GPA_l <- renderUI({
      if(nrow(data %>% filter(!is.na(GPA)))>0){
        output$myPlot_GPA_l <- renderPlot({
          p5
        })
        plotOutput("myPlot_GPA_l", height = "250px")  
      }else{
        img(src = "https://www.anychart.com/products/anychart/history/images/no-data-label.svg", height = "250px", align = "center")
      }
    })
    
    output$GRE_V_l <- renderUI({
      if(nrow(data %>% filter(!is.na(GPA)))>0){
        output$myPlot_GRE_V_l <- renderPlot({
          p6
        })
        plotOutput("myPlot_GRE_V_l", height = "250px")  
      }else{
        img(src = "https://www.anychart.com/products/anychart/history/images/no-data-label.svg", height = "250px", align = "center")
      }
    })
    
    output$GRE_Q_l <- renderUI({
      if(nrow(data %>% filter(!is.na(GPA)))>0){
        output$myPlot_GRE_Q_l <- renderPlot({
          p7
        })
        plotOutput("myPlot_GRE_Q_l", height = "250px")  
      }else{
        img(src = "https://www.anychart.com/products/anychart/history/images/no-data-label.svg", height = "250px", align = "center")
      }
    })
    
    output$GRE_W_l <- renderUI({
      if(nrow(data %>% filter(!is.na(GPA)))>0){
        output$myPlot_GRE_W_l <- renderPlot({
          p8
        })
        plotOutput("myPlot_GRE_W_l", height = "250px")  
      }else{
        img(src = "https://www.anychart.com/products/anychart/history/images/no-data-label.svg", height = "250px", align = "center")
      }
    })
    
    output$GPA_summary <- renderTable(
      data %>% 
        group_by(status) %>%
        summarize(min = min(GPA, na.rm = TRUE), median = median(GPA, na.rm = TRUE), max = max(GPA, na.rm = TRUE)) %>%
        as.data.frame()
    )
    
    output$GRE_V_summary <- renderTable(
      data %>% 
        group_by(status) %>%
        summarize(min = min(GRE_General_V, na.rm = TRUE), median = median(GRE_General_V, na.rm = TRUE), max = max(GRE_General_V, na.rm = TRUE)) %>%
        as.data.frame()
    )
    
    output$GRE_Q_summary <- renderTable(
      data %>% 
        group_by(status) %>%
        summarize(min = min(GRE_General_Q, na.rm = TRUE), median = median(GRE_General_Q, na.rm = TRUE), max = max(GRE_General_Q, na.rm = TRUE)) %>%
        as.data.frame()
    )
    
    output$GRE_W_summary <- renderTable(
      data %>% 
        group_by(status) %>%
        summarize(min = min(GRE_General_W, na.rm = TRUE), median = median(GRE_General_W, na.rm = TRUE), max = max(GRE_General_W, na.rm = TRUE)) %>%
        as.data.frame()
    )
    
    ## Calculate Gre Subject participant rate, avg score, and accept rate
    total_df = cleaned_gradcafe %>% 
      filter(standard==input$school, degree==input$program)
    if(input$is_type){
      total_df = total_df %>% filter(student==input$stu_type)
    }
    greSubject = total_df %>% 
      filter(!is.na(GRE_Subject)) %>% 
      summarize(count = n(), avg = mean(GRE_Subject), 
                min_score = min(GRE_Subject), max_score = max(GRE_Subject))
    total_num = total_df %>% nrow()
    greSub_perc = round((greSubject$count)*100/total_num, digits=2)
    greSub_perc = ifelse(is.na(greSub_perc), "--", as.character(greSub_perc))
    greSub_score = ifelse(is.na(greSubject$avg), "--", 
                          as.character(round(greSubject$avg, digits=2)))
    greSub_min = ifelse(is.na(greSubject$min_score) | is.infinite(greSubject$min_score), "--", 
                        as.character(round(greSubject$min_score, digits=2)))
    greSub_max = ifelse(is.na(greSubject$max_score) | is.infinite(greSubject$max_score), "--", 
                        as.character(round(greSubject$max_score, digits=2)))
    accept_num = total_df %>% filter(status=="Accepted") %>% nrow()
    accept_rate = ifelse(is.na(accept_num/total_num), "--",
                         as.character(round(accept_num*100/total_num, digits=2)))
    wait_num = total_df %>% filter(status=="Wait listed") %>% nrow()
    wait_rate = ifelse(is.na(wait_num/total_num), "--",
                       as.character(round(wait_num*100/total_num, digits=2)))
    
    ## infoBox for Gre subject rate
    output$participantsBox <- renderInfoBox({
      infoBox(
        "Participants:", paste0(greSub_perc, "%"), 
        icon = icon("id-card-o"), color = "light-blue"
      )
    })
    
    ## info Box for Gre Subject score
    output$avgScoreBox <- renderInfoBox({
      infoBox(
        "Average Score:", greSub_score, 
        icon = icon("institution"), color = "light-blue"
      )
    })
    
    ## info Box for Gre Score Range
    output$rangeScoreBox <- renderInfoBox({
      infoBox(
        "Score Range:", paste0(greSub_min, " to ", greSub_max), 
        icon = icon("database"), color = "light-blue"
      )
    })
    
    ## info Box for accept rate
    output$acceptRateBox <- renderInfoBox({
      infoBox(
        "Accept Rate:", paste0(accept_rate, "%"), 
        icon = icon("trophy"), color = "light-blue"
      )
    })
    
    ## info Box for waitlist rate
    output$waitRateBox <- renderInfoBox({
      infoBox(
        "Waitlist Rate:", paste0(wait_rate, "%"), 
        icon = icon("question"), color = "light-blue"
      )
    })
    
    ## info Box for total number
    output$totalBox <- renderInfoBox({
      infoBox(
        "Applicants Provided:", total_num, 
        icon = icon("users"), color = "light-blue"
      )
    })
    
    ## University Introduction
    intro = wiki %>% filter(college == input$school) %>% 
      select(introduction) %>% unname() %>% unlist()
    logo_url = wiki %>% filter(college == input$school) %>% 
      select(logo_url) %>% unname() %>% unlist()
    seal_url = wiki %>% filter(college == input$school) %>% 
      select(seal_url) %>% unname() %>% unlist()
    school_name = input$school
    output$name <- renderUI({
      h4(school_name)
    })
    output$text <- renderUI({
      HTML(intro)
    })
    output$logo <- renderUI({
      img(src = logo_url,
          align = "center")
    })
    output$seal <- renderUI({
      img(src = seal_url,
          align = "center")
    })
    
    
  },ignoreInit = TRUE)
}

shinyApp(ui, server)





