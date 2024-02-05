library(shiny)
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyverse)
library(lubridate)
library(shinyWidgets)
library(readxl)

###'*PREPROCESSING*###

#'*FUNCTIONAL*
#Directory reader
read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

timesheets_appended <-
  list.files(pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_plus(.))

timesheet_directory <- 'C:/Users/tommy/Desktop/Projects/R/Timesheet Analysis Unit'

#'*FUNCTIONAL*
#Remove unnecessary rows and columns
timesheets_appended <- timesheets_appended[!(timesheets_appended$Date == "" | is.na(timesheets_appended$Date)), ]

timesheets_appended <- select(timesheets_appended, -c(
  X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, 'Name of Company'))

#'*FUNCTIONAL*
#Rename spacious columns
timesheets_appended <- timesheets_appended %>%
  rename("hours_worked" = `# of Hours Worked`, 
         "hours_sick" = `# of Hours Sick`, 
         "hours_vacation" = `# of Hours Annual Leave`, 
         "activity_descrip" = `Activity Description (Choose from Drop-Down List for Hours Worked)`,
         "project_code" = `Charge to Project / Service`,
         "serial_num" = `#`)

#'*FUNCTIONAL*
#Decoding abbreviations
timesheets_appended <- timesheets_appended %>%  
  mutate(Name = ifelse(filename %in% c(grep("* TM *", filename, value=T)), "Tomas Momcilovic",NA))

#'*FUNCTIONAL*
#Make a name list except for NA or ?? values
names_list <- unique(timesheets_appended$Name, incomparables = FALSE)
remove <- c("??",NA)
names_list [! names_list %in% remove]

#'*FUNCTIONAL*
#Clean and reformat dates
timesheets_appended$Date <- 
  as.Date(timesheets_appended$Date,
          ifelse(timesheets_appended$Date %in% c(grep("*/*/201*", timesheets_appended$Date, value=T)), "%m/%d/%Y", 
          ifelse(timesheets_appended$Date %in% c(grep("*/*/1*", timesheets_appended$Date, value=T)), "%m/%d/%y",
           "%d-%b-%y")))

#'*FUNCTIONAL*
#Add year, month, and week columns
timesheets_appended$Month <- format(timesheets_appended$Date,"%B")
timesheets_appended$Year <- format(timesheets_appended$Date,"%Y")
timesheets_appended$Week <- isoweek(timesheets_appended$Date)


#'@Code_just_in_case: timesheets_appended2$Date <- gsub("/", "-", timesheets_appended2$Date)


###'*SHINY APP*###


##USER INTERFACE SIDE##
ui <- fluidPage(
  titlePanel("Timesheet Assessment Unit"),
  sidebarLayout(
    sidebarPanel(
      
      #Input file for expected hours per week
      fileInput('file1', 'Upload file for expected hours to be worked (hours_expected)',
                accept = c('.xlsx')
                ),
      
      #Input file for employee salaries
      fileInput('file2', 'Upload file for salaries (salaries_all)',
                accept = c('.xlsx')
                ),
      
      #Gives the actual number of timesheet files
      tableOutput('timesheet_count'),
      
      #Provides employee selection list
      pickerInput('employee', "Choose the employee", choices = names_list, options = list(`actions-box` = TRUE,`deselect-all-text` = "Select None",`select-all-text` = "Select All",`none-selected-text` = "(!) No Employee Selected (!)"), multiple = TRUE, selected = "Tomas Momcilovic"), 
      
      #Displays the number of days per category for chosen employee
      tableOutput('employee_days'),
      
      #Provides date selection calendar range
      dateRangeInput('date', "Choose the period", min = min(timesheets_appended$Date, na.rm = TRUE), start = '2018-1-1', end = max(timesheets_appended$Date, na.rm = TRUE), autoclose = TRUE),
      
      #Provides a selection of calculations to be applied to timesheets
      pickerInput('calculations',"Select calculations", 
                  choices = c("Salary Weights", "Per Week Comparison", "Per Month Comparison", 
                              "Overtime", "Project Charge", "Request Overflow"), multiple = TRUE, selected = "Per Week Comparison",
                  options = list(`actions-box` = TRUE,`deselect-all-text` = "Select None",`select-all-text` = "Select All",`none-selected-text` = "(!) No Calculation Selected (!)"),)
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Main",
                 #main analysis table
                 tableOutput('timesheet_calcs'),
                 #Gives the overview of all employees covered by these timesheets
                 plotOutput('employee_overview'),
                 #Provides pie chart for hours worked/sick/vacation
                 plotOutput('pie_comparative'),
                 #Provides year-month table
                 tableOutput('year_month')),
        tabPanel("Workload",
                 #Provides the plot for workload per month/year
                 plotOutput('workload')),
        tabPanel("Leftover",
                 #Provides the table of employee hours
                 tableOutput('employee_hours')),
        tabPanel("Week & Month",
                 #calc for per week comparison
                 tableOutput('per_week_comp'),
                 #calc for per month comparison
                 tableOutput('per_month_comp')),
        tabPanel("Salary & Overtime",
                 #calc for salary weights
                 tableOutput('salary_weights'),
                 #calc for overtime
                 tableOutput('overtime_calc')),
        tabPanel("Projects & Requests",
                 #calc for project charge
                 tableOutput('project_charge'),
                 #calc for overflow of requests
                 tableOutput('request_overflow'))
        )
      )
    )
  )

##SERVER SIDE##
server <- function(input, output, session) {

  #'*FUNCTIONAL*
  #table for number of timesheets imported
  output$timesheet_count <- renderTable({
    timesheet_all <- timesheets_appended %>%
      summarise("Imported:" = n_distinct(filename))
    timesheet_all$Earliest <- as.character(as.Date(min(timesheets_appended$Date, na.rm = TRUE), format="%d-%m-%Y"))
    timesheet_all$Latest <- as.character(as.Date(max(timesheets_appended$Date, na.rm = TRUE), format="%d-%m-%Y"))
    timesheet_all
  }, striped = T, hover = T, spacing = "xs", digits = 0)
  
  #'*FUNCTIONAL*
  #table for total employee days
  output$employee_days <- renderTable({
    timesheet_breakdown <- timesheets_appended %>%
      filter(Name == input$employee) %>%
      summarise_at(funs(sum),.vars = c(Worked = "hours_worked",Sick = "hours_sick",Vacation = "hours_vacation"), na.rm=TRUE) / 8
    timesheet_breakdown
  }, striped = T, hover = T, spacing = "xs", width ="5cm", digits = 1)

  #'*FUNCTIONAL; AVERAGE NEEDS FIXING*
  #plot for workload of all employees per month and year
  output$workload <- renderPlot({
    timesheet_workload <- timesheets_appended %>%
      mutate(month2 = as.Date(paste0("2000-", month(timesheets_appended$Date),"-01"),"%Y-%m-%d")) %>%
      filter(Date >= as.Date("2017-1-1")) %>%
      filter(Date <= as.Date("2019-12-31")) %>%
      group_by(Year,month2) %>%
      summarise(Hrs = mean(hours_worked,na.rm = TRUE)) %>%
      ggplot(aes(x = month2, y = Hrs), group = Year) +
      geom_line(aes(color = Year)) +
      geom_point(size = 2) +
      labs(
        colour = "Years",
        title = "Average Workload per Month, 2017-2019",
        x = "Month",
        y = "Number of Hours Worked") +
      scale_x_date(date_labels = "%B")
    timesheet_workload
  })
  
  #'*FUNCTIONAL*  
  #plot for employee hours in plot
  output$employee_overview <- renderPlot({
    timesheet_emp_num <- timesheets_appended %>%
      group_by(Name) %>%
      summarize(unique_files = n_distinct(filename)) %>% 
      ggplot(mapping = aes(x = unique_files, y = reorder(Name, unique_files))) + 
      geom_col() + geom_label(aes(label = unique_files))
    timesheet_emp_num
  })

  #'*FUNCTIONAL*
  #piechart for all worked v. sick v. vacation
  output$pie_comparative <- renderPlot({
    time_pie <- data.frame(
      Hours = c(sum(timesheets_appended$hours_worked, na.rm = TRUE),sum(timesheets_appended$hours_sick, na.rm = TRUE),sum(timesheets_appended$hours_vacation, na.rm = TRUE)),
      Type = c("hours_worked", "hours_sick", "hours_vacation"))
    
    time_pie <- time_pie %>% 
      mutate(per=Hours/sum(Hours))
    time_pie$label <- scales::percent(time_pie$per)

    time_pie <- time_pie %>% 
      arrange(desc(Type)) %>%
      mutate(ypos = cumsum(per*100)- 0.5*(per*100))
        
    timesheet_piechart <- time_pie %>%
      ggplot(aes(x = "", y = Hours, fill = Type)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start = 2) +
      theme_void() +
      theme(legend.position="top") +
      geom_label(aes(label=label), color="white", size = 6, position = position_stack(vjust = 0.5)) +
      scale_fill_brewer(palette="Set2") +
      labs(fill = "Type",
           x = NULL,
           y = NULL,
           title = "Hours Worked, Sick & Vacation")
    timesheet_piechart
  })

  #'*FUNCTIONAL*
  #table for all worked on a specific day
  output$employee_hours <- renderTable({
    timesheet_emp_hrs <- timesheets_appended %>%
      filter(Name == input$employee) %>%
      filter(Date %in% seq(from = input$date[1], to = input$date[2], by = "day")) %>%
      group_by(Date) %>%
      summarise(sum = sum(hours_worked, na.rm = TRUE))
    timesheet_emp_hrs$Date <- as.character(timesheet_emp_hrs$Date)
    head(timesheet_emp_hrs)
  }, striped = T, hover = T, spacing = "xs", digits = 1)
  
  #'*FUNCTIONAL*
  #table for whole year overview
  output$year_month <- renderTable({
    timesheet_ym <- timesheets_appended %>%
      group_by(Year, Month) %>%
      summarise(Total = sum(hours_worked, na.rm = TRUE)) %>%
      arrange(desc(Year), desc(Month))
    timesheet_ym
  }, striped = T, hover = T, spacing = "xs", width ="5cm")

  #'*FUNCTIONAL*
  #table for per week comparison
  output$per_week_comp <- renderTable({
    timesheet_pwc <- timesheets_appended %>% 
      filter(Name == input$employee) %>%
      filter(Date %in% seq(from = input$date[1], to = input$date[2], by = "day")) %>%
      group_by(Year,Month,Week,Name) %>% 
      summarise_at(funs(sum),.vars=c(Worked="hours_worked",Sick="hours_sick",Vacation="hours_vacation"),na.rm=TRUE) %>%
      mutate(Total = sum(Worked, Sick, Vacation, na.rm = TRUE))
    timesheet_pwc
  }, striped = T, hover = T, spacing = "xs", width ="5cm")
  
  #'*FUNCTIONAL*
  #table for per month comparison
  output$per_month_comp <- renderTable({
    timesheet_pmc <- timesheets_appended %>%
      filter(Name == input$employee) %>%
      filter(Date %in% seq(from = input$date[1], to = input$date[2], by = "day")) %>%
      group_by(Year,Month,Name) %>% 
      summarise_at(funs(sum),.vars=c(Worked="hours_worked",Sick="hours_sick",Vacation="hours_vacation"),na.rm=TRUE) %>%
      mutate(Total = sum(Worked, Sick, Vacation, na.rm = TRUE))
    timesheet_pmc
  }, striped = T, hover = T, spacing = "xs", width ="5cm")

  #'*UNKNOWN DATAPATH PROBLEM WITH READ_EXCEL*
  #table for salary weights
  output$salary_weights <- renderTable({
    salaries_all <- input$file2
    
    if (is.null(salaries_all))
      return(NULL)
    
    read_excel(paste(salaries_all$datapath, ".xlsx", sep=""), 1)
    
    timesheet_sw <- timesheets_appended %>%
      filter(Name == input$employee) %>%
      filter(salaries_all$Name == input$employee) %>%
      filter(Date %in% seq(from = input$date[1], to = input$date[2], by = "day")) %>%
      group_by(Name, Net_Salary = salaries_all$Net_Salary) %>%
      summarise('Per_Hour' = average(hours_worked / unique(Week)) * Net_Salary)
    timesheet_sw
  })
  
  #'*CODE NEEDS FIXING*
  #table for overtime calculations
  output$overtime_calc <- renderTable({
    hours_expected <- input$file1
    
    if (is.null(hours_expected))
      return(NULL)
    
    read_excel(paste(hours_expected$datapath, ".xlsx", sep=""), 1)
    
    hours_expected$Week <- isoweek(hours_expected$Date)

    hours_expected$Date <- as.Date(hours_expected$Date)
    
    leftie <- left_join(x = timesheets_appended, y = hours_expected, by = "Date", suffix = c(".w",".e"))
    
    timesheet_oc <- leftie %>%
      filter(Name == input$employee) %>%
      filter(Date == Date %in% seq(from = input$date[1], to = input$date[2], by = "day")) %>%
      group_by(Name,Year,Month,Week) %>%
      summarise(
        Worked = sum(hours_worked,na.rm = TRUE),
        Expected = sum(hours_2B_work, na.rm = TRUE),
        Difference = Worked - Expected)
    timesheet_oc
  })
  
  #'*NEEDS TESTING*
  #table for project charge calc
  output$project_charge <- renderTable({
    timesheet_pc <- timesheets_appended %>%
      filter(Name == input$employee) %>%
      filter(Date == Date %in% seq(from = input$date[1], to = input$date[2], by = "day")) %>%
      group_by(project_code) %>%
      summarise(sum(hours_worked, na.rm = TRUE)) %>%
      summarise(hours_invested = sum(hours_worked, na.rm = TRUE))
    timesheet_pc
  })

  #'*NEEDS SHINY TESTING*
  #table for measuring request overflow (needs more keywords!)  
  output$request_overflow <- renderTable({
    timesheet_ro <- timesheets_appended %>%
      filter(Name == input$employee) %>%
      filter(grepl('request',Notes)) %>%
      filter(Date == Date %in% seq(from = input$date[1], to = input$date[2], by = "day")) %>%
      group_by(Name) %>%
      summarise(Invested = sum(hours_worked, na.rm = TRUE))
    timesheet_ro
  })
    
  #'*TO BE DEVELOPED*
  #table for measuring amount of hours going on staff and team meetings
  
  #'*TO BE DEVELOPED*
  #table for measuring amount of hours going on revisions and mission creep
}

##START THE APP##
shinyApp(ui = ui, server = server)
