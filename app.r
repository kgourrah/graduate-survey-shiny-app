library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)

# Load data
load_data <- function() {
  data <- read.csv("graduate_survey.csv", stringsAsFactors = FALSE)
  
  # Preprocessing code from your previous answers
  relevant_columns <- c("Campus", "StudyField", "Branch", "Role", "EduLevel", "ProgLang", "Databases", "Platform", "WebFramework", "Industry", "AISearch", "AITool", "Employment")
  data <- data %>% select(all_of(relevant_columns))
  data$Campus <- str_replace_all(data$Campus, "(Durban|Umhlanga)", "Durban")
  data$StudyField <- str_replace_all(data$StudyField, "Data Science", "IT")
  campus_count <- data %>% group_by(Campus) %>% tally(sort = TRUE)
  top_campuses <- head(campus_count$Campus, 5)
  data <- data %>% filter(Campus %in% top_campuses)
  
  data <- data %>% mutate(EmploymentStatus = case_when(str_detect(Employment, "Employed, full-time|Employed, part-time|Independent contractor, freelancer, or self-employed") ~ "Employed", str_detect(Employment, "Not employed, but looking for work") ~ "Unemployed", TRUE ~ NA_character_))
  filtered_data <- data %>% filter(!is.na(EmploymentStatus))
  employment_rate <- filtered_data %>% group_by(StudyField) %>% summarise(TotalGraduates = n(), EmployedCount = sum(EmploymentStatus == "Employed"), UnemployedCount = sum(EmploymentStatus == "Unemployed"), EmploymentRate = EmployedCount / (EmployedCount + UnemployedCount))
  
  return(data)
}

data <- load_data()

# Helper function to split and count tools
split_and_count <- function(column_data) {
  tools <- strsplit(as.character(column_data), ";")
  tools <- unlist(lapply(tools, function(x) if (length(x) == 1 && x == "") character(0) else x))
  tools <- tools[tools != ""]
  tools <- table(tools) %>% sort(decreasing = TRUE)
  return(tools)
}

# UI
ui <- fluidPage(
  titlePanel("Eduvos Graduate Survey Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("studyFieldFilter", "Select Study Field:", choices = c("All", unique(data$StudyField)), selected = "All"),
      selectInput("campusFilter", "Select Campus:", choices = c("All", unique(data$Campus)), selected = "All")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Top Tools", plotOutput("topToolsPlot")),
        tabPanel("Industry Trends", plotOutput("industryTrendsPlot")),
        tabPanel("Job Roles", plotOutput("jobRolesPlot")),
        tabPanel("Employment Rates", plotOutput("employmentRatesPlot"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  filteredData <- reactive({
    localData <- data
    if (input$studyFieldFilter != "All") {
      localData <- localData[localData$StudyField == input$studyFieldFilter, ]
    }
    if (input$campusFilter != "All") {
      localData <- localData[localData$Campus == input$campusFilter, ]
    }
    return(localData)
  })
  
  output$topToolsPlot <- renderPlot({
    localData <- filteredData()
    prog_lang_tools <- split_and_count(localData$ProgLang)
    databases_tools <- split_and_count(localData$Databases)
    platform_tools <- split_and_count(localData$Platform)
    web_framework_tools <- split_and_count(localData$WebFramework)
    ai_search_tools <- split_and_count(localData$AISearch)
    ai_tool_tools <- split_and_count(localData$AITool)
    
    if (length(prog_lang_tools) == 0 && length(databases_tools) == 0 &&
        length(platform_tools) == 0 && length(web_framework_tools) == 0 &&
        length(ai_search_tools) == 0 && length(ai_tool_tools) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, "No data to display with selected filters.")
    } else {
      tools_df <- data.frame(
        Tool = c(names(prog_lang_tools), names(databases_tools), names(platform_tools), names(web_framework_tools), names(ai_search_tools), names(ai_tool_tools)),
        Count = c(as.numeric(prog_lang_tools), as.numeric(databases_tools), as.numeric(platform_tools), as.numeric(web_framework_tools), as.numeric(ai_search_tools), as.numeric(ai_tool_tools)),
        Category = rep(c("Programming Languages", "Databases", "Platforms", "Web Frameworks", "AI Search Tools", "AI Developer Tools"),
                       c(length(prog_lang_tools), length(databases_tools), length(platform_tools), length(web_framework_tools), length(ai_search_tools), length(ai_tool_tools)))
      )
      
      ggplot(tools_df %>% arrange(desc(Count)) %>% head(20), aes(x = reorder(Tool, Count), y = Count, fill = Category)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Top 20 Tools Used by Graduates", x = "Tool", y = "Frequency", fill = "Tool Category") +
        theme_minimal()
    }
  })
  
  output$industryTrendsPlot <- renderPlot({
    localData <- filteredData()
    industry_count <- localData %>% separate_rows(Industry, sep = ";") %>% group_by(StudyField, Industry) %>% tally(sort = TRUE)
    ggplot(industry_count, aes(x = reorder(Industry, n), y = n, fill = StudyField)) + geom_bar(stat = "identity") + coord_flip() + labs(title = "Most popular industries by study field", x = "Industry", y = "Number of graduates", fill = "Study Field") + theme_minimal()
  })
  
  output$jobRolesPlot <- renderPlot({
    localData <- filteredData()
    role_count <- localData %>% separate_rows(Role, sep = ";") %>% group_by(StudyField, Role) %>% tally(sort = TRUE)
    ggplot(role_count %>% arrange(desc(n)) %>% head(20), aes(x = reorder(Role, n), y = n, fill = StudyField)) + geom_bar(stat = "identity") + coord_flip() + labs(title = "Top 20 job roles by study field", x = "Role", y = "Number of graduates", fill = "Study Field") + theme_minimal()
  })
  
  output$employmentRatesPlot <- renderPlot({
    localData <- filteredData()
    localEmployment <- localData %>% mutate(EmploymentStatus = case_when(str_detect(Employment, "Employed, full-time|Employed, part-time|Independent contractor, freelancer, or self-employed") ~ "Employed", str_detect(Employment, "Not employed, but looking for work") ~ "Unemployed", TRUE ~ NA_character_))
    filteredEmployment <- localEmployment %>% filter(!is.na(EmploymentStatus))
    employmentRateLocal <- filteredEmployment %>% group_by(StudyField) %>% summarise(TotalGraduates = n(), EmployedCount = sum(EmploymentStatus == "Employed"), UnemployedCount = sum(EmploymentStatus == "Unemployed"), EmploymentRate = EmployedCount / (EmployedCount + UnemployedCount))
    ggplot(employmentRateLocal, aes(x = StudyField, y = EmploymentRate, fill = StudyField)) + geom_bar(stat = "identity") + labs(title = "Employment Rate by Study Field", x = "Study Field", y = "Employment Rate") + theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)