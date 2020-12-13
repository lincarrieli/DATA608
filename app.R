library(ggplot2)
library(dplyr)
library(shiny)
library(tidyr)

# Data tyding and transformation

df <- read.csv("https://raw.githubusercontent.com/lincarrieli/DATA608/master/Children_Under_6_yrs_with_Elevated_Blood_Lead_Levels__BLL_.csv")

# remove NOTES columns
df1 <- df %>% select(-contains("NOTES"))

# rename columns
df1 <- df1 %>% 
    rename(
        BLL5μg_dL = Children.under.6.years.with.elevated.blood.lead.levels..BLL..Number.BLL...5.µg.dL,
        BLL10μg_dL = Children.under.6.years.with.elevated.blood.lead.levels..BLL..Number.BLL..10.µg.dL,
        BLL15μg_dL = Children.under.6.years.with.elevated.blood.lead.levels..BLL..Number.BLL..15.µg.dL,
        NumTested = Children.under.6.years.with.elevated.blood.lead.levels..BLL..Number.Tested,
        BLL5μg_dL_per1000tested = Children.under.6.years.with.elevated.blood.lead.levels..BLL..Rate..BLL..5.µg.dL.per.1.000.tested,
        BLL10μg_dL_per1000tested = Children.under.6.years.with.elevated.blood.lead.levels..BLL..Rate.BLL..10.µg.dL.per.1.000.tested,
        BLL15μg_dL_per1000tested = Children.under.6.years.with.elevated.blood.lead.levels..BLL..Rate.BLL..15.µg.dL.per.1.000.tested,
        Year = time_period
        
    )
# convert borough ID into borough names
df1 <- df1 %>%
    mutate(Borough = case_when(borough_id == 1 ~ 'Bronx',
                               borough_id == 2 ~ 'Brooklyn',
                               borough_id == 3 ~ 'Manhattan',
                               borough_id == 4 ~ 'Queens',
                               borough_id == 5 ~ 'Staten Island'))

# select and change column names
dfBLL5 <- df1 %>% select(Year, BLL5μg_dL, Borough, NumTested, BLL5μg_dL_per1000tested)
colnames(dfBLL5)[which(names(dfBLL5) == "BLL5μg_dL")] <- "Cases"
colnames(dfBLL5)[which(names(dfBLL5) == "BLL5μg_dL_per1000tested")] <- "per1000tested"
dfBLL5['BLL'] <- '5μg_dL'
dfBLL5['BLLper1000tested'] <- '5μg_dL'

dfBLL10 <- df1 %>% select(Year, BLL10μg_dL, Borough, NumTested, BLL10μg_dL_per1000tested)
colnames(dfBLL10)[which(names(dfBLL10) == "BLL10μg_dL")] <- "Cases"
colnames(dfBLL10)[which(names(dfBLL10) == "BLL10μg_dL_per1000tested")] <- "per1000tested"
dfBLL10['BLL']='10μg_dL'
dfBLL10['BLLper1000tested']='10μg_dL'

dfBLL15 <- df1 %>% select(Year, BLL15μg_dL, Borough, NumTested, BLL15μg_dL_per1000tested)
colnames(dfBLL15)[which(names(dfBLL15) == "BLL15μg_dL")] <- "Cases"
colnames(dfBLL15)[which(names(dfBLL15) == "BLL15μg_dL_per1000tested")] <- "per1000tested"
dfBLL15['BLL']<-'15μg_dL'
dfBLL15['BLLper1000tested']<-'15μg_dL'

# stack dataframes to transform from wide to long
longBLL <- rbind(dfBLL5, dfBLL10, dfBLL15)

# remove NAs
longBLL <- longBLL[complete.cases(longBLL), ]


# Build Shiny app
df <- longBLL
ui <- fluidPage(
    titlePanel(title = div(img(src="picture.jpg"), "Exploring Blood Lead Levels in Children in New York City")),
    mainPanel(

        h4("Questions:"),
        h5("Which borough has the highest number of children with blood lead levels that requires action (BLL > 5μg/dL)?"),
        h5("Which year has the worst cases of children with blood lead levels of concern across all borough?")
            


),
    headerPanel(h4('Blood Lead Level by Year')
                ),
    sidebarPanel(
        selectInput('year', 'Select year', unique(longBLL$Year), selected = '2006'),
        selectInput('borough', 'Select borough', unique(longBLL$Borough), selected='Queens')
        
    ),
    mainPanel(
        
        plotOutput('plot1'),
        plotOutput('plot2')
    )
)

server <- function(input, output) {
    
    output$plot1 <- renderPlot({
        
        dfSlice1 <- longBLL %>%
            filter(Year == input$year)
        
        
        ggplot() +
            geom_bar(dfSlice1, mapping = aes(fill = BLL, x=Borough, y=Cases), stat = 'identity') +
        
            geom_line(dfSlice1, mapping = aes(fill = BLL, x=Borough, y=NumTested, colour = "black"), stat = 'identity') +
            scale_color_discrete(name = "", labels = "Total Tested")
            
        
    })
    output$plot2 <- renderPlot({
        dfSlice2 <- longBLL %>%
            filter(Borough == input$borough)
        
        dfSlice2 <- dfSlice2 %>%
            group_by(Year, BLL) %>%
            summarise(Cases = sum(Cases),
                      NumTested = sum(NumTested))
        
        ggplot(dfSlice2, aes(x = Year, y = Cases, color = BLL, group = BLL)) + geom_line() +
           
            geom_line(dfSlice2, mapping = aes(fill = BLL, x=Year, y=NumTested, color = "Total tested"), stat = 'identity') +
            scale_y_log10()
    })
}
shinyApp(ui = ui, server = server)