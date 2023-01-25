library(shiny)
library(tidyverse)
library(ggthemes)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(RColorBrewer)
library(stringr)
setwd("C:/Users/marcu/Downloads/STA404Final")

t1<-read.csv("table1.csv")
Chidf<-read.csv("table2.csv")
t3<-read.csv("table3.csv")



t1<-t1%>%pivot_longer(cols=3:4,
                      names_to="age",
                      values_to="percent")
csv <- t1 %>%
  filter(Year!=2019 &
           Country != "China" &
           Country != "NewZealand" &
           Country != "Chile" &
           Country != "Colombia") %>%
  mutate(Year=as.factor(Year))
head(csv)
tail(csv)

csv <- csv %>%
  filter(Year!=2019) %>%
  filter(Country == "UnitedStates" |
           Country == "UnitedKingdom" |
           Country == "Ireland" |
           Country == "KoreaRepublicof" |
           Country == "Canada" |
           Country == "Latvia" |
           Country == "Mexico" |
           Country == "CzechRepublic") %>%
  mutate(Year=as.factor(Year))


csv <- csv %>%mutate(Country=gsub("([a-z])([A-Z])", "\\1 \\2", Country))%>%drop_na()

#color line plot
plotcolor <- brewer.pal(n=9, name='Paired')
names(plotcolor) <- sort(unique(csv$Country))

min(t3$GDP)
max(t3$GDP)
median(t3$GDP)
#var greater or less mean
GDPcats<-t3%>%group_by(Year)%>%
  mutate(level=ifelse(GDP>=mean(t3$GDP),"Greater","Less"))
head(GDPcats)

#yrs select input bubble
years<-c(2005,2010,2015,2016,2017,2018)

#colors bar
mycol <- rgb(135, 206, 235, max = 255, alpha = 125)
mycol1 <- rgb(255, 62, 150, max = 255, alpha = 125)
mycol2 <- rgb(104, 34, 139, max = 255, alpha = 125)
mycol3 <- rgb(255, 193, 37, max = 255, alpha = 125)



ui <- fluidPage(theme=shinytheme("spacelab"),
                
                # Application title
                titlePanel("Education Data Years 2000-2018"),
                
                
                tabsetPanel(
                  #introduction
                  tabPanel("Introduction", 
                           fluidRow(column(12,br(), 
                                           p("This application explores data trends in national education. We
                       are interested in viewing educational attainment against employment rates and GDP to attempt finding a hypothetical
                       reason for a country's educational attainment. Do countries have lower educational attainment because they spend less funding,
                       don't need education for work, or varying reasons? This is an exploratory data analysis, thus we are unable to prove
                       causality, but can investigate trends and correlations. We aim to determine some of the factors that play a role in 
                       education attainment in many countries. We also investigate the differing impact educational attainment has on employment across countries.  ",
                                             br(),br(),"This project displays the plots outlined below to answer the following research questions:",br(),
                                             "(1) Does educational attainment vary between countries?",br(),
                                             "(2) Do differing proficiency levels have differing employment rates?", br(),
                                             "(3) Are expenditures on education influenced by a country's GDP?",
                                             style="text-align:justify;color:black;background-color:#DFE5FD;padding:18px;border-radius:10px;font-size:16px"))),
                           hr(),
                           fluidRow(column(3, tags$img(src = "nces2.png", height = "300px", width = "300px",
                                                       alt="error!",deleteFile=F)),
                                    column(
                                      br(),
                                      p("The data used in this application are publicly available on the website", em("National Center for Education Statistics"), "in their list of current digest tables. 
                          The data extracted from this site corresponds to a number of social, educational, and financial variables in countries around the world. 
                          The data ranges from years 2000 to 2018. All dollar values are listed in current U.S. dollars.",
                                        style="text-align:justify;color:black;background-color:#BBCEDF;padding:9px;border-radius:15px;font-size:16px"),
                                      br(),
                                      
                                      p("We will explore the differences in educational
                          attainment between varying countries during selected years in the ", strong("Line Plot"), ". In our", strong("Bar Chart"),"we investigate the employment rates by the selected
                          proficiency level. We compare the employment rate among different proficiencies (1-5) and additionally compare these rates
                          to the average employment rate for the corresponding proficiency level. The", strong("Bubble Plot"),"displays how a country's GDP impacts the 
                          expenditures in Secondary and Post Secondary Education.",
                                        style="text-align:justify;color:black;background-color:#507AA0;padding:9px;border-radius:15px;font-size:16px"),
                                      
                                      width=9)),
                           
                           
                           
                           hr(),
                           
                  ),
                  #graph 1
                  tabPanel("Line Plot",
                           sidebarPanel(
                             #line
                             selectInput(inputId = "age",
                                         label="Select an Age Range:",
                                         choices=c("25 to 34"="X25to34",
                                                   "25 to 64"="X25to64"),
                                         selected="X25to34"),
                             
                             selectInput(inputId="degree",
                                         label="Select a Degree Level:",
                                         choices=c(csv$Degree),
                                         selected=" Secondary"),
                             
                             selectInput(inputId="country", label="Select Countries:",
                                         choices=c(csv$Country),
                                         multiple=TRUE,
                                         selected=c("Canada", "United States","Ireland"))),
                           mainPanel(plotOutput(outputId = "timeplot"))),
                  #graph 3
                  tabPanel("Bar Chart",
                           sidebarPanel(
                             selectInput(inputId="Country",
                                         label="Select a Country for the Bar Chart:",
                                         choices=c(Chidf$Country),
                                         selected=" Austria"),
                             selectInput(inputId="Type",
                                         label="Select a Type of Proficiency",
                                         choices=c("Literacy"=1,
                                                   "Numeracy"=2))),
                           mainPanel(plotlyOutput(outputId = "barchart"))),
                  #graph 4
                  tabPanel("Bubble Plot",
                           sidebarPanel(
                             sliderTextInput(inputId = "yrs",
                                             label="Select a Year for the Bubble plot:",
                                             choices=years,
                                             selected=2010,
                                             grid=T),
                             checkboxInput(inputId = "GDPval",
                                           label="Only Display Countries with GDP above Average",
                                           value=F)),
                           mainPanel(plotlyOutput(outputId = "Bubble"))),
                  
                  #conclusion
                  tabPanel("Conclusion", 
                           fluidRow(column(12,br(), 
                                           p("LINE PLOT:",br(),br(),
                                             
                                             "On the 25 to 34 age group and secondary degree setting, we see that 
                    all of the countries seem to be clustered in a range from 65% to 99% 
                    of population having a secondary degree. The country with the highest 
                    rate is Korea, with Czech Republic close to it as well. However, we
                    also see that Mexico is much further down compared to all the other 
                    countries. This could be due to the fact that poverty in Mexico drives 
                    high school students to find jobs rather than completing their 
                    education.",
                                             
                                             br(),
                                             "From the 25 to 64 age group and secondary degree setting, we see very 
                    similar results in terms of the rates and clusters of the countries. 
                    But here, every country seems to follow more of a clear, upward
                    trend (even Mexico, which is still much lower than the rest). It is 
                    interesting that now Korea is much lower than Czech Republic, and the 
                    United States is very close to Czech Republic's rates.",
                                             
                                             br(),
                                             "On the 25 to 34 age group and post-secondary setting, we see these 
                    rates are overall much lower than the rates for secondary degrees. 
                    This makes intuitive sense because most people graduate high school, 
                    but not everyone attends college. There is also more of an evenspread 
                    of these rates, as opposed to having a huge gap between Mexico and 
                    the rest of the countries in the secondary degree settings. Here, 
                    Korea seems to have the most drastic incline of percentage of population 
                    with a post-secondary degree, ranging from about 37% all the way to 
                    around 70% from 2000 to 2015. This is most likely because Korea started 
                    spending comparatively more of its GDP on private educations and education 
                    levels of primary, secondary, and tertiary.",
                                             
                                             br(),
                                             "Switching to 25 to 64 age group for post-secondary degrees, we again 
                    see more of a consistent, upward trend for each country. Here, Canada has 
                    a consistent rate and is much higher than the rest of the countries. It 
                    also seems that the US started off close to Canada in 2000, but did not 
                    increase as drastically, and allowed Korea, the UK, and Ireland to catch 
                    up to its higher rates in 2015. Mexico is unfortunately still near the 
                    bottom of the chart, along with Czech Republic, which is surprising since 
                    Czech Republic has been near the top of the rates with the secondary 
                    settings. This implies that a lot of high school graduates in the 
                    Czech Republic do not attend college.
                    ",
                                             style="text-align:justify;color:black;background-color:#F1F2FF;padding:10px;border-radius:10px;font-size:16px"))),
                           fluidRow(column(12,
                                           p("BAR CHART:", br(),br(),
                                             
                                             "In these charts we see the average employment rate for the selected country
                    across differing proficiency levels. These values are plotted against the
                    overall average for all countries by proficiency level.",
                                             br(),
                                             "When looking at the average, we see that for both Literacy and Numeracy, 
                    the employment rates increased from Proficiency Level 1 to Level 2. 
                    However, it's interesting that it took a dip at Level 3, since our 
                    general assumption was that the higher the proficiency level, the better 
                    the employment rates. The average employment rates for Proficiency Level 
                    4 or 5 were both the highest across the board.",
                                             br(),
                                             "In most countries, it happened to be the case that the higher the 
                    proficiency level, the better the employment rates.",
                                             br(),
                                             "A few countries did not follow this trend. For example, in Japan, the 
                    employment rates for Level 1 and 2 were a lot higher than the upper 
                    levels. Meanwhile, in Korea and Mexico, the rates are generally equal 
                    throughout.",
                                             style="text-align:justify;color:black;background-color:#F1F2FF;padding:10px;border-radius:10px;font-size:16px"))),
                           
                           fluidRow(column(12, 
                                           p("BUBBLE PLOT:", br(),br(),
                                             
                                             "We see in the plot that the points follow a general trend. As the graph 
                    points increase in size (implying higher GDP per capita), we typically 
                    see a country have an increased spending on education expenditures.", 
                                             br(),
                                             "We also see that as the year increases, the majority of countries 
                    invest more money in education. In our plot the axis ranges tend to
                    increase with an increase in year. This makes intuitive sense as the 
                    cost of education has increased over the years.",
                                             br(),
                                             "Countries additionally tend to invest more money in Post-Secondary 
                    education. We see this in the plot as the y-axis range is about
                    double of the x-axis for all years. This is significant as the
                    countries in the plot appear to follow an upward pattern that appears 
                    somewhat linear. This can likely be explained by the difference in 
                    cost between elementary/secondary and post secondary education.",
                                             br(),
                                             "However, some countries don't follow the patterns above. Korea spent 
                    more money on elementary/secondary education for all years. This may be
                    explained by the government funding both private and public elementary
                    and secondary education.",
                                             br(),
                                             "Some countries don't seem to increase in expenditures at the same rates 
                    as the general changes in other countries (as years increase). In this data,
                    this is typically explained by these outlying countries having a slower 
                    increase in their GDP over the years. This makes sense as governments 
                    typically contribute a standard percentage of their GDP towards education.
                    ",
                                             style="text-align:justify;color:black;background-color:#F1F2FF;padding:10px;border-radius:10px;font-size:16px"))),
                           hr(),
                           fluidRow(column(12, 
                                           p("REFERENCES",br(),br(),    
                                             "Colors in R - department of statistics. (n.d.). Retrieved December 3, 
                    2022, from http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf",
                                             br(),br(),
                                             "Icfdn. (2022, November 8). 4 barriers to education in Mexico. 
                    International Community Foundation. Retrieved December 2, 2022, 
                    from https://icfdn.org/barriers-quality-education-mexico/#:~:text=
                    Among%20OECD%20countries%2C%20Mexico%20has,rather%20than%20complete
                    %20their%20education.",
                                             br(),br(),
                                             "National Center for Education Statistics. (n.d.). Digest of Education 
                    Statistics. Digest of education statistics-most current Digest Tables. 
                    Retrieved December 2, 2022, from https://nces.ed.gov/programs/digest/
                    current_tables.asp",
                                             br(),br(),
                                             "OECD. (n.d.). Education GPS - Korea - overview of the education system 
                    (EAG 2022) - OECD. Education GPS. Retrieved December 2, 2022, from 
                    https://gpseducation.oecd.org/CountryProfile?primaryCountry=KOR&treshold
                    =10&topic=EO",
                                             br(),br(),
                                             "Roser, M., & Ortiz-Ospina, E. (2016, June 22). Financing education. Our 
                     World in Data. Retrieved December 2, 2022, from 
                     https://ourworldindata.org/financing-education",
                                             br(),br(),
                                             "South Korean Education Reforms. Asia Society. (n.d.). Retrieved December 2, 
                    2022, from https://asiasociety.org/global-cities-education-network/
                    south-korean-education-reforms",
                                             style="text-align:justify;color:black;background-color:#D8D8D9;padding:10px;border-radius:10px;font-size:16px"))))
                  
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #graph 1
  output$timeplot <- renderPlot({
    
    csv2 <- csv %>%
      filter(Country %in% c(input$country),
             Degree %in% c(input$degree),
             age==input$age)
    
    labA<-gsub("X", "", input$age)
    A<-gsub("([0-9]{2}to)([0-9]{2})", "\\1 \\2", labA)
    label.age<-gsub("([0-9]{2})(to [0-9]{2})", "\\1 \\2", A)
    
    ggplot() +
      geom_point(aes(x=Year,y=percent,color=Country),
                 data=csv2,size=1.4) +
      geom_line(aes(x=Year,y=percent, group=Country, color=Country), 
                data=csv2,size=1.015) +
      ggtitle(paste0("Percentage of Population Aged ", label.age, " with ", input$degree, " Degree \n per Country by Year")) +
      labs(subtitle="2000-2015",x="Year",y=paste0("Percentage of People Aged ", label.age, "\n with ", input$degree ," Degree")) +
      scale_y_continuous(limits = c(0, 100)) +
      scale_color_manual(values=plotcolor[input$country]) +
      # center title
      theme(plot.title = element_text(hjust=0.5),
            plot.subtitle=element_text(hjust=0.5)) +
      # remove unnecessary elements for aesthetic purposes
      theme(panel.background = element_blank(),
            axis.line = element_line())
    
  })
  
  #graph 2
  output$barchart<-renderPlotly({
    
    trydata <- Chidf  %>%
      filter(Country==input$Country,
             Type==input$Type)
    
    tdatave <- data.frame(Prof=c("1 - Average",
                                 "2 - Average",
                                 "3 - Average",
                                 "4 or 5 - Average"),
                          Emp_rate=trydata[,7])
    trydata1 <- rbind(tdatave,trydata[,c(6,3)])
    
    #plotly variables (weird in shiny)
    trydata1$ProfLevel<-trydata1$Prof
    trydata1$ProfType<-ifelse(input$Type==1,"Literacy","Numeracy")
    trydata1$EmpRate<-paste0(round(trydata1$Emp_rate,2), "%")
    
    bar<-ggplot()+
      geom_col(aes(x=Prof,y=Emp_rate,fill=Prof, ProfLevel=ProfLevel,
                   ProfType=ProfType, EmpRate=EmpRate),data=trydata1)+
      coord_cartesian(ylim = c(min(trydata$Emp_rate)-10,100))+
      labs(title=paste0("Employment Rates by Proficiency Level in ",
                        input$Country),
           x=paste0(ifelse(input$Type==1,"Literacy","Numeracy"),
                    " Proficiency Level"),
           y="Employment Rate (%)")+
      theme_bw()+
      theme(legend.position = "none",
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=10),
            plot.title = element_text(hjust=0.5),
            plot.background = element_blank(),
            axis.line = element_line())+
      scale_fill_manual(
        breaks = c("1","1 - Average","2","2 - Average","3",
                   "3 - Average","4 or 5","4 or 5 - Average"),
        values = c("skyblue",mycol,"violetred1",mycol1,
                   "darkorchid4",mycol2,"goldenrod1",mycol3))
    ggplotly(bar, tooltip=c("ProfType", "ProfLevel", "EmpRate"))
    
  })
  
  
  
  #graph 3  
  output$Bubble<-renderPlotly({
    
    if(input$GDPval==T){
      df3<-GDPcats%>%filter(Country!="OECDaverage",
                            Year==input$yrs,
                            level=="Greater")
      
    } else {
      df3<-GDPcats%>%filter(Country!="OECDaverage",
                            Year==input$yrs)
    }
    
    #plotly variables (weird in shiny)
    df3$GDP.per.capita<-paste0("$",scales::label_comma(accuracy = .01)(df3$GDP))
    df3$Exp.Elem_Secnd<-paste0("$",scales::label_comma(accuracy = .01)(df3$Elem_secExp))
    df3$Exp.PostSecnd<-paste0("$",scales::label_comma(accuracy = .01)(df3$PostsecExp))
    
    bub<-ggplot()+
      geom_point(aes(x=Elem_secExp, y=PostsecExp, size=GDP, color=Country,
                     GDP.per.capita=GDP.per.capita,Exp.Elem_Secnd=Exp.Elem_Secnd,
                     Exp.PostSecnd=Exp.PostSecnd), 
                 data=df3, alpha=0.5)+
      labs(title="Education Expenditures by GDP",
           x="Expenditures on Elementary and Secondary Education ($/FTE Student)",
           y="Expenditures on Post Secondary Education ($/FTE Student)")+
      theme_bw()+
      theme(legend.position = "none",
            axis.title.x = element_text(size=8),
            axis.title.y = element_text(size=8),
            plot.title = element_text(hjust=0.5, size=12),
            plot.subtitle=element_text(hjust=0.5),
            plot.background = element_blank(),
            axis.line = element_line())+
      scale_y_continuous(labels=scales::comma)+
      scale_x_continuous(labels=scales::comma)
    
    ggplotly(bub,tooltip = c("Country","GDP.per.capita","Exp.Elem_Secnd","Exp.PostSecnd"))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)



