
require(tidyverse)
require(sf)
# require(terra)
# require(tidyterra)
require(tmap)
require(magrittr)
require(highcharter)
require(plotly)
require(leaflet)
require(shiny)
require(shinyWidgets)
require(shinydashboard)
require(highcharter)
require(shinyalert)

library(bslib)
library(showtext)
library(thematic)

options(scipen = 999)

tmap_mode(mode = "view")
tmap_options(max.categories = 36) 

getwd()
cbd = st_read("kwala layers/CBD/Kwala CBD.shp") %>% 
  st_make_valid() %>% 
  janitor::clean_names()%>% 
  mutate(area_m2 = st_area(geometry) %>% as.numeric(),
         area_m2 = round(area_m2, digits = 0),
         area_ha = area_m2/10000) %>% 
  janitor::clean_names()

cbd = cbd %>% 
  mutate(state = st_is_empty(geometry)) %>% 
  filter(!state == TRUE)

cbd.tb = cbd %>% 
  st_drop_geometry() %>% 
  as_tibble() 

usesIdentified = unique(cbd.tb$uses)

mytheme = bslib::bs_theme(version = 5, 
                          bootswatch = "journal", 
                          base_font = font_google("Roboto Slab"),
                          code_font = font_google("Fira Code"),
                          heading_font = font_google("Fira Sans"))


ui = navbarPage(
  theme = mytheme, 
  collapsible = TRUE,
  HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">KWALA tracker</a>'), 
  id="nav",
  windowTitle = "Kwala Commercial City",
  ## link CSS 
  tags$head(
    useShinydashboard(),
    tags$link(rel = "stylesheet", type = "text/css", href = "kwala.css")),
  
  tabPanel(
    title = "CBD", 
    div(
      class="outer",
      tmapOutput("bbmap", height = 1000) %>% shinycustomloader::withLoader(type = "html", loader = "loader1"),
      
      
      absolutePanel(id = "controls", class = "panel panel-default",
                    top = 75, left = 75, width = 250, fixed=TRUE,
                    draggable = TRUE, height = "auto",
                    tags$br(),
                    helpText("The selected layer is overlaid on top of the planned land use to visualize......You can drag the layer to position it in the place you prefer if it masked the below layer!"),
                    # span(tags$i(p("The selected layer is overlaid on top of the planned land use to visualize....")), style="color:#045a8d"),
                    # p(textOutput("reactive_case_count"), align = "right"),
                    # p(textOutput("reactive_death_count"), align = "right"),
                    # h6(textOutput("clean_date_reactive"), align = "right"),
                    # h6(textOutput("reactive_country_count"), align = "right"),
                    # plotOutput("epi_curve", height="130px", width="100%"),
                    # plotOutput("cumulative_plot", height="130px", width="100%"),
                    highchartOutput(outputId = "barArea", height="350px", width="100%"),
                    # infoBoxOutput(outputId = "area_id", width = NULL) %>% shinycustomloader::withLoader(type = "html", loader = "loader1"),
                    
                    
                    pickerInput(inputId = "use_id",
                                    label = helpText("Select the uses. You can select more than one land use. Toggle the basemap layer to choose the appropriate layer..."),
                                    choices = usesIdentified,
                                    selected = usesIdentified[2],
                                multiple = TRUE)
                    ),
      absolutePanel(
        top = 100, left = 1000, draggable = TRUE, width = "20%", style = "z-index:500; min-width: 300px;",
        textInput("geocode", "Type an address or location", placeholder = "Vigwaza, Kibaha | PWANI"),
        checkboxInput("use_location", "Or use your current location?"),
        actionButton("go", "Find Area!", class = "btn-primary"),
        highchartOutput("selectstat")
      ),
      absolutePanel(
        top = 10, right = 10, style = "z-index:500; text-align: right;",
        tags$h2("Kwala Commercial City"),
        tags$a("More information found here", href="https://semba.netlify.app/")
      ),
      
      absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                    tags$a(href='https://semba.netlify.app/', tags$img(src='coatT.png',height='92',width='80'))),
      
      absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                    actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                                 onclick = sprintf("window.open('%s')", 
                                                   "https://twitter.com/intent/tweet?text=%20@LSHTM_Vaccines%20outbreak%20mapper&url=https://bit.ly/2uBvnds&hashtags=coronavirus")))
      
    )
    ),
  
  tabPanel(
    title =  "Data", 
    fluidRow(
      column(width = 1),
      column(width = 8,
             DT::dataTableOutput("data")
      )
    )
    ),
  
  tabPanel(
    title = "About",
    div(
    fluidRow(
      column(width = 1),
      column(width = 10, 
           
             tags$div(
               tags$h4("Last update"), 
               h6(paste0(lubridate::today())),
               "This site is updated once daily. There are several artilce and newspost that have hinted about Kwala City available, including", 
               tags$a(href="https://www.thecitizen.co.tz/tanzania/news/business/how-kwala-satellite-city-is-set-to-change-millions-of-lives-3921568", "How Kwala Satellite City is set to change millions of lives,"),
               tags$a(href="https://constructionreviewonline.com/news/us-3bn-kwala-satellite-city-under-development-in-tanzania/", "US$ 3bn Kwala Satellite City under development in Tanzania,"),"and",
               tags$a(href="https://www.youtube.com/watch?v=BBp7_sbUdWY", "HAIJAWAHI TOKEA! MRADI wa MABILIONI KUJENGWA KIBAHA, DC SARA ATOA NONDO KONKI AKIUELEZEA."),
               "Our aim is to complement these resources with several interactive features, including the timeline function and the ability to overlay past outbreaks.",
               tags$br(),
               
               tags$h4("Background"), 
               "The Government of the United Republic of Tanzania has intended to plan, establish and develop a new Commercial City located within the districts of Kibaha, Chalinze and Kisarawe in Pwani Region to enhance the potential of the Dar es Salaam and Bagamoyo Ports. The choice of the proposed area for establishment of commercial city attribute to several factors such as; proximity to Dar es Salaam City; Accessibility of the area with various modes of transport including railways and road networks; establishment of Inland Containers Depot – ICD in Kwala village; Ongoing construction of  the marshalling yard for Train engines and wagons repair and assembling by TRC in a designated  1,000 hectare at Kwala village; Designation of 2,500 hectare for Industrial Park by SINO TAN -  Kibaha at Kwala village;  Designation of 712.27 Hectare by TPA  and 425 Hectare by Ease Network  for ICDs at Vihingo and Mihugwe villages in Kisarawe District which is very close to the Dry port in Kwala village.",
               tags$br(),tags$br(),
               "Also, the Government designated an area of 504 Hectares for Kwala Dry Port at Kibaha District, Pwani Region. The Dry Port will serve all cargo to and from the upcountry except Dar es Salaam cargo will only be served at Dar es Salaam port. Further, the dry port will serve all cargo to and from the land locked Countries of Malawi, Zambia, DRC, Bulundi, Rwanda, Uganda, Southern Sudani and Kenya. Additionally, the ongoing construction of Standard Gauge Railway (SGR) from Dar es Salaam to Mwanza and from Tabora to Mpanda and Kasanga/Kalemi/Kipili ports along Lake Tanganyika and from Tabora to Kigoma including construction of a 15.5 km port access road from Vigwaza in Chalinze district that connects the dry port to the National trunk roads is part of the Capital Improvement Plan for the Dry Port.",
               tags$br(),
               
               tags$h4("Rationale"),
               "Tanzania has been among the countries that experienced positive economic Growth in recent years. This has been demonstrated by her graduation to the middle-Income Country in 2020. However, in enhancing this growth, deliberate efforts should be taken in tapping all the available competitive and comparatives advantages on Geographical economics of the Country. Furthermore, enhancement of economic growth should move parallel with the development of new Cities and Towns with strong attributes in integrating social, economic, and environmental aspects to ensure that, their contribution to the National GDP is realized.",
               tags$br(),
               "The massive ongoing construction of national infrastructure projects such as SGR, proposal for construction of the new port at Bagamoyo and the existing Dar es Salaam Port should be well capitalized in enhancing National Economic Growth. Effective tapping of all these competitive and comparative advantages requires the country to have plans and projects that capitalize on the full potential of the existing competitive and comparative advantages. It is from this juncture the Government decided to establish, plan, and develop a new commercial city along with the dry port in the area being initiatives on tapping the Geographical lavational potential a Country has. This Master Plan will also be used as a guiding tool in the development of Kwala commercial city and other areas of influence such as Dar es Salaam and Bagamoyo to ensure the growth and development of the proposed city is done in a sustainable manner for the coming 28 years (2022-2050).",
               tags$br(),
               
               tags$h4("Sources"),
               tags$b("National Bereau of Statistics: "), tags$a(href="https://www.nbs.go.tz/index.php/en/", "The housing and population censor and Basemaps layers,")," with additional information from the ",tags$a(href="https://worldpop.org/", "Open Spatial Demographic Data and Research."),tags$br(),
               tags$b("Ofisi ya Rais - Tawala za Mikoa na Serikali za Mitaa: "), tags$a(href="https://www.tamisemi.go.tz/", "Information and Communication page,"),tags$br(),
               tags$b("Tanzania Rural and Urban Roads Agency: "), tags$a(href="https://www.tarura.go.tz/", "News page"),tags$br(),
               tags$b("Tanzania Ports Authoity: "), tags$a(href="https://www.ports.go.tz/", "Bandari Kavu reports"),tags$br(),
               tags$b("Tanzania Telecommunications Corporation: "), "Mkongo wa Taif ", tags$a(href="https://www.ttcl.co.tz/", "GLaMOR Project"),tags$br(),
               tags$br(),
               
               tags$h4("Developer"),
               "Masumbuko Semba, The Nelson Mandela African Institution of Science and Technology",tags$br(),
               "Nyamisi Peter, University of Dar es Salaam",
               tags$br(),

               tags$h4("Contact"),
               "miringay@gmail.com",
               tags$br(),
               
               tags$h3("Acknowledgments"),
               "We can put the acknolwegment here",
               tags$br(),
               tags$img(src = "coat.png", width = "100px", height = "115px"), 
               tags$img(src = "ttcl.png", width = "100px", height = "115px"), 
               tags$img(src = "nbs.png", width = "110px", height = "115px"), 
               tags$img(src = "kibaha.jpg", width = "115px", height = "115px"),
               tags$br(),
               tags$br()
            
               )
             )
    )
    )
    )
  )


server = function(input, output, session)({
  
  
  cdb.reactive = reactive({
    
    cbd %>% filter(uses %in% input$use_id)
  })
  
  output$bbmap = renderTmap({
    
    tm_shape(shp = cbd, name = "CBD") +
      tm_fill(col = "uses", legend.show = FALSE, id = "uses", popup.vars = c("Area (m2)" = "area_m2"))+
      tm_shape(shp = cdb.reactive(), name = "Features")+
      tm_fill(col = "uses", legend.show = TRUE) +
      tm_view(alpha = 1)
    
  })
  
  
  output$reactive_case_count <- renderText({
    paste0("CBD: ",prettyNum(sum(cbd$area_m2)/10000, big.mark=","), " km2")
  })
  
  output$reactive_death_count <- renderText({
    paste0("Selected: ",prettyNum(sum(cdb.reactive()$area_m2)/10000, big.mark=","), " km2")
  })
  
  
output$data = DT::renderDataTable({
  
  cbd %>% st_drop_geometry() %>% select(uses, area_m2)
})


output$selectstat <- renderHighchart({
 
  mtcars %>% 
    hchart(type = "scatter", hcaes(wt, mpg, z = drat, color = hp)) %>%
    hc_title(text = "Area size and population") %>% 
      hc_yAxis(title = list(text = "Population"), gridLineWidth = 0, minorGridLineWidth = 0) %>%
      hc_xAxis(title = list(text = "Area Size"), gridLineWidth = 0, minorGridLineWidth = 0) %>%
      hc_legend(enabled = FALSE) %>%
      hc_tooltip(pointFormat = "Incidents: <b>{point.y}</b>") %>%
      hc_plotOptions(series = list(cursor = "default")) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_chart(backgroundColor = "transparent")
  
})

# output$area_id = renderInfoBox({
#   
#   aa = sum(cdb.reactive()$area_m2)
#   aa = round(aa/10000, digits = 0)
#                  
#                 
#   infoBox(title = HTML("Features<br>"),
#           value = HTML("<p style='font-size:50px'>",
#                        aa, "</p>"),
#           color = if_else(aa < 1000, "maroon", "purple"), 
#           icon = icon("vote-yea"),
#           fill = TRUE)
# })


output$barArea = renderHighchart({
  
  cbd.tb %>% 
    group_by(uses) %>% 
    summarise(area = sum(area_ha)) %>% 
    ungroup() %>% 
    arrange(-area) %>% 
    slice(1:20) %>% 
    hchart(type = "bar", hcaes(x = uses, y = area), color = "darkorchid")%>% 
    hc_yAxis(title = list(text = "Area size (Ha)"), gridLineWidth = 0, minorGridLineWidth = 0) %>%
    hc_xAxis(title = list(text = "Planning UseS"), gridLineWidth = 0, minorGridLineWidth = 0) %>%
    hc_legend(enabled = FALSE) %>%
    hc_tooltip(pointFormat = "Incidents: <b>{point.y}</b>") %>%
    hc_plotOptions(series = list(cursor = "default")) %>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_chart(backgroundColor = "transparent")
  
})


})

shinyApp(ui, server)



