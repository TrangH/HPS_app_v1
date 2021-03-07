#check if all required packages are installed
mypackages <- c("leaflet", "sp", "shiny", "ggplot2", "geojsonio", 
                "RColorBrewer", "dplyr")
checkpkg <- mypackages[!(mypackages %in% installed.packages()[,"Package"])];
#if not, then install the missing packages  
if(length(checkpkg))install.packages(checkpkg, dependencies = TRUE)
#loading packages
library(shiny); library(leaflet); 
library(RColorBrewer); library(ggplot2); 
library(dplyr); 
library(geojsonio); library(sp)

####objects created outside of server----
#transfrom .json file into a spatial polygons data frame
states <- 
  geojson_read( 
    x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", 
    what = "sp"
  )
 
 ###
  server <- function(input, output, session) {
    ####Reactively load data (if not pre-loaded, then drag data in)----
  Q1 <- reactive({
    Q1_df <- if (is.null(input$file1)) {
      read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", 
                       "15ruoxPtiTC-WuKEPwZHv-pWH_Uh69BUa"),
               header=TRUE,check.names=FALSE,stringsAsFactors=FALSE)[-1]
    } else {
      read.csv(input$file1$datapath, 
               check.names=FALSE,stringsAsFactors=FALSE)
    }
    Q1_df <- Q1_df %>% 
      filter(state %in% state[nchar(state) == 2] & state != 'US') %>%
      mutate(state = state.name[match(state,state.abb)]) %>%
      tidyr::replace_na(., list(state = "District of Columbia")) %>%
      mutate(`Much less` = rowSums(select(.,matches('much_less')), na.rm = TRUE),
             `Little less` = rowSums(select(., matches('little_less')), na.rm = TRUE),
             Unchanged = rowSums(select(., matches('as_much')), na.rm = TRUE),
             `Little more` = rowSums(select(., matches('little_more')), na.rm = TRUE),
             `Much more` = rowSums(select(., matches('much_more')), na.rm = TRUE)) %>%
      select(1:5, `Much less`,`Little less`, Unchanged, `Little more`,`Much more`) %>%
      mutate(Total = rowSums(select(., `Much less`,`Little less`, Unchanged, `Little more`,`Much more`), 
                             na.rm = TRUE)) %>%
      mutate(`Much less` = round(100*`Much less`/Total,1),
             `Little less` = round(100*`Little less`/Total,1),
             Unchanged = round(100*Unchanged/Total,1),
             `Little more` = round(100*`Little more`/Total,1),
             `Much more` = round(100*`Much more`/Total,1))
    Q1_df[sapply(Q1_df, is.nan)] <- NA 
    Q1_df <- Q1_df[rowSums(is.na(Q1_df[,6:10]))!=5,] #remove rows that have all NAs
    return(Q1_df)
  })
  #
    Q2 <- reactive({
      Q2_df <- if (is.null(input$file2)) {
      read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", 
                       "1rD5zotPIRoKopEomKFm_cuP013Qezsnp"),
               header=TRUE,check.names=FALSE,stringsAsFactors=FALSE)[-1]
      } else {
      read.csv(input$file2$datapath, 
               check.names=FALSE,stringsAsFactors=FALSE)
      }
      Q2_df <- Q2_df %>% 
        filter(state %in% state[nchar(state) == 2] & state != 'US') %>%
        mutate(state = state.name[match(state,state.abb)]) %>%
        tidyr::replace_na(., list(state = "District of Columbia")) %>%
        mutate(Online = rowSums(select(.,matches('Class_online')), na.rm = TRUE),
               Paper = rowSums(select(., matches('Class_paper')), na.rm = TRUE),
               Cancelled = rowSums(select(., matches('Class_cancelled')), na.rm = TRUE),
               Unchanged = rowSums(select(., matches('No_change')), na.rm = TRUE)) %>%
        select(1:5, Online, Paper, Cancelled, Unchanged) %>%
        mutate(Total = rowSums(select(., Online, Paper, Cancelled, Unchanged), 
                               na.rm = TRUE)) %>%
        mutate(Online = round(100*Online/Total,1),
               Paper = round(100*Paper/Total,1),
               Cancelled = round(100*Cancelled/Total,1),
               Unchanged = round(100*Unchanged/Total,1))
      Q2_df[sapply(Q2_df, is.nan)] <- NA 
      return(Q2_df)
    })
    #
    Q3 <- reactive({
      Q3_df <- if (is.null(input$file3)) {
       read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", 
                       "18ecFURKRA0PTjLfeZPO6lMrMZ59wiir1"),
               header=TRUE,check.names=FALSE,stringsAsFactors=FALSE)[-1]
        } else {
        read.csv(input$file3$datapath, 
               check.names=FALSE,stringsAsFactors=FALSE)
        }
      Q3_df <- Q3_df %>% 
        filter(state %in% state[nchar(state) == 2] & state != 'US') %>%
        mutate(state = state.name[match(state,state.abb)]) %>%
        tidyr::replace_na(., list(state = "District of Columbia")) %>%
        #mutate(state = tidyr::replace_na(state, 'District of Columbia')) %>%
        mutate(PC = rowSums(select(.,matches('pc_always|pc_usually')), 
                 na.rm = TRUE),
               Internet = rowSums(
                 select(., matches('internet_always|internet_usually')), 
                 na.rm = TRUE)) %>%
        select(1:5, PC, Internet, Total) %>%
        mutate(PC = round(100*PC/Total,1),
               Internet = round(100*Internet/Total,1))
      Q3_df[sapply(Q3_df, is.nan)] <- NA 
      return(Q3_df)  
    })
  
    ###Display loaded data & add download button----
    output$download_Q1 <- downloadHandler(
      filename = function(){"q1_all.csv"}, 
      content = function(fname){
        write.csv(Q1(), fname)
      }
    )
    output$Q1 <- renderTable({
      df <- Q1() %>% filter(Dimension == input$T1_Dimension) %>%
        filter(Week_end == input$T1_Round)
      if(input$disp_T1 == "all"){
        return(df)
      } else {return(head(df))}
    })
    #
    output$download_Q2 <- downloadHandler(
      filename = function(){"q2_all.csv"}, 
      content = function(fname){
        write.csv(Q2(), fname)
      }
    )
    output$Q2 <- renderTable({
      df <- Q2() %>% filter(Dimension == input$T2_Dimension) %>%
        filter(Week_end == input$T2_Round)
      if(input$disp_T2 == "all"){
        return(df)
      } else {return(head(df))}
    })
    #
    output$download_Q3 <- downloadHandler(
      filename = function(){"q3_all.csv"}, 
      content = function(fname){
        write.csv(Q3(), fname)
      }
    )
    output$Q3 <- renderTable({
        df <- Q3() %>% filter(Dimension == input$T3_Dimension) %>%
            filter(Week_end == input$T3_Round)
        if(input$disp_T3 == "all"){
            return(df)
        } else {return(head(df))}
    })
    
    ####Filter data tables----
    ##for both pre-load and upload data
    observeEvent(is.null(input$file1), {
      updateSelectInput(session, "T1_Dimension", 
                        label = "Select a demographic characteristic", 
                        choices = unique(Q1()$Dimension))
    })
    observeEvent(input$file1, {
      updateSelectInput(session, "T1_Dimension", 
                        label = "Select a demographic characteristic", 
                        choices = unique(Q1()$Dimension))
    })
    observeEvent(is.null(input$file1), {
      updateSelectInput(session, "T1_Round", 
                        label = "Select a survey round", 
                        choices = unique(Q1()$Week_end)) 
    })
    observeEvent(input$file1, {
      updateSelectInput(session, "T1_Round", 
                        label = "Select a survey round", 
                        choices = unique(Q1()$Week_end))
    })
    #
    observeEvent(is.null(input$file2), {
      updateSelectInput(session, "T2_Dimension", 
                        label = "Select a demographic characteristic", 
                        choices = unique(Q2()$Dimension))
    })
    observeEvent(input$file2, {
      updateSelectInput(session, "T2_Dimension", 
                        label = "Select a demographic characteristic", 
                        choices = unique(Q2()$Dimension))
    })
    observeEvent(is.null(input$file2), {
      updateSelectInput(session, "T2_Round", 
                        label = "Select a survey round", 
                        choices = unique(Q2()$Week_end))
    })
    observeEvent(input$file2, {
      updateSelectInput(session, "T2_Round", 
                        label = "Select a survey round", 
                        choices = unique(Q2()$Week_end))
      })
    #
    observeEvent(is.null(input$file3), {
      updateSelectInput(session, "T3_Dimension", 
           label = "Select a demographic characteristic", 
           choices = unique(Q3()$Dimension))
      })
    observeEvent(input$file3, {
      updateSelectInput(session, "T3_Dimension", 
           label = "Select a demographic characteristic", 
           choices = unique(Q3()$Dimension))
      })
    observeEvent(is.null(input$file3), {
      updateSelectInput(session, "T3_Round", 
                        label = "Select a survey round", 
                        choices = unique(Q3()$Week_end))
      })
    observeEvent(input$file3, {
      updateSelectInput(session, "T3_Round", 
                        label = "Select a survey round", 
                        choices = unique(Q3()$Week_end))
      })
    
    ####Plot US maps----
    ##Use CRS map projection
    epsg2163 <- leafletCRS(
      crsClass = "L.Proj.CRS",
      code = "EPSG:2163",
      proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
      resolutions = 2^(16:7))
    
    ###Table 1----
    ##coloring
    pal_Q1 <- colorNumeric(palette = "YlOrRd", domain = NULL)
    pal_Q1_na <- colorNumeric(palette = "YlOrRd", 
                              domain = NULL, na.color=rgb(0,0,0,0))
    ##Reactive filter for map data 
    df_Q1 <- reactive({sp::merge(states, Q1() %>% 
                      filter(Week_end == input$inputvar_Round) %>% 
                      filter(Dimension == input$inputvar_Dimension),
                      by.x = "name", by.y = "state",
                      duplicateGeoms = TRUE) 
      })
    observe({
      updateSelectInput(session, "inputvar_Dimension",
                        choices = unique(Q1()$Dimension)
      )})
    observe({
      updateSelectInput(session, "inputvar_Round",
                        choices = sort(unique(Q1()$Week_end),
                                       decreasing = TRUE)
      )})
    ##Reactive variable selector
    var_Q1 <- reactive({switch(input$inputvar_Q1, 
                               "Much less" = df_Q1()$`Much less`, 
                               "Little less" = df_Q1()$`Little less`,
                               "Unchanged" = df_Q1()$`Unchanged`,
                               "Little more" = df_Q1()$`Little more`,
                               "Much more" = df_Q1()$`Much more`)
      })
    ##Reactive labels and popups
    labels_Q1 <- reactive({switch(input$inputvar_Q1, 
                    "Much less" = sprintf(
                    "<strong>%s</strong><br/>%g percent",
                     df_Q1()$name, df_Q1()$`Much less`) %>% lapply(htmltools::HTML), 
                     "Little less" = sprintf(
                     "<strong>%s</strong><br/>%g percent",
                     df_Q1()$name, df_Q1()$`Little less`) %>% lapply(htmltools::HTML),
                     "Unchanged" = sprintf(
                     "<strong>%s</strong><br/>%g percent",
                     df_Q1()$name, df_Q1()$`Unchanged`) %>% lapply(htmltools::HTML),
                     "Little more" = sprintf(
                     "<strong>%s</strong><br/>%g percent",
                     df_Q1()$name, df_Q1()$`Little more`) %>% lapply(htmltools::HTML),
                     "Much more" = sprintf(
                     "<strong>%s</strong><br/>%g percent",
                     df_Q1()$name, df_Q1()$`Much more`) %>% lapply(htmltools::HTML)
    )
    })
    #
    popups_Q1 <- reactive({switch(input$inputvar_Q1, 
                                  "Much less" = paste0("<strong> State: </strong>", 
                                            df_Q1()$name,"<br/>", "<strong> Share: </strong>",
                                            df_Q1()$`Much less`),
                                  "Little less" = paste0("<strong> State: </strong>", 
                                                       df_Q1()$name,"<br/>", "<strong> Share: </strong>",
                                                       df_Q1()$`Little less`),
                                  "Unchanged" = paste0("<strong> State: </strong>", 
                                                       df_Q1()$name,"<br/>", "<strong> Share: </strong>",
                                                       df_Q1()$`Unchanged`),
                                  "Little more" = paste0("<strong> State: </strong>", 
                                                       df_Q1()$name,"<br/>", "<strong> Share: </strong>",
                                                       df_Q1()$`Little more`),
                                  "Much more" = paste0("<strong> State: </strong>", 
                                                       df_Q1()$name,"<br/>", "<strong> Share: </strong>",
                                                       df_Q1()$`Much more`)
    )
    })
    ##Create heatmap of selected variables
    output$map_Q1 <- renderLeaflet({
      leaflet(df_Q1(), options = leafletOptions(
        crs = epsg2163,
        zoomControl = FALSE,
        minZoom = 1, maxZoom = 6)) %>%
        addControl(tags$div(
          HTML('<img border="0" 
               alt = "Share of Students with Time Spent On Learning Activities" 
               width="100" height="100"> 
               </a>')
        ), position = "bottomright") %>%
        setView(-95, 35, zoom = 2) %>%
        #addProviderTiles(providers$Stamen.TonerLite) %>% 
        addPolygons(data = df_Q1(),
                    fillColor = ~pal_Q1(var_Q1()),
                    weight = 1, opacity = 1, color = "white",
                    dashArray = "1", fillOpacity = 0.7,
                    highlight = highlightOptions(
                      weight = 1, color = "#666", dashArray = "",
                      fillOpacity = 0.7, bringToFront = TRUE),
                    label = labels_Q1(),
                    popup = popups_Q1()) %>%
        addLegend(pal = pal_Q1_na, values = ~var_Q1(), opacity = 0.7, title = NULL,
                  na.label = "", position = "bottomright") 
    })
    ###Table 2----
    ##coloring
    pal_Q2 <- colorNumeric(palette = "Blues", domain = NULL)
    pal_Q2_na <- colorNumeric(palette = "Blues", 
                              domain = NULL, na.color=rgb(0,0,0,0))
    ##Reactive filter for map data 
    df_Q2 <- reactive({sp::merge(states, Q2() %>% 
                       filter(Week_end == input$inputvar_Round) %>% 
                       filter(Dimension == input$inputvar_Dimension),
                                 by.x = "name", by.y = "state",
                              duplicateGeoms = TRUE) 
    })
    observe({
      updateSelectInput(session, "inputvar_Q2_Dimension",
                        choices = unique(Q2()$Dimension)
      )})
    observe({
      updateSelectInput(session, "inputvar_Q2_Round",
                        choices = sort(unique(Q1()$Week_end),
                                       decreasing = TRUE)
      )})
    ##Reactive variable selector
    var_Q2 <- reactive({switch(input$inputvar_Q2, 
             "Using online resources" = df_Q2()$Online, 
             "Using paper materials sent home" = df_Q2()$Paper,
             "Cancelled" = df_Q2()$Cancelled, 
             "Unchanged" = df_Q2()$Unchanged)
      })
    ##Reactive labels and popups
    labels_Q2 <- reactive({switch(input$inputvar_Q2, 
                 "Using online resources" = sprintf(
                  "<strong>%s</strong><br/>%g percent",
                 df_Q2()$name, df_Q2()$Online) %>% lapply(htmltools::HTML), 
                  "Using paper materials sent home" = sprintf(
                   "<strong>%s</strong><br/>%g percent",
                   df_Q2()$name, df_Q2()$Paper) %>% lapply(htmltools::HTML),
                 "Cancelled" = sprintf(
                   "<strong>%s</strong><br/>%g percent",
                   df_Q2()$name, df_Q2()$Cancelled) %>% lapply(htmltools::HTML),
                 "Unchanged" = sprintf(
                   "<strong>%s</strong><br/>%g percent",
                   df_Q2()$name, df_Q2()$Unchanged) %>% lapply(htmltools::HTML)
      )
      })
    #
    popups_Q2 <- reactive({switch(input$inputvar_Q2, 
        "Using online resources" = paste0("<strong> State: </strong>", 
                  df_Q2()$name,"<br/>", "<strong> Share: </strong>",
                  df_Q2()$Online),
         "Using paper materials sent home" = paste0("<strong>State: </strong><br/>", 
                  df_Q2()$name,"<br/>","<strong> Share: </strong>",
                  df_Q2()$Paper),
        "Cancelled" = paste0("<strong>State: </strong><br/>", 
                  df_Q2()$name,"<br/>","<strong> Share: </strong>",
                  df_Q2()$Cancelled),
        "Unchanged" = paste0("<strong>State: </strong><br/>", 
                  df_Q2()$name,"<br/>","<strong> Share: </strong>",
                  df_Q2()$Unchanged),
      )
      })
    ##Create heatmap of selected variables
    output$map_Q2 <- renderLeaflet({
      leaflet(df_Q2(), 
              options = leafletOptions(crs = epsg2163,
              zoomControl = FALSE,
              minZoom = 1, maxZoom = 6)) %>%
        setView(-95, 35, zoom = 2) %>%
        #addProviderTiles(providers$Stamen.TonerLite) %>% 
        addControl(tags$div(
          HTML('<img border="0" 
               alt = " Share of Students Adopting Different Learning Modes" 
               width="300" height="100"> 
               </a>')
        ), position = "bottomright") %>%
        addPolygons(data = df_Q2(),
                    fillColor = ~pal_Q2(var_Q2()),
                    weight = 1, opacity = 1, color = "white",
                    dashArray = "1", fillOpacity = 0.7,
                    highlight = highlightOptions(
                      weight = 1, color = "#666", dashArray = "",
                      fillOpacity = 0.7, bringToFront = TRUE),
                    label = labels_Q2(),
                    popup = popups_Q2()) %>%
        addLegend(pal = pal_Q2_na, values = ~var_Q2(), opacity = 0.7, title = NULL,
                  na.label = "", position = "bottomright") 
    })
    ###Table 3----
    ##coloring
    pal_Q3 <- colorNumeric(palette = "YlGn", domain = NULL)
    pal_Q3_na <- colorNumeric(palette = "YlGn", domain = NULL, na.color=rgb(0,0,0,0))
    
    ##Reactive filter for map data 
    df_Q3 <- reactive({sp::merge(states, Q3() %>% 
                    filter(Week_end == input$inputvar_Round) %>% 
                    filter(Dimension == input$inputvar_Dimension),
                    by.x = "name", by.y = "state",
                    duplicateGeoms = TRUE) 
                    })
    observe({
      updateSelectInput(session, "inputvar_Dimension",
                        choices = unique(Q3()$Dimension)
         )})
    observe({
      updateSelectInput(session, "inputvar_Round",
                        choices = sort(unique(Q1()$Week_end),
                                       decreasing = TRUE)
         )})
    ##Reactive variable selector
    var_Q3 <- reactive({switch(input$inputvar_Q3, 
                        "Digital devices" = df_Q3()$PC, 
                        "Internet" = df_Q3()$Internet)
                        })
    ##Reactive labels and popups
    labels_Q3 <- reactive({switch(input$inputvar_Q3, 
              "Digital devices" = sprintf(
                   "<strong>%s</strong><br/>%g percent",
                   df_Q3()$name, df_Q3()$PC) %>% lapply(htmltools::HTML), 
              "Internet" = sprintf(
                   "<strong>%s</strong><br/>%g percent",
                   df_Q3()$name, df_Q3()$Internet) %>% lapply(htmltools::HTML)
                   )
              })
    #
    popups_Q3 <- reactive({switch(input$inputvar_Q3, 
              "Digital devices" = paste0("<strong> State: </strong>", 
                     df_Q3()$name,"<br/>", "<strong> Share: </strong>",
                     df_Q3()$PC),
              "Internet" = paste0("<strong>State: </strong><br/>", 
                     df_Q3()$name,"<br/>","<strong> Share: </strong>",
                     df_Q3()$Internet)
                     )
              })
    ##Create heatmap of selected variables
    output$map_Q3 <- renderLeaflet({
      leaflet(df_Q3(), options = leafletOptions(
              crs = epsg2163,
              zoomControl = FALSE,
              minZoom = 1, maxZoom = 6)) %>%
        addControl(tags$div(
          HTML('<img border="0" 
               alt = "Share of Students with Access to Digital Devices and Internet" 
               width="300" height="100"> 
               </a>')
          ), position = "bottomright") %>%
        setView(-95, 35, zoom = 2) %>%
        #addProviderTiles(providers$Stamen.TonerLite) %>% 
        addPolygons(data = df_Q3(),
          fillColor = ~pal_Q3(var_Q3()),
          weight = 1, opacity = 1, color = "white",
          dashArray = "1", fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 1, color = "#666", dashArray = "",
            fillOpacity = 0.7, bringToFront = TRUE),
          label = labels_Q3(),
          popup = popups_Q3()) %>%
         addLegend(pal = pal_Q3_na, values = ~var_Q3(), opacity = 0.7, title = NULL,
                   na.label = "", position = "bottomright") 
      })
}
