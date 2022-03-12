#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(nflfastR)
library(ggplot2)
library(ggimage)
library(tidyverse)
library(ggrepel)
library(dplyr)
library(gt)
library(paletteer)
library(webshot)
library(ggthemes)
library(readr)
library(ggtext)
library(ggforce)
library(stats)
library(mclust)
library(mdthemes)
library(gghighlight)
library(na.tools)
library(stringr)
library(magick)
library(ggbeeswarm)
library(vip)
library(gtExtras)

stuntsoe_github <- read_csv(url("https://raw.githubusercontent.com/arjunmenon10/Stunts-over-Expected/main/stunts_proj.csv"))

pblk_grades <- read_csv(url("https://raw.githubusercontent.com/arjunmenon10/Stunts-over-Expected/main/pblk_grades.csv")) %>% 
  left_join(teams_colors_logos, by = c('Team' = 'team_nick')) %>% 
  filter(!team_abbr %in% c("LAR", "SD", "STL", "OAK")) %>% 
  select(Team, team_abbr, PBLK...6, S) %>% rename(PBLKG = "PBLK...6")


stuntsoe_github <- stuntsoe_github %>%
  group_by(defense) %>%
  mutate(count = n()) %>%
  filter(count >= 50) %>%
  ungroup()

stuntsoe_github <- stuntsoe_github %>% 
  group_by(offense) %>%
  mutate(count = n()) %>%
  filter(count >= 50) %>%
  ungroup()

stuntsoe_github <- stuntsoe_github %>% 
  left_join(pblk_grades, by = c('offense' = 'team_abbr', 'season' = 'S'))

teams <- nflfastR::teams_colors_logos %>%
  filter(!team_abbr %in% c("LAR", "SD", "STL", "OAK"))

defense <- unique(stuntsoe_github$defense)
offense <- unique(stuntsoe_github$offense)
seasons <- unique(stuntsoe_github$season)

colSums(is.na(stuntsoe_github))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Stunt Rate over Expected"),
  
  mainPanel(
    navbarPage("By Arjun Menon at PFF",
               tabPanel("Defenses",
                        fluidRow(
                          column(7, align = "center",
                                 selectInput(
                                   inputId =  "Season",
                                   label = "Season:",
                                   choices = 2015:2021,
                                   selected = 2021
                                 ),
                          ),
                          mainPanel(
                            plotOutput(outputId = "defense_graph",
                                       width = "100%",
                                       height = "50%"),
                            br(),
                            tableOutput("defense_table"),
                            br()
                          )
                        )
               ),
               tabPanel("Offenses",
                        fluidRow(
                          column(7, align = "center",
                                 selectInput(
                                   inputId =  "Season",
                                   label = "Season:",
                                   choices = 2015:2021,
                                   selected = 2021
                                 ),
                          ),
                          mainPanel(
                            plotOutput(outputId = "offense_graph",
                                       width = "100%",
                                       height = "50%"),
                            br(),
                            tableOutput("offense_table"),
                            br()
                          ),
                        )
               ),
               
    )
  )
)

server <- function(input, output){
  
  output$defense_graph <- renderPlot({
    
    stunt_season <- stuntsoe_github %>% 
      filter(season == input$Season)
    
    def_colors <- stunt_season %>% 
      group_by(defense) %>% 
      summarise(plays = n()) %>% 
      arrange(-plays) %>% 
      group_by(defense) %>% 
      top_n(n = 1) %>% 
      left_join(teams_colors_logos, by = c('defense' = 'team_abbr'))
    
    defense_stuntoe <- stunt_season %>%
      filter(!is.na(defense)) %>%
      group_by(defense) %>%
      summarize(actual_stunts = sum(stunt, na.rm = T),
                expec_stuntrate = mean(stunt_pred, na.rm = T),
                SROE = mean(stunts_oe, na.rm = T),
                plays = n(),
                pressures = sum(pressure),
                pressure_rate = pressures/plays,
                EPA_play = mean(EPA, na.rm = T))
    
    defense_stuntoe <- defense_stuntoe %>%
      left_join(def_colors, by = c("defense"))
    
    defense_stuntoe %>% 
      ggplot()+
      geom_image(aes(x = SROE, y = pressure_rate), image = defense_stuntoe$team_logo_espn,
                 asp = 16/9, size = 0.055)+
      geom_hline(yintercept = mean(defense_stuntoe$pressure_rate), color = "red", linetype = "dashed", alpha=0.7)+
      geom_vline(xintercept =  mean(defense_stuntoe$SROE), color = "red", linetype = "dashed", alpha=0.7)+
      theme_fivethirtyeight()+
      labs( title = paste0("Defensive Stunt Rate Over Expected and Pressure Rate in ", input$Season),
            subtitle = paste0(""),
            caption = "By Arjun Menon | @arjunmenon100 | PFF")+
      theme(axis.title = element_text(size = 18)) + xlab('Stunt Rate Over Expected (SROE)') + ylab("Pressure Rate")+
      theme(panel.grid.minor=element_blank(),
            legend.position = 'none')+
      theme(axis.text = element_text(size = 17))+
      theme(plot.title = element_markdown(size = 21, hjust = 0.5, face = "bold"),
            plot.subtitle = element_markdown(size = 20, hjust = 0.5))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 8))+
      scale_x_continuous(breaks = scales::pretty_breaks(n = 8))
  },  height = 600, width = 850)
  
  output$defense_table <- render_gt({
    
    stunt_season <- stuntsoe_github %>% 
      filter(season == input$Season)
    
    def_colors <- stunt_season %>% 
      group_by(defense) %>% 
      summarise(plays = n()) %>% 
      arrange(-plays) %>% 
      group_by(defense) %>% 
      top_n(n = 1) %>% 
      left_join(teams_colors_logos, by = c('defense' = 'team_abbr'))
    
    defense_stuntoe <- stunt_season %>%
      filter(!is.na(defense)) %>%
      group_by(defense) %>%
      summarize(actual_stunts = sum(stunt, na.rm = T),
                expec_stuntrate = mean(stunt_pred, na.rm = T),
                SROE = mean(stunts_oe, na.rm = T),
                plays = n(),
                pressures = sum(pressure),
                pressure_rate = pressures/plays,
                EPA_play = mean(EPA, na.rm = T))
    
    defense_stuntoe <- defense_stuntoe %>%
      left_join(def_colors, by = c("defense"))
    
    defense_gt <- defense_stuntoe %>% 
      select(defense, team_logo_espn, actual_stunts, pressure_rate, EPA_play, SROE) %>% 
      mutate_if(is.numeric, ~round(., 2)) %>%
      arrange(-SROE) %>%
      ungroup() %>%
      mutate(rank = row_number()) %>%
      dplyr::select(rank, everything())
    
    defense_gt %>% 
      gt() %>%
      text_transform(
        locations = cells_body(c(team_logo_espn)),
        fn = function(x){
          web_image(
            url = x,
            height = px(35)
          )
        }
      ) %>%
      cols_label(
        rank = "Rank",
        defense = "Defense",
        team_logo_espn = "",
        actual_stunts = "# Stunts",
        SROE = "SROE",
        pressure_rate = "Pressure Rate",
        EPA_play = "EPA/Pass allowed",
        SROE = "Stunt Rate Over Expected") %>%
      data_color(
        columns = c(SROE),
        colors = scales::col_numeric(
          palette = c("#cf3e53", "#bfb202", "#00a2b3"),
          domain = NULL
        )
      ) %>%
      opt_align_table_header(align = "center") %>%
      cols_align("center") %>%
      opt_row_striping() %>%
      gt_theme_538()
  }, width = 850)
  
  output$offense_graph <- renderPlot({
      
      stunt_season <- stuntsoe_github %>% 
        filter(season == input$Season)
      
      off_colors <- stunt_season %>% 
        group_by(offense) %>% 
        summarise(plays = n()) %>% 
        arrange(-plays) %>% 
        group_by(offense) %>% 
        top_n(n = 1) %>% 
        left_join(teams_colors_logos, by = c('offense' = 'team_abbr')) %>% 
        select(-plays)
      
      offense_stuntoe <- stunt_season %>%
        filter(!is.na(offense)) %>%
        group_by(offense) %>%
        summarize(actual_stunts = sum(stunt, na.rm = T),
                  expec_stuntrate = mean(stunt_pred, na.rm = T),
                  SROE = mean(stunts_oe, na.rm = T),
                  PBLK = mean(PBLKG))
      
      offense_stuntoe <- offense_stuntoe %>%
        left_join(off_colors, by = c("offense"))
      
      offense_stuntoe %>% 
        ggplot()+
        geom_image(aes(x = SROE, y = PBLK), image = offense_stuntoe$team_logo_espn,
                   asp = 16/9, size = 0.055)+
        geom_hline(yintercept = mean(offense_stuntoe$PBLK), color = "red", linetype = "dashed", alpha=0.7)+
        geom_vline(xintercept =  mean(offense_stuntoe$SROE), color = "red", linetype = "dashed", alpha=0.7)+
        theme_fivethirtyeight()+
        labs( title = paste0("Offensive Stunt Rate Over Expected Faced and Pass Block Grade in ", input$Season),
              subtitle = paste0(""),
              caption = "By Arjun Menon | @arjunmenon100 | PFF")+
        theme(axis.title = element_text(size = 18)) + xlab('Stunt Rate Over Expected (SROE)') + ylab("Pass Block Grade")+
        theme(panel.grid.minor=element_blank(),
              legend.position = 'none')+
        theme(axis.text = element_text(size = 17))+
        theme(plot.title = element_markdown(size = 21, hjust = 0.5, face = "bold"),
              plot.subtitle = element_markdown(size = 20, hjust = 0.5))+
        scale_y_continuous(breaks = scales::pretty_breaks(n = 8))+
        scale_x_continuous(breaks = scales::pretty_breaks(n = 8))
    },  height = 600, width = 850)
  
  output$offense_table <- render_gt({
    
    stunt_season <- stuntsoe_github %>% 
      filter(season == input$Season)
    
    off_colors <- stunt_season %>% 
      group_by(offense) %>% 
      summarise(plays = n()) %>% 
      arrange(-plays) %>% 
      group_by(offense) %>% 
      top_n(n = 1) %>% 
      left_join(teams_colors_logos, by = c('offense' = 'team_abbr'))%>% 
      select(-plays)
    
    offense_stuntoe <- stunt_season %>%
      filter(!is.na(offense)) %>%
      group_by(offense) %>%
      summarize(actual_stunts = sum(stunt, na.rm = T),
                expec_stuntrate = mean(stunt_pred, na.rm = T),
                SROE = mean(stunts_oe, na.rm = T),
                PBLK = mean(PBLKG))
    
    offense_stuntoe <- offense_stuntoe %>%
      left_join(off_colors, by = c("offense"))
    
    offense_gt <- offense_stuntoe %>% 
      select(offense, team_logo_espn, actual_stunts, PBLK, SROE) %>% 
      mutate_if(is.numeric, ~round(., 2)) %>%
      arrange(-SROE) %>%
      ungroup() %>%
      mutate(rank = row_number()) %>%
      dplyr::select(rank, everything())
    
    offense_gt %>% 
      gt() %>%
      text_transform(
        locations = cells_body(c(team_logo_espn)),
        fn = function(x){
          web_image(
            url = x,
            height = px(35)
          )
        }
      ) %>%
      cols_label(
        rank = "Rank",
        offense = "Offense",
        team_logo_espn = "",
        actual_stunts = "# Stunts",
        PBLK = "Pass Block Grade",
        SROE = "Stunt Rate Over Expected") %>%
      data_color(
        columns = c(SROE),
        colors = scales::col_numeric(
          palette = c("#cf3e53", "#bfb202", "#00a2b3"),
          domain = NULL
        )
      ) %>%
      opt_align_table_header(align = "center") %>%
      cols_align("center") %>%
      opt_row_striping() %>%
      gt_theme_538()
  }, width = 850)
  
}
# Run the application 
shinyApp(ui = ui, server = server)
