require(rvest) # Web Scraping
require(ggplot2) # Graficos
require(ggrepel) # Evitar que se pisen los textos

#################################
# DATA
#################################
page <- "https://uk.soccerway.com/national/england/premier-league/20172018/regular-season/r41547/?ICID=SN_01_01"
scraped_page <- read_html(page)
Teams  <- scraped_page %>% html_nodes("#page_competition_1_block_competition_tables_6_block_competition_league_table_1_table .large-link") %>% html_text() %>% as.character()
Teams <- gsub("\n","",Teams)
Teams <- trimws(Teams)
League <- scraped_page %>% html_node("h1") %>% html_text() %>% as.character()
GoalsFor <- scraped_page %>% html_nodes(".team_rank .total_gf") %>% html_text() %>% as.numeric()
GoalsAgainst <- scraped_page %>% html_nodes(".team_rank .total_ga") %>% html_text() %>% as.numeric()
df <- data.frame(Teams, GoalsFor, GoalsAgainst)

# save(df, file = "teams_EPL_2017_18.rda")

#################################
# GGPLOT IT
#################################

annotations <- data.frame(
  xpos = c(-Inf,Inf),
  ypos =  c(Inf,-Inf),
  annotateText = c("Poor Attack, Poor Defense","Strong Attack, Strong Defense"),
  hjustvar = c(0,1) ,
  vjustvar = c(1,0))



ggplot(data = df, aes(x=GoalsFor, y=GoalsAgainst)) + 
  geom_vline(xintercept=mean(GoalsFor), linetype="dashed", alpha = 0.4, colour = "red") +
  geom_hline(yintercept=mean(GoalsAgainst), linetype="dashed", alpha = 0.4, colour = "red") +
  geom_label(data = annotations, aes(x=xpos,y=ypos,hjust=hjustvar, vjust=vjustvar,label=annotateText, colour = "red", size = 1)) +
  geom_text_repel(aes(GoalsFor, GoalsAgainst, label = Teams)) +
  geom_point() +
  theme(legend.position="none") +
  ggtitle(paste0("Goals For & Against : ",League,"2017/18"))

