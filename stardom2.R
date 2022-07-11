# 1.1) Working Directory & Packages ---------------------------------------
options(java.parameters = "-Xmx8000m")
options(scipen=999)

# UPDATE TO YOUR WORKING DIRECTORY
setwd("D:/Alejandro/Documents/R Projects/StardomData")

library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
library(scales)
library(colorspace)
library(ggthemes)

library(showtext)

library(igraph)
library(visNetwork)


# IBM PLEX SANS
font_add(family = "IBM Plex Sans", # Name you want to use 
         regular = "IBMPlexSans-Regular.ttf",
         bold = "IBMPlexSans-Bold.ttf")

# IBM PLEX SANS LIGHT (FOR PLOT TEXT)
font_add(family = "IBM Plex Sans Light", # Name you want to use 
         regular = "IBMPlexSans-Light.ttf")

showtext_auto()


# 1.2) CUSTOM FUNCTIONS & PALETTES ----------------------------------------

# levels for belts
belt_hierarchy <- c('NONE', 'FUTURE BELT', 'HIGH SPEED BELT', 
                    "ARTIST BELT", "TAG BELT",
                    'SWA BELT', 'WHITE BELT', 'RED BELT') %>% rev()

# belt color palette
belt_pal <- c('#fb6a4a',
              '#eff3ff',
              '#dfc27d',
              # "#8dd3c7",
              # "#fcc5c0",
              '#969696',
              '#abd9e9')

show_col(belt_pal)

# custom function for title defenses
belter <- function(string = NULL) {
  out <- case_when(string %>% str_detect('CONTENDER|TOURNAMENT') ~ 'NONE',
                   string %>% str_detect('RED') ~ 'RED BELT',
                   string %>% str_detect('WHITE') ~ "WHITE BELT",
                   string %>% str_detect('TAG') ~ "TAG BELT",
                   string %>% str_detect('ARTIST') ~ "ARTIST BELT",
                   string %>% str_detect("HIGH SPEED") ~ "HIGH SPEED BELT",
                   string %>% str_detect('SWA') ~ "SWA BELT",
                   string %>% str_detect('FUTURE') ~ 'FUTURE BELT',
                   T ~ "NONE")
  out <-  factor(out, levels = belt_hierarchy)
  return(out)
}

# faction color palette function
gang_pal <- function(string = NULL) {
  out <- case_when(string %>% str_detect("QUEEN'S QUEST") ~ "#fdcc8a",
                   string %>% str_detect("OEDO TAI") ~ "#8856a7",
                   string %>% str_detect("DONNA DEL MONDO") ~ "#252525",
                   string %>% str_detect("COSMIC ANGELS") ~ "#fcc5c0",
                   string %>% str_detect("STARS") ~ "#f7f7f7",
                   string %>% str_detect("GOD'S EYE") ~ "#fb6a4a",
                   string %>% str_detect("UNAFFILIATED") ~ "#8cc2ca",
                   T ~ "#969696")
  return(out)
}

# faction color palette function
belter_pal <- function(string = NULL) {
  out <- case_when(string %>% str_detect('RED') ~ '#ef6f6a',
                   string %>% str_detect('WHITE') ~ "#fcfbfd",
                   string %>% str_detect("HIGH SPEED") ~ "#6388b4",
                   string %>% str_detect('SWA') ~ "#EDC948",
                   string %>% str_detect('FUTURE') ~ '#55ad89',
                   T ~ "NONE")
  return(out)
}


time_arrow <- function(date = NULL) {
  curr_yr = year(Sys.Date())
  diff = curr_yr - year(date)
  
  out <- case_when(diff %in% 0:1 ~ 0.7,
                   diff %in% 2:3 ~ 0.6,
                   diff %in% 4:5 ~ 0.4,
                   diff %in% 6:8 ~ 0.3,
                   T ~ 0.2)
  
  return(out)
}




# title %>% count(FACTION) %>% arrange(FACTION) %>%
#   mutate(pal = gang_pal(FACTION)) %>%
#   pull(pal) %>%
#   # lighten(amount = 0.5) %>%
#   show_col()




# 1.3) LOAD DATA ----------------------------------------------------------
# manual mapping, kinda sucks
faction_map <- read_csv('Stardom Factions.csv') %>%
  mutate(NAME = str_squish(NAME))

# SET "Date" FIELD AS DATE-TYPE USING ymd(...)
# FLAG SINGLE MATCHES (BASED ON MATCH FIELDS WITH "/" OR "&)
# ADDITIONAL FLAG FOR 3 / 4 WAY MATCHES (MAY NOT BE CAPTURED IN SINGLE_FLG)
stardom <- read_excel("Stardom Match Guide.xlsx",
                      col_types = c("text", "text", "text", 
                                    "text", "text", "text", "text")) %>%
  rename_with(toupper) %>%
  mutate(DATE = ymd(DATE),
         MATCH = toupper(MATCH),
         NOTES = toupper(NOTES),
         WINNER = toupper(WINNER),
         SINGLE_FLG = !str_detect(string = MATCH, pattern = "/|&|MATCH"),
         MULTI_FLG = str_count(string = MATCH, pattern = "VS") > 1,
         BELT = belter(NOTES),
         TITLE_FLG = BELT != 'NONE',
         WINNER = case_when(WINNER == 'KAIRI HOJO' ~ "KAIRI", 
                            WINNER %in% c('KAORI YONEYAMA', "DEATH YAMA-SAN") ~ "FUKIGEN DEATH",
                            T ~ WINNER)) %>%
  mutate(across(ends_with("_FLG"), ~ replace_na(., FALSE)))



# 2.1) VISNETWORK prep ----------------------------------------------------
data <- stardom %>%
  # filter(DATE >= '2019-01-01') %>%
  filter(SINGLE_FLG) %>%
  filter(!str_detect(MATCH, 'RUMBLE')) %>%
  mutate(INDEX = row_number()) %>%
  separate_rows(MATCH, sep = 'VS') %>%
  mutate(MATCH = str_squish(MATCH)) %>%
  left_join(faction_map) %>%
  select(-MATCH)


title <- data %>%
  filter(TITLE_FLG,
         # BELT %in% c('FUTURE BELT'),
         # FACTION != 'NOT ACTIVE',
         DATE >= '2010-01-01')

# title <- title %>%
#   group_by(INDEX) %>%
#   filter(sum(NAME == 'UTAMI HAYASHISHITA') >= 1 ) %>%
#   ungroup()

# title <- title %>%
#   filter(FACTION == 'NOT ACTIVE')

# title <- title %>%
#   filter(BELT %in% c('RED BELT'))



nodes <- title %>%
  distinct(NAME, FACTION) %>%
  filter(!is.na(NAME)) %>%
  arrange(NAME) %>%
  mutate(ID = row_number()) %>%
  left_join(title %>%
              count(NAME) %>%
              mutate(SIZE = n * 3.5)) %>%
  rename(GROUP = FACTION,
         LABEL = NAME) %>%
  mutate(color.background = gang_pal(GROUP),
         color.border = 'black',
         color.highlight.background = lighten(color.background, amount = 0.5),
         color.highlight.border = color.background,
         font.color = lighten(color.background, amount = 0.8),
         font.face = "IBM Plex Sans",
         font.size = case_when(GROUP != 'NOT ACTIVE' ~ 50,
                               T ~ 20),
         borderWidth = 2)



edges <- title %>%
  distinct(WINNER, NAME, BELT, INDEX, LINK, DATE) %>%
  filter(WINNER != NAME) %>%
  left_join(nodes %>% select(LABEL, ID, GROUP),
            by = c('WINNER' = "LABEL")) %>%
  rename(FROM = ID,
         FACTION = GROUP) %>%
  left_join(nodes %>% select(LABEL, ID),
            by = c('NAME' = "LABEL")) %>%
  rename(TO = ID) %>%
  filter(!is.na(FROM),
         !is.na(TO)) %>%
  rename(GROUP = BELT,
         TITLE = LINK) %>%
  mutate(WIDTH = 5,
         COLOR.COLOR = belter_pal(GROUP),
         COLOR.HIGHLIGHT = COLOR.COLOR,
         COLOR.OPACITY = time_arrow(DATE),
         ARROWS = "to",
         smooth.type = 'curvedCW',
         smooth.roundness = .5,
         selectionWidth = 7)


visNetwork(nodes %>%
             rename_with(tolower, -borderWidth), 
           edges %>%
             rename_with(tolower, -selectionWidth),
           height = "1000px", width = "100%",
           background = '#303030') %>%
  visOptions(highlightNearest = TRUE,
             selectedBy = 'group',
             nodesIdSelection = TRUE) %>%
  visPhysics(solver = "forceAtlas2Based",
             forceAtlas2Based = list(gravitationalConstant = -300,
                                     springConstant = 0.02,
                                     avoidOverlap = 0.5,
                                     springLength = 20,
                                     damping = 0.15),
             minVelocity = 25)


# 3.1) Stats: Degree  --------------------------------------------------------------
degree_df <- title %>%
  group_by(NAME, FACTION) %>%
  summarise(degree = n_distinct(INDEX),
            win = sum(WINNER == NAME),
            loss = sum(WINNER != NAME),
            win_prct = win / degree,
            loss_prct = 1 - win_prct) %>%
  arrange(-degree) %>%
  ungroup() %>%
  mutate(degree.bin = case_when(degree >= 20 ~ "[20, INF)",
                                degree >= 10 & degree < 20 ~ "[10, 20)",
                                degree >= 5 & degree < 10  ~ "[5, 10)",
                                degree >= 3 & degree < 5 ~ "[3, 5)",
                                T ~ '[1, 3)'),
         degree.bin = factor(degree.bin, levels = c("[1, 3)", "[3, 5)",  "[5, 10)", "[10, 20)", "[20, INF)")))



title %>%
  filter(BELT %in% c('RED BELT', 'WHITE BELT')) %>%
  group_by(NAME, FACTION) %>%
  summarise(degree = n_distinct(INDEX),
            win = sum(WINNER == NAME),
            loss = sum(WINNER != NAME)) %>%
  mutate(loss = loss * -1) %>%
  gather(RESULT, COUNT, -NAME:-degree) %>%
  filter(degree >= 1,
         FACTION != 'NOT ACTIVE') %>%
  ggplot(aes(reorder(NAME, abs(COUNT), sum), COUNT, fill = RESULT)) +
  geom_col(color = 'grey20')+
  geom_hline(yintercept = 0)+
  scale_y_continuous(name = '\nCount of White & Red Belt Matches',
                     breaks = seq(-10,15, 5),
                     labels = c('10\nLosses', '5\nLosses', '0', '5\nWins', '10\nWins', '15\nWins'))+
  coord_flip()+
  theme_classic()+
  theme(text = element_text(family="IBM Plex Sans"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 16, family = 'IBM Plex Sans Light'),
        legend.title = element_text(size = 20),
        plot.title = element_text(face = 'bold', color = 'grey20', size = 30),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 24),
        panel.border = element_blank(),
        plot.background = element_blank())


# 3.2) igraph sandbox -----------------------------------------------------
# https://kateto.net/wp-content/uploads/2018/03/R%20for%20Networks%20Workshop%20-%20Ognyanova%20-%202018.pdf

net <- graph_from_data_frame(d = edges %>%
                               select(FROM, TO, GROUP) %>%
                               rename_with(tolower),
                             vertices = nodes %>%
                               select(ID, LABEL, GROUP, n) %>%
                               rename_with(tolower),
                             directed = T)


plot(net, edge.arrow.size=.04, vertex.label=NA, pch=21, vertex.size = sqrt(V(net)$n),
     vertex.color = gang_pal(V(net)$group),
     layout=layout_with_fr(net))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# density: The proportion of present edges from all possible edges in the network.
ecount(net)/(vcount(net)*(vcount(net)-1))
# reciprocity: The proportion of reciprocated ties (for a directed network).
reciprocity(net)

# diameter: A network diameter is the longest geodesic distance (length of the shortest path between two nodes)
#   in the network. In igraph, diameter() returns the distance, while get_diameter() returns the
#   nodes along the first found path of that distance.
diameter(net, directed=T, weights=NA)
# Average path length: the mean of the shortest distance between each pair of nodes in the network
mean_distance(net, directed=T)
# longest path sequence
tibble(ID = get_diameter(net, directed=T) %>% 
         as_ids %>% 
         as.numeric()) %>%
  left_join(nodes %>%
              select(ID, GROUP, LABEL))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# HUB: large numbers of OUTGOING links
hub <- hub_score(net, weights = NA)$vector %>% 
  as_tibble(rownames = NA) %>%
  rownames_to_column("ID") %>%
  mutate(ID = as.numeric(ID)) %>%
  left_join(nodes %>%
              select(ID, GROUP, LABEL)) %>%
  arrange(-value) %>%
  rename(NAME = LABEL,
         HUB = value)

# AUTHORITY: large number of INCOMING LINKS
auth <- authority_score(net, weight = NA)$vector %>% 
  as_tibble(rownames = NA) %>%
  rownames_to_column("ID") %>%
  mutate(ID = as.numeric(ID)) %>%
  left_join(nodes %>%
              select(ID, GROUP, LABEL)) %>%
  arrange(-value) %>%
  rename(NAME = LABEL,
         AUTHORITY = value)

# degree / hub / authority table
degree_df <- title %>%
  group_by(NAME, FACTION) %>%
  summarise(degree = n_distinct(INDEX),
            win = sum(WINNER == NAME),
            loss = sum(WINNER != NAME),
            win_prct = win / degree,
            loss_prct = 1 - win_prct) %>%
  arrange(-degree) %>%
  ungroup() %>%
  mutate(degree.bin = case_when(degree >= 20 ~ "[20, INF)",
                                degree >= 10 & degree < 20 ~ "[10, 20)",
                                degree >= 5 & degree < 10  ~ "[5, 10)",
                                degree >= 3 & degree < 5 ~ "[3, 5)",
                                T ~ '[1, 3)'),
         degree.bin = factor(degree.bin, levels = c("[1, 3)", "[3, 5)",  "[5, 10)", "[10, 20)", "[20, INF)"))) %>%
  left_join(hub %>%
              select(NAME, HUB)) %>%
  left_join(auth %>%
              select(NAME, AUTHORITY))

# Closeness (centrality based on distance to others in the graph)
#   Closeness centrality indicates how close a node is to all other nodes in the network. 
#   It is calculated as the average of the shortest path length from the node to every other node in the network
#   Individuals with high 'influence' over network
closeness(net, mode="all", weights=NA) %>% 
  as_tibble(rownames = NA) %>%
  rownames_to_column("ID") %>%
  mutate(ID = as.numeric(ID)) %>%
  left_join(nodes %>%
              select(ID, GROUP, LABEL)) %>%
  arrange(-value)

# Eigenvector 
#   Eigenvector centrality is used to measure the level of influence of a node within a network. 
#   Each node within the network will be given a score or value: 
#   the higher the score the greater the level of influence within the network. 
#   This score is relative to the number of connections a node will have to other nodes. 
#   Connections to high-scoring eigenvector centrality nodes 
#   contribute more to the score of the node than equal connections to low-scoring nodes.
eigen_df <-eigen_centrality(net, directed=T, weights=NA)$vector %>% 
  as_tibble(rownames = NA) %>%
  rownames_to_column("ID") %>%
  mutate(ID = as.numeric(ID)) %>%
  left_join(nodes %>%
              select(ID, GROUP, LABEL)) %>%
  arrange(-value)

# Betweenness (Number of geodesics that pass through the node or the edge.)
#   Vertices with high betweenness may have considerable influence 
#   within a network by virtue of their control over information passing between others. 
#   They are also the ones whose removal from the network will most disrupt communications 
#   between other vertices because they lie on the largest number of paths taken by messages.
between_df <- betweenness(net, directed=T, weights=NA) %>% 
  as_tibble(rownames = NA) %>%
  rownames_to_column("ID") %>%
  mutate(ID = as.numeric(ID)) %>%
  left_join(nodes %>%
              select(ID, GROUP, LABEL)) %>%
  arrange(-value)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# shortest path between first and current RED BELT CHAMPIONS
tibble(ID = shortest_paths(net,
               from = V(net)[label == "SYURI"],
               to = V(net)[label == "NANAE TAKAHASHI"],
               output = 'vpath')$vpath[[1]] %>%
  as_ids %>%
  as.numeric()) %>%
  left_join(nodes %>%
              select(ID, GROUP, LABEL))
# white belt
tibble(ID = shortest_paths(net,
                           from = V(net)[label == "YUZUKI AIKAWA"],
                           to = V(net)[label == "SAYA KAMITANI"],
                           output = 'vpath')$vpath[[1]] %>%
         as_ids %>%
         as.numeric()) %>%
  left_join(nodes %>%
              select(ID, GROUP, LABEL))

# HIGH SPEED
tibble(ID = shortest_paths(net,
                           from = V(net)[label == "NATSUKI TAIYO"],
                           to = V(net)[label == "AZM"],
                           output = 'vpath')$vpath[[1]] %>%
         as_ids %>%
         as.numeric()) %>%
  left_join(nodes %>%
              select(ID, GROUP, LABEL))

# FUTURE
tibble(ID = shortest_paths(net,
                           from = V(net)[label == "HANAN"],
                           to = V(net)[label == "STARLIGHT KID"],
                           output = 'vpath')$vpath[[1]] %>%
         as_ids %>%
         as.numeric()) %>%
  left_join(nodes %>%
              select(ID, GROUP, LABEL))


# CLIQUE
net.sym <- as.undirected(net, mode= "collapse",
                         edge.attr.comb=list(weight="sum", "ignore"))


clique_df <- tibble(LABEL = lapply(cliques(net.sym, min = 3),
              function(x) { paste(V(net)$label[x], collapse = ", ") }) %>%
         unlist()) %>%
  mutate(CLIQUE = row_number(),
         CLIQUE_SIZE = str_count(LABEL, ", ") + 1,
         FULL = LABEL)%>%
  separate_rows(LABEL, sep = ", ", ) %>%
  left_join(nodes %>%
              select(ID, GROUP, LABEL)) %>%
  arrange(-CLIQUE_SIZE, CLIQUE) %>%
  ungroup()



# dumb and doesn't work ignore
ceb <- cluster_edge_betweenness(net)

cluster_df <- tibble(ID = membership(ceb) %>% names %>% as.numeric,
       MEMBERSHIP = membership(ceb) %>% unname) %>%
  group_by(MEMBERSHIP) %>%
  mutate(CLUST_SIZE = n_distinct(ID)) %>%
  arrange(-CLUST_SIZE, MEMBERSHIP) %>%
  left_join(nodes %>%
              select(ID, GROUP, LABEL)) %>%
  mutate(FULL = paste(LABEL, collapse = ", "))
  





# 4.1) SANDBOX - TITLE MATCHES PER FACTION (BAR) ------------------------------------------------------------
# Title Matches per Faction
# might be double counting if two members of same faction fought for a belt...
data %>%
  filter(TITLE_FLG,
         DATE >= '2022-01-01') %>%
  count(FACTION, BELT) %>%
  ggplot(aes(reorder(FACTION, -n, sum), n, fill = BELT))+
  geom_col(color = 'grey20')+
  scale_fill_manual(values = belt_pal, name = 'Singles Belt')+
  scale_y_continuous(limits = c(0, 13), expand = c(0,0),
                     pretty_breaks(5),
                     name = 'Count of Title Matches\n')+
  theme_classic()+
  theme(text = element_text(family="IBM Plex Sans"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 16, family = 'IBM Plex Sans Light'),
        legend.title = element_text(size = 20),
        plot.title = element_text(face = 'bold', color = 'grey20', size = 30),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(angle = 37, hjust = 1, size = 20),
        axis.text.y = element_text(size = 24),
        panel.border = element_blank(),
        plot.background = element_blank())


# 4.2) SANDBOX - TITLE MATCHES PER WRESTLER X FACTION ---------------------

data %>%
  filter(TITLE_FLG,
         DATE >= '2022-01-01') %>%
  count(NAME, FACTION, BELT) %>%
  arrange(-n) %>%
  ggplot(aes(reorder(NAME, -n, sum), n, fill = BELT))+
  geom_col(color = 'grey20')+
  scale_fill_manual(values = belt_pal, name = 'Singles Belt')+
  scale_y_continuous(limits = c(0, 6), expand = c(0,0),
                     pretty_breaks(5),
                     name = 'Count of Title Matches\n')+
  facet_wrap(~ FACTION, scales = 'free_x') +
  theme_classic()+
  theme(text = element_text(family="IBM Plex Sans"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 16, family = 'IBM Plex Sans Light'),
        legend.title = element_text(size = 20),
        plot.title = element_text(face = 'bold', color = 'grey20', size = 30),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(angle = 37, hjust = 1, size = 20),
        axis.text.y = element_text(size = 24),
        panel.border = element_blank(),
        plot.background = element_blank())


# 4.3) SANDBOX - ALL SINGLES MATCHES PER WRESTLER X FACTION ---------------
data %>%
  filter(DATE >= '2022-01-01',
         !FACTION %in% c('CHOCO PRO', 'PROMINENCE', 'N/A')) %>%
  count(NAME, FACTION, BELT) %>%
  arrange(-n) %>%
  ggplot(aes(reorder(NAME, -n, sum), n, fill = BELT))+
  geom_col(color = 'grey20')+
  scale_fill_manual(values = c(belt_pal, "#d9d9d9"), name = 'Singles Belt')+
  scale_y_continuous(limits = c(0, 14), expand = c(0,0),
                     pretty_breaks(5),
                     name = 'Count of Title Matches\n')+
  facet_wrap(~ FACTION, scales = 'free_x') +
  theme_classic()+
  theme(text = element_text(family="IBM Plex Sans"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 16, family = 'IBM Plex Sans Light'),
        legend.title = element_text(size = 20),
        plot.title = element_text(face = 'bold', color = 'grey20', size = 30),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(angle = 37, hjust = 1, size = 20),
        axis.text.y = element_text(size = 24),
        panel.border = element_blank(),
        plot.background = element_blank())


# 4.4) TILE MAP? ----------------------------------------------------------
red_tile_df <- data %>%
  filter(BELT %in% c('RED BELT', 'WHITE BELT'),
         DATE >= '2018-01-01',
         WINNER != NAME) %>%
  group_by(INDEX) %>%
  summarise(A = paste(WINNER, NAME, sep = '_'),
            B = paste(NAME, WINNER, sep = '_')) %>%
  ungroup() %>%
  distinct(A, B) %>%
  gather(var, MATCH) %>%
  select(-var) %>%
  separate(col = MATCH, into = c("A", "B"), sep = '_') %>%
  mutate(FLG = 1)

red_tile_df %>%
  expand(A, B) %>%
  full_join(red_tile_df) %>%
  mutate(FLG = replace_na(FLG, 0) %>% as.factor) %>%
  ggplot(aes(A, B, fill = FLG)) +
  geom_tile(color = 'grey20')+
  scale_fill_viridis_d(option = "F")+
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits=rev)








