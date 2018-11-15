library(tidyverse)
library(lubridate)

us <- read_csv("data/NASA astronauts - US ASTRONAUTS.csv") %>%
  separate(`If military include details`, sep = ", ", into = c("rank", "wing")) %>% 
  mutate(
    retired = str_detect(wing, "\\([rR]etired\\)") %>% 
      if_else("Yes", "No", "NA"),
    reserve = str_detect(wing, "Reserve") %>% 
      if_else("Yes", "No", "NA"),
    wing = str_replace_all(wing, " \\([rR]etired\\)", ""),
    wing = str_replace_all(wing, " Reserve", ""),
    yob = as.integer(str_sub(`Date of birth`, start = -4)),
    dob = mdy(`Date of birth`),
  ) %>%
  rename(
    name = Astronaut,
    selection = `Selection Year`,
    n_flghts = `# Flights`,
    flight_hours = `Cumulative hours of space flight time`,
    group = Group,
    missions = `Missions flown`,
    job = Job,
    status = Status,
    mil_civ = `Military or civilian`,
    gender = Gender
  ) %>% 
  mutate(
    missions = str_replace_all(missions, "and", ", "),
    rank = if_else(is.na(rank), "", rank)
  ) %>% 
  select(-`Date of birth`) %>% 
  arrange(selection, dob)


us$id <- 1:dim(us)[1]

glimpse(us)


missions <- us %>% 
  select(missions) %>% 
  separate(missions, sep = "[,/]", into = LETTERS) %>% 
  gather() %>% 
  filter(!is.na(value)) %>% 
  .$value %>% 
  str_trim() %>% 
  tibble(name_raw = .) %>% 
  mutate(
    name_clean = str_replace_all(name_raw, " ", "-"),
    name_clean = str_replace(name_clean, "ST-", "STS-") %>% 
      str_replace_all("- ", "-") %>% 
      str_replace_all("\\.", "") %>%
      str_replace_all("--", "-") %>%
      str_replace("Expedition", "Exp")
  ) %>% 
  distinct()

m <- map_dfc(unique(missions$name_raw), function(x) {
   y <- str_detect(us$missions, paste0("", x, "(,| ,|$)")) %>% 
    if_else(is.na(.), FALSE, .)
   l <- list()
   l[[x]] <- y
   l
})


names(m)
m$id <- 1:dim(m)[1]

m %>% select(id, everything())

astronaut_missions <- m %>% 
  gather("name_raw", "yes", -id) %>% 
  arrange(id) %>% 
  left_join(missions) %>% 
  group_by(id, name_clean) %>% 
  summarise(yes = sum(yes) > 0) %>% 
  filter(yes) %>% 
  arrange(name_clean) %>% 
  select(-yes) %>% 
  ungroup()

nodes <- us %>% 
  transmute(
    id = id,
    label = name,
    title = glue::glue("<p><b>{rank} {name}</b></p><p>Flight Hours: {flight_hours}</p><p></p>"),
    #group = cut(selection, breaks = seq(1959, 2009, 10), include.lowest = T, dig.lab = 5),
    group = selection,
    value = sqrt(if_else(is.na(flight_hours), 0L, flight_hours))
  )

nodes <- tibble(
  group = unique(nodes$group),
  color = viridis::viridis_pal()(length(unique(nodes$group)))
) %>% left_join(nodes)


edges <- astronaut_missions %>%
  group_by(name_clean) %>% 
  nest() %>% pmap_dfr(function(name_clean , data) {
    y <- expand.grid(data$id, data$id)
    y %>% 
      filter(Var1 < Var2) %>% 
      transmute(
        from = Var1,
        to = Var2,
        label = name_clean
      )
  }) %>% 
  group_by(from, to) %>% 
  summarise(
    value = n(),
    label = glue::glue_collapse(label, sep = ", ")
  ) %>% 
  ungroup()

write_csv(nodes, "data/nodes.csv")
write_csv(edges, "data/edges.csv")


rmarkdown::render("index.Rmd", output_dir = "docs")
