Titanic <- readRDS("titanicBoats.rds")

mean_age <- mean(na.omit(Titanic$age))
Titanic$age[is.na(Titanic$age)] <- mean_age

survivers <- Titanic[Titanic$survived == 1,]

classsurviver <- as.data.frame(table(survivers$pclass))

DEAD <- Titanic[Titanic$survived == 0,]

survive <- Titanic %>% grouping(pclass) %>%
  summary(
    alive=survived ==1,
    dead = survived == 0
  )


