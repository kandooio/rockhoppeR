library(tidyverse)
library(rvest)
library(magrittr)
library(ggmap)
library(stringr)

getSQL <- function(sqlText){}

genem <- getSQL("SELECT
    CASE
                WHEN r.country_name = 'United States'
                THEN 'USA'
                WHEN r.country_name = 'United Kingdom'
                THEN 'UK'
                ELSE r.country_name
                END                               AS country_name,
                COUNT(DISTINCT(r.mpp_account_id)) AS user_count
                FROM
                datalayer.vw_Realtime_data_latest r
                WHERE
                r.logdate >= CURRENT_DATE - 7
                AND r.logdate <> CURRENT_DATE
AND NOT regexp_like('Ireland|United Kingdom|United States|France|Germany|Spain|Poland|Italy|Portugal|Canada|Australia|Netherlands|Belgium|Luxembourg',r.country_name)
                AND r.mpp_account_id IN
                (
                SELECT DISTINCT
                y.accountid AS mpp_account_id
                FROM
                localuse.mailing_list_extract x
                INNER JOIN
                datalayer.subscriptions y
                ON
                x.email = y.uki
                WHERE
                x.list ='gen_em_digest'
                AND y.logdate::DATE = CURRENT_DATE - 1
                AND y.subscriptionstatus = 'Active Subscription')
                GROUP BY
                1")

europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","United Kingdom","Northern Ireland","Norway")

library(rworldxtra)
worldMap <- getMap(resolution="high")
indEU <- which(worldMap$NAME%in%europeanUnion)


europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

europeCoords <- do.call("rbind", europeCoords)
value <- sample(x = seq(0,3,by = 0.1), size = length(europeanUnion),
                replace = TRUE)
europeanUnionTable <- data.frame(country = europeanUnion, value = value)
europeCoords$value <- europeanUnionTable$value[match(europeCoords$region,europeanUnionTable$country)]

P <- ggplot() + geom_polygon(data = europeCoords, aes(x = long, y = lat, group = region, fill = value),
                             colour = "black", size = 0.1) +
  coord_map(xlim = c(-13, 35),  ylim = c(32, 71))

P <- P + scale_fill_gradient(name = "Growth Rate", low = "#FF0000FF", high = "#FFFF00FF", na.value = "grey50")


P <- P + theme(#panel.grid.minor = element_line(colour = NA), panel.grid.minor = element_line(colour = NA),
  #panel.background = element_rect(fill = NA, colour = NA),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(), axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(), axis.title = element_blank(),
  #rect = element_blank(),
  plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))


P



map.world <- map_data("world")




as.factor(genem$country_name) %>% levels()

map.world_joined <- left_join(map.world, genem, by
                              = c('region' = 'country_name'))

map.world_joined <-
  map.world_joined %>% mutate(fill_flg = ifelse(user_count >= -100, F, T))
  # filter(!grepl('Ireland|UK|USA|France',region))
head(map.world_joined)

ggplot() +
  geom_polygon(data = map.world_joined,
               aes(
                 x = long,
                 y = lat,
                 group = group,
                 fill = fill_flg,
                 alpha = user_count
               )) +
  ggtitle("Diaspora Distributions (Abroad Email Digest)") +
  #scale_fill_manual(values = c("#CCCCCC","#e60000")) +
  theme(
    text = element_text(family = "Gill Sans", color = "#FFFFFF"),
    panel.background = element_rect(fill = "#444444"),
    plot.background = element_rect(fill = "#444444"),
    panel.grid = element_blank(),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 10),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )

