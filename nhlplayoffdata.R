# team data collection ----------------------------------------------------
win.rate <- 95/82

team.ids <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\nhlstats\\teams.csv', 
                     col_names=c('team', 'name'))

Team.Data.Collect <- function(x){
  print(paste('https://www.hockey-reference.com/teams/', x, '/2018_games.html', sep = ''))
  Sys.sleep(1)
  getURL(paste('https://www.hockey-reference.com/teams/', x, '/2018_games.html', sep = '')) %>% 
    htmlParse() %>%
    xpathSApply('//*[@id="games"]/tbody/tr//td[position()=7 or position()=8]', xmlValue) %>%
    matrix(ncol=2, byrow=T) %>%
    as.tibble() %>%
    rowid_to_column('game') %>%
    add_row(game = 0, V1 = '', V2 = '', .before = 1) %>%
    mutate(points = ifelse(V1 == 'W', 2, ifelse(V1 == 'L' & (V2 %in% c('OT', 'SO')), 1, 0)),
           total.points = cumsum(points),
           resid = total.points - game*win.rate, 
           team = x,
           last = last(game[resid > 0 & lag(resid) < 0])
    )
}

points.data <- team.ids$team %>%
  map_df(~Team.Data.Collect(.)) %>%
  select(-V1, -V2) %>%
  select(team, everything())

points.data %>%
  ggplot(aes(game, resid)) +
  geom_hex() +
  geom_hline(yintercept=0, color='yellow')

points.data %>%
  ggplot(aes(game, resid)) +
  geom_point() + 
  geom_hline(yintercept=0, color='yellow') +
  facet_wrap(~team)

summary.points.data <- points.data %>%
  group_by(team) %>%
  summarise(min = min(resid),
            max = max(resid),
            total.points = max(total.points), 
            end.position = last(resid),
            last = last(game[(resid >= 0 & lag(resid) <= 0) | (resid <= 0 & lag(resid) >= 0)])
  ) %>%
  arrange(desc(total.points))

ggplot(summary.points.data, aes(reorder(team, total.points), end.position, color=total.points > 95)) +
  geom_point(size=2, show.legend=FALSE) +
  geom_segment(aes(x=team, xend=team, y=min, yend=max), show.legend=FALSE) +
  geom_hline(yintercept=0, color='black') +
  geom_text(aes(y=-35, label=last), show.legend=FALSE) +
  scale_y_continuous(limits=c(-35, 25), breaks=seq(-35, 25, 5)) +
  coord_flip() +
  labs(y='Net Points',
       x='Team',
       title='NHL Team Position and Season Range') +
  theme_bw() + 
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank())










#http://www.sportsclubstats.com/NHL.html
