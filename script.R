df <- readxl::read_xlsx("Pure_data_number_authors.xlsx", 1)
df <- df[df[["Publication category"]] == "Scientific",]
df$Year <- ordered(df$Year)
# df$Single <- df$Total.number.of.authors < 2
df_plot <- do.call(rbind, lapply(levels(df$Year), function(y){
  data.frame(Year = y,
             Single = prop.table(table(df[["Total number of authors"]][df$Year == y] < 2))[2],
             Team = prop.table(table(df[["Total number of authors"]][df$Year == y] > 5))[1]

  )
}))
df_plot$Year = ordered(df_plot$Year)
df_anal <- df
names(df_anal) <- c("Title", "date", "Year", "Authors", "Category")
res_null <- glm(Authors~1, data = df_anal, family = "poisson")
res <- glm(Authors~I(as.integer(Year)), data = df_anal, family = "poisson")
anova(res_null, res, test="Chisq")
coefs <- exp(res$coefficients)
# Annual percent increase in number of authors
(coefs[2]-1)*100


library(ggplot2)
p <- ggplot(df_plot, aes(x = Year, group = 1)) +
  geom_line(aes(y = Single), size = 1) +
  geom_point(aes(y = Single), size = 2, shape = 21, stroke = 1) +
  geom_point(aes(y = Team), size = 2, shape = 21, stroke = 1) +
  geom_line(aes(y = Team), size = 1) +
  geom_area(aes(y = Single), fill = "red", alpha =.2)+
  geom_ribbon(aes(ymin = Single, ymax = Team), fill = "blue", alpha =.2)+
  geom_ribbon(aes(ymin = Team, ymax = 1), fill = "green", alpha =.2)+
  scale_y_continuous(limits = c(0,1), expand = c(0,0))+
  scale_x_discrete(expand = c(0,0))
ggsave("proportion_single_team.png", p, "png")
