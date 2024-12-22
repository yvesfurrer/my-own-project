#my own project


# load packages -----------------------------------------------------------

library(tidyverse)
library(ggthemes)
library(cowplot)

# load data ---------------------------------------------------------------

#with read_csv, it did not work, because ; were use to separate the colums.
#therefore, i used the read_delim function:

imdb <- 
  read_delim(
    "data/imdb.csv", 
    delim = ";"
  )

# spec(imdb)
# view(imdb)
# problems(imdb)

#now everything works (smile)


# save data ---------------------------------------------------------------

write_rds(
  x = imdb,
  file = "df_imdb.rds",
  compress = "gz"
)


# work with the data ------------------------------------------------------

#looking at the data:
#
# imdb$IMDb_rating
# imdb$Title
# imdb$actor_1_name
#
# imdb %>% 
#   colnames()
#
#   imdb %>% 
#     mutate(
#       budget = format(budget, scientific = FALSE)
#     )
# 
# 1:20 %>% 
#   sum()
# 
# view(a)

#changing the dataset:

a <- 
  imdb %>% 
    separate(
      col = 'actor_1_name',
      into = c("actor_1_first_name", "actor_1_surname"),
      sep = " ",
      extra = "merge"
    ) %>% 
    rename(
      "minutes" = Runtime,
      "year" = title_year
          ) %>% 
    select(
    -actor_3_name,
    -actor_2_name,
    -actor_1_facebook_likes,
    -actor_2_facebook_likes,
    -actor_3_facebook_likes
    ) 

# view(a)

b <- 
  a %>% 
    mutate(
      budget = round(budget / 1e6, digits = 0)
        ) %>% 
    rename(
      "budget_mil" = budget
      ) %>% 
    filter(
      year >=2011
      ) %>% 
    arrange(
      desc(budget_mil)
    )

# view(b)

#experimenting with if_else:

c <- 
  b %>% 
    mutate(
      age = if_else(year > 2015, "newer", "older"),
      profitability = 100*(IMDb_rating / budget_mil),
      profitability = round(profitability, digits = 0),
      highly_profitable = if_else(profitability > 100, "yes", "no")
    ) %>% 
    arrange(
      desc(profitability)
    ) %>% 
    relocate(IMDb_rating, .after = budget_mil
    ) %>% 
    relocate(profitability, .after = IMDb_rating
    ) %>% 
    relocate(highly_profitable, .after = profitability
    )

# view(c)

x <- 
  c %>% 
    filter(highly_profitable == "yes") %>% 
    count() %>% 
    pull(n)

paste("Number of highly profitable movies:", x)

y <- 
  c %>% 
    summarise(
      mean_profitability = mean(profitability)) %>% 
    pull(mean_profitability)

paste("mean profitability:", round(y, 2))

z <- 
  c %>% 
  summarise(
    mean_IMDb_rating = mean(IMDb_rating)) %>% 
  pull(mean_IMDb_rating)

paste("mean IMDb rating:", round(z, 2))

# plot --------------------------------------------------------------------

d <- 
  c %>% 
    select(
      IMDb_rating,
      budget_mil
    )

view(d)

d_plot <- 
  ggplot(
    data = d,
    mapping = aes(
      x = budget_mil,
      y = IMDb_rating
      )
  ) +
  geom_point(
    size= 2, colour ="blue"
  ) + 
  #change dots
  geom_hline(
    yintercept = z, 
    colour = "lightblue", 
    size = 1,
    alpha = 0.5
  ) + 
  #change text
  labs(
    title = "Profitability of recent movies",
    subtitle = "a comparisson of IMDb ratings and the movie budgets",
    y = "IMDb rating",
    x = "Budget (in million dollars)",
    caption = "© Yves Furrer"
  )+ 
  #change design
  theme_minimal() +  
    theme(
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  
      plot.subtitle = element_text(hjust = 0.5, size = 11),  
      axis.title = element_text(size = 14),  
      axis.text = element_text(size = 12),  
      panel.grid.major = element_line(size = 0.5, linetype = "dotted", colour = "gray"),  
      panel.grid.minor = element_blank()  
    ) + 
    #change axis
    scale_x_continuous(expand = c(0, 0), limits = c(0, 270)
    ) +  
    scale_y_continuous(expand = c(0, 0), limits = c(7, 9)
    ) + 
    #regression
    geom_smooth(method = "lm", se = TRUE, colour = "red"
    )

plot(d_plot)

# save the plot:
ggsave(
  filename = "Profitability of movies.jpg",
  plot = d_plot, 
  units = "cm",
  width = 15,height = 10, 
  dpi = 300
)


# analysis ----------------------------------------------------------------

#is the correlation significant?

d_test <- cor.test(d$budget_mil, d$IMDb_rating, use = "complete.obs")

d_test

#result sentence:

d_test_significant <- 
  if_else(d_test$p.value < 0.05, "is significant.", "is not significant.")

result <- 
  paste(
    "The correlation between a movies budget and its IMDb rating", 
    d_test_significant
)

result  



