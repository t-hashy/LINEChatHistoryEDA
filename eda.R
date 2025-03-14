# Loadings ----
library(dplyr) # Munipulate the data frame
library(ggplot2) # Rich plotting
library(lubridate) # Rich date-times

# Import the chat data ----
data <- readRDS("Chat.rds")
head(data)
glimpse(data)

# Shape the summarized data ----
data_terms_each <- data |>
  select(date, name, term) |>
  unique() |>
  group_by(date, name) |>
  summarise( terms_each = n() )
head(data_terms_each)
glimpse(data_terms_each)
data_terms_all <- data_terms_each |>
  select(date, terms_each) |>
  group_by(date) |>
  summarise(terms_all = sum(terms_each))

data_terms_all_seq <- data.frame(
  date = seq(from = min(data_terms_all$date), to = max(data_terms_all$date), by = 1)
) |> mutate(
  ma = sapply(date, function(current_date) {
      duration = 14
      start_date <- current_date - days(duration) # Calculate the start of the 7-day window
      # Filter for dates within the window
      relevant_counts <- data_terms_all$terms_all[data_terms_all$date > start_date & data_terms_all$date <= current_date]
      if (length(relevant_counts) > 0) {
        sum(relevant_counts, na.rm = TRUE)/duration # Calculate the average if there are values
      } else {
        0 # Return NA if no values in the window
      }
    })
  ) |>
  left_join(data_terms_all)
data_terms_all_seq$terms_all[is.na(data_terms_all_seq$terms_all)] <- 0
head(data_terms_all_seq)
glimpse(data_terms_all_seq)

data_daycount <- data_terms_all_seq |>
  mutate(
    day_count = ifelse(terms_all > 0, 1, 0),
    ym = format(date, "%Y年%m月") |> as.factor()
  ) |>
  group_by(ym) |>
  summarise( av = mean(day_count))
head(data_daycount)
glimpse(data_daycount)

data_letters <- data |>
  mutate( letters = gsub("\\(emoji\\)", "E", message) ) |>
  mutate( letters = gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", letters) ) |>
  mutate( letters = gsub("[[:space:]]", "", letters)) |>
  mutate( message_len = nchar(letters)) |>
  group_by(date, name, term) |>
  summarise( text_len = sum(message_len))
head(data_letters)
glimpse(data_letters)

# EDA ----

color_names <- c("#337777", "#ee8888")

## Daily Terms ----
ggplot() +
  geom_bar(data = data_terms_each, aes(x = date, y = terms_each, fill = name, group = name),stat = "identity") +
  geom_line(data = data_terms_all_seq, aes(x = date, y = ma, color = "送信頻度の２週間移動平均（２人合計）"), linewidth = 1.5, alpha = .8) +
  geom_text(aes(
    x = ymd("2024-12-12"),
    y = 4,
    vjust = 0,
    label = paste0(
      "メッセージを１回以上やりとりした日数の月別平均\n",
      paste(
        sep = "\n",
        paste(data_daycount$ym[1], formatC(data_daycount$av[1], digits = 1, format = "f"), "回（",formatC(1/data_daycount$av[1], digits =  1, format = "f"), "日に１回程度）" ),
        paste(data_daycount$ym[2], formatC(data_daycount$av[2], digits = 1, format = "f"), "回（",formatC(1/data_daycount$av[2], digits =  1, format = "f"), "日に１回程度）" ),
        paste(data_daycount$ym[3], formatC(data_daycount$av[3], digits = 1, format = "f"), "回（",formatC(1/data_daycount$av[3], digits =  1, format = "f"), "日に１回程度）" ),
        paste(data_daycount$ym[4], formatC(data_daycount$av[4], digits = 1, format = "f"), "回（",formatC(1/data_daycount$av[4], digits =  1, format = "f"), "日に１回程度）" )
        
        )
    ),
    hjust = 0
    )
  ) +
  scale_x_date(date_breaks = "3 days", limits = c(min(data_terms$date), max(data_terms$date))) +
  scale_y_continuous(breaks = 1:max(data_terms_all$terms_all)) +
  scale_fill_manual(values = color_names) +
  scale_color_manual(values = c("送信頻度の２週間移動平均（２人合計）" = "#777733")) +
  labs(
    title = "LINEでのやりとりの数の推移",
    caption = "*メッセージ送信頻度：メッセージ送信の回数。１回に複数メッセージ送った場合も頻度１として計測。",
    x = NULL,
    y = "メッセージ送信頻度"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = .5, colour = "#333333"),
    plot.caption = element_text(hjust = 1, colour = "#555555"),
    axis.title = element_text(colour = "#333333"),
    axis.text.x = element_text(angle = 330, hjust = 0.1),
    legend.title = element_blank(),
    legend.position = "top"
  )

## Message Letters ----
ggplot(data = data_letters, aes(x = date, y = text_len, colour = name, fill = name, group = name)) +
  geom_point(shape = 8, size = 4) +
  geom_smooth(se = F) +
  scale_x_date(date_breaks = "3 days") +
  scale_fill_manual(values = color_names) +
  scale_color_manual(values = color_names) +
  labs(
    title = "メッセージ１回あたりの文字数の推移",
    caption = "*メッセージは１度に複数送った場合でも１ターンを１回として測定。またURLは除く。",
    x = NULL,
    y = "メッセージ１回あたりの文字数"
  ) +
  theme_classic() +
  theme(
    text = element_text(colour = "#333333"),
    plot.title = element_text(face = "bold", hjust = .5),
    plot.caption = element_text(hjust = 1, colour = "#777777"),
    axis.text.x = element_text(angle = 330),
    legend.title = element_blank()
  )
