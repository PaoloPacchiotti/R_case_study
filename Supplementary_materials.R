
# RESEARCH QUESTION 1

library(tidyverse)

#=================================================================================
# 1.1 MOST PRODUCTIVE PHILOSOPHERS
#=================================================================================

# NULL or missing ID check
sum(is.na(Authorships_df$Phi_ID))
sum(is.na(Authorships_df$Art_ID))

# Duplicate check: uniqueness of each Phi_ID-Art_ID pair
sum(duplicated(Authorships_df[, c("Phi_ID", "Art_ID")]))

# We measure productivity by number of articles for each Phi_ID in Authorship_df
most_prod_phil <- Authorships_df |>
  group_by(Phi_ID) |>
  summarise(N_Articles = n_distinct(Art_ID)) |>
  left_join(Philosophers_df, by = "Phi_ID") |>
  arrange(desc(N_Articles)) |>
  select(Phi_ID, Philosopher, N_Articles)

# We check for ties and construct a top 10 ranking
top_philosophers <- most_prod_phil |>
  slice_max(order_by = N_Articles, n = 10)

# Visualisation of top philosophers
phil_plot <- ggplot(top_philosophers, 
                    aes(x = reorder(Philosopher, N_Articles), 
                        y = N_Articles)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0,10,1)) +
  labs(title = "Top 10 most productive philosophers (2005-2019, 5 journals)",
       x = NULL,
       y = "Number of distinct articles authored") +
  theme_minimal()
phil_plot

ggsave("top_philosophers.png", 
       plot = phil_plot, 
       width = 8, 
       height = 6, 
       dpi = 300)


#===================================================================================
# 1.2 MOST PRODUCTIVE INSTITUTIONS
#===================================================================================

# NULL check 
sum(is.na(Authorships_df$Inst_ID))
n_missing_inst <- sum(is.na(Authorships_df$Inst_ID))
cat("Missing Inst_ID:", n_missing_inst, "out of", nrow(Authorships_df),
    "(", round(100*n_missing_inst/nrow(Authorships_df),1), "%)\n") # Missing Inst_ID: 1.4% 

# We measure the productivity of an institution by the number of articles associated to it
most_prod_institutions <- Authorships_df |>
  filter(!is.na(Inst_ID)) |> # exclude rows with missing Inst_ID
  group_by(Inst_ID) |>
  summarise(N_Articles_x_Inst = n_distinct(Art_ID)) |>
  arrange(desc(N_Articles_x_Inst))|>
  left_join(Affiliations_df, by = "Inst_ID") |> # join Affiliations_df to get Affiliation_name and Affiliation_country
  select(Inst_ID, Affiliation_name, N_Articles_x_Inst, Affiliation_country)

# We check for ties and construct a top 10 ranking
top_institutions <- most_prod_institutions |>
  slice_max(order_by = N_Articles_x_Inst, n = 10)

# Visualisation of top institutions and their geography
inst_plot <- ggplot(top_institutions, 
                    aes(x = reorder(Affiliation_name, N_Articles_x_Inst),
                        y = N_Articles_x_Inst,
                        fill = Affiliation_country)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 10 most productive institutions (2005-2019, 5 journals)",
       x = NULL,
       y = "Number of distinct articles",
       fill = "Country") +
  theme_classic()
inst_plot

ggsave("top_institutions.png",
       plot = inst_plot, 
       width = 8, 
       height = 6, 
       dpi = 300)

##################################################################################
##################################################################################
##################################################################################
##################################################################################

# RESEARCH QUESTION 2

#=================================================================================
# 2.1 AVERAGE NUMBER OF ACKNOWLEDGMENTS FOR EACH ANALYTIC PHILOSOPHER
#=================================================================================

# 231 articles with 0 rows in Acknowledgments
missing_ack <- left_join(Articles_df, Acknowledgments_df,
                         by = "Art_ID") |>
  filter(is.na(Phi_ID))

# 103 articles mention 0 Philosophers
missing_ack_phil <- left_join(Articles_df, Acknowledgments_df,
                              by = "Art_ID") |>
  filter(!is.na(Ack_text) & is.na(Phi_ID)) |>
  summarise(no_ack_phil = n_distinct(Art_ID))

# Duplicate check: 144 rows where the same philosopher is mentioned more than once in the same article
sum(duplicated(Acknowledgments_df[, c("Art_ID", "Phi_ID")]))

#  We de-duplicate acknowledgees per article and count the number of acknowledgees per article in Acknowledgments_df
ack_per_article <- Acknowledgments_df |>
  distinct(Art_ID, Phi_ID) |>
  count(Art_ID, name = "N_Informal_Collaborators")

# We add to ack_per_article only the articles from Articles_df with an acknowledgment section
ack_per_article_full <- Articles_df |>
  filter(!is.na(Ack_text)) |>
  select(Art_ID) |>
  left_join(ack_per_article, by = "Art_ID") |> 
  mutate(N_Informal_Collaborators = replace_na(N_Informal_Collaborators, 0)) # no acknowledgees = 0, not NA

# We attribute each article's count to every one of its authors
author_ack <- Authorships_df |>
  distinct(Phi_ID, Art_ID) |>
  inner_join(ack_per_article_full, by = "Art_ID") # inner_join is used to drop the 128 articles with no acknowledgments section (still included in Authorships_df)

# We average within each philosopher first
avg_per_philosopher <- author_ack |>
  group_by(Phi_ID) |>
  summarise(mean_collab = mean(N_Informal_Collaborators))

# Mean, median, sd and mode for avg_per_philosopher
get_mode <- function(x) {
  freq_table <- table(x)
  as.numeric(names(freq_table)[freq_table == max(freq_table)])
}

mean_phi   <- mean(avg_per_philosopher$mean_collab)
median_phi <- median(avg_per_philosopher$mean_collab)
sd_phi     <- sd(avg_per_philosopher$mean_collab)
mode_phi   <- get_mode(avg_per_philosopher$mean_collab)


# Visualisation 1 (CDF based on avg_per_philosopher) -> philosopher as unit of analysis
cdf_plot <- ggplot(avg_per_philosopher,
                   aes(x = mean_collab)) +
  stat_ecdf() +
  geom_vline(aes(xintercept = mean_phi, color = "Mean"), linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median_phi, color = "Median"), linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "gray50") +
  scale_color_manual(
    name = "Central tendency",
    values = c("Mean" = "red", "Median" = "darkgreen")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(0, 75, 10)) +
  annotate("label", x = 33, y = 0.75, hjust = 0,
           label = paste0(
             "Mode = ", round(mode_phi, 2),
             "\nMean = ", round(mean_phi, 2),
             "\nMedian = ", round(median_phi, 2),
             "\nSD = ", round(sd_phi, 2))) +
  labs(title = "Cumulative distribution of informal collaborators per philosopher",
       x = "Average number of informal collaborators mentioned",
       y = "Cumulative proportion of philosophers") +
  theme_minimal()
cdf_plot

ggsave("avg_per_philosopher_cdf.png",
       plot = cdf_plot,
       width = 8,
       height = 6,
       dpi = 500)

# Mean, median, sd and mode for ack_per_article_full
mean_article   <- mean(ack_per_article_full$N_Informal_Collaborators)
median_article <- median(ack_per_article_full$N_Informal_Collaborators)
sd_article     <- sd(ack_per_article_full$N_Informal_Collaborators)
mode_article   <- get_mode(ack_per_article_full$N_Informal_Collaborators)

# Visualisation 2 (CDF based on ack_per_article_full) -> article as unit of analysis
cdf2_plot <- ggplot(ack_per_article_full,
                    aes(x = N_Informal_Collaborators)) +
  stat_ecdf(geom = "step") +
  geom_vline(aes(xintercept = mean_article, color = "Mean"), linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median_article, color = "Median"), linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "gray50") +
  scale_color_manual(name = "Central tendency",
                     values = c("Mean" = "red", "Median" = "darkgreen")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(0, 75, 10)) + 
  annotate("label", x = 33, y = 0.75, hjust = 0,
           label = paste0(
             "Mode = ", round(mode_article, 2),
             "\nMean = ", round(mean_article, 2),
             "\nMedian = ", round(median_article, 2),
             "\nSD = ", round(sd_article, 2)))  +
  labs(title = "Cumulative distribution of informal collaborators per article",
       x = "Number of informal collaborators mentioned",
       y = "Cumulative proportion of articles") +
  theme_minimal()
cdf2_plot

ggsave("ack_per_article_full_cdf.png",
       plot = cdf2_plot,
       width = 8,
       height = 6,
       dpi = 500)

# Visualisation 3 (Histogram based on avg_per_philosopher) -> philosopher as unit of analysis
hist_plot <- ggplot(avg_per_philosopher, 
                    aes(x = mean_collab)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white", boundary = 0) +
  geom_vline(aes(xintercept = mean_phi, color = "Mean"), linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median_phi, color = "Median"), linetype = "dashed", linewidth = 1) +
  scale_color_manual(name = "Central tendency",
                     values = c("Mean" = "red", "Median" = "darkgreen")) +
  scale_x_continuous(breaks = seq(0, 75, 10)) +
  annotate("label", x = 30, y = Inf, vjust = 1.5, hjust = 0, size = 3.5,
           label = paste0("Mode = ", round(mode_phi),
                          "\nMean = ", round(mean_phi, 2),
                          "\nMedian = ", round(median_phi, 2),
                          "\nSD = ", round(sd_phi, 2))) +
  labs(title = "How many informal collaborators does the average analytic philosopher mention?",
       x = "Average number of informal collaborators mentioned (per philosopher)",
       y = "Number of philosophers") +
  theme_minimal()
hist_plot

ggsave("avg_per_philosopher_hist.png", 
       plot = hist_plot,
       width = 8,
       height = 6,
       dpi = 500)

# Visualisation 4 (Histogram based on ack_per_article_full) -> article as unit of analysis
hist2_plot <- ggplot(ack_per_article_full, 
                     aes(x = N_Informal_Collaborators)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white", boundary = 0) +
  geom_vline(aes(xintercept = mean_article, color = "Mean"), linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median_article, color = "Median"), linetype = "dashed", linewidth = 1) +
  scale_color_manual(name = "Central tendency",
                     values = c("Mean" = "red", "Median" = "darkgreen")) +
  scale_x_continuous(breaks = seq(0, 75, 10)) +
  annotate("label", x = 30, y = Inf, vjust = 1.5, hjust = 0, size = 3.5,
           label = paste0("Mode =", round(mode_article, 2),
                          "\nMean = ", round(mean_article, 2),
                          "\nMedian = ", round(median_article, 2),
                          "\nSD = ", round(sd_article, 2))) +
  labs(title = "How many informal collaborators does the average article mention?",
       x = "Number of informal collaborators mentioned (per article)",
       y = "Number of articles") +
  theme_minimal()
hist2_plot

ggsave("ack_per_article_full_hist.png",
       plot = hist2_plot,
       width = 8,
       height = 6,
       dpi = 500)

#==================================================================================
# 2.2 IS INFORMAL COLLABORATION COMMON IN ANALYTIC PHILOSOPHY?
#==================================================================================

# Article-level answer (percentage of articles with ack: 94.7%)
article_ack_prevalence <- ack_per_article_full |>
  summarise(
    n_total = n(),
    n_with_ack = sum(N_Informal_Collaborators > 0),
    pct_with_ack = 100 * n_with_ack / n_total)
article_ack_prevalence

# Philosopher-level answer (percentage of philosophers with ack: 96%)
phi_ack_prevalence <- author_ack |>
  group_by(Phi_ID) |>
  summarise(max_ack = max(N_Informal_Collaborators)) |>
  summarise(
    n_total = n(),
    n_with_ack   = sum(max_ack > 0),
    pct_with_ack = 100 * n_with_ack / n_total)
phi_ack_prevalence

# Summary table
prevalence_summary <- bind_rows(
  article_ack_prevalence |> mutate(level = "Articles"),
  phi_ack_prevalence     |> mutate(level = "Philosophers")) |>
  select(level, n_total, n_with_ack, pct_with_ack)
prevalence_summary

# We tidy the summary table 
plot_data <- prevalence_summary |>
  mutate(pct_without_ack = 100 - pct_with_ack) |>
  select(level, pct_with_ack, pct_without_ack) |>
  pivot_longer(cols = c(pct_with_ack, pct_without_ack),
               names_to = "category", 
               values_to = "pct") |>
  mutate(category = recode(category,
                           pct_with_ack    = "Has informal collaboration",
                           pct_without_ack = "No informal collaboration"))

# Visualisation
geom_col_prev <- ggplot(plot_data, 
                        aes(x = level, 
                            y = pct, 
                            fill = category)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_stack(vjust = 0.5), color = "white", size = 4) +
  scale_fill_manual(values = c("Has informal collaboration" = "steelblue",
                               "No informal collaboration" = "gray70")) +
  labs(title = "Prevalence of informal collaboration in analytic philosophy",
       x = NULL, y = "Percentage of total", fill = NULL) +
  theme_minimal()
geom_col_prev

ggsave("plot_data.png",
       plot = geom_col_prev,
       width = 8,
       height = 6,
       dpi = 500)

###################################################################################
###################################################################################
###################################################################################
###################################################################################

# RESEARCH QUESTION 3

#==================================================================================
# 3.1 MOST MENTIONED PHILOSOPHERS
#==================================================================================

# Duplicate check 
sum(duplicated(Acknowledgments_df[, c("Art_ID", "Phi_ID")]))

# We rank Philosophers by number of mentions and remove duplicates
mentions_distribution <- Acknowledgments_df |>
  distinct(Art_ID, Phi_ID) |> # de-duplicate mentions per article
  group_by(Phi_ID) |>
  summarise(n_mentions = n_distinct(Art_ID)) |> # count distinct articles for each philosopher
  arrange(desc(n_mentions)) |>
  left_join(Philosophers_df, by = "Phi_ID") |> # attach full name
  select(Philosopher, n_mentions)

# We check for ties and construct a top 10 ranking
top_mentioned_phi <- mentions_distribution |>
  slice_max(order_by = n_mentions, n = 10)

# Visualisation
most_mentioned_plot <- ggplot(top_mentioned_phi,
                              aes(x = reorder(Philosopher, n_mentions),
                                  y = n_mentions)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 most mentioned philosophers (2005-2019, 5 journals)",
       x = NULL,
       y = "Number of distinct mentions received") +
  theme_minimal()
most_mentioned_plot

ggsave("top_mentioned_phi.png",
       plot = most_mentioned_plot,
       width = 8,
       height = 6,
       dpi = 500)

#=================================================================================
# 3.2 WHO ARE THE MOST MENTIONED PHILOSOPHERS WITHIN ITS VARIOUS SUB-AREAS?
#=================================================================================

# We attach each acknowledgment mention to the sub-area of the article it came from
mentions_by_area <- Acknowledgments_df |>
  distinct(Art_ID, Phi_ID) |> # de-duplicate mentions per article
  left_join(Articles_df |> select(Art_ID, Research_Area), by = "Art_ID") |>  # bring in each article's sub-area
  filter(!is.na(Research_Area)) # drop articles with no sub-area assigned

# We rank philosophers by mentions within each sub-area
most_mentioned_by_area <- mentions_by_area |>
  group_by(Research_Area, Phi_ID) |>
  summarise(n_mentions = n_distinct(Art_ID), .groups = "drop") |> # count distinct articles for each area-philosopher
  left_join(Philosophers_df |> select(Phi_ID, Philosopher), by = "Phi_ID") |> # attach full name
  arrange(Research_Area, desc(n_mentions)) # sort by area, then most mentioned first

# Top 3 most mentioned philosophers within each sub-area
top_mentioned_per_area <- most_mentioned_by_area |>
  group_by(Research_Area) |>
  slice_max(order_by = n_mentions, n = 3) |> # top 3 per area (ties included)
  select(Research_Area, Philosopher, n_mentions)
top_mentioned_per_area

# Visualisation
mentions_area_plot <- ggplot(top_mentioned_per_area,
                             aes(x = reorder(Philosopher, n_mentions), # order Philosopher according to the values of n_mentions
                                 y = n_mentions, 
                                 fill = Research_Area)) +
  geom_col() +
  coord_flip() +
  facet_wrap(facets = vars(Research_Area), scales = "free_y") +
  labs(title = "Most mentioned philosophers by sub-area of analytic philosophy",
       x = NULL, y = "Number of distinct mentions") +
  theme_bw()
mentions_area_plot

ggsave("top_mentioned_by_area.png",
       plot = mentions_area_plot,
       width = 8,
       height = 6,
       dpi = 500)


#=================================================================================
# 3.3 HOW ARE MENTIONS DISTRIBUTED AMONG THE MEMBERS OF THE COMMUNITY?
#=================================================================================

# Population 1: only philosophers who are acknowledged at least once. We count how many acknowledgments they get
mentions_among_acknowledged <- Acknowledgments_df |>
  distinct(Art_ID, Phi_ID) |>
  group_by(Phi_ID) |>
  summarise(n_mentions = n_distinct(Art_ID)) |>
  arrange(desc(n_mentions))

# Population 2: all philosophers in the database, including those never mentioned
mentions_distribution_full <- mentions_among_acknowledged |>
  right_join(Philosophers_df, by = "Phi_ID") |>
  mutate(n_mentions = replace_na(n_mentions, 0)) |> # never mentioned = 0 instead of NA
  arrange(desc(n_mentions)) |>
  select(Phi_ID, Philosopher, n_mentions)

# Mean, median and sd
get_mode <- function(x) {
  freq_table <- table(x)
  as.numeric(names(freq_table)[freq_table == max(freq_table)])
}
mean_mentions   <- mean(mentions_distribution_full$n_mentions)
median_mentions <- median(mentions_distribution_full$n_mentions)
sd_mentions     <- sd(mentions_distribution_full$n_mentions)
mode_mentions   <- get_mode(mentions_distribution_full$n_mentions)

# Visualisation 
mentions_cdf_plot <- ggplot(mentions_distribution_full, 
                            aes(x = n_mentions)) +
  stat_ecdf() +
  geom_vline(aes(xintercept = mean_mentions, color = "Mean"), linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median_mentions, color = "Median"), linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "gray50") +
  scale_color_manual(name = "Central tendency",
                     values = c("Mean" = "red", "Median" = "darkgreen")) +
  scale_y_continuous(labels = scales::percent) +
  annotate("label", x = 33, y = 0.75, hjust = 0,
           label = paste0(
             "Mode = ", round(mode_mentions, 2),
             "\nMean = ", round(mean_mentions, 2),
             "\nMedian = ", round(median_mentions, 2),
             "\nSD = ", round(sd_mentions, 2))) +
  labs(title = "Cumulative distribution of mentions across the community",
       x = "Number of times mentioned in acknowledgments",
       y = "Cumulative proportion of philosophers") +
  theme_minimal()
mentions_cdf_plot

ggsave("mentions_distribution_full_cdf.png",
       plot = mentions_cdf_plot,
       width = 8,
       height = 6,
       dpi = 500)

# Some examples drew on the cdf
mentions_cdf_fn <- ecdf(mentions_distribution_full$n_mentions)

# Probabilities of getting 1 mentions or fewer and more than 1
mentions_cdf_fn(1) # 61%
1 - mentions_cdf_fn(1) # 39%

# Probabilities of getting 5 mentions or fewer and more than 5
mentions_cdf_fn(5) # 89%
1 - mentions_cdf_fn(5) # 11%

# Probabilities of getting 50 mentions or fewer and more than 50
mentions_cdf_fn(50) # 100%
1 - mentions_cdf_fn(50) # 0%

#==================================================================================
# 3.4 ARE MENTIONS CORRELATED WITH OTHER INDICATORS OF PRESTIGE (E.G. NUMBER OF
# CITATIONS OR NUMBER OF PUBLISHED ARTICLES)?
#==================================================================================
library(ggpmisc)

# Correlation of mentions with citations, publications, articles
mentions_prestige_corr <- mentions_among_acknowledged |>
  right_join(Philosophers_df, by = "Phi_ID") |> # keep all philosophers (full population)
  mutate(n_mentions = replace_na(n_mentions, 0)) |> # never-mentioned = 0, not NA
  left_join(most_prod_phil |> select(Phi_ID, N_Articles), by = "Phi_ID") |> # attach DB productivity
  mutate(N_Articles = replace_na(N_Articles, 0)) |> # never-authored = 0, not NA
  arrange(desc(n_mentions)) |>
  select(Phi_ID, Philosopher, n_mentions, Citations, Publications, N_Articles)

# Regression lines
model_citations    <- lm(data = mentions_prestige_corr, formula = n_mentions ~ Citations)
model_publications <- lm(data = mentions_prestige_corr, formula = n_mentions ~ Publications)
model_articles     <- lm(data = mentions_prestige_corr, formula = n_mentions ~ N_Articles)

# R2
summary(model_citations)$r.squared
summary(model_publications)$r.squared
summary(model_articles)$r.squared

# Summary table
r2_table <- tibble(
  Model = c("n_mentions ~ Citations", "n_mentions ~ Publications", "n_mentions ~ N_Articles"),
  n = c(nobs(model_citations), nobs(model_publications), nobs(model_articles)), # nobs() extracts how many observations each model actually used. Citations and Publications are missing for 2,674 philosophers (45%, due to incomplete Scopus matching).
  R_squared = c(summary(model_citations)$r.squared,
                summary(model_publications)$r.squared,
                summary(model_articles)$r.squared))
r2_table


# Visualisations
# 1) Mentions vs Citations
plot_citations <- ggplot(mentions_prestige_corr, 
                         aes(x = Citations, 
                             y = n_mentions)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  stat_poly_line(se = FALSE, color = "red") +
  stat_poly_eq(use_label("eq", "R2")) +
  labs(title = "Mentions vs. Citations",
       x = "Career-wide citations (Scopus)",
       y = "Number of distinct mentions") +
  theme_minimal()
plot_citations

ggsave("mentions_prestige_corr1.png",
       plot = plot_citations,
       width = 8,
       height = 6,
       dpi = 500)

# 2) Mentions vs Publications
plot_publications <- ggplot(mentions_prestige_corr, 
                            aes(x = Publications, 
                                y = n_mentions)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  stat_poly_line(se = FALSE, color = "red") +
  stat_poly_eq(use_label("eq", "R2")) +
  labs(title = "Mentions vs. Publications",
       x = "Career-wide publications (Scopus)",
       y = "Number of distinct mentions") +
  theme_minimal()
plot_publications

ggsave("mentions_prestige_corr2.png",
       plot = plot_publications,
       width = 8,
       height = 6,
       dpi = 500)

# 3) Mentions vs Articles
plot_articles <- ggplot(mentions_prestige_corr, 
                        aes(x = N_Articles, 
                            y = n_mentions)) +
  geom_jitter(alpha = 0.3, color = "steelblue", width = 0.15, height = 0.15) + # add some "noise" to make density easier to see
  geom_point(alpha = 0.3, color = "steelblue") +
  stat_poly_line(se = FALSE, color = "red") +
  stat_poly_eq(use_label("eq", "R2")) +
  scale_x_continuous(breaks = seq(0,10,1)) +
  labs(title = "Mentions vs. Authored Articles",
       x = "Articles authored in this database",
       y = "Number of distinct mentions") +
  theme_minimal()
plot_articles

ggsave("mentions_prestige_corr3.png",
       plot = plot_articles,
       width = 8,
       height = 6,
       dpi = 500)

#==================================================================================
# 3.5 DOES GENDER AFFECT THE CHANCES OF ACCUMULATING MENTIONS?
#==================================================================================

# Overview on the proportion of the population based on gender (excluding NA values) -> males 78%, females 22%
gender_overview <- Philosophers_df |>
  filter(Gender %in% c("male", "female")) |>
  count(Gender) |>
  mutate(pct = round(100 * n / sum(n), 1))
gender_overview

# We consider the full population of philosophers with known gender
gender_mentions <- mentions_among_acknowledged |>
  right_join(Philosophers_df, by = "Phi_ID") |> # keep all philosophers, not just mentioned ones
  mutate(n_mentions = replace_na(n_mentions, 0)) |> # never-mentioned = 0, not NA
  filter(Gender %in% c("male", "female")) |>  # drop the 92 "unknown" gender cases
  select(Phi_ID, Philosopher, Gender, n_mentions)

# Median
median(gender_mentions$n_mentions)

# We create a mention tier and we segment mention tiers with respect to the median and the 90th percentile
gender_mentions <- gender_mentions |>
  mutate(tier = case_when(   # put each philosopher into a mention tier
    n_mentions == 0 ~ "Below median (0)",
    n_mentions <= 1 ~ "At median (1)",
    n_mentions <= 5 ~ "Above median (2-5)",
    TRUE            ~ "Top 10% (6+)"
  ))

# We visualise the tier in a table
gender_tier_table <- gender_mentions |>
  mutate(tier = factor(tier, levels = c("Below median (0)", "At median (1)",
                                        "Above median (2-5)", "Top 10% (6+)"))) |>  # fix display order, following the order established in levels
  group_by(tier) |>
  summarise(
    n_female   = sum(Gender == "female"),   # count women in this tier
    n_male     = sum(Gender == "male"),     # count men in this tier
    pct_female = round(100 * n_female / (n_female + n_male), 1) # % women within this tier
  )
gender_tier_table



