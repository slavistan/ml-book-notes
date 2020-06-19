---
title: Notes
author: Stan
urlcolor: teal
---

# Data Preparation

## Download data

```{.bash .cb.run}
# Download the data
URL="https://raw.githubusercontent.com/ageron/handson-ml2/master/datasets/housing/housing.csv"
mkdir -p _datasets
curl -so - "$URL" > _datasets/housing.csv
```

```{.R .cb.run hide=stdout+stderr}
options(repr.plot.width=14, repr.plot.height=9)
library(tidyverse)
library(cowplot)
housing_orig <- read.csv("_datasets/housing.csv") %>% as_tibble
```

## Augment attributes

```
housing <- housing_orig %>%
  mutate(rooms_per_household = total_rooms / households) %>%
  mutate(bedrooms_per_room = total_bedrooms / total_rooms) %>%
  mutate(population_per_household = population / households) %>%
  mutate(incomecat = cut(median_income, c(0, 1.5, 3, 4.5, 6, Inf)))
```

# Data Exploration

## Overview

```{.R .cb.run hide=stdout+stderr}
housing %>%
    keep(is.numeric) %>%
    pivot_longer(longitude:population_per_household, names_to="what", values_to="value") %>%
    filter(!is.na(value)) %>%
ggplot +
    geom_histogram(aes(x=value), bins=70) +
    facet_wrap(~what, scales="free")
ggsave("assets/housing_histograms.png", height=6, width=6)
```

```{.R .cb.run hide=stdout+stderr}
ggplot(housing_orig) +
  geom_point(aes(x=longitude, y=latitude, color=median_house_value), alpha=0.3)

ggsave("assets/housing_scatter_value.png", height=6, width=10)
```
![Housing data overview](assets/housing_histograms.png){height=400px}

![Geographical distribution of house prices](assets/housing_scatter_value.png){height=400px}

## Correlation between features

```{.R .cb.run hide=stdout+stderr}
# compute the correlation matrix
corr_matrix <- housing %>%
  keep(is.numeric) %>% # remove non-numeric attributes
  filter(!is.na(total_bedrooms)) %>% # remove na-rows
  cor %>% # correlation matrix
  as_tibble(rownames=NA) %>% # convert to tibble ...
  rownames_to_column("attribute") # and keep rownames

# Plot correlations for house values
corr_matrix %>% select(c(attribute, median_house_value)) %>%
  arrange(desc(median_house_value)) %>%
  ggplot + geom_col(aes(x=reorder(attribute, -median_house_value), y=median_house_value))
ggsave("assets/housing_correlation.png")
```
Attribute Correlation wrt. Housing Prices

![](assets/housing_correlation.png){height=400px}

```{.R .cb.run}
library(GGally)
housing %>%
  select(c(median_house_value, median_income, total_rooms, housing_median_age)) %>%
  # scatter matrix via GGally::ggpairs. Content of upper and lower triangles and diagonal
  # can be controlled.
  ggpairs(upper = list(continuous = wrap("points", alpha=0.1, size=0.5)),
          lower = list(continuous = "cor"))
ggsave("assets/housing_scatter_matrix.png")
```
![](assets/housing_scatter_matrix.png){height=400px}

Zoom in to median income

```{.R .cb.run}
housing %>% ggplot +
  geom_point(aes(x=median_income, y=median_house_value), alpha=0.1)
ggsave("assets/housing_scatter_income_vs_housevalue.png")
```
![](assets/housing_scatter_income_vs_housevalue.png){height=400px}

# Modeling

## Stratified Sampling

### Median Income Categories

```{.R .cb.run hide=stdout+stderr}
p1 <- ggplot(housing) + geom_histogram(aes(x=median_income), bins=70)
p2 <- ggplot(housing) + geom_bar(aes(x=incomecat))
plot_grid(p1, p2)
ggsave("assets/housing_income_as_category.png", height=6, width=12)
```
![](assets/housing_income_as_category.png){height=400px}

```{.R .cb.run hide=stdout+stderr}
library(zeallot) # tuple assignment %<-%
library(knitr)
library(gridExtra)

shuffle_split_rng <- function(data, proportion, seed=NA) {
    if (! is.na(seed)) { set.seed(seed) }

    # shuffle indices
    permutated_indices <- sample(nrow(data))

    # re-index and split according to proportion
    num_of_elems_in_split <- floor(nrow(data) * proportion)
    split <- data[permutated_indices,][1:num_of_elems_in_split,]
    remainder <- data[permutated_indices,][(num_of_elems_in_split+1):nrow(data),]
    list(split, remainder)
}
shuffle_split_stratified <- function(data, proportion, seed=NA) {
    if (! is.na(seed)) { set.seed(seed) }

    # generate a row index column which we use to distinguish the sample's row from
    # the remainder.
    dd <- data %>% mutate(rownum = row_number())

    # generate the split using `sample_frac' which samples inside each group
    split <- dd %>% group_by(incomecat) %>% sample_frac(proportion) %>% ungroup

    # remove the split from the full dataframe by matching against the row indices.
    # TODO(perf): Remove split from original without combinatorial complexity :(
    #             For each of the N rows in the original, we perform one comparison
    #             for every one of the M rows in the split. There has to be a better
    #             way to achieve this.
    remainder <- dd %>% filter(! rownum %in% split$rownum ) %>% select(-rownum)
    list(split %>% select(-rownum), remainder)
}

c(split_rng, remainder_rng) %<-% shuffle_split_rng(housing, 0.2, seed=1)
c(split_stratified, remainder_stratified) %<-% shuffle_split_stratified(housing, 0.2, seed=1)

# create a data frame which stores the relative number of samples for every
# income class in the (original) housing data. Add the relative numbers
# of the dataframes generated by random and stratified sampling.
df <- housing %>% select(incomecat) %>% group_by(incomecat) %>%
        summarize(overall_freq=n()/nrow(housing) * 100)
for (dfname in list("split_rng", "split_stratified")) {
    dd=get(dfname) # string to variable
    dftemp <- dd %>% select(incomecat) %>% group_by(incomecat) %>% summarize(rel_prevalence=100 * n()/nrow(dd))
    colname <- paste0(dfname,"_freq")
    df[colname] <- dftemp$rel_prevalence

    # compute the relative error
    df[paste0(colname,"_rel_error")] <- 1000 * (df$overall_freq - df[colname]) / df$overall_freq
}
df <- df %>% rapply(f = function(x) { round(x, 2) }, classes="numeric", how="replace")
names(df) <- c("Income Class", "Overall [%]", "Rand. [%]","Rand. δ [‰]","Stratif. [%]","Stratif. δ [‰]")

plot_grid(tableGrob(df, rows=NULL), nrow=1)
ggsave("assets/housing_stratified_vs_rng_sampling.png", height=6)
```

![Stratified vs. Random Sampling Accuracy](assets/housing_stratified_vs_rng_sampling.png){height=400px}

# Tidyverse Cheat Sheet

`df %>% keep(is.numeric)`{.R} - Select numeric columns

`df %>% summarize(mean=mean(colname)) %>% pull`{.R} - get mean/median/sd .. of a df column. `pull` extracts the single
value from the 1x1 dataframe.

`fg %>% slice_head(n=10)` - head .. 
