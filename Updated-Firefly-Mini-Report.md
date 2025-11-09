Firefly Mini-Report
================
Sara Deschaine
2025-11-09

- [Abstract](#abstract)
- [Background](#background)
- [Study Question and Hypothesis](#study-question-and-hypothesis)
  - [Question](#question)
  - [Hypothesis](#hypothesis)
  - [Prediction](#prediction)
- [Methods](#methods)
  - [Log-Transformed Box Plot](#log-transformed-box-plot)
  - [Violin Plot](#violin-plot)
  - [Poisson Regression](#poisson-regression)
- [Discussion](#discussion)
  - [Intrepetation of Plots](#intrepetation-of-plots)
  - [Intrepetation of Poisson
    Regression](#intrepetation-of-poisson-regression)
- [Conclusion](#conclusion)
- [References](#references)

# Abstract

Fireflies are important insects that help with pollination and serve as
food for other animals. This study looked at whether firefly numbers are
higher in northern or southern Utah. Using citizen science data, we
compared firefly counts between regions with graphs and a statistical
model. Contrary to our expectations, southern Utah had more fireflies
than the north, with counts about three times higher. While there was a
lot of variation, the difference was statistically significant. These
results suggest that local habitat, vegetation, or other environmental
factors may influence firefly populations more than temperature or
moisture alone. Understanding where fireflies thrive can help guide
conservation efforts and protect these unique insects.

# Background

Fireflies are an ecologically important group of insects because they
contribute to pollination, are a natural pest control agent, and are a
food source for other animals. Understanding more about fireflies,
including where they are most abundant in local areas, provides insight
into habitat quality and guides conservation efforts. In our study we
hypothesize that northern counties in Utah will have a higher abundance
in fireflies compared to south. Fireflies are bioluminescent beetles
that use light signals for mating and communication, this makes them
sensitive to environmental changes that affect light or habitat
conditions (Evans et al., 2018). “Most fireflies need moisture to
complete their life cycle, preventing desiccation of vulnerable immature
stages and ensuring availability of their soft-bodied invertebrate prey.
Because of their reliance on moisture, fireflies are typically found
near permanent water sources.”(Lewis et al., 2024). Northern Utah
generally has cooler, wetter habitats, while southern Utah is hotter and
drier. This supports the idea that northern regions may provide better
habitat conditions for fireflies. By examining regional differences in
firefly abundance, this study helps reveal how climate and habitat
conditions influence local insect populations.

# Study Question and Hypothesis

## Question

Do northern counties in Utah have a higher abundance of fireflies
compared to southern counties?

## Hypothesis

We hypothesize that northern counties in Utah will have a higher
abundance of fireflies compared to southern counties, potentially due to
differences in environmental conditions such as temperature, moisture,
and habitat availability that are more favorable in the north.

## Prediction

If geographic location affects firefly abundance, then northern counties
in Utah will have a noticeably greater number of fireflies than southern
counties, reflecting regional differences in suitable habitat and
environmental conditions.

# Methods

We obtained data from Fireflies Citizen Science in Logan, which compiles
local firefly observation records. From this dataset, we extracted the
relevant information on firefly abundance and county locations in Utah,
which we then categorized into northern and southern regions. We drew a
line from Juab County to Carbon County, defining everything north of the
line as Northern Utah and everything south of it as Southern Utah. Using
RStudio Cloud, we created two visualizations, a box plot and a violin
plot which compared firefly abundance between the north and south.
Finally, we conducted a t-test to determine whether the observed
differences were statistically significant.

## Log-Transformed Box Plot

We visualized the data using a log-transformed box plot, this code
specifically reads in a dataset of firefly counts and regions, removes
any rows with missing or blank region values, and then creates a boxplot
of firefly abundance by region. The counts are then log10-transformed
(with +1 to avoid taking the log of zero), which helps visualize
differences when counts vary widely.

``` r
# Firefly Boxplot (Log-Transformed, No Blanks)

library(ggplot2)

# Read in the data
fireflies <- read.csv("Copy of firefliesUtah - Usable Data.csv", stringsAsFactors = FALSE)
colnames(fireflies) <- c("firefly_count", "region")

# Remove blank or missing region values
fireflies <- subset(fireflies, region != "" & !is.na(region))

# Box plot with log10 transformation (+1 to avoid log(0))
ggplot(fireflies, aes(x = region, y = log10(firefly_count + 1), fill = region)) +
  geom_boxplot(width = 0.6, color = "black", alpha = 0.7) +
  labs(
    title = "Firefly Abundance by Region (Log Scale)",
    x = "Region",
    y = "Log-Transformed Firefly Count"
  ) +
  scale_fill_manual(values = c("north" = "#8EC9E8", "south" = "#F4A261")) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
```

    ## Warning: Removed 1 row containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](Updated-Firefly-Mini-Report_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

## Violin Plot

We also made a split violin plot to compare firefly counts in the north
and south. The plot shows the distribution of counts on each side, with
colors for each region and the y-axis capped at 50 to keep it readable.

``` r
# Split Violin Plot (y-axis limited to 50)

library(ggplot2)
library(gghalves)
library(stringi)

# Read and clean your data
fireflies <- read.csv("Copy of firefliesUtah - Usable Data.csv", stringsAsFactors = FALSE)
colnames(fireflies) <- c("firefly_count", "region")

fireflies$region[fireflies$region == ""] <- NA
fireflies$region <- stri_trans_general(fireflies$region, "NFKC")
fireflies$region <- stri_replace_all_regex(fireflies$region, "\\p{C}", "")
fireflies$region <- gsub("\u00A0", " ", fireflies$region)
fireflies$region <- trimws(tolower(fireflies$region))
fireflies$region[fireflies$region %in% c("n", "nrth", "noth")] <- "north"
fireflies$region[fireflies$region %in% c("s", "sth", "soth")] <- "south"
fireflies$region <- factor(fireflies$region, levels = c("north", "south"))
fireflies_clean <- droplevels(subset(fireflies, !is.na(region)))

# Split violin plot
ggplot() +
  geom_half_violin(
    data = subset(fireflies_clean, region == "north"),
    aes(x = factor(1), y = firefly_count, fill = region),
    side = "l", trim = TRUE, color = "black", alpha = 0.7
  ) +
  geom_half_violin(
    data = subset(fireflies_clean, region == "south"),
    aes(x = factor(1), y = firefly_count, fill = region),
    side = "r", trim = TRUE, color = "black", alpha = 0.7
  ) +
  scale_fill_manual(values = c("north" = "#8EC9E8", "south" = "#F4A261")) +
  coord_cartesian(ylim = c(0, 50)) +   # y-axis capped at 50
  labs(
    title = "Firefly Abundance: North vs South",
    x = NULL,
    y = "Firefly Count"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
```

    ## Warning: Removed 1 row containing non-finite outside the scale range
    ## (`stat_half_ydensity()`).

![](Updated-Firefly-Mini-Report_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

## Poisson Regression

We ran a poison regression to see if the difference in firefly counts
between the north and south is statistically significant. This code
specifically looks at how firefly counts differ between regions, checks
if the data are too spread out for the Poisson model, adjusts with a
quasi-Poisson model if needed, and calculates predicted counts for each
site.

``` r
# Read in the data
fireflies <- read.csv("Copy of firefliesUtah - Usable Data.csv", stringsAsFactors = FALSE)
colnames(fireflies) <- c("firefly_count", "region")

# Remove blank or missing region values
fireflies <- subset(fireflies, region != "" & !is.na(region))

# Poisson regression
poisson_model <- glm(firefly_count ~ region, data = fireflies, family = "poisson")

# Summary of the model
summary(poisson_model)
```

    ## 
    ## Call:
    ## glm(formula = firefly_count ~ region, family = "poisson", data = fireflies)
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  2.62561    0.01293  203.06   <2e-16 ***
    ## regionsouth  1.13788    0.02340   48.63   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 30429  on 493  degrees of freedom
    ## Residual deviance: 28450  on 492  degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## AIC: 30188
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
# Exponentiated coefficients (rate ratios)
exp(coef(poisson_model))
```

    ## (Intercept) regionsouth 
    ##   13.812933    3.120145

``` r
# Check for overdispersion
dispersion <- sum(residuals(poisson_model, type = "pearson")^2) / poisson_model$df.residual
dispersion  # If > 1.5, overdispersion may be present
```

    ## [1] 273.6239

``` r
# Quasi-Poisson if overdispersion
quasi_model <- glm(firefly_count ~ region, data = fireflies, family = "quasipoisson")
summary(quasi_model)
```

    ## 
    ## Call:
    ## glm(formula = firefly_count ~ region, family = "quasipoisson", 
    ##     data = fireflies)
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   2.6256     0.2139   12.28  < 2e-16 ***
    ## regionsouth   1.1379     0.3871    2.94  0.00344 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for quasipoisson family taken to be 273.6309)
    ## 
    ##     Null deviance: 30429  on 493  degrees of freedom
    ## Residual deviance: 28450  on 492  degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## AIC: NA
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
# Add predicted counts to the data
fireflies$predicted_count <- predict(poisson_model, newdata = fireflies, type = "response")
```

# Discussion

## Intrepetation of Plots

Although it may be difficult to interpret from the graphs alone, both
the box plot and split violin plot suggest that Southern Utah counties
generally have higher firefly abundance than Northern counties. While
the overall regional differences are not large, they are still
noteworthy. We used a log-transformed box plot to include all data
points, even the extreme outliers with counts around 1,000, which helped
condense the data and make it easier to read. For the violin plot, we
capped firefly counts at 50 to focus on the main bulk of the data, where
most observations occur. Additionally, we removed a few points from
other states or locations near the boundary between northern and
southern Utah, where it was difficult to tell which region the point
belonged to. The box plot provides a clear overview and comparison of
the data, while the split violin plot offered a more detailed view of
the distribution and density of firefly counts.

## Intrepetation of Poisson Regression

We used a Poisson Regression to examine the effect of region on firefly
abundance. The model revealed that firefly counts differed significantly
between northern and southern sites. Southern sites had a significant
p-value of 0.003, which means we can confidently come to the conclusion
that the difference in firefly abundance between northern and southern
Utah counties is not due to chance. On average, firefly counts in the
south were approximately 3 times higher than in the north with the
predicted counts being 13.8 in the north and 43.1 for the south. Our
data was over dispersed, with a dispersion value of 273.6, indicating
that the counts were much more spread out than expected so we used a
quasi-Poisson model to produce more reliable standard errors and
p-values. Although region explained some of the variation in firefly
abundance, there was still substantial unexplained variability,
suggesting other environmental factors may also influence firefly
populations.

# Conclusion

Contrary to our original hypothesis, firefly abundance was generally
higher in southern Utah counties compared to northern counties. Both the
visualizations and the Poisson regression supported this pattern, with
southern sites having roughly three times the predicted firefly counts
of northern sites. Although our data were overdispersed and there was
substantial variability among sites, the difference between regions was
statistically significant, indicating that geographic location plays a
role in firefly abundance. These results suggest that factors other than
moisture and cooler temperatures, such as local habitat features,
vegetation, or microclimate conditions, may be driving firefly
populations in Utah. Understanding these regional patterns can help
guide conservation efforts and highlight the importance of considering
multiple environmental factors when assessing firefly habitats.

Future studies with larger sample sizes and more precise regional
assignment could help confirm these trends and inform local conservation
efforts. There are a few limitations to our study the main one being the
data we received wasn’t very accurate, the firefly abundance were based
off of an estimation. Overall, our results provide a valuable foundation
for future research on how regional environmental factors influence
firefly populations in Utah.

# References

1.  ChatGPT. OpenAI, version Jan 2025. Used as a reference for functions
    such as plot() and to correct syntax errors. Accessed 2025-11-09

2.  Evans, T. R., Salvatore, D., van de Pol, M., & Musters, C. J. M.
    (2018). Adult firefly abundance is linked to weather during the
    larval stage in the previous year. Ecological Entomology, 44(2),
    265–273. <https://doi.org/10.1111/een.12702>

3.  Lewis, S. M., Jusoh, W. F. A., Walker, A. C., Fallon, C. E., Joyce,
    R., & Yiu, V. (2024). Illuminating Firefly Diversity: Trends,
    Threats and Conservation Strategies. Insects, 15(1), 71.
    <https://doi.org/10.3390/insects15010071>
