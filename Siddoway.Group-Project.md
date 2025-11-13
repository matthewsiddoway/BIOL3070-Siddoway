Opioid use in Cancer Patients
================
Matthew Siddoway
2025-11-13

- [ABSTRACT](#abstract)
- [BACKGROUND](#background)
  - [Questions](#questions)
  - [Hypothesis](#hypothesis)
- [METHODS](#methods)
  - [Generalized Linear Model](#generalized-linear-model)
- [DISCUSSION](#discussion)
  - [Interpretation of barplots](#interpretation-of-barplots)
  - [Interpretation of the generalized linear
    model](#interpretation-of-the-generalized-linear-model)
- [CONCLUSION](#conclusion)
- [REFERENCES](#references)

# ABSTRACT

# BACKGROUND

Effective pain management in cancer patients remains a critical
challenge, as individuals often exhibit substantial variability in their
responses to opioid therapy. This variability can result from a
combination of physiological, environmental, and genetic factors that
influence both opioid metabolism and receptor function. Recent
pharmacogenetic studies have emphasized the role of single nucleotide
polymorphisms (SNPs) in genes associated with opioid signaling pathways
in determining the efficacy and dosage requirements of pain treatment.
One influential study by Galvan et al. (2011) identified multiple
genetic loci that modulate opioid therapy response in cancer patients
across Europe. Their findings indicated that variants such as rs12948783
were significantly associated with differences in pain relief and opioid
dose requirements, suggesting that specific genetic profiles can affect
a patient’s analgesic sensitivity. These results highlight the
importance of integrating genetic screening into clinical pain
management to optimize therapeutic outcomes and minimize adverse
effects. Based on this evidence, the present study seeks to investigate
how genetic and phenotypic factors influence pain management outcomes
among European cancer patients treated with opioids. Specifically, we
hypothesize that genetic variation at the rs12948783 locus, along with
phenotypic variables such as sex and country, significantly affect
normalized pain relief. We predict that carriers of the minor allele (AG
or GG genotypes) will experience greater pain relief compared to AA
homozygotes, reflecting enhanced opioid responsiveness. Understanding
these genetic and demographic determinants of opioid efficacy may
provide valuable insights for the development of personalized pain
management strategies, ultimately improving quality of life for cancer
patients undergoing treatment. \# STUDY QUESTION and HYPOTHESIS

## Questions

What effect does genetics and phenotypes have on pain management among
European cancer patients who have been treated with opioids?

## Hypothesis

Genetic variation in opioid receptor-related genes significantly affect
pain relief and opioid requirements. \## Prediction I predict that there
will be certain phenotypes and genetics that correspond to lesser pain
responses depending on the unique genetic makeup of each demographic.

# METHODS

``` r
library(readr)
library(ggplot2)

data <- read_csv("epos_style_rs12948783_simulated_large.csv")
```

    ## Rows: 1982 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (4): genotype, carrier, sex, country
    ## dbl (2): id, norm_pain_relief
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
data$genotype <- as.factor(data$genotype)
data$sex <- as.factor(data$sex)
data$country <- as.factor(data$country)

sample_size <- min(5000, nrow(data))

model_data <- data[sample(nrow(data), sample_size), ]
model <- lm(norm_pain_relief ~ genotype + sex + country, data = model_data)
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = norm_pain_relief ~ genotype + sex + country, data = model_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -36.760  -5.172   0.919   5.818  26.756 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 76.75153    1.18604  64.712  < 2e-16 ***
    ## genotypeGA   9.63566    1.17005   8.235 3.22e-16 ***
    ## genotypeGG  22.19112    1.14459  19.388  < 2e-16 ***
    ## sexMale      0.00591    0.37752   0.016    0.988    
    ## countryGB    0.49204    0.50958   0.966    0.334    
    ## countryIT   -0.14456    0.57632  -0.251    0.802    
    ## countryNO    0.55745    0.52729   1.057    0.291    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.385 on 1975 degrees of freedom
    ## Multiple R-squared:  0.3792, Adjusted R-squared:  0.3773 
    ## F-statistic:   201 on 6 and 1975 DF,  p-value: < 2.2e-16

``` r
plot_data <- data[sample(nrow(data), sample_size), ]

ggplot(plot_data, aes(x = genotype, y = norm_pain_relief, fill = genotype)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.4) +
  labs(
    title = "Effect of Genotype on Normalized Pain Relief",
    x = "Genotype (rs12948783)",
    y = "Normalized Pain Relief (%)"
  ) +
  theme_minimal()
```

![](Siddoway.Group-Project_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
suppressPackageStartupMessages({ library(readr); library(dplyr); library(ggplot2) })


dat <- read_csv("epos_style_rs12948783_simulated_large.csv")
```

    ## Rows: 1982 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (4): genotype, carrier, sex, country
    ## dbl (2): id, norm_pain_relief
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
names(dat) <- make.names(trimws(names(dat)), unique = TRUE)

stopifnot(all(c("genotype","norm_pain_relief","carrier") %in% names(dat)))
dat$genotype <- as.factor(dat$genotype)
dat$carrier  <- as.factor(dat$carrier)
dat$pain     <- suppressWarnings(as.numeric(dat$norm_pain_relief))
dat <- dat[is.finite(dat$pain), , drop = FALSE]

summ <- dat %>%
  dplyr::group_by(genotype, carrier) %>%
  dplyr::summarise(
    n  = dplyr::n(),
    mn = mean(pain, na.rm = TRUE),
    sd = sd(pain,   na.rm = TRUE),
    se = ifelse(n > 1, sd / sqrt(n), 0),
    .groups = "drop"
  )

geno_order <- summ %>%
  dplyr::group_by(genotype) %>%
  dplyr::summarise(overall = mean(mn, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(overall)) %>%
  dplyr::pull(genotype) %>%
  as.character()
summ$genotype <- factor(summ$genotype, levels = geno_order)

dodge_w <- 0.75
p <- ggplot(summ, aes(x = genotype, y = mn, fill = carrier)) +
  geom_col(position = position_dodge(width = dodge_w), width = 0.7) +
  geom_errorbar(aes(ymin = mn - se, ymax = mn + se),
                position = position_dodge(width = dodge_w), width = 0.15) +
  geom_text(aes(label = paste0("n=", n), y = mn + se),
            position = position_dodge(width = dodge_w),
            vjust = -0.4, size = 3) +
  labs(
    title = "Normalized pain relief (mean ± SE) by genotype and carrier",
    x = "Genotype", y = "Mean normalized pain relief", fill = "Carrier"
  ) +
  theme_minimal()
print(p)
```

![](Siddoway.Group-Project_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->
\## Barplots

## Generalized Linear Model

# DISCUSSION

## Interpretation of barplots

## Interpretation of the generalized linear model

# CONCLUSION

# REFERENCES

Galvan, A., Skorpen, F., Klepstad, P., Fragoso, M., Ando, M., Nilsen,
T., … & Ciotti, P. (2011). Multiple loci modulate opioid therapy
response for cancer pain. Clinical Cancer Research, 17(13), 4581–4587.
<https://doi.org/10.1158/1078-0432.CCR-10-3407>

2.  ChatGPT. OpenAI, version Jan 2025. Used as a reference for functions
    such as plot() and to correct syntax errors. Accessed 2025-11-13.
