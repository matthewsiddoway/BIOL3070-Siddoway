Warm-up mini-Report: Mosquito Blood Hosts in Salt Lake City, Utah
================
Matthew Siddoway
2025-10-09

- [ABSTRACT](#abstract)
- [BACKGROUND](#background)
- [STUDY QUESTION and HYPOTHESIS](#study-question-and-hypothesis)
  - [Questions](#questions)
  - [Hypothesis](#hypothesis)
  - [Prediction](#prediction)
- [METHODS](#methods)
  - [Barplots](#barplots)
  - [Generalized Linear Model](#generalized-linear-model)
- [DISCUSSION](#discussion)
  - [Interpretation of barplots](#interpretation-of-barplots)
  - [Interpretation of the generalized linear
    model](#interpretation-of-the-generalized-linear-model)
- [CONCLUSION](#conclusion)
- [REFERENCES](#references)

# ABSTRACT

Control and prevention of diseases is crucial in keeping communities
safe. West Nile Virus (WNV) is a disease that is transmitted between
birds and mosquitoes; however, infected mosquitoes can cause spillover
into humans and other mammals. We collected blood meal samples from
mosquitoes in Salt Lake City, Utah. The DNA from these blood meals was
then extracted, amplified using PCR, then sequenced to determine
different hosts. These hosts were visualized using bar plots to
determine feeding patterns and common hosts of WNV. It was noted that
house finches, along with some local bird species, were the most commons
source of blood meals for mosquitoes. This supports our hypothesis that
house finches are important as an amplifying host of WNV in Salt Lake
City.

# BACKGROUND

West Niles Virus (WNV) is a vector-borne virus that has a primary
reservoir in birds. These avian species have developed a high enough
viremia to infect the mosquitoes when they feed for a blood meal. These
mosquitoes can then transmit the virus to humans, as well as other
animals. Outbreaks of WNV in the Salt Lake City area have initiated
research into identifying host species to determine insights with WNV
amplification and possible spillover risks. Mosquito samples were
collected, and the blood meals were analyzed using DNA extraction, PCR,
and sequencing to help identify the hosts. Previous studies, like Komar
et al., 2003, have shown that WNV is not as viable in certain bird
species compared to others. Identifying the species that can sustain the
viremia long enough to infect the mosquitoes is critical to
understanding the risk of WNV in the Salt Lake City area.

``` r
# Manually transcribe duration (mean, lo, hi) from the last table column
duration <- data.frame(
  Bird = c("Canada Goose","Mallard", 
           "American Kestrel","Northern Bobwhite",
           "Japanese Quail","Ring-necked Pheasant",
           "American Coot","Killdeer",
           "Ring-billed Gull","Mourning Dove",
           "Rock Dove","Monk Parakeet",
           "Budgerigar","Great Horned Owl",
           "Northern Flicker","Blue Jay",
           "Black-billed Magpie","American Crow",
           "Fish Crow","American Robin",
           "European Starling","Red-winged Blackbird",
           "Common Grackle","House Finch","House Sparrow"),
  mean = c(4.0,4.0,4.5,4.0,1.3,3.7,4.0,4.5,5.5,3.7,3.2,2.7,1.7,6.0,4.0,
           4.0,5.0,3.8,5.0,4.5,3.2,3.0,3.3,6.0,4.5),
  lo   = c(3,4,4,3,0,3,4,4,4,3,3,1,0,6,3,
           3,5,3,4,4,3,3,3,5,2),
  hi   = c(5,4,5,5,4,4,4,5,7,4,4,4,4,6,5,
           5,5,5,7,5,4,3,4,7,6)
)

# Choose some colors
cols <- c(rainbow(30)[c(10:29,1:5)])  # rainbow colors

# horizontal barplot
par(mar=c(5,12,2,2))  # wider left margin for names
bp <- barplot(duration$mean, horiz=TRUE, names.arg=duration$Bird,
              las=1, col=cols, xlab="Days of detectable viremia", xlim=c(0,7))

# add error bars
arrows(duration$lo, bp, duration$hi, bp,
       angle=90, code=3, length=0.05, col="black", xpd=TRUE)
```

<img src="Siddoway.Warm-up-mosquitoes-TEMPLATE-2_files/figure-gfm/viremia-1.png" style="display: block; margin: auto auto auto 0;" />

# STUDY QUESTION and HYPOTHESIS

## Questions

Which bird species acts as an amplifying host of WNV in Salt Lake City?

## Hypothesis

House finches are acting as important amplifying hosts of WNV in Salt
Lake City

## Prediction

Locations with mosquito blood meals from house finches will also have
higher numbers of WNV-positive mosquito pools

# METHODS

Following blood meal extraction, PCR, and sequencing of the mosquitoes
blood meal, data was obtained regarding the hosts that mosquitoes feed
on, and whether they contain WNV. Data was obtained regarding how long
there was detectable viremia in each host. This is done to determine if
certain species of birds can act as long term carriers of the virus to
then infect other hosts. Then, at each site, the number of blood meals
was recorded for each host, and if they were positive or negative for
WNV to determine if any species in particular are acting as amplifying
hosts for WNV. A generalized linear model was then performed to
determine if there is a correlation between WNV positive areas and
mosquitoes feeding on house finches.

## Barplots

Data was obtained from each of the sites that were WNV-Positive and
WNV-Negative. Then the number of blood meal counts from each individual
host were totaled with respect to each site. Comparing how many blood
meals were taken from each host with respect to if the area was positive
or negative for WNV helps visualize which hosts are potentially acting
as amplifying hosts.

``` r
## import counts_matrix: data.frame with column 'loc_positives' (0/1) and host columns 'host_*'
counts_matrix <- read.csv("./bloodmeal_plusWNV_for_BIOL3070.csv")

## 1) Identify host columns
host_cols <- grep("^host_", names(counts_matrix), value = TRUE)

if (length(host_cols) == 0) {
  stop("No columns matching '^host_' were found in counts_matrix.")
}

## 2) Ensure loc_positives is present and has both levels 0 and 1 where possible
counts_matrix$loc_positives <- factor(counts_matrix$loc_positives, levels = c(0, 1))

## 3) Aggregate host counts by loc_positives
agg <- stats::aggregate(
  counts_matrix[, host_cols, drop = FALSE],
  by = list(loc_positives = counts_matrix$loc_positives),
  FUN = function(x) sum(as.numeric(x), na.rm = TRUE)
)

## make sure both rows exist; if one is missing, add a zero row
need_levels <- setdiff(levels(counts_matrix$loc_positives), as.character(agg$loc_positives))
if (length(need_levels)) {
  zero_row <- as.list(rep(0, length(host_cols)))
  names(zero_row) <- host_cols
  for (lv in need_levels) {
    agg <- rbind(agg, c(lv, zero_row))
  }
  ## restore proper type
  agg$loc_positives <- factor(agg$loc_positives, levels = c("0","1"))
  ## coerce numeric host cols (they may have become character after rbind)
  for (hc in host_cols) agg[[hc]] <- as.numeric(agg[[hc]])
  agg <- agg[order(agg$loc_positives), , drop = FALSE]
}

## 4) Decide species order (overall abundance, descending)
overall <- colSums(agg[, host_cols, drop = FALSE], na.rm = TRUE)
host_order <- names(sort(overall, decreasing = TRUE))
species_labels <- rev(sub("^host_", "", host_order))  # nicer labels

## 5) Build count vectors for each panel in the SAME order
counts0 <- rev(as.numeric(agg[agg$loc_positives == 0, host_order, drop = TRUE]))
counts1 <- rev(as.numeric(agg[agg$loc_positives == 1, host_order, drop = TRUE]))

## 6) Colors: reuse your existing 'cols' if it exists and is long enough; otherwise generate
if (exists("cols") && length(cols) >= length(host_order)) {
  species_colors <- setNames(cols[seq_along(host_order)], species_labels)
} else {
  species_colors <- setNames(rainbow(length(host_order) + 10)[seq_along(host_order)], species_labels)
}

## 7) Shared x-limit for comparability
xmax <- max(c(counts0, counts1), na.rm = TRUE)
xmax <- if (is.finite(xmax)) xmax else 1
xlim_use <- c(0, xmax * 1.08)

## 8) Plot: two horizontal barplots with identical order and colors
op <- par(mfrow = c(1, 2),
          mar = c(4, 12, 3, 2),  # big left margin for species names
          xaxs = "i")           # a bit tighter axis padding

## Panel A: No WNV detected (loc_positives = 0)
barplot(height = counts0,
        names.arg = species_labels, 
        cex.names = .5,
        cex.axis = .5,
        col = rev(unname(species_colors[species_labels])),
        horiz = TRUE,
        las = 1,
        xlab = "Bloodmeal counts",
        main = "Locations WNV (-)",
        xlim = xlim_use)

## Panel B: WNV detected (loc_positives = 1)
barplot(height = counts1,
        names.arg = species_labels, 
        cex.names = .5,
        cex.axis = .5,
        col = rev(unname(species_colors[species_labels])),
        horiz = TRUE,
        las = 1,
        xlab = "Bloodmeal counts",
        main = "Locations WNV (+)",
        xlim = xlim_use)
```

![](Siddoway.Warm-up-mosquitoes-TEMPLATE-2_files/figure-gfm/first-analysis-1.png)<!-- -->

``` r
par(op)

## Keep the colors mapping for reuse elsewhere
host_species_colors <- species_colors
```

## Generalized Linear Model

A generalize linear model (GLM) was performed to determine if sites
where mosquitoes feed on house finches are also sites that are positive
for WNV. This test utilizes the relationships shown by the barplots to
statistically evaluate if there is a correlation, and determine if house
finches act as an amplifying host for WNV in the SLC area.

``` r
# second-analysis-or-plot, glm with house finch alone against binary +/_
glm1 <- glm(loc_positives ~ host_House_finch,
            data = counts_matrix,
            family = binomial)
summary(glm1)
```

    ## 
    ## Call:
    ## glm(formula = loc_positives ~ host_House_finch, family = binomial, 
    ##     data = counts_matrix)
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)       -0.1709     0.1053  -1.622   0.1047  
    ## host_House_finch   0.3468     0.1586   2.187   0.0287 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 546.67  on 394  degrees of freedom
    ## Residual deviance: 539.69  on 393  degrees of freedom
    ## AIC: 543.69
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
#glm with house-finch alone against positivity rate
glm2 <- glm(loc_rate ~ host_House_finch,
            data = counts_matrix)
summary(glm2)
```

    ## 
    ## Call:
    ## glm(formula = loc_rate ~ host_House_finch, data = counts_matrix)
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      0.054861   0.006755   8.122 6.07e-15 ***
    ## host_House_finch 0.027479   0.006662   4.125 4.54e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 0.01689032)
    ## 
    ##     Null deviance: 6.8915  on 392  degrees of freedom
    ## Residual deviance: 6.6041  on 391  degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## AIC: -484.56
    ## 
    ## Number of Fisher Scoring iterations: 2

# DISCUSSION

Results for each of the graphical and analytical methods were obtained
as shown above. The barplots and GLM showed valuable information
regarding WNV positive areas with blood meals taken from house finches.

## Interpretation of barplots

The first bat plot shows how long viremia was detected in each of hte
hosts. We found that house finches maintain viremia for 4 days (+/-2
days). Because house finches are able to maintain the virus for a long
period of time, it allows mosquitoes who get blood meals from the house
finches to carry the infected blood to other hosts. This can ultimately
lead to spillover to humans and other mammals. The second bar plot shows
how many blood meal counts were obtained for each host at sites that
were WNV positive and WNV negative. We found that house finches are the
most common hosts that mosquitoes take blood meals from. In WNV positive
areas, house finches are by far the most common host. This supports our
prediction that areas that were WNV positive will also have higher
counts of blood meals from house finches.

## Interpretation of the generalized linear model

The GLM was used to determine if there was a correlation between blood
meals taken from house finches and WNV positive areas. It was determined
that there was a very high statistical correlation between blood meals
taken from house finches and WNV positive areas (p-value = 4.54e-5).
While this doesn’t show a direct causation, it shows that there is a
very strong correlation between the two, meaning it’s likely that house
finches act as amplifying hosts for WNV.

# CONCLUSION

We wanted to determine if there were specific bird species that acted as
amplifying hosts for WNV in Salt Lake City, Utah. After obtaining and
analyzing data from the area, we found evidence to help support our
hypothesis that house finches act as amplifying hosts for the disease.
House finches have long periods of detectable viremia, are the most
common source of blood meals for mosquitoes, and were by far the most
common host in WNV positive areas. The GLM showed a strong correlation
between blood meals taken from house finches and WNV positive areas.
This helps to support the hypothesis that house finches act as a
amplifying host for WNV in Salt Lake City. This can help lead to
additional research and action taken to help reduce the spread of the
disease throughout the local area.

# REFERENCES

1.  Komar N, Langevin S, Hinten S, Nemeth N, Edwards E, Hettler D, Davis
    B, Bowen R, Bunning M. Experimental infection of North American
    birds with the New York 1999 strain of West Nile virus. Emerg Infect
    Dis. 2003 Mar;9(3):311-22. <https://doi.org/10.3201/eid0903.020628>

2.  ChatGPT. OpenAI, version Jan 2025. Used as a reference for functions
    such as plot() and to correct syntax errors. Accessed 2025-10-09.
