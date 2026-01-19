
## Chp. 8 Meta-analysis Part 2 Exercise Answers

# First import the datasets for exercises

library(rio)
library(here)

# Before running commands below, you will need to first download the datasets
# Here: https://drive.google.com/drive/u/1/folders/1n184_5BPsdvkmuszzYed3omJhiU5ScKz
# Then place them into the same folder as your R Project in a folder called "assets"

Bradt_2021 <- import(here("assets", "Bradt_2021.xlsx"))
Hartmann_2018 <- import(here("assets", "Hartmann_2018.xlsx"))
Young_2018 <- import(here("assets", "Young_2018.xlsx"))
Young_2019 <- import(here("assets", "Young_2019_ma.xlsx"))
Soga_2017 <- import(here("assets", "Soga_2017.xlsx"))

# Load extra packages needed for formatting and analysis

library(dplyr)
library(meta)

# Sub-group analysis

Hartmann_meta <- Hartmann_2018 |> 
  metabin(event.e = x.nrt,
          n.e = n.nrt, 
          event.c = x.ctrl, 
          n.c = n.ctrl, 
          studlab = study,
          sm = "RR",
          method = "MH", 
          random = TRUE,
          fixed = FALSE, 
          method.tau = "PM",
          hakn = TRUE,
          MH.exact = TRUE,
          subgroup = treatment,  # specify our subgroup variable
          tau.common = FALSE,    # calculate tau separately for each subgroup
          title = "Nicotine Replacement Therapy and Smoking Cessation"
  )

Hartmann_meta

Hartmann_meta <- Hartmann_2018 |> 
  filter(treatment == "gum" | treatment == "patch" | 
           treatment == "tablets/lozenges" | treatment == "choice of product") |> 
  metabin(event.e = x.nrt,
          n.e = n.nrt, 
          event.c = x.ctrl, 
          n.c = n.ctrl, 
          studlab = study,
          sm = "RR",
          method = "MH", 
          random = TRUE,
          fixed = FALSE, 
          method.tau = "PM",
          hakn = TRUE,
          MH.exact = TRUE,
          subgroup = treatment,  # specify our subgroup variable
          tau.common = FALSE,    # calculate tau separately for each subgroup
          title = "Nicotine Replacement Therapy and Smoking Cessation"
  )

# Meta-regression

Bradt_meta <- Bradt_2021 |> 
  metacont(n.e = int_n,
           mean.e = int_mean,
           sd.e = int_sd,
           n.c = con_n, 
           mean.c = con_mean,
           sd.c = con_sd,  
           studlab = study,
           sm = "MD", 
           random = TRUE,
           fixed = FALSE,
           method.tau = "REML",
           hakn = TRUE, 
           title = "Music Intervention and Anxiety")

Bradt_reg <- metareg(Bradt_meta, ~year)
Bradt_reg

bubble(Bradt_reg, studlab = TRUE)

# Exercise 1: Sub-group and meta-regression

# RCTs vs. Non-randomized studies (Study design): Behaviour outcome

Young_2019_meta_beh <- Young_2019 |> 
  filter(outcome == "Behaviour") |> 
  metagen(TE = g,
          seTE = g_se,
          studlab = author_year,
          sm = "SMD", 
          random = TRUE,
          common = FALSE,
          method.tau = "REML",
          method.random.ci = "HK",
          subgroup = study_design,  # specify our subgroup variable
          tau.common = TRUE,    # calculate tau separately for each subgroup
          title = "Food Handler Training and Behaviour - by Study Design")

Young_2019_meta_beh

Young_2019_meta_beh |> 
  forest(prediction = TRUE,
         print.pval.Q = FALSE, 
         leftcols = "author_year",
         leftlabs = "Study", 
         smlab = "SMD",
         print.subgroup.name = FALSE,
         col.diamond = "steelblue",
         col.predict = "darkblue",
         fontsize = 9,
         spacing = 0.5)

Young_2019_meta <- Young_2019 |> 
  filter(outcome == "Behaviour") |> 
  metagen(TE = g,
          seTE = g_se,
          studlab = author_year,
          sm = "SMD", 
          random = TRUE,
          common = FALSE,
          method.tau = "REML",
          method.random.ci = "HK",
          title = "Food Handler Training and Behaviour")

Young_2019_metreg <- metareg(Young_2019_meta, ~year)
Young_2019_metreg 

Young_2019_metreg2 <- metareg(Young_2019_meta, ~doc_type)
Young_2019_metreg2


# RCTs vs. Non-randomized studies (Study design): Inspection scores outcome

Young_2019_meta_inspection <- Young_2019 |> 
  filter(outcome == "Inspection") |> 
  metagen(TE = g,
          seTE = g_se,
          studlab = author_year,
          sm = "SMD", 
          random = TRUE,
          common = FALSE,
          method.tau = "REML",
          method.random.ci = "HK",
          subgroup = study_design,  # specify our subgroup variable
          tau.common = TRUE,    # calculate tau separately for each subgroup
          title = "Food Handler Training and Inspection Scores - by Study Design")

Young_2019_meta_inspection

Young_2019_meta_inspection |> 
  forest(prediction = TRUE,
         print.pval.Q = FALSE, 
         leftcols = "author_year",
         leftlabs = "Study", 
         smlab = "SMD",
         print.subgroup.name = FALSE,
         col.diamond = "steelblue",
         col.predict = "darkblue",
         fontsize = 9,
         spacing = 0.5)

Young_2019_meta2 <- Young_2019 |> 
  filter(outcome == "Inspection") |> 
  metagen(TE = g,
          seTE = g_se,
          studlab = author_year,
          sm = "SMD", 
          random = TRUE,
          common = FALSE,
          method.tau = "REML",
          method.random.ci = "HK",
          title = "Food Handler Training and Inspection Scores")

Young_2019_metreg <- metareg(Young_2019_meta2, ~year)
Young_2019_metreg 

Young_2019_metreg2 <- metareg(Young_2019_meta2, ~doc_type)
Young_2019_metreg2


# Publication Bias

Hartmann_meta <- Hartmann_2018 |> 
  metabin(event.e = x.nrt,
          n.e = n.nrt, 
          event.c = x.ctrl, 
          n.c = n.ctrl, 
          studlab = study,
          sm = "RR",
          method = "MH", 
          random = TRUE,
          common = FALSE, 
          method.tau = "PM",
          method.random.ci = "HK",
          MH.exact = TRUE,
          title = "Nicotine Replacement Therapy and Smoking Cessation"
  )
Hartmann_meta


col.contour = c("gray50", "gray70", "gray90")  # set custom contour colours

funnel(Hartmann_meta, 
       contour = c(0.9, 0.95, 0.99),      # set contour P value regions
       col.contour = col.contour)         # apply custom contour colour

# Add legend in top right position based on x and y coordinates
legend(x = 7, y = 0.01, 
       legend = c("P < 0.1", "P < 0.05", "P < 0.01"),
       fill = col.contour)


metabias(Hartmann_meta, method.bias = "Peters")

Hartmann_meta_trim <- trimfill(Hartmann_meta)
Hartmann_meta_trim

funnel(Hartmann_meta_trim, 
       contour = c(0.9, 0.95, 0.99),  
       col.contour = col.contour)

legend(x = 7, y = 0.01, 
       legend = c("P < 0.1", "P < 0.05", "P < 0.01"),
       fill = col.contour)


# Publication bias example

Bradt_meta

col.contour = c("gray50", "gray70", "gray90")  # set custom contour colours

funnel(Bradt_meta, 
       contour = c(0.9, 0.95, 0.99),      # set contour P value regions
       col.contour = col.contour)         # apply custom contour colour


metabias(Bradt_meta, method.bias = "linreg")

Bradt_meta_trim <- trimfill(Bradt_meta)
Bradt_meta_trim

funnel(Bradt_meta_trim, 
       contour = c(0.9, 0.95, 0.99),  
       col.contour = col.contour)

legend(x = -20, y = 0.01, 
       legend = c("P < 0.1", "P < 0.05", "P < 0.01"),
       fill = col.contour)

# Complex data

M <- (33+46)/(8+8)
ICC <- 0.1
DEFF <- 1 + (M-1)*(ICC)
DEFF

# Pool group function

pool.groups <- function(n1, n2, m1, m2, sd1, sd2) {
  
  n1 = n1
  n2 = n2
  m1 = m1
  m2 = m2
  sd1 = sd1
  sd2 = sd2
  
  if (is.numeric(n1) == FALSE) {
    stop("'n1' must by of type numeric().")
  }
  
  if (is.numeric(n2) == FALSE) {
    stop("'n2' must by of type numeric().")
  }
  
  if (n1 < 1 | n2 < 1) {
    stop("'n1' and 'n2' must both the greater than 0.")
  }
  
  if (is.numeric(m1) == FALSE) {
    stop("'m1' must by of type numeric().")
  }
  
  if (is.numeric(m2) == FALSE) {
    stop("'m2' must by of type numeric().")
  }
  
  if (is.numeric(sd1) == FALSE) {
    stop("'sd1' must by of type numeric().")
  }
  
  if (is.numeric(sd2) == FALSE) {
    stop("'sd2' must by of type numeric().")
  }
  
  Npooled = n1 + n2
  Mpooled = (n1 * m1 + n2 * m2)/(n1 + n2)
  SDpooled = sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2 + (((n1 * n2)/(n1 + n2)) * (m1^2 + m2^2 - 2 * m1 *
                                                                                     m2)))/(n1 + n2 - 1))
  
  return(data.frame(Mpooled, SDpooled, Npooled))
}

# Pool groups example

pool.groups(n1 = 50, n2 = 50,
            m1 = 2.0, sd1 = 1.5,
            m2 = 2.5, sd2 = 2.3)

data <- data.frame(study_ID = as.factor(c(1,2,3,3)),
                   intervention = c("Training", "Training", "Training-Group 1", "Training-Group 2"),
                   treat_mean = c(1.5, 2.1, 2.2, 2.7),
                   treat_SD = c(1.0, 1.7, 2.0, 2.1),
                   treat_n = c(100, 50, 70, 60),
                   control_mean = c(1.2, 1.5, 2.0, 1.9),
                   control_SD = c(1.0, 1.3, 1.75, 1.8),
                   control_n = c(100, 50, 75, 75))
data

combined <- pool.groups(n1 = data$treat_n[3], 
                        n2 = data$treat_n[4],
                        m1 = data$treat_mean[3], 
                        sd1 = data$treat_SD[3],
                        m2 = data$treat_mean[4], 
                        sd2 = data$treat_SD[4])
combined

# First create the new dataframe with 1 row per study
data_combined <- data |> 
  group_by(study_ID) |> 
  slice_head() |> 
  ungroup()

# Update Study 3's description and values with the new combined values
data_combined$intervention["study_ID" = 3] <- "Training-combined" 
data_combined$treat_mean["study_ID" = 3] <- combined$Mpooled
data_combined$treat_SD["study_ID" = 3] <- combined$SDpooled
data_combined$treat_n["study_ID" = 3] <- combined$Npooled
data_combined


# Meta-analysis practice

# Example with depression outcome

meta <- Soga_2017 |> 
  filter(outcome == "Depression") |> 
  metacont(n.e = treat_n,  
           mean.e = treat_mean,
           sd.e = treat_SD,   
           n.c = control_n, 
           mean.c = control_mean,  
           sd.c = control_SD, 
           studlab = author_year,   
           sm = "SMD", 
           random = TRUE,
           common = FALSE, 
           method.tau = "REML",
           method.random.ci = "HK",
           title = "Association between Gardening and Depression")

summary(meta)

meta |> forest(sortvar = TE,
               prediction = TRUE,  
               leftcols = "author_year",
               leftlabs = "Study, Year")

meta2 <- Soga_2017 |> 
  filter(outcome == "Depression") |> 
  metacont(n.e = treat_n,  
           mean.e = treat_mean,
           sd.e = treat_SD,   
           n.c = control_n, 
           mean.c = control_mean,  
           sd.c = control_SD, 
           studlab = author_year,   
           sm = "SMD", 
           random = TRUE,
           common = FALSE, 
           method.tau = "REML",
           method.random.ci = "HK",
           subgroup = comparison_type,
           tau.common = TRUE)

summary(meta2)


col.contour = c("gray50", "gray70", "gray90")  # set custom contour colours

funnel(meta, 
       contour = c(0.9, 0.95, 0.99),      # set contour P value regions
       col.contour = col.contour)         # apply custom contour colour

legend(x = -1.5, y = 0.01, 
       legend = c("P < 0.1", "P < 0.05", "P < 0.01"),
       fill = col.contour)

metabias(meta, method.bias = "Pustejovsky")

