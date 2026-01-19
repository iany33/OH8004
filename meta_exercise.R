
## Chp. 7 Meta-analysis Part 1 Exercise Answers

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

# Load extra packages needed for formatting and analysis

library(dplyr)
library(meta)

# Conduct meta-analyses

Bradt_meta <- Bradt_2021 |> 
  metacont(n.e = int_n,             # number in intervention group
           mean.e = int_mean,       # intervention group mean
           sd.e = int_sd,           # intervention group SD
           n.c = con_n,             # number in control group
           mean.c = con_mean,       # control group mean
           sd.c = con_sd,           # control group SD
           studlab = study,         # study ID column
           sm = "MD",               # summary measure (MD or SMD)
           random = TRUE,           # conduct random-effects analysis
           common = FALSE,          # do not conduct a fixed-effect analysis
           method.tau = "REML",     # method of calculating tau
           method.random.ci = "HK", # apply the Hartung-Knapp adjustment
           title = "Music Intervention and Anxiety"
  )

summary(Bradt_meta)

Hartmann_meta <- Hartmann_2018 |> 
  filter(treatment == "patch") |>   # subset data for meta-analysis 
  metabin(event.e = x.nrt,          # events in intervention group
          n.e = n.nrt,              # number in intervention group
          event.c = x.ctrl,         # events in control group
          n.c = n.ctrl,             # number in control group
          studlab = study,          # study ID column
          sm = "RR",                # summary measure (OR or RR)
          method = "MH",            # use the Mantel-Haenszel method
          random = TRUE,            # conduct random-effects analysis
          common = FALSE,           # do not conduct a fixed-effect analysis
          method.tau = "PM",        # method of calculating tau
          method.random.ci = "HK",  # apply the Hartung-Knapp adjustment
          MH.exact = TRUE,          # do not apply a continuity correction
          title = "Nicotine Patch and Smoking Cessation"
  )

summary(Hartmann_meta)

Young_2018_meta <- 
  metaprop(event = n.positive, 
           n = n.total,
           studylab = "author.year", 
           subset = outcome.category == "Training status/policies",
           data = Young_2018,
           method = "GLMM",
           sm = "PLOGIT",
           random = TRUE,
           common = FALSE,
           method.random.ci = "HK", 
           title = "Food Allergy Training Prevalence"
  )

summary(Young_2018_meta)


Bradt_meta <- update(Bradt_meta, prediction = TRUE)
Bradt_meta

# Forest plots

Bradt_meta |> forest(digits.mean = 1,  # Limiting significant digits on left columns
                     digits.sd = 1,
                     fontsize = 7,           # Decrease fontsize so all text fits on image
                     spacing = 0.9           # Decrease spacing to ensure text fits
)

Bradt_meta |> forest(sortvar = TE,        # sort studies by effect size
                     prediction = TRUE,         # add prediction interval
                     print.tau2 = FALSE,        # do not show tau2 at the bottom
                     leftcols = "study",        # show only the study ID on the left
                     leftlabs = "Study",        # relabel study column
                     col.diamond = "steelblue", # colour customization
                     col.predict = "darkblue",
                     spacing = 0.85)


# Exercise

Young_2019_meta <- Young_2019 |> 
  filter(study_design == "RCT" & outcome == "Behaviour") |>
  metagen(TE = g,
          seTE = g_se,
          studlab = author_year,
          sm = "SMD", 
          random = TRUE,
          common = FALSE,
          method.tau = "REML",
          method.random.ci = "HK",
          title = "Food Handler Training and Behaviour - RCTs"
  )

summary(Young_2019_meta)

Young_2019_meta |> forest(sortvar = TE,         # sort studies by effect size
                          prediction = TRUE,          # add prediction interval
                          print.tau2 = FALSE,         # do not show tau2 at the bottom
                          leftcols = "author_year",   # show only the study ID on the left
                          leftlabs = "Author (Year)", # relabel study column
                          col.diamond = "steelblue",  # colour customization
                          col.predict = "darkblue",
                          spacing = 0.85)

# Exercise results 

# RCTs and the knowledge outcome

Young_2019_meta_rct_know <- Young_2019 |> 
  filter(study_design == "RCT" & outcome == "Knowledge") |>
  metagen(TE = g,
          seTE = g_se,
          studlab = author_year,
          sm = "SMD", 
          random = TRUE,
          common = FALSE,
          method.tau = "REML",
          method.random.ci = "HK",
          title = "Food Handler Training and Knowledge - RCTs"
  )

summary(Young_2019_meta_rct_know)

Young_2019_meta_rct_know |> 
  forest(sortvar = year, 
         prediction = TRUE,      
         print.tau2 = FALSE,      
         leftcols = "author_year",
         leftlabs = "Author (Year)",
         col.diamond = "steelblue", 
         col.predict = "darkblue",
         spacing = 0.85)

# RCTs and the inspection scores outcome

Young_2019_meta_rct_inspection <- Young_2019 |> 
  filter(study_design == "RCT" & outcome == "Inspection") |>
  metagen(TE = g,
          seTE = g_se,
          studlab = author_year,
          sm = "SMD", 
          random = TRUE,
          common = FALSE,
          method.tau = "REML",
          method.random.ci = "HK",
          title = "Food Handler Training and Inspection Scores - RCTs"
  )

summary(Young_2019_meta_rct_inspection)

Young_2019_meta_rct_inspection |> 
  forest(sortvar = year, 
         prediction = TRUE,      
         print.tau2 = FALSE,      
         leftcols = "author_year",
         leftlabs = "Author (Year)",
         col.diamond = "steelblue", 
         col.predict = "darkblue",
         spacing = 0.85)

# Non-randomized studies and the inspection scores outcome

Young_2019_meta_nr_inspection <- Young_2019 |> 
  filter(study_design == "Non-randomized study" & outcome == "Inspection") |>
  metagen(TE = g,
          seTE = g_se,
          studlab = author_year,
          sm = "SMD", 
          random = TRUE,
          common = FALSE,
          method.tau = "REML",
          method.random.ci = "HK",
          title = "Food Handler Training and Inspection Scores - NRTs"
  )

summary(Young_2019_meta_nr_inspection)

Young_2019_meta_nr_inspection |> 
  forest(sortvar = year, 
         prediction = TRUE,      
         print.tau2 = FALSE,      
         leftcols = "author_year",
         leftlabs = "Author (Year)",
         col.diamond = "steelblue", 
         col.predict = "darkblue",
         spacing = 0.85)

# Non-randomized studies and the attitudes outcome

Young_2019_meta_nr_attitudes <- Young_2019 |> 
  filter(study_design == "Non-randomized study" & outcome == "Attitudes") |>
  metagen(TE = g,
          seTE = g_se,
          studlab = author_year,
          sm = "SMD", 
          random = TRUE,
          common = FALSE,
          method.tau = "REML",
          method.random.ci = "HK",
          title = "Food Handler Training and Attitudes - NRTs"
  )

summary(Young_2019_meta_nr_attitudes)

Young_2019_meta_nr_attitudes |> 
  forest(sortvar = year, 
         prediction = TRUE,      
         print.tau2 = FALSE,      
         leftcols = "author_year",
         leftlabs = "Author (Year)",
         col.diamond = "steelblue", 
         col.predict = "darkblue",
         spacing = 0.85)



