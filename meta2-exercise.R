
pacman::p_load(
  rio,        # load in an Excel file
  here,       # loading data using relative path
  tidyverse,  # data management
  meta,       # meta-analysis
  DT          # to visualize the dataset 
)

Hartmann_2018 <- import(here("assets", "Hartmann_2018.xlsx"))

Hartmann_2018 |> datatable(
  rownames = FALSE,
  options = list(
    columnDefs = list(list(className = 'dt-center', 
                           targets = 0:4)))) 

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

Hartmann_meta

Bradt_2021 <- import(here("assets", "Bradt_2021.xlsx"))

Bradt_2021 |> datatable(
  rownames = FALSE,
  options = list(
    columnDefs = list(list(className = 'dt-center', 
                           targets = 0:4))))


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


# First load necessary packages
pacman::p_load(
  rio,        # load in an Excel file
  here,       # loading data using relative path
  tidyverse,  # data management
  meta,       # meta-analysis
  DT          # to visualize the dataset 
)

Young_2019 <- import(here("assets", "Young_2019_ma.xlsx"))

Young_2019 |> datatable(
  rownames = FALSE,
  options = list(
    columnDefs = list(list(className = 'dt-center', 
                           targets = 0:4)))) |> 
  formatStyle(columns = colnames(Young_2019), fontSize = '70%')


Young_meta <- Young_2019 |> 
  filter(outcome == "Behaviour") |> 
  metagen(TE = g,
          seTE = g_se,
          studlab = author_year,
          prediction = TRUE,
          sm = "SMD", 
          random = TRUE,
          fixed = FALSE,
          method.tau = "REML",
          hakn = TRUE,
          subgroup = study_design,  # specify our subgroup variable
          tau.common = TRUE,    # calculate tau separately for each subgroup
          title = "Food Handler Training and Behaviour"
  )
Young_meta

Young_meta |> 
  forest(prediction = TRUE,
         print.pval.Q = FALSE, 
       leftcols = "author_year",
       leftlabs = "Study", 
       print.subgroup.name = FALSE,
       col.diamond = "steelblue",
       col.predict = "darkblue",
       fontsize = 6,
       spacing = 0.5)


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

# Pub bias example
Bradt_meta

col.contour = c("gray50", "gray70", "gray90")  # set custom contour colours

funnel(Bradt_meta, 
       contour = c(0.9, 0.95, 0.99),      # set contour P value regions
       col.contour = col.contour)         # apply custom contour colour
legend(x =-20, y = 0.01, 
       legend = c("P < 0.1", "P < 0.05", "P < 0.01"),
       fill = col.contour)


metabias(Bradt_meta, method.bias = "linreg")

Bradt_meta_trim <- trimfill(Bradt_meta)
Bradt_meta_trim

funnel(Bradt_meta_trim, 
       contour = c(0.9, 0.95, 0.99),      # set contour P value regions
       col.contour = col.contour)         # apply custom contour colour
legend(x =-20, y = 0.01, 
       legend = c("P < 0.1", "P < 0.05", "P < 0.01"),
       fill = col.contour)

# Complex data

M <- (33+46)/(8+8)
ICC <- 0.1
DEFF <- 1 + (M-1)*(ICC)
DEFF