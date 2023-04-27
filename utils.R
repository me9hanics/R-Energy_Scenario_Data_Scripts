#' Useful functions for the `decent` software package.


# |||| ---------------------------------
# Decent Living Energy utils -----------
# |||| ---------------------------------

load_pkgs_runscript <- function(first.time = F, dev = F, plotting = F) {
  pkgs <<- c(
    "maps",
    "rworldmap",
    "mapproj",
    "vroom", # for loading in CSV files fast
    "zoo", # for imputation using `na.approx`
    "lestat", # for inverse cumulative distribtuion for depth of deficit (note - also loads the MASS function `select`, so tidyverse needs to be loaded later)
    "countrycode", # for working easily with country codes
    "readxl", # for reading in excel files
    "writexl", # for writing out excel files
    "sitools", # for clear unit changes, required by DLE_integration_data_structure.R
    "WDI", # World Bank Development indicators package
    "table1", # for creating a table with descriptive statistics
    "plotly", # for interactive plots
    "htmlwidgets", # for saving interactive plots
    "here", # for specifying relative paths
    "lme4", # for LmList
    "tidyverse", # for data wrangling
    "logger" # for info (log_info), warnings (log_warn), and debugging (log_debug) -- with the option set in log_threshold, i.e. `log_threshold(TRACE)` to log everything
  )
  if (dev) {
    # add packages only used by code developers
    pkgs <- c(pkgs, "styler")
  }
  if (plotting) {
    # add packages only used for plotting (in the plotting gallery)
    pkgs <- c(
      pkgs,
      # "see", # for geom_violinhalf
      # "MethylCapSig", # for multivariate lognormal model prediction and sampling
      "patchwork", # for multipanel handling
      "ggsci", # for some nice colour palettes
      "treemapify", # for making treemaps
      "scales", # for scaling axes of ggplot objects
      "rgdal", # for transforming map projection to a more reasonable projection method
      "rvg", # to make ggplot objects into editable DML objects to write out to powerpoint
      "officer" # e.g. to write out plots as editable figures in powerpoint
    )
  }
  if (first.time) {
    install.packages(pkgs)
  }
  load <- lapply(pkgs, library, character.only = TRUE)
  select <- dplyr::select # explicitly say that we mean dplyr's select function whenever we use select (not the one from the MASS library...)
  filter <- dplyr::filter # explicitly say that we mean dplyr's filter function whenever we use filter (not the one from the stats library...)
  mutate <- dplyr::mutate # explicitly say that we mean dplyr's mutate function whenever we use mutate
}
load_pkgs_runscript(first.time = T, dev = T, plotting = T) # for now, we by default always load packages if utils.R is loaded


# Load and initialize DLS dimensions ====

load_dimensions <- function() {
  source("DLE_integration_data_structure.R") # integration structure for dimensions

  source("DLE_clothing.R")
  source("DLE_nutrition.R")
  source("DLE_health.R")
  source("DLE_water.R")
  source("DLE_sanitation.R")
  source("DLE_roads.R")
  source("DLE_housing.R")
  source("DLE_cooling_con.R")
  source("DLE_cooling_op.R")
  source("DLE_heating_con.R")
  source("DLE_heating_op.R")
  source("DLE_hotwater_op.R")
  source("DLE_appliances.R")
  source("DLE_education.R")
  source("DLE_transport.R")
}

generate_all_dimensions <- function() {
  # NB. currently should follow exactly the function 'AggregateDimensions()'!!
  return(
    list(
      transport = DLE.dimension.transport(
        name_dim = "Transport",
        indicator = "pkm/cap/year",
        unit_rollout = "cap",
        grp = c("car", "bus", "rail", "twothree")
      ),
      appliances = DLE.dimension.appliances(
        name_dim = "Appliance",
        indicator = "unit/hh", # percentage household that have a certain appliance
        unit_rollout = "hh", # all of these are considered on a household level
        grp = c("clean_cooking_fuel", "television", "mobile_telephone", "refrigerator")
      ),
      water = DLE.dimension.water(
        name_dim = "Water",
        indicator = "m3/cap/year",
        unit_rollout = "cap",
        grp = "water"
      ),
      sanit = DLE.dimension.sanit(
        name_dim = "Sanitation",
        indicator = "cap",
        unit_rollout = "cap",
        grp = "sanitation"
      ),
      nutrition = DLE.dimension.nutrition(
        name_dim = "Nutrition",
        unit_rollout = "cap",
        indicator = "kcal/day",
        grp = "nutrition"
      ),
      clothing = DLE.dimension.clothing(
        name_dim = "Clothing",
        indicator = "kg/year",
        unit_rollout = "cap",
        grp = c("clothing", "footwear")
      ),
      health = DLE.dimension.health(
        name_dim = "Health",
        indicator = "$/cap/year",
        unit_rollout = "cap",
        grp = "health"
      ),
      education = DLE.dimension.education(
        name_dim = "Education",
        indicator = "$/cap/year",
        unit_rollout = "cap",
        grp = c("primary", "lower_secondary")
      ),
      housing = DLE.dimension.housing(
        name_dim = "Housing",
        indicator = "m2/cap",
        unit_rollout = "cap",
        grp = c("rural", "urban")
      ),
      heating_con = DLE.dimension.heating_con(
        name_dim = "Heating CON",
        indicator = "cap",
        unit_rollout = "cap",
        grp = c("rural", "urban")
      ),
      cooling_con = DLE.dimension.cooling_con(
        name_dim = "Cooling CON",
        indicator = "cap",
        unit_rollout = "cap",
        grp = c("rural", "urban")
      ),
      heating_op = DLE.dimension.heating_op(
        name_dim = "Heating OP",
        indicator = "m2/cap",
        unit_rollout = "cap",
        grp = c("rural", "urban")
      ),
      cooling_op = DLE.dimension.cooling_op(
        name_dim = "Cooling OP",
        unit_rollout = "cap",
        indicator = "m2/cap",
        grp = c("rural", "urban")
      ),
      roads = DLE.dimension.roads(
        name_dim = "Roads",
        indicator = "km", ## Note: Threshold (road density km/km2) does not have same unit as indicator (km)!!
        unit_rollout = "abs",
        grp = "roads"
      ),
      hotwater_op = DLE.dimension.hotwater_op(
        name_dim = "Hot Water OP",
        indicator = "cap",
        unit_rollout = "cap",
        grp = c("rural", "urban")
        # df.input = data.frame(expenditure=3) # Some custom inputs to the dimension (placeholder)
      )
    )
  )
}

# Input data ====
run_initiatialization_input_data <- function(ssp = "SSP2", pov.thres = 20, year.base = 2015) {
  # MESSAGE R11 mapping to country iso
  message.R11 <<- read_excel(paste0(data.path, "/iso_region_MESSAGE.xlsx")) %>%
    mutate(iso = toupper(iso)) %>%
    rename(R11.region = `MESSAGE-GLOBIOM`) %>%
    select(-RCP_REG) %>%
    mutate(iso = ifelse(iso == "ROM", "ROU", iso)) # Typo fix for Romania

  # placeholder population - TODO: import SSP population projections and replace this
  # pop <- population %>%
  #   filter(year == 2013) %>% select(-year) %>%  # why is this for 2013? The latest year in this R-provided 'population' df
  #   mutate(iso = countrycode(country, 'country.name', 'iso3c'))

  # Population and Urbanization rate
  # For now, focus on SSP2
  pop <<- read_excel(paste0(data.path, "/iamc_db_SSP_population.xlsx")) %>%
    filter(Model == "IIASA-WiC POP", Scenario == ssp) %>%
    select(-Variable, -Model, -Scenario) %>%
    pivot_longer(cols = `2010.0`:`2100.0`, names_to = "year", values_to = "population") %>%
    mutate(year = as.numeric(year), population = population * mega) %>%
    select(-Notes, -Unit) %>%
    rename(iso = Region)

  # Filter countries based on available pop data
  message.R11 <<- message.R11 %>% filter(iso %in% unique(pop$iso))

  # Urbanization
  urbanization <<- read_excel(paste0(data.path, "/iamc_db_SSP_urbanshare.xlsx")) %>%
    filter(Scenario == ssp) %>%
    select(-Variable, -Model, -Scenario) %>%
    pivot_longer(cols = `2010.0`:`2100.0`, names_to = "year", values_to = "urb.rate") %>%
    mutate(year = as.numeric(year), urb.rate = urb.rate / 100) %>%
    select(-Notes, -Unit) %>%
    rename(iso = Region)

  # household size data - constant over time
  hh_size <<- read.csv(paste0(data.path, "/hh_size.csv"), stringsAsFactors = FALSE)
  hh_size <<- message.R11 %>%
    left_join(hh_size) %>% # fill NAs
    group_by(R11.region) %>%
    mutate(hh_size_avg = mean(na.omit(hh_size))) %>%
    ungroup() %>%
    mutate(hh_size = ifelse(is.na(hh_size), hh_size_avg, hh_size)) %>%
    select(-c(country_name, R11.region, hh_size_avg))

  # Merge three demographic DFs above
  pop <<- message.R11 %>%
    left_join(pop) %>%
    left_join(urbanization) %>%
    left_join(hh_size) %>%
    mutate(n_hh = round(population / hh_size)) %>%
    mutate(population.urb = population * urb.rate, population.rur = population * (1 - urb.rate)) %>%
    select(-c(country_name, R11.region))

  # Exchange rate (for EXIO accounting)
  usd2eur.baseyr <<- WDI(country = "XC", indicator = "PA.NUS.FCRF", start = year.base, end = year.base)$PA.NUS.FCRF

  # poverty headcount closing rate
  pov.pop.var <<- paste0("pop_", pov.thres) # Name of the thres variable in the csv input file
  pov.gap <<- read.csv(paste0(data.path, "/gdpginipop_povcountdata.csv")) %>%
    rename(iso = country) %>%
    filter(scenario == ssp, year >= year.base) %>%
    select(scenario, iso, year, {{ pov.pop.var }}) %>%
    group_by(iso) %>%
    left_join(pop %>% select(iso, year, population)) %>%
    mutate(pov.pcap = get(pov.pop.var) / population) %>%
    mutate(r.closing = pov.pcap / first(pov.pcap)) %>% # gap closing rate index per capita (year.base = 1)
    mutate(r.diff = r.closing - lead(r.closing, default = tail(r.closing, 1))) %>% # % difference at each time period
    mutate(r.diff = pmax(0, r.diff))

  # gdp pathway for GDP-driven (income) scenario pathways
  gdp <<- read_csv("P:/ene.general/DecentLivingEnergy/DLE_scaleup/Data/gdp_gini_pop_ssp.csv") %>%
    mutate(gdp.pcap = gdp_ppp_2005_bil / pop_mil * 1000) %>%
    rename(iso = country) %>%
    filter(scenario == ssp) %>%
    select(iso, year, gdp.pcap) %>%
    drop_na()

  # some potentially useful population aggregates
  # regional population in base year
  R11.pop.baseyr <<- message.R11 %>%
    left_join(pop %>% filter(year == year.base)) %>%
    select(R11.region, population) %>%
    group_by(R11.region) %>%
    summarise(population = sum(population))
  # regional population
  R11.pop <<- message.R11 %>%
    left_join(pop) %>%
    select(iso, year, R11.region, population) %>%
    group_by(R11.region, year) %>%
    summarise(population = sum(population))
  R11.pop.urbrur <<- message.R11 %>%
    left_join(pop) %>%
    select(iso, year, R11.region, population, population.urb, population.rur) %>%
    group_by(R11.region, year) %>%
    summarise(population = sum(population), population.urb = sum(population.urb), population.rur = sum(population.rur))
  # # visualise regional population
  # ggplot(R11.pop, aes(x=year,y=population)) + geom_line(size=2) + facet_wrap(~R11.region)
  # ggplot(R11.pop.urbrur%>% pivot_longer(c(population.urb,population.rur), names_to="population.urbrur"), aes(x=year,y=value, colour=population.urbrur)) + geom_line(size=2) + facet_wrap(~R11.region)
  # global population
  G.pop <<- R11.pop %>%
    group_by(year) %>%
    summarise(population = sum(population))
}

# DLS deprivations ====
get_mobility_gap <- function(data.path) {
  DLE.transport <- DLE.dimension.transport(
    name_dim = "Transport",
    indicator = "pkm/cap/year",
    unit_rollout = "cap",
    grp = c("car", "bus", "rail", "twothree")
  )

  DLE.transport$DeriveThreshold()
  DLE.transport$IdentifyGap()
  return(DLE.transport$DF.DLS %>% select(iso, share.pop) %>% distinct(iso, .keep_all = TRUE)) # distinct because it returns for all 4 transport modes, with same share.pop, which is calculated based on the total pkm threshold
}

get_water_gap <- function(data.path) {
  fname_water <- "/API_SH.H2O.SMDW.ZS_DS2_en_excel_v2_802993.xls" ## Improved water access
  fname_inf_mort <- "/API_SP.DYN.IMRT.IN_DS2_en_excel_v2_992068.xls" ## Infant mortality data
  year.base <- 2015 # year for data to be loaded

  print("Load data: water")
  # Load data: access to water supply
  water_acc <- read_excel(paste0(data.path, "/Water", fname_water), sheet = "Data", skip = 3, col_names = TRUE)
  water_acc <- water_acc %>%
    select_at(c("Country Code", paste(year.base))) %>% # Extract data for the base year
    rename_at(paste(year.base), list(~ paste("water_acc"))) %>% # Rename column
    mutate(water_acc = water_acc / 100) %>% # Convert % numbers to range 0:1
    rename(iso = "Country Code")

  # Load data: Infant mortality
  mort <- read_excel(paste0(data.path, "/Water", fname_inf_mort), sheet = "Data", skip = 3, col_names = TRUE)
  mort <- mort %>%
    select_at(c("Country Code", paste(year.base))) %>% # Extract data for the base year
    rename_at(paste(year.base), list(~ paste("mort"))) %>%
    rename(iso = "Country Code")

  # Join data
  water_acc <- water_acc %>% # Merge data: water and infant mortality
    left_join(mort, by = "iso") %>%
    left_join(message.R11, by = "iso") %>% # message region data
    select(-c(country_name, R11.region))

  # Fit a regression model
  print("Regression model: fitting")
  lm_water <- lm(water_acc ~ log(mort), data = water_acc)
  print(summary(lm_water))

  # Extrapolate results
  print("Start results extrapolation")
  water_extr <- message.R11 %>%
    left_join(water_acc, by = "iso") # initialize
  water_extr <- water_extr %>%
    mutate(water_pred = predict(lm_water, water_extr)) %>% # predicted results
    mutate(water_extr = water_pred) %>% # copy predicted results into a new column for extrapolated results
    mutate_cond(!is.na(water_acc), water_extr = water_acc) %>% # copy original data, where available
    select(iso, R11.region, water_extr) %>% # Keep only iso and extrapolation results
    rename(water_access = water_extr)
  return(water_extr)
}

get_sanit_gap <- function(data.path) {
  fname_sanit <- "/API_SH.STA.SMSS.ZS_DS2_en_excel_v2_804737.xls" ## Improved sanitation data
  fname_inf_mort <- "/API_SP.DYN.IMRT.IN_DS2_en_excel_v2_992068.xls" ## Infant mortality data
  year.base <- 2015 # year for the gap data loading

  # Load data: access to sanit supply
  print("Load data")
  sanit_acc <- read_excel(paste0(data.path, "/Sanitation", fname_sanit), sheet = "Data", skip = 3, col_names = TRUE)
  sanit_acc <- sanit_acc %>%
    select_at(c("Country Code", paste(year.base))) %>% # Extract data for the base year
    rename_at(paste(year.base), list(~ paste("sanit_acc"))) %>% # Rename column
    mutate(sanit_acc = sanit_acc / 100) %>% # Convert % numbers to range 0:1
    rename(iso = "Country Code")


  # Load data: Infant mortality
  mort <- read_excel(paste0(data.path, "/Sanitation", fname_inf_mort), sheet = "Data", skip = 3, col_names = TRUE)
  mort <- mort %>%
    select_at(c("Country Code", paste(year.base))) %>% # Extract data for the base year
    rename_at(paste(year.base), list(~ paste("mort"))) %>%
    rename(iso = "Country Code")

  # Join data
  sanit_acc <- sanit_acc %>% # Merge data: sanitation and infant mortality
    left_join(mort, by = "iso") # %>%
  # left_join(message.R11, by="iso") %>% # message region data
  # select(-country_name)

  # Fit a regression model
  print("Regression model: fitting")
  lm_sanit <- lm(sanit_acc ~ log(mort), data = sanit_acc)
  print(summary(lm_sanit))

  # Extrapolate results
  print("Start results extrapolation")
  sanit_extr <- message.R11 %>% left_join(sanit_acc, by = "iso") # initialize
  sanit_extr <- sanit_extr %>%
    mutate(sanit_pred = predict(lm_sanit, sanit_extr)) %>% # predicted results
    mutate(sanit_extr = sanit_pred) %>% # copy predicted results into a new column for extrapolated results
    mutate_cond(!is.na(sanit_acc), sanit_extr = sanit_acc) %>% # copy original data, where available
    select(iso, R11.region, sanit_extr) %>% # Keep only iso and extrapolation results
    rename(sanit_access = sanit_extr)
  return(sanit_extr)
}

get_nutri_gap <- function(data.path) {
  DLE.nutrition <- DLE.dimension.nutrition(
    name_dim = "Nutrition",
    indicator = "kcal/day",
    grp = "nutrition"
  )

  DLE.nutrition$DeriveThreshold()
  DLE.nutrition$IdentifyGap()

  return(DLE.nutrition$DF.DLS %>% select(iso, share.pop))
}

get_edu_gap <- function(data.path) {
  method <- "regression"
  threshold_prim <- 95 # in %
  threshold_ls <- 90 # in %

  fname_prim <- "/WorldBank_EducationStatistics_primary.csv" # SE.PRM.CMPT.ZS
  fname_ls <- "/WorldBank_EducationStatistics_lowersecondary.csv" # SE.SEC.CMPT.LO.ZS
  fname_exp <- "/unesco_govspending_student_cleaned_copiedFromDLE3.csv"

  if (method == "regression") {
    ## read in completion rate files.
    education_completion_prim <- read_csv(paste0(data.path, "/Education", fname_prim)) %>%
      rename(
        iso = CountryCode
      ) %>%
      mutate(grp = "primary") %>%
      select(-c(IndicatorCode, IndicatorName, CountryName))
    education_completion_ls <- read_csv(paste0(data.path, "/Education", fname_ls)) %>%
      rename(
        iso = CountryCode
      ) %>%
      mutate(grp = "lower_secondary") %>%
      select(-c(IndicatorCode, IndicatorName, CountryName))

    ### drop rows if not containing enough information and convert to long format
    cleanandlong <- function(df_toclean, newvar = "completionrate", years = list(2011, 2012, 2013, 2014, 2015)) {
      # drop countries for which there is no data for 2011-2015
      df_toclean <- df_toclean[rowSums(is.na(select(df_toclean, as.character(years)))) != ncol(select(df_toclean, as.character(years))), ] # drop all rows where all NA
      # drop countries for which there is less than 2 datapoints in the expenditure dataset
      IndexMat <- sapply(select(df_toclean, -c(iso)), is.na)
      df_toclean <- df_toclean %>% subset(rowSums(!IndexMat) > 2)
      # pivot to long
      # convert to long format (for regression)
      df_clean <- df_toclean %>%
        pivot_longer(-c(iso, grp), names_to = "year", values_to = newvar)
      df_clean$year <- as.numeric(df_clean$year)
      return(df_clean)
    }

    education_completion_prim <- cleanandlong(education_completion_prim)
    education_completion_ls <- cleanandlong(education_completion_ls)

    ### predict values and return baseyear
    predictNAs <- function(df_tofill, var = "completionrate", method = "linear") {
      # do regressions per country
      if (var == "completionrate") {
        if (method == "linear") {
          # find intercept and linear regression coefficient
          regressions_lin <- lme4::lmList(completionrate ~ year | iso, df_tofill)
        }
        if (method == "log") {
          # find intercept and logarithmic regression coefficient
          regressions_log <- lme4::lmList(completionrate ~ log(as.numeric(Year)) | iso, df_tofill)
        }
        # predict values for the full dataset
        df <- df_tofill %>% select(-completionrate)
        df$completionratepred <- predict(regressions_lin, newdata = df)

        # merge the predicted values if NA, keep the reported values
        df_filled <- left_join(df_tofill, df) %>%
          mutate(completion_rate = coalesce(completionrate, completionratepred)) %>%
          select(-c(completionrate, completionratepred))
      }

      return(filter(df_filled, year == year.base))
    }
    education_completion_prim <- predictNAs(education_completion_prim)
    education_completion_ls <- predictNAs(education_completion_ls)
  }

  compl.rate <- left_join(education_completion_prim, education_completion_ls, by = "iso") %>%
    select(iso, completion_rate.x, completion_rate.y) %>%
    mutate(gap = pmax(0, 100 - pmax(completion_rate.x, completion_rate.y)) / 100) %>%
    select(iso, gap)

  return(compl.rate)
}

get_housing_gap <- function(data.path) {
  fname_slum <- "/API_EN.POP.SLUM.UR.ZS_DS2_en_excel_v2_893267.xls" ## slum population (% of urban)
  fname_perm_rur <- "/data_perm_wall_rur.csv" ## pop share with permanent wall (% of rural)
  # fname_perm_urb <- "/data_perm_wall_rur.csv" ## pop share with permanent wall (% of urban)
  fname_pov <- "/API_SI.POV.UMIC.GP_DS2_en_excel_v2_1002313.xls" ## Data: poverty gap below 5.5$ (WB)
  fname_gdp <- "/API_NY.GDP.PCAP.PP.KD_DS2_en_excel_v2_887670.xls" ## Data: GDP 2011 PPP (WB)

  year.base <- 2015 # year for data to be loaded
  year.slum <- 2014 # year for the slum data (different to ensure data availability)

  print("Load data: housing")
  # Load data: slum population (% pop)
  slum <- read_excel(paste0(data.path, "/Housing", fname_slum), sheet = "Data", skip = 3, col_names = TRUE)
  slum <- slum %>%
    select_at(c("Country Code", paste(year.slum))) %>% # Extract data for the base year
    rename_at(paste(year.slum), list(~ paste("slum"))) %>% # Rename column
    mutate(slum = slum / 100) %>% # Convert % numbers to range 0:1
    rename(iso = "Country Code")

  # Load data: permanent walls in rural (% pop)
  perm_rur <- read.csv(paste0(data.path, "/Housing", fname_perm_rur),
    stringsAsFactors = FALSE,
    header = T
  )
  perm_rur <- perm_rur %>% rename(perm_rur = share_perm)

  # Load data: Poverty gap
  pov <- read_excel(paste0(data.path, "/Housing", fname_pov), sheet = "Data", skip = 3, col_names = TRUE)
  pov <- pov %>%
    select_at(c("Country Code", paste(year.base))) %>% # Extract data for the base year
    rename_at(paste(year.base), list(~ paste("pov"))) %>%
    rename(iso = "Country Code")

  # Join data
  housing <- message.R11 %>%
    select(iso, R11.region) %>%
    left_join(slum, by = "iso") %>% # Merge data: slum pop
    left_join(perm_rur, by = "iso") %>% # Permanent walls - rural
    # left_join(perm_urb, by = "iso") %>% # Permanent walls - urban
    # left_join(gdp, by = "iso") %>% # GDP
    left_join(pov, by = "iso") # Poverty

  # Fit a regression model (Urban)
  print("Regression model: fitting")
  lm_hous_urb <- lm(slum ~ pov, data = housing)
  print(summary(lm_hous_urb))

  # Fit a regression model (Rural)
  print("Regression model: fitting")
  lm_hous_rur <- lm(perm_rur ~ pov, data = housing)
  print(summary(lm_hous_rur))

  # Extrapolate results
  print("Start results extrapolation")
  housing_extr <- housing # initialize
  housing_extr <- housing_extr %>%
    # urban gap
    mutate(gap_urb_pred = predict(lm_hous_urb, housing_extr)) %>% # predicted results
    mutate(gap_urb_extr = gap_urb_pred) %>% # copy predicted results into a new column for extrapolated results
    mutate_cond(!is.na(slum), gap_urb_extr = slum) %>% # copy original data, where available
    # rural gap
    mutate(gap_rur_pred = 1 - predict(lm_hous_rur, housing_extr)) %>% # predicted results
    mutate(gap_rur_extr = gap_rur_pred) %>% # copy predicted results into a new column for extrapolated results
    mutate_cond(!is.na(perm_rur), gap_rur_extr = 1 - perm_rur) %>% # copy original data, where available

    select(iso, R11.region, gap_urb_extr, gap_rur_extr) %>% # Keep only iso and extrapolation results
    rename(urban = gap_urb_extr) %>%
    rename(rural = gap_rur_extr) %>%
    gather(key = "grp", value = "gap_perc", c(urban, rural)) %>%
    # impose no gaps for developed countries
    mutate_cond(R11.region %in% c("NAM", "PAO", "WEU", "EEU"), gap_perc = 0)

  # Calculate average gaps by R11.region and urban/rural
  housing_reg <- housing_extr %>%
    group_by(R11.region, grp) %>%
    summarise(gap_perc = mean(na.omit(gap_perc))) %>%
    rename(gap_reg_avg = gap_perc)

  # Fill in data gaps
  housing_extr <- housing_extr %>%
    left_join(housing_reg, by = c("R11.region", "grp")) %>%
    mutate_cond(is.na(gap_perc), gap_perc = gap_reg_avg) %>%
    select(-gap_reg_avg)

  # combine urban and rural gaps to get an average gap per capita
  housing_extr <- housing_extr %>%
    filter(grp == "urban" | grp == "rural") %>%
    pivot_wider(names_from = "grp", values_from = "gap_perc") %>%
    left_join(pop %>% filter(year == 2015) %>% select(iso, urb.rate)) %>%
    mutate(gap = (urban * urb.rate) + (rural * (1 - urb.rate))) %>%
    select(iso, gap)

  return(housing_extr)
}

get_gaps <- function(data.path, out.path) {
  gaps.dls <- vroom(file = file.path(out.path, "DLS_all_gaps.csv")) # input file - get it after running a scenario once.
  gaps <- gaps.dls %>% filter(!is.na(gap)) # remove construction dimensions


  gaps.transport <- get_mobility_gap(data.path) %>%
    mutate(dim = "Transport") %>%
    mutate(indicator = "% of population below threshold") %>%
    rename(gap = share.pop)
  gaps.appliance.cookstove <- gaps %>%
    filter(grp == "clean_cooking_fuel") %>%
    mutate(gap = ifelse(iso == "CHN",
      ifelse(
        grp == "clean_cooking_fuel", 0.41,
        gap
      ),
      gap
    )) %>%
    group_by(iso, dim, indicator) %>%
    summarise(gap = max(gap)) %>%
    mutate(dim = "Clean cooking") %>%
    mutate(indicator = "% of population without access")
  gaps.appliance.fridge <- gaps %>%
    filter(grp == "refrigerator") %>%
    mutate(gap = ifelse(iso == "CHN",
      ifelse(
        grp == "refrigerator", 0,
        gap
      ),
      gap
    )) %>%
    group_by(iso, dim, indicator) %>%
    summarise(gap = max(gap)) %>%
    mutate(dim = "Cold storage") %>%
    mutate(indicator = "% of population without access")
  gaps.appliance.television <- gaps %>%
    filter(grp == "television") %>%
    mutate(gap = ifelse(iso == "CHN",
      ifelse(
        grp == "television", 0,
        gap
      ),
      gap
    )) %>%
    group_by(iso, dim, indicator) %>%
    summarise(gap = max(gap)) %>%
    mutate(dim = "Television") %>%
    mutate(indicator = "% of population without access")
  gaps.appliance.telephone <- gaps %>%
    filter(grp == "mobile_telephone") %>%
    mutate(gap = ifelse(iso == "CHN",
      ifelse(
        grp == "mobile_telephone", 0,
        gap
      ),
      gap
    )) %>%
    group_by(iso, dim, indicator) %>%
    summarise(gap = max(gap)) %>%
    mutate(dim = "Mobile telephone") %>%
    mutate(indicator = "% of population without access")
  gaps.water <- get_water_gap(data.path) %>%
    mutate(dim = "Water access") %>%
    mutate(indicator = "% of population below threshold") %>%
    mutate(gap = 1 - water_access) %>%
    select(iso, dim, indicator, gap)
  gaps.sanit <- get_sanit_gap(data.path) %>%
    mutate(dim = "Sanitation") %>%
    mutate(indicator = "% of population below threshold") %>%
    mutate(gap = 1 - sanit_access) %>%
    select(iso, dim, indicator, gap)
  gaps.nutrition <- get_nutri_gap(data.path) %>%
    mutate(dim = "Nutrition") %>%
    mutate(indicator = "% of population below threshold") %>%
    rename(gap = share.pop) %>%
    select(iso, dim, indicator, gap)
  # gaps.clothing
  # gaps.health
  gaps.education <- message.R11 %>%
    select(iso) %>%
    left_join(get_edu_gap(data.path)) %>%
    mutate(dim = "Education") %>%
    mutate(indicator = "% of population without 9-yr education") %>%
    select(iso, dim, indicator, gap) %>%
    mutate(gap = ifelse(
      iso == "USA" | iso == "CAN", 0,
      ifelse(
        iso == "AUS" | iso == "JPN" | iso == "NZL", 0,
        gap
      )
    ))
  gaps.housing <- get_housing_gap(data.path) %>%
    mutate(dim = "Housing") %>%
    mutate(indicator = "% of population without decent housing") %>%
    select(iso, dim, indicator, gap)
  gaps.cooling <- gaps %>% # combine urban and rural gaps to get an average gap per capita
    filter(dim == "Cooling CON") %>%
    pivot_wider(names_from = "grp", values_from = "gap") %>%
    left_join(pop %>% filter(year == 2015) %>% select(iso, urb.rate)) %>%
    mutate(gap = (urban * urb.rate) + (rural * (1 - urb.rate))) %>%
    mutate(dim = "Cooling") %>%
    mutate(indicator = "% of population below threshold") %>%
    select(iso, gap, dim, indicator)
  gaps.heating <- gaps %>% # combine urban and rural gaps to get an average gap per capita
    filter(dim == "Heating CON") %>%
    pivot_wider(names_from = "grp", values_from = "gap") %>%
    left_join(pop %>% filter(year == 2015) %>% select(iso, urb.rate)) %>%
    mutate(gap = (urban * urb.rate) + (rural * (1 - urb.rate))) %>%
    mutate(dim = "Heating") %>%
    mutate(indicator = "% of population below threshold") %>%
    select(iso, gap, dim, indicator)
  # gaps.roads
  # gaps.hotwater <- gaps %>% # combine urban and rural gaps to get an average gap per capita
  #   filter(dim=="Hot Water OP") %>%
  #   pivot_wider(names_from="grp", values_from="gap") %>%
  #   left_join(pop %>% filter(year==2015) %>% select(iso,urb.rate)) %>%
  #   mutate(gap=(urban*urb.rate)+(rural*(1-urb.rate))) %>%
  #   mutate(dim="Hot Water") %>%
  #   mutate(indicator="% of population below threshold")%>%
  #   select(iso,gap,dim,indicator)

  gaps.all <- bind_rows(list(
    gaps.transport %>% mutate(need = "Social", group = "Mobility"),
    gaps.appliance.cookstove %>% mutate(need = "Physical", group = "Nutrition"),
    gaps.appliance.fridge %>% mutate(need = "Physical", group = "Nutrition"),
    gaps.appliance.television %>% mutate(need = "Social", group = "Socialization"),
    gaps.appliance.telephone %>% mutate(need = "Social", group = "Socialization"),
    gaps.water %>% mutate(need = "Physical", group = "Health"),
    gaps.sanit %>% mutate(need = "Physical", group = "Health"),
    gaps.nutrition %>% mutate(need = "Physical", group = "Nutrition"),
    gaps.education %>% mutate(need = "Social", group = "Socialization"),
    gaps.housing %>% mutate(need = "Physical", group = "Shelter"),
    gaps.cooling %>% mutate(need = "Physical", group = "Shelter"),
    gaps.heating %>% mutate(need = "Physical", group = "Shelter") # ,
    # gaps.hotwater %>% mutate(need="Physical", group="Health")
  ))
  gaps.all

  return(gaps.all)
}

combine_dls_gaps <- function(data.path, out.path, save.option = c("R11.meanDLS", "R11.DLS", "national.meanDLS", "national.DLS")) {
  # TODO: make this function independent from earlier run to produce "DLS_all_gaps.csv"
  dls.depr <- get_gaps(data.path, out.path)

  if ("national.DLS" %in% save.option) {
    write_delim(dls.depr,
      file = file.path(out.path, "DLS_deprivations.csv"),
      delim = ","
    )
  }
  if ("national.meanDLS" %in% save.option) {
    dls.depr.phys.soc <- dls.depr %>%
      group_by(iso, need) %>%
      summarise(gap = mean(gap, na.rm = TRUE))
    dls.depr.phys.soc.agg <- dls.depr %>%
      group_by(iso, need) %>%
      summarise(gap = mean(gap, na.rm = TRUE)) %>%
      group_by(iso) %>%
      summarise(gap = mean(gap, na.rm = TRUE))
    dls.depr.index <- dls.depr.phys.soc.agg %>%
      rename(mean.gap = gap)

    write_delim(dls.depr.index,
      file = file.path(out.path, "DLS_deprivation_index.csv"),
      delim = ","
    )
  }
  if ("R11.DLS" %in% save.option) {
    dls.depr.r11 <- dls.depr %>%
      left_join(message.R11 %>% select(iso, R11.region)) %>%
      left_join(pop %>% filter(year == year.base) %>% select(iso, population)) %>%
      group_by(R11.region, dim) %>%
      summarise(
        gap = weighted.mean(gap, population, na.rm = TRUE)
      )

    write_delim(dls.depr.r11,
      file = file.path(out.path, "DLS_deprivations_R11.csv"),
      delim = ","
    )
  }
  if ("R11.meanDLS" %in% save.option) {
    dls.depr.phys.soc <- dls.depr %>%
      group_by(iso, need) %>%
      summarise(gap = mean(gap, na.rm = TRUE))
    dls.depr.phys.soc.agg <- dls.depr %>%
      group_by(iso, need) %>%
      summarise(gap = mean(gap, na.rm = TRUE)) %>%
      group_by(iso) %>%
      summarise(gap = mean(gap, na.rm = TRUE))

    dls.depr.r11 <- dls.depr.phys.soc.agg %>%
      left_join(message.R11 %>% select(iso, R11.region)) %>%
      left_join(pop %>% filter(year == year.base) %>% select(iso, population)) %>%
      group_by(R11.region) %>%
      summarise(
        gap = weighted.mean(gap, population, na.rm = TRUE)
      )

    write_delim(dls.depr.r11,
      file = file.path(out.path, "DLS_deprivation_index_R11.csv"),
      delim = ","
    )
  }
}



# Running scenarios ====

# Running scenarios: basic scenario steps ====
run_housing <- function(scen = scen) {
  # do housing separately, ad-hoc solution

  # Initiate an inherited, specific dimension object
  DLE.housing <<- DLE.dimension.housing(
    name_dim = "Housing",
    indicator = "m2/cap",
    unit_rollout = "cap",
    grp = c("rural", "urban") # ,
    # df.input = data.frame(expenditure=3) # Some custom inputs to the dimension (placeholder)
  )

  DLE.housing$DeriveThreshold()
  DLE.housing$IdentifyGap()
  DLE.housing$DeriveEnergyIntensity()
  DLE.housing$ConstructRolloutScenario(scen)
  DLE.housing$UpdateDLE()

  if (exists("SAVE.DLE.HOUSING.STOCK")) {
    if (SAVE.DLE.HOUSING.STOCK == TRUE) {
      housing.stock.timeseries <- DLE.housing$DLE.tot %>%
        ungroup() %>%
        select(iso, grp, year, stock_new_pcap, stock_old_pcap) %>%
        distinct()

      # check that there's no duplicates or extra information not captured in the selected columns above
      if (
        !(nrow(housing.stock.timeseries) == nrow(DLE.housing$DLE.tot %>% ungroup() %>% select(iso, grp, year) %>% distinct()))
      ) {
        stop("Housing stock file that you are saving contains non-unique data. There is more information than is captured in the file.")
      } else {
        write_delim(
          x = housing.stock.timeseries,
          file = file.path(out.path, paste0(
            ssp, "_",
            as.character(year.target),
            # "_lct", as.character(lct)), # N.B.: this will lead to a slight naming inconsistency, because others will have lctTRUE/lctFALSE, but comes with the benefit of not having two files with the same information (housing stock is not affected by transport mode convergence).
            "_housingstock.csv"
          )),
          delim = ","
        )
      }
    }
  }
}


run_accel_scenario <- function(ssp = "SSP2", year.target = year.target, lct = FALSE) {
  # Construct a normative target scenario (ACCEL)
  load_dimensions()

  # Build a list of dimensions
  dim.list <<- generate_all_dimensions()

  # Initiate a scenario object - thres/gap/E.int are taken care of here.
  DLE.ACCEL <<- DLE.scenario(scenario.name = "ACCEL", dims = dim.list, year.tgt.scen = year.target, lct = lct)

  # Create the gap df from all the dims
  DLE.ACCEL$CollectAllGap()

  # do housing
  run_housing(scen = "ACCEL")

  # roll out scenario
  DLE.ACCEL$SetupRollout(lct = lct)

  # get energy intensities
  DLE.ACCEL$CallDeriveEnergyIntensity()

  # do aggregates.
  DLE.ACCEL$SumDLE()
  DLE.ACCEL$AggregateRegions()
  DLE.ACCEL$AggregateDimensions()
  DLE.ACCEL$PlotDLEpcap_ByRegByDim()
  DLE.ACCEL$PlotDLEpcap_ByRegByNeed()
  DLE.ACCEL$PlotDLEpcap_GlobalByDim()

  # we use global DLE.ACCEL here instead of return because we use it as a global object throughout all steps.
}

run_gap_closing_proxy_scenario <- function(ssp, pov.thres) {
  # Construct an poverty income gap closing scenario (Income)

  load_dimensions()

  # Build a list of dimensions
  dim.list <<- generate_all_dimensions()

  # Initiate a scenario object - thres/gap/E.int are taken care of here.
  DLE.income <<- DLE.scenario(scenario.name = "Income", dims = dim.list, year.tgt.scen = Inf)

  # Create the gap df from all the dims
  DLE.income$CollectAllGap()

  # do housing
  run_housing(scen = "Income")

  # roll out scenario
  DLE.income$SetupRollout() # lct would need a target year - for transport, where it's forced to be set to Inf now

  # get energy intensities
  DLE.income$CallDeriveEnergyIntensity()


  # do aggregates.
  DLE.income$SumDLE()
  DLE.income$AggregateRegions()
  DLE.income$AggregateDimensions()
  DLE.income$PlotDLEpcap_ByRegByDim()
  DLE.income$PlotDLEpcap_ByRegByNeed()
  DLE.income$PlotDLEpcap_GlobalByDim()

  # we use global DLE.ACCEL here instead of return because we use it as a global object throughout all steps.
}

run_income_crosssection_regression_scenario <- function(ssp, income.indicator) {
  # Construct a GDP regression scenario (Income.regression)

  load_dimensions()

  # Build a list of dimensions
  dim.list <<- generate_all_dimensions()

  # Initiate a scenario object - thres/gap/E.int are taken care of here.
  DLE.income.regression <<- DLE.scenario(scenario.name = "Income.regression", dims = dim.list, year.tgt.scen = Inf) # scenario name, used later in ConstructRolloutScenario

  # Create the gap df from all the dims
  DLE.income.regression$CollectAllGap()

  # do housing
  run_housing(scen = "Income.regression") # Called first to prevent thermal comfort (and later hot water) depending on this while not called yet.

  # roll out scenario
  DLE.income.regression$SetupRollout(lct = lct)

  # get energy intensities
  DLE.income.regression$CallDeriveEnergyIntensity()


  # do aggregates.
  DLE.income.regression$SumDLE()
  DLE.income.regression$AggregateRegions()
  DLE.income.regression$AggregateDimensions()
  DLE.income.regression$PlotDLEpcap_ByRegByDim()
  DLE.income.regression$PlotDLEpcap_ByRegByNeed()
  DLE.income.regression$PlotDLEpcap_GlobalByDim()

  # we use global DLE.ACCEL here instead of return because we use it as a global object throughout all steps.
}

# Running scenarios: scenario wrappers for easier use ====
run_accel_scenario_wrapper <- function(ssp = "SSP2", year.target = 2040, lct = FALSE, save.option = c("R11.csvneeds")) {
  # run scenario
  if (SUPPRESS.DLE.PRINTLOG) {
    quiet(
      run_accel_scenario(ssp, year.target, lct)
    )
  } else {
    run_accel_scenario(ssp, year.target, lct)
  }

  # scenario results by dimensions - national
  out.df.national <- DLE.ACCEL$DLE.alldims
  # scenario results by dimensions - R11
  out.df.r11 <- DLE.ACCEL$DLE.alldims.agg
  # scenario results aggregated by needs group - R11
  out.df.r11.need.aggregation <- DLE.ACCEL$DLE.group.agg

  # save.options (style; regional_level.save_format): c("national.csvdims", "R11.csvdims", "R11.csvneeds", "R11.htmldims", "R11.htmlneeds", "global.htmlneeds")
  save_options_wrapper(
    df.national = out.df.national,
    df.r11 = out.df.r11,
    df.r11.need.aggregation = out.df.r11.need.aggregation,
    save.option = save.option,
    save.string = paste0(ssp, "_", as.character(year.target), "_lct", as.character(lct))
  )
}

run_income_scenario_wrapper <- function(ssp = "SSP2", pov.thres = 10, save.option = c("R11.csvneeds")) {
  # run scenario
  run_gap_closing_proxy_scenario(ssp, pov.thres)


  # scenario results by dimensions - national
  out.df.national <- DLE.income$DLE.alldims
  # scenario results by dimensions - R11
  out.df.r11 <- DLE.income$DLE.alldims.agg
  # scenario results aggregated by needs group - R11
  out.df.r11.need.aggregation <- DLE.income$DLE.group.agg


  # save.options (style; regional_level.save_format): c("national.csvdims", "R11.csvdims", "R11.csvneeds", "R11.htmldims", "R11.htmlneeds", "global.htmlneeds")
  save_options_wrapper(
    df.national = out.df.national,
    df.r11 = out.df.r11,
    df.r11.need.aggregation = out.df.r11.need.aggregation,
    save.option = save.option,
    save.string = paste0(ssp, "_", as.character(pov.thres))
  )
}


run_income_regression_scenario_wrapper <- function(ssp = "SSP2", income.indicator = "log.gdp", save.option = c("R11.csvneeds")) {
  run_income_crosssection_regression_scenario(ssp, income.indicator)

  # scenario results by dimensions - national
  out.df.national <- DLE.income.regression$DLE.alldims
  # scenario results by dimensions - R11
  out.df.r11 <- DLE.income.regression$DLE.alldims.agg
  # scenario results aggregated by needs group - R11
  out.df.r11.need.aggregation <- DLE.income.regression$DLE.group.agg

  # save.options (style; regional_level.save_format): c("national.csvdims", "R11.csvdims", "R11.csvneeds", "R11.htmldims", "R11.htmlneeds", "global.htmlneeds")
  save_options_wrapper(
    df.national = out.df.national,
    df.r11 = out.df.r11,
    df.r11.need.aggregation = out.df.r11.need.aggregation,
    save.option = save.option,
    save.string = paste0(ssp, "_regression_", as.character(income.indicator))
  )
}




# Saving results ====

save_scenario_to_html_timeseries <- function(df, filename = "SSP2_2030", needs.aggregation = F, regional.level = "R11") {
  type.remove <- "all"

  if (regional.level == "global") {
    if (needs.aggregation) {
      # global picture:
      df.world.pc <- df %>%
        filter(elec != "total") %>%
        ungroup() %>%
        select(type, elec, year, group, region, DLE) %>%
        filter(type != type.remove) %>%
        group_by(year, group) %>%
        summarise(val = sum(DLE, na.rm = TRUE))

      p.need.world <- ggplot(
        data = df.world.pc,
        aes(
          x = year,
          y = val,
          fill = group
        )
      ) +
        geom_area() +
        labs(
          title = "Total decent living energy per capita, by need group",
          y = "EJ/year"
        )

      phtml.need.world <- plotly::ggplotly(p.need.world)
      htmlwidgets::saveWidget(phtml.need.world, paste0(out.path, "/", filename, "_Globaldle_need.html"))
    } else {
      message("Not implemented yet ...")
    }
  } else if (regional.level == "R11") {
    if (needs.aggregation) {
      df.region.pc <- df %>%
        filter(elec != "total") %>%
        ungroup() %>%
        select(type, elec, year, group, region, DLE) %>%
        filter(type != type.remove) %>%
        group_by(year, group, region) %>%
        summarise(val = sum(DLE, na.rm = TRUE)) %>%
        left_join(R11.pop %>% rename(region = R11.region)) %>%
        mutate(val = val / population * exa / giga) %>%
        select(-population)

      p.need.region <- ggplot(
        data = df.region.pc,
        aes(
          x = year,
          y = val,
          fill = group
        )
      ) +
        geom_area() +
        facet_wrap(~region) +
        labs(
          title = "Total decent living energy per capita, by need group",
          y = "GJ/cap/year"
        )

      phtml.need.region <- plotly::ggplotly(p.need.region)
      htmlwidgets::saveWidget(phtml.need.region, paste0(out.path, "/", filename, "_R11dle_need.html"))
    } else {
      df.grp.region.pc <- df %>%
        filter(elec != "total") %>%
        ungroup() %>%
        select(type, elec, year, dimension, region, DLE) %>%
        filter(type != type.remove) %>%
        group_by(year, dimension, region) %>%
        summarise(val = sum(DLE, na.rm = TRUE)) %>%
        left_join(R11.pop %>% rename(region = R11.region)) %>%
        mutate(val = val / population * exa / giga) %>%
        select(-population)

      p.grp.region <- ggplot(
        data = df.grp.region.pc,
        aes(
          x = year,
          y = val,
          fill = dimension
        )
      ) +
        geom_area() +
        facet_wrap(~region) +
        labs(
          title = "Total decent living energy per capita",
          y = "GJ/cap/year"
        )

      phtml.grp.region <- plotly::ggplotly(p.grp.region)
      htmlwidgets::saveWidget(phtml.grp.region, paste0(out.path, "/", filename, "_R11dle.html"))
    }
  }
}

save_options_wrapper <- function(df.national, df.r11, df.r11.need.aggregation, save.option, save.string) {
  if ("national.csvdims" %in% save.option) {
    write_delim(
      x = df.national,
      file = file.path(out.path, paste0(save.string, "_country.csv")),
      delim = ","
    )
  }
  if ("R11.csvdims" %in% save.option) {
    write_delim(
      x = df.r11,
      file = file.path(out.path, paste0(save.string, "_R11.csv")),
      delim = ","
    )
  }
  if ("R11.csvneeds" %in% save.option) {
    write_delim(
      x = df.r11.need.aggregation,
      file = file.path(out.path, paste0(save.string, "_R11need.csv")),
      delim = ","
    )
  }
  if ("R11.htmldims" %in% save.option) {
    save_scenario_to_html_timeseries(df.r11,
      filename = save.string,
      needs.aggregation = F
    )
  }
  if ("R11.htmlneeds" %in% save.option) {
    save_scenario_to_html_timeseries(df.r11.need.aggregation,
      filename = save.string,
      needs.aggregation = T
    )
  }
  if ("global.htmlneeds" %in% save.option) {
    save_scenario_to_html_timeseries(df.r11.need.aggregation,
      filename = save.string,
      needs.aggregation = T,
      regional.level = "global"
    )
  }
}


# |||| ---------------------------------
# Decent Living Energy utils -------
# |||| ---------------------------------



calculate_price_index_change <- function() {
  # options
  # - d.fromto
  #   * ppp2mer
  #   * mer2ppp
  # - year.from
  # - year.to

  # yet to be implemented;
  # for at minimum DLE_clothing, DLE_education, DLE_health.



  return()
}

# |||| ---------------------------------
# Decent Living Calculator utils -------
# |||| ---------------------------------

# Decent Living Calculator utils: when ssp variant is in the scenario name, add a column that returns the ssp variant
add_ssp_mapping <- function(df) {
  return(
    df %>%
      mutate(scenario.mapping = ifelse(grepl("SSP1", x = scenario, fixed = T), "SSP1",
        ifelse(grepl("SSP2", x = scenario, fixed = T), "SSP2",
          ifelse(grepl("SSP3", x = scenario, fixed = T), "SSP3", NA)
        )
      ))
  )
}

# Decent Living Calculator utils: preparing input data -------
format_final_energy_downscaled_cdlinksmessage <- function(df, include.aggregation.check = IAM.SCENARIO.DATA.AGGREGATION.CHECK) {
  df.out.temp <- df %>%
    rename(model = MODEL, scenario = SCENARIO, iso = ISO, variable = VARIABLE, unit = UNIT) %>%
    filter(grepl(x = variable, pattern = "Final Energy", fixed = T)) %>%
    pivot_longer(
      !!as.symbol(IAM.SCENARIO.DATA.STARTYEAR):!!as.symbol(IAM.SCENARIO.DATA.ENDYEAR),
      names_to = "year",
      values_to = "value"
    ) %>%
    select(model, scenario, iso, variable, unit, year, value) %>% # in case there's years outside the range of interest

    # ad-hoc edit 01.02.21: seems like NA is sometimes reported where we can estimate this to be zero.
    mutate(value = ifelse(is.na(value), 0, value)) %>%
    mutate(year = as.numeric(year)) %>%
    # # ad-hoc edit 01.02.21: since sectoral aggregate baskets for final demand seem to not be reported, let's manually sum over subsidiaries
    # mutate(agg.var = ifelse(
    #   variable %in% c("Final Energy|Industry|Electricity", "Final Energy|Industry|Gases", "Final Energy|Industry|Heat", "Final Energy|Industry|Hydrogen", "Final Energy|Industry|Liquids", "Final Energy|Industry|Solids"), "Final Energy|Industry", ifelse(
    #     variable %in% c("Final Energy|Transportation|Electricity", "Final Energy|Transportation|Hydrogen", "Final Energy|Transportation|Liquids", "Final Energy|Transportation|Gases"), "Final Energy|Transportation", ifelse(
    #       variable %in% c("Final Energy|Residential and Commercial|Electricity", "Final Energy|Residential and Commercial|Gases", "Final Energy|Residential and Commercial|Heat", "Final Energy|Residential and Commercial|Liquids", "Final Energy|Residential and Commercial|Solids"), "Final Energy|Residential and Commercial",
    #       ifelse(
    #         variable == "Final Energy",
    #         "Final Energy",
    #         NA
    #       )
    #     )
    #   )
    # )) %>%
    mutate(agg.var = ifelse(
      variable %in% c("Final Energy|Industry"), "Final Energy|Industry", ifelse(
        variable %in% c("Final Energy|Transportation"), "Final Energy|Transportation", ifelse(
          variable %in% c("Final Energy|Residential and Commercial"), "Final Energy|Residential and Commercial",
          ifelse(
            variable == "Final Energy",
            "Final Energy",
            NA
          )
        )
      )
    )) %>%
    filter(!is.na(agg.var)) %>% # drop final energy variables that are not needed for the sectoral aggregates

    group_by(model, scenario, iso, unit, year, agg.var) %>%
    summarise(
      value = sum(value)
    )



  if (include.aggregation.check) {
    # do aggregation check
    df.out.temp.diff <-
      df.out.temp %>%
      pivot_wider(names_from = agg.var, values_from = value) %>%
      mutate(difference.sectors.with.total = `Final Energy` - (`Final Energy|Industry` + `Final Energy|Transportation` + `Final Energy|Residential and Commercial`)) %>%
      mutate(difference.sectors.with.total.missing.share.percentage = (`Final Energy` - (`Final Energy|Industry` + `Final Energy|Transportation` + `Final Energy|Residential and Commercial`)) / `Final Energy` * 100) %>%
      ungroup() %>%
      group_by(scenario, iso) %>%
      summarise(
        max.rel.absdiff.country = max(abs(difference.sectors.with.total.missing.share.percentage))
      ) %>%
      ungroup() %>%
      mutate(rel.diff.too.high = ifelse(max.rel.absdiff.country < 20, 0, 1))

    print("Excluding the following number of countries per scenario because they fail a sectoral aggregation check:")
    print(df.out.temp.diff %>% filter(rel.diff.too.high == 1) %>% group_by(scenario) %>% summarise(sum(rel.diff.too.high)))

    # write_delim(
    #   df.out.temp %>%
    #     left_join(
    #       df.out.temp.diff %>% select(scenario, iso, rel.diff.too.high)
    #     ) %>%
    #     filter(rel.diff.too.high == 1) %>%
    #     select(-rel.diff.too.high) %>%
    #     select(
    #       model, scenario, iso, agg.var, unit, year, value
    #     ),
    #   file = here("data", "failing-country-scenario-FE-aggregation-check.csv"),
    #   delim = ","
    # )

    df.out <- df.out.temp %>%
      left_join(
        df.out.temp.diff %>% select(scenario, iso, rel.diff.too.high)
      ) %>%
      filter(rel.diff.too.high == 0) %>%
      select(-rel.diff.too.high) %>%
      select(
        model, scenario, iso, agg.var, unit, year, value
      )

    # print("Remaining number of countries per scenario:")
    # print(df.out %>% ungroup() %>% distinct(scenario, iso) %>% group_by(scenario) %>% count())
  } else {
    df.out <- df.out.temp %>%
      select(
        model, scenario, iso, agg.var, unit, year, value
      )
  }

  return(df.out)
}

format_fetoue_ratio_regional_cdlinksmessage <- function(df, include.aggregation.check = ADDITIONAL.REGIONAL.IAM.SCENARIO.DATA.AGGREGATION.CHECK) {
  vars.ue.industry <- c(
    "Useful Energy|Feedstocks", # industry
    "Useful Energy|Industrial Specific", # industry
    "Useful Energy|Industrial Thermal" # industry
  )
  vars.ue.rc <- c(
    "Useful Energy|RC Specific", # residential and commercial
    "Useful Energy|RC Thermal" # residential and commercial
  )
  vars.ue.transport <- c(
    "Useful Energy|Shipping", # transportation
    "Useful Energy|Transport" # transportation
  )
  # "Useful Energy|Non-Commercial Biomass", # NA #TODO: allocate somewhere? is this household biomass burning?

  df.out.temp.1 <- df %>%
    rename(model = MODEL, scenario = SCENARIO, region.message = REGION, variable = VARIABLE, unit = UNIT) %>%
    pivot_longer(
      !!as.symbol(IAM.SCENARIO.DATA.STARTYEAR):!!as.symbol(IAM.SCENARIO.DATA.ENDYEAR),
      names_to = "year",
      values_to = "value"
    ) %>%
    select(model, scenario, region.message, variable, unit, year, value) %>% # in case there's years outside the range of interest

    mutate(year = as.numeric(year))


  df.out.temp.2 <- df.out.temp.1 %>%
    mutate(
      agg.var =
        ifelse(
          variable %in% vars.ue.transport, "Useful Energy|Transportation",
          ifelse(
            variable %in% vars.ue.rc, "Useful Energy|Residential and Commercial",
            ifelse(
              variable %in% vars.ue.industry, "Useful Energy|Industry",
              ifelse(
                grepl(x = variable, pattern = "Final Energy", fixed = T),
                variable,
                NA
              )
            )
          )
        )
    ) %>%
    # sum over useful energy sectors
    group_by(model, scenario, region.message, agg.var, unit, year) %>%
    summarise(value = sum(value)) %>%
    # calculate total useful energy (total final already reported, but not total useful)
    pivot_wider(names_from = agg.var, values_from = value) %>%
    mutate(
      `Useful Energy` = `Useful Energy|Transportation` + `Useful Energy|Residential and Commercial` + `Useful Energy|Industry`
    ) %>%
    # calculate ratios
    mutate(
      fe.to.ue = `Useful Energy` / `Final Energy`,
      fe.to.ue.transportation = `Useful Energy|Transportation` / `Final Energy|Transportation`,
      fe.to.ue.rescom = `Useful Energy|Residential and Commercial` / `Final Energy|Residential and Commercial`,
      fe.to.ue.industry = `Useful Energy|Industry` / `Final Energy|Industry`
    ) %>%
    select(model, scenario, region.message, unit, year, fe.to.ue, fe.to.ue.transportation, fe.to.ue.rescom, fe.to.ue.industry) %>%
    pivot_longer(cols = fe.to.ue:fe.to.ue.industry, names_to = "variable", values_to = "fe.to.ue.ratio") %>%
    mutate(
      variable =
        ifelse(
          variable == "fe.to.ue.transportation", "Final Energy|Transportation",
          ifelse(
            variable == "fe.to.ue.rescom", "Final Energy|Residential and Commercial",
            ifelse(
              variable == "fe.to.ue.industry", "Final Energy|Industry",
              ifelse(
                variable == "fe.to.ue", "Final Energy",
                NA
              )
            )
          )
        )
    ) %>%
    rename(agg.var = variable)

  # assume same fe.to.ue.ratio for all countries in each messageix region
  df.out.temp.iso <- regions.message %>%
    left_join(
      df.out.temp.2
    ) %>%
    select(-region.message) %>%
    select(
      model, scenario, iso, agg.var, unit, year, fe.to.ue.ratio
    ) %>%
    # align model / scenario naming with the downscaled data
    mutate(scenario = paste0(model, scenario)) %>%
    mutate(model = "MESSAGEix-GLOBIOM 1.0")


  # ggplot(df.out.temp.2,aes(x=year, y=fe.to.ue.ratio, colour=scenario, linetype=model)) +
  #   facet_grid(variable~region.message) +
  #   geom_line() +
  #   theme_bw()




  if (include.aggregation.check) {
    message("include.aggregation.check is not yet implemented!")

    # # do aggregation check
    # df.out.temp.diff <-
    #   df.out.temp %>% pivot_wider(names_from = agg.var, values_from = value) %>%
    #   mutate(difference.sectors.with.total=`Final Energy`-(`Final Energy|Industry`+`Final Energy|Transportation`+`Final Energy|Residential and Commercial`)) %>%
    #   mutate(difference.sectors.with.total.missing.share.percentage=(`Final Energy`-(`Final Energy|Industry`+`Final Energy|Transportation`+`Final Energy|Residential and Commercial`))/`Final Energy`*100) %>%
    #   ungroup() %>% group_by(scenario,iso) %>%
    #   summarise(
    #     max.rel.absdiff.country = max(abs(difference.sectors.with.total.missing.share.percentage))
    #   ) %>%
    #   ungroup() %>%
    #   mutate(rel.diff.too.high = ifelse(max.rel.absdiff.country<20,0,1))
    #
    # print("Excluding the following number of countries per scenario because they fail a sectoral aggregation check:")
    # print(df.out.temp.diff %>% filter(rel.diff.too.high==1) %>% group_by(scenario) %>% summarise(sum(rel.diff.too.high)))
    #
    # df.out <- df.out.temp %>%
    #   left_join(
    #     df.out.temp.diff %>% select(scenario,iso,rel.diff.too.high)
    #   ) %>% filter(rel.diff.too.high==0) %>%
    #   select(-rel.diff.too.high)
    #
    # print("Remaining number of countries per scenario:")
    # print(df.out %>% ungroup() %>% distinct(scenario,iso) %>% group_by(scenario) %>% count())
  } else {
    df.out <- df.out.temp.iso
  }

  return(df.out)
}


# Decent Living Calculator utils: mathematical functions -------

GetGiniToGiniElasticity_new <- function(energy.gini, income.gini.old, income.gini.new, sector = variable) {
  gini_to_gini_data <- readRDS(here("data", "gini_to_gini_data.RData"))

  energy.gini <- energy.gini / 100
  income.gini.old <- income.gini.old / 100
  gini.new <- income.gini.new

  energy.variable <- ifelse(sector == "Final Energy|Transportation",
    "energy.transport",
    ifelse(sector == "Final Energy|Residential and Commercial",
      "energy.rescom",
      ifelse(sector == "Final Energy|Industry",
        "energy.industry", # NB. this is currently assumed to be the same as energy.total!
        ifelse(sector == "Final Energy",
          "energy.total", # TODO: think of consistent way of treating total final energy for elasticities; (a) to check consistency, or (b) just discard and only work on sectoral? or (c) something else?
          NA
        )
      )
    )
  )

  low.income <-
    elasticity.gini <-
    # = delta energy.gini / delta income gini
    (
      # end point (find the closest numerical interpolated value to the actual future income.gini and return the energy.gini at the same level)
      (sectoral.elasticity.gini.to.gini %>% select(
        c(
          "income.total",
          energy.variable
        )
      ) %>% filter(abs(income.total - (gini + gini.diff)) == min(abs(income.total - (gini + gini.diff)))) %>% pull(energy.variable)
      ) -
        # starting point (find the closest numerical interpolated value to the actual current income.gini and return the energy.gini at the same level)
        (sectoral.elasticity.gini.to.gini %>% select(
          c(
            "income.total",
            energy.variable
          )
        ) %>% filter(abs(income.total - gini) == min(abs(income.total - gini))) %>% pull(energy.variable)
        )
    ) /
      gini.diff


  return(
    elasticity.gini
  )
}
GetGiniToGiniElasticity_new <- Vectorize(GetGiniToGiniElasticity_new) # to allow use on tibbles (vectors)

# function too slow? (i suspect due to having to find the minimum difference every time)
# --> maybe make model, do linear fit, and return "predicted" 't' and 't+1' values
GetGiniToGiniElasticity <- function(gini, gini.diff, sector = variable) {
  # this function takes in two values, and calculates the change.
  # NB. due to some funny R magic i don't understand, the passing of a tibble doesn't seem to work so we will just make a global variable now and continue
  # gini.data.file = sectoral.elasticity.gini.to.gini
  if (is.na(gini) | is.na(gini.diff) | is.na(sector)) {
    return(NA)
  }
  if (gini.diff == 0 | (abs(gini.diff) < 0.05)) {
    return(1)
  }

  gini <- gini / 100
  gini.diff <- gini.diff / 100
  energy.variable <- ifelse(sector == "Final Energy|Transportation",
    "energy.transport",
    ifelse(sector == "Final Energy|Residential and Commercial",
      "energy.rescom",
      ifelse(sector == "Final Energy|Industry",
        "energy.industry", # NB. this is currently assumed to be the same as energy.total!
        ifelse(sector == "Final Energy",
          "energy.total", # TODO: think of consistent way of treating total final energy for elasticities; (a) to check consistency, or (b) just discard and only work on sectoral? or (c) something else?
          NA
        )
      )
    )
  )
  elasticity.gini <-
    # = delta energy.gini / delta income gini
    (
      # end point (find the closest numerical interpolated value to the actual future income.gini and return the energy.gini at the same level)
      (sectoral.elasticity.gini.to.gini %>% select(
        c(
          "income.total",
          energy.variable
        )
      ) %>% filter(abs(income.total - (gini + gini.diff)) == min(abs(income.total - (gini + gini.diff)))) %>% pull(energy.variable)
      ) -
        # starting point (find the closest numerical interpolated value to the actual current income.gini and return the energy.gini at the same level)
        (sectoral.elasticity.gini.to.gini %>% select(
          c(
            "income.total",
            energy.variable
          )
        ) %>% filter(abs(income.total - gini) == min(abs(income.total - gini))) %>% pull(energy.variable)
        )
    ) /
      gini.diff

  if (length(elasticity.gini) == 2) {
    # sometimes there can be two minima that are exactly the same, then we take the average
    elasticity.gini <- mean(elasticity.gini)
  } else if (length(elasticity.gini) > 2) {
    stop("something wrong with finding a numerical solution here")
  }

  return(
    elasticity.gini
  )
}
GetGiniToGiniElasticity <- Vectorize(GetGiniToGiniElasticity) # to allow use on tibbles (vectors)

# |||| ---------------------------------
# generic utils ------------------------
# |||| ---------------------------------

quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

# IAM utils ====
filter_wildcard_var <- function(df, variable.string,
                                lowercase.var.variable = F) {
  # NB/TOFIX: this function does not work like pyam in that it does not respect that e.g. "*Emissions|CO2" should end with "|CO2", rather it implicitly treats it as "*Emissions|CO2*"

  split.string <- str_split(string = variable.string, pattern = "\\*", n = Inf, simplify = FALSE)[[1]]
  n.split.string <- length(split.string)

  if (lowercase.var.variable) {
    # TODO: find a way to map this to go beyond 2 * characters
    if (n.split.string > 3) {
      message("Maximum wildcards that can be used in one string search is currently set to 2.")
    } else if (n.split.string == 2) {
      df <- df %>%
        filter(grepl(x = variable, pattern = split.string[1], fixed = T) & grepl(x = variable, pattern = split.string[2], fixed = T))
    } else if (n.split.string == 3) {
      df <- df %>%
        filter(grepl(x = variable, pattern = split.string[1], fixed = T) & grepl(x = variable, pattern = split.string[2], fixed = T) & grepl(x = variable, pattern = split.string[3], fixed = T))
    }
  } else {
    # TODO: find a way to map this to go beyond 2 * characters
    if (n.split.string > 3) {
      message("Maximum wildcards that can be used in one string search is currently set to 2.")
    } else if (n.split.string == 2) {
      df <- df %>%
        filter(grepl(x = Variable, pattern = split.string[1], fixed = T) & grepl(x = Variable, pattern = split.string[2], fixed = T))
    } else if (n.split.string == 3) {
      df <- df %>%
        filter(grepl(x = Variable, pattern = split.string[1], fixed = T) & grepl(x = Variable, pattern = split.string[2], fixed = T) & grepl(x = Variable, pattern = split.string[3], fixed = T))
    }
  }



  return(df)
}


iamc_wide_to_long <- function(df, upper.to.lower = F) {
  # function assumes all five basic IAMC columns are there, and nothing more

  if (upper.to.lower) {
    df <- df %>%
      rename(
        model = Model,
        scenario = Scenario,
        region = Region,
        variable = Variable,
        unit = Unit
      )
  }

  first.year <- colnames(df)[6] # assumes all five basic IAMC columns are there, and nothing more
  last.year <- colnames(df)[length(colnames(df))]

  df <- df %>%
    pivot_longer(
      cols = first.year:last.year,
      names_to = "year",
      values_to = "value"
    ) %>%
    drop_na(value) %>%
    mutate(year = as.numeric(year))

  return(df)
}

load_climate_var <- function(variable, file = CLIMATE.EMULATOR.FILE, key.var = F) {
  # key.var could be used to filter faster from the smaller snapshot with key climate file

  full.climate.data <- vroom(
    here("data-raw", "scenario_data", "climate", file)
  )

  if (grepl(pattern = "\\*", x = variable)) {
    df <- filter_wildcard_var(
      full.climate.data,
      variable.string = variable
    )
  } else {
    df <- full.climate.data %>%
      filter(
        Variable == variable
      )
  }

  return(df %>%
    iamc_wide_to_long(upper.to.lower = T))
}

# Functions for data manipulation ====
# Function for manipulating subsets of data with dplyr
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

# interpolate NA values by year
interpolate_NA_annual <- function(df) {
  return(
    df %>%
      mutate(
        value = na.approx(value, maxgap = Inf, rule = 2)
      )
  )
}

# Code development utils ====
## Function for cleaning up the code in this directory
clean_code_style <- function() {
  library(styler)
  styler::style_dir()
}

# Visualisation utils ====

## Function for desaturating colors by specified proportion
desat <- function(cols, sat = 0.5) {
  X <- diag(c(1, sat, 1)) %*% rgb2hsv(col2rgb(cols))
  hsv(X[1, ], X[2, ], X[3, ])
}
# c.pal <- desat(brewer.pal(n = 12, name = "Set3"), 1.5): example usage, with colours from the brewer.pal function of RColorBrewer


# Distribution utils ====
# Function for deriving Depth of Deficit using the lognormality assumtpion ----
GetDepthofDeficit_lognormal <- function(nu, sigma, thres, ret = "share") {
  if (is.na(nu) | is.na(sigma) | is.na(thres)) {
    return(NA)
  }

  # Typical lognormal distribution to be used for integration
  f <- function(x, nu, sigma) {
    dlnorm(x, meanlog = nu, sdlog = sigma, log = FALSE)
  }
  xf <- function(x, nu, sigma) {
    x * f(x, nu, sigma)
  }

  mean.subthres <- integrate(xf, 0, thres, nu, sigma)
  sh.subthres <- integrate(f, 0, thres, nu, sigma)

  DoD <- thres - mean.subthres$value / sh.subthres$value

  if (ret == "DoD") {
    return(DoD)
  }
  if (ret == "share") {
    return(sh.subthres$value)
  }
}
GetDepthofDeficit_lognormal <- Vectorize(GetDepthofDeficit_lognormal)
# Function for lognormal cumulative distribution function, for depth-of-deficit calculation (methodology: see https://www.econstor.eu/bitstream/10419/173337/1/wp-gut-fme-a-41-Kot.pdf, page 9) ====
GetInverseCDF_lognormal <- function(g) {
  if (is.list(length(sqrt(2) * invcdf(normal(), (g + 1) / 2)[[1]]))) {
    stop("error")
  }

  return(
    sqrt(2) * invcdf(normal(), (g + 1) / 2)[[1]] %>% unlist()
  )
}
GetInverseCDF_lognormal <- Vectorize(GetInverseCDF_lognormal)

# Gini function ====
get_gini <- function(df, var = "fe") { # input on per capita variable
  # get shares
  df.pop.total <- df %>%
    group_by(iso) %>%
    summarise(total.pop = sum(population))
  df <- df %>% mutate(
    var.mul.pop = (!!as.name(var)) * population,
    na.rm = T
  )
  df.var.total <- df %>%
    group_by(iso) %>%
    summarise(total.var = sum(var.mul.pop, na.rm = T))

  df.shares <- df %>%
    left_join(df.pop.total) %>%
    left_join(df.var.total) %>%
    arrange(iso, (!!as.name(var))) %>%
    group_by(iso) %>%
    mutate(
      pop.share = population / total.pop,
      var.share = ifelse(population == 0, 0, var.mul.pop / total.var)
    )

  # get cumulative shares
  df.cum.shares <- df.shares %>%
    arrange(iso, (!!as.name(var))) %>%
    group_by(iso) %>%
    mutate(
      cum.pop.share = cumsum(pop.share),
      cum.var.share = cumsum(var.share)
    ) %>%
    select(iso, cum.pop.share, cum.var.share, pop.share, var.share)

  # check cumulative shares adding up to 1
  # View(df.shares %>% group_by(iso) %>% summarise(pop=sum(pop.share), var=sum(var.share)))


  # get gini
  gini <- df.cum.shares %>%
    group_by(iso) %>%
    mutate(
      area.points.rec = lag(cum.var.share, default = 0) * pop.share,
      area.points.tri = (cum.var.share - lag(cum.var.share, default = 0)) * pop.share * 0.5
    ) %>%
    mutate(area.points = area.points.rec + area.points.tri) %>%
    summarise(
      area = sum(area.points)
    ) %>%
    mutate(
      gini = (0.5 - area) / 0.5
    )

  return(gini %>% select(iso, gini))
}


# Functions for region aggregation ====
load_official_country_grouping <- function(grouping.to.load,
                                           keep.long.names = F) {
  #' Load grouping of ISO (with longer names, which are removed by default)
  #' options for region groupings:
  #' "region_ar6_6_ipcc_fgd"     "region_ar6_10_ipcc_fgd"    "region_ar6_22_ipcc_fgd"    "M49_Hi_M49_Regions"        "M49_Med__M49_Regions"      "M49_lo_M49_Regions"
  #' [9] "Developing_2021_M49_other" "SIDS_M49_other"            "LLDC_M49_other"            "LDC_M49_other"             "Annex_I_unfccc"            "Annex _II_unfccc"          "WMO"                       "EU"
  #' [17] "OECD"                      "Income _status_WB"


  grp <- vroom(
    here("data-raw", "countrygroupings_2022_07.csv")
  ) %>%
    rename(iso = ISO) %>%
    select(
      iso,
      name,
      !!as.symbol(grouping.to.load)
    )

  if (!keep.long.names) {
    grp <- grp %>% select(-name)
  }


  return(grp)
}

# region_aggregate_sum <- function(
#   df,
#   region.col.name,
#   group.cols = c("model", "scenario", "variable", "year")
# ){
#   # only works for one variable at the moment (& column must be named `value`)
#
#   group.cols <- c(group.cols, region.col.name)
#   group.cols <- enquo(group.cols) # need to quote
#
#   grp.df <- df %>%
#     group_by(vars(!!group.cols)) %>%
#     summarise(
#       value = sum(value)
#     )
#
#   return(grp.df)
# }

# Statistics utils =====

add_percentile_columns <- function(df, percentiles = c(0.25, 0.5, 0.75),
                                   group.cols = c("model", "scenario", "variable", "year")) {
  # standard = across countries (iso is left out of group.cols)

  p_names <- map_chr(percentiles, ~ paste0("p", .x * 100))

  p_funs <- map(percentiles, ~ partial(quantile, probs = .x, na.rm = TRUE)) %>%
    set_names(nm = p_names)

  group.cols <- enquo(group.cols) # need to quote

  df.percentiles <- df %>%
    group_by_at(vars(!!group.cols)) %>%
    summarize_at(vars(value), .funs = p_funs)

  return(df %>% left_join(df.percentiles))
}
