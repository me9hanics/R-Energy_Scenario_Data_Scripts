#' Useful functions for the `decent` software package - for DLE calculator

# load_pkgs_runscript <- function(first.time = F, dev = F, plotting = F) {
#   pkgs <<- c(
#     "maps",
#     "rworldmap",
#     "mapproj",
#     "rgdal",
#     "vroom", # for loading in CSV files fast
#     "zoo", # for imputation using `na.approx`
#     "lestat", # for inverse cumulative distribtuion for depth of deficit (note - also loads the MASS function `select`, so tidyverse needs to be loaded later)
#     "countrycode", # for working easily with country codes
#     "readxl", # for reading in excel files
#     "writexl", # for writing out excel files
#     "sitools", # for clear unit changes, required by DLE_integration_data_structure.R
#     "WDI", # World Bank Development indicators package
#     "table1", # for creating a table with descriptive statistics
#     "plotly", # for interactive plots
#     "htmlwidgets", # for saving interactive plots
#     "here", # for specifying relative paths
#     "lme4", # for LmList
#     "tidyverse" # for data wrangling
#   )
#   if (dev) {
#     # add packages only used by code developers
#     pkgs <- c(pkgs, "styler")
#   }
#   if (plotting) {
#     # add packages only used for plotting (in the plotting gallery)
#     pkgs <- c(pkgs,
#               # "see", # for geom_violinhalf
#               # "MethylCapSig", # for multivariate lognormal model prediction and sampling
#               "patchwork", # for multipanel handling
#               "ggsci", # for some nice colour palettes
#               "treemapify", # for making treemaps
#               "scales", # for scaling axes of ggplot objects
#               "rvg", # to make ggplot objects into editable DML objects to write out to powerpoint
#               "officer" # e.g. to write out plots as editable figures in powerpoint
#               )
#   }
#   if (first.time) {
#     install.packages(pkgs)
#   }
#   load <- lapply(pkgs, library, character.only = TRUE)
#   select <- dplyr::select # explicitly say that we mean dplyr's select function whenever we use select (not the one from the MASS library...)
#   filter <- dplyr::filter # explicitly say that we mean dplyr's filter function whenever we use filter (not the one from the stats library...)
#   mutate <- dplyr::mutate # explicitly say that we mean dplyr's mutate function whenever we use mutate
# }
# load_pkgs_runscript(first.time = T, dev = T, plotting = T) # for now, we by default always load packages if utils.R is loaded


# |||| ---------------------------------
# Decent Living Calculator utils -------
# |||| ---------------------------------

# Decent Living Calculator utils: when ssp variant is in the scenario name, add a column that returns the ssp variant
add_ssp_mapping <- function(df, default = "NA") {
  # TODO: consider replacing this by a config / YAML mapping file


  if (default == "NA") {
    df <- df %>%
      mutate(scenario.mapping = ifelse(grepl("SSP1", x = scenario, fixed = T), "SSP1",
        ifelse(grepl("SSP2", x = scenario, fixed = T), "SSP2",
          ifelse(grepl("SSP3", x = scenario, fixed = T), "SSP3",
            ifelse(grepl("SSP4", x = scenario, fixed = T), "SSP4",
              ifelse(grepl("SSP5", x = scenario, fixed = T), "SSP5",
                NA
              )
            )
          )
        )
      ))
  } else {
    df <- df %>%
      mutate(scenario.mapping = ifelse(grepl("SSP1", x = scenario, fixed = T), "SSP1",
        ifelse(grepl("SSP2", x = scenario, fixed = T), "SSP2",
          ifelse(grepl("SSP3", x = scenario, fixed = T), "SSP3",
            ifelse(grepl("SSP4", x = scenario, fixed = T), "SSP4",
              ifelse(grepl("SSP5", x = scenario, fixed = T), "SSP5",
                default
              )
            )
          )
        )
      ))
  }


  return(df)
}

add_shape_mapping_gini <- function(df, default = "NA") {
  # TODO: consider replacing this by a config / YAML mapping file

  if (default == "NA") {
  df <- df %>%
    mutate(scenario.mapping = ifelse(grepl("EI", x = scenario, fixed = T), "EI",
                                     ifelse(grepl("MC", x = scenario, fixed = T), "MC",
                                            ifelse(grepl("RC", x = scenario, fixed = T), "RC",
                                                                 NA
                                            )
                                     )
    ))
  } else {
    stop(
      "not yet implemented, could add some is.na() based mapping "
    )
  }

  return(df)
}


# Decent Living Calculator utils: preparing input data -------
rename_remind_scenarios <- function(df){

  df <- df %>%
    mutate_cond(startsWith(scenario, "SusDev_"), scenario = substr(scenario,8,nchar(scenario))) %>%
    mutate_cond(endsWith(scenario, "-PkBudg650"), scenario = paste0(substr(scenario, 1, nchar(scenario)-10), "-1p5C"))

  return(df)
}


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

  return(df.out %>% rename(variable = agg.var))
}

format_final_energy_downscaled_shape <- function(df) {

  # IMAGE top-level variables:
  # Final Energy
  # Final Energy|Agriculture
  # Final Energy|Commercial
  # Final Energy|Industry
  # Final Energy|Non-Energy Use
  # Final Energy|Other Sector
  # Final Energy|Residential
  # Final Energy|Transportation
  # REMIND top-level variables:
  # Final Energy
  # Final Energy (w/o bunkers) // don't use
  # Final Energy|Bunkers
  # Final Energy|Industry
  # Final Energy|Non-Energy Use
  # Final Energy|Residential and Commercial
  # Final Energy|Transportation





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
    mutate(agg.var = ifelse(
      variable %in% c("Final Energy|Industry", "Final Energy|Agriculture", "Final Energy|Other Sector"), "Final Energy|Industry", ifelse(
        variable %in% c("Final Energy|Transportation", "Final Energy|Bunkers"), "Final Energy|Transportation", ifelse(
          variable %in% c("Final Energy|Residential and Commercial"), "Final Energy|Residential and Commercial", ifelse(
            variable %in% c("Final Energy|Residential"), "Final Energy|Residential", ifelse(
              variable %in% c("Final Energy|Commercial"), "Final Energy|Commercial",
          ifelse(
            variable == "Final Energy",
            "Final Energy",
            NA
          )
        )
      )
    )))) %>%
    filter(!is.na(agg.var)) %>% # drop final energy variables that are not needed for the sectoral aggregates

    group_by(model, scenario, iso, unit, year, agg.var) %>%
    summarise(
      value = sum(value)
    )



  df.out <- df.out.temp %>%
    select(
      model, scenario, iso, agg.var, unit, year, value
    ) %>% rename(variable = agg.var) %>%
    ungroup()

  return(df.out)
}

load_iam_region_mappings <- function() {
  # TODO: rewrite to function with return-only, instead of global variables

  if (exists("MESSAGE.REGION.MAPPING")) {
    f.regions.message <- here("data-raw", "iam_regions", MESSAGE.REGION.MAPPING)
    regions.message <<- vroom(f.regions.message) %>%
      select(iso, region.message) %>%
      mutate(iso = ifelse(iso == "ROM", "ROU", iso)) # Typo fix for Romania
  }
  if (exists("IMAGE.REGION.MAPPING")) {
    f.regions.image <- here("data-raw", "iam_regions", IMAGE.REGION.MAPPING)
    regions.image <<- vroom(f.regions.image) %>% select(iso, region.image)
  }
  if (exists("REMIND.REGION.MAPPING")) {
    f.regions.remind <- here("data-raw", "iam_regions", REMIND.REGION.MAPPING)
    regions.remind <<- vroom(f.regions.remind) %>% select(iso, region.remind)
  }
  if (exists("MESSAGE.REGION.MAPPING") & exists("IMAGE.REGION.MAPPING") & exists("REMIND.REGION.MAPPING")) {
    regions <<- regions.message %>%
      left_join(regions.image) %>%
      left_join(regions.remind)
  }
}

load_population_projection <- function(ssp) {
  if (is.vector(ssp) & length(ssp) > 1) {
    p <- read_excel(here("data-raw", "iamc_db_SSP_population.xlsx")) %>%
      filter(Model == "IIASA-WiC POP", Scenario %in% ssp) %>%
      select(-Variable, -Model) %>%
      pivot_longer(cols = `2010.0`:`2100.0`, names_to = "year", values_to = "population") %>%
      mutate(year = as.numeric(year), pop_mil = population) %>%
      select(-Notes, -Unit) %>%
      rename(
        iso = Region,
        scenario.mapping = Scenario
      )

    # Given that IIASA-WIC does not report data for Taiwan, we use data from "OECD_Env-Growth" (source: https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome)
    p.twn <- vroom(here("data-raw", "twn_oecd_ssp_pop.csv")) %>%
      filter(Scenario %in% ssp) %>%
      select(-Variable, -Model) %>%
      pivot_longer(cols = `2010`:`2100`, names_to = "year", values_to = "population") %>%
      mutate(year = as.numeric(year), pop_mil = population) %>%
      select(-Unit) %>%
      rename(
        iso = Region,
        scenario.mapping = Scenario
      )

    p <- p %>% bind_rows(p.twn)
  } else if (ssp %in% c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")) {
    p <- read_excel(here("data-raw", "iamc_db_SSP_population.xlsx")) %>%
      filter(Model == "IIASA-WiC POP", Scenario == ssp) %>%
      select(-Variable, -Model, -Scenario) %>%
      pivot_longer(cols = `2010.0`:`2100.0`, names_to = "year", values_to = "population") %>%
      mutate(year = as.numeric(year), pop_mil = population) %>%
      select(-Notes, -Unit) %>%
      rename(iso = Region)

    # Given that IIASA-WIC does not report data for Taiwan, we use data from "OECD_Env-Growth" (source: https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome)
    p.twn <- vroom(here("data-raw", "twn_oecd_ssp_pop.csv")) %>%
      filter(Scenario == ssp) %>%
      select(-Variable, -Model, -Scenario) %>%
      pivot_longer(cols = `2010`:`2100`, names_to = "year", values_to = "population") %>%
      mutate(year = as.numeric(year), pop_mil = population) %>%
      select(-Unit) %>%
      rename(iso = Region)

    p <- p %>% bind_rows(p.twn)
  } else if (ssp == "all") {
    p <- read_excel(here("data-raw", "iamc_db_SSP_population.xlsx")) %>%
      filter(Model == "IIASA-WiC POP") %>%
      select(-Variable, -Model) %>%
      pivot_longer(cols = `2010.0`:`2100.0`, names_to = "year", values_to = "population") %>%
      mutate(year = as.numeric(year), pop_mil = population) %>%
      select(-Notes, -Unit) %>%
      rename(
        iso = Region,
        scenario.mapping = Scenario
      )

    # Given that IIASA-WIC does not report data for Taiwan, we use data from "OECD_Env-Growth" (source: https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome)
    p.twn <- vroom(here("data-raw", "twn_oecd_ssp_pop.csv")) %>%
      select(-Variable, -Model) %>%
      pivot_longer(cols = `2010`:`2100`, names_to = "year", values_to = "population") %>%
      mutate(year = as.numeric(year), pop_mil = population) %>%
      select(-Unit) %>%
      rename(
        iso = Region,
        scenario.mapping = Scenario
      )

    p <- p %>% bind_rows(p.twn)
  }

  return(p)
}


load_urbanisation_projection <- function(ssp) {
  if (is.vector(ssp) & length(ssp) > 1) {
    u <- read_excel(here("data-raw", "iamc_db_SSP_urbanshare.xlsx")) %>%
      select(-Variable, -Model) %>%
      filter(Scenario %in% ssp) %>%
      pivot_longer(cols = `2010.0`:`2100.0`, names_to = "year", values_to = "urb.rate") %>%
      mutate(year = as.numeric(year), urb.rate = urb.rate / 100) %>%
      select(-Notes, -Unit) %>%
      rename(
        iso = Region,
        scenario.mapping = Scenario
      )
  } else if (ssp %in% c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")) {
    u <- read_excel(here("data-raw", "iamc_db_SSP_urbanshare.xlsx")) %>%
      filter(Scenario == ssp) %>%
      select(-Variable, -Model, -Scenario) %>%
      pivot_longer(cols = `2010.0`:`2100.0`, names_to = "year", values_to = "urb.rate") %>%
      mutate(year = as.numeric(year), urb.rate = urb.rate / 100) %>%
      select(-Notes, -Unit) %>%
      rename(iso = Region)
  } else if (ssp == "all") {
    u <- read_excel(here("data-raw", "iamc_db_SSP_urbanshare.xlsx")) %>%
      select(-Variable, -Model) %>%
      pivot_longer(cols = `2010.0`:`2100.0`, names_to = "year", values_to = "urb.rate") %>%
      mutate(year = as.numeric(year), urb.rate = urb.rate / 100) %>%
      select(-Notes, -Unit) %>%
      rename(
        iso = Region,
        scenario.mapping = Scenario
      )
  }

  return(u)
}


format_fetoue_ratio_regional_cdlinksmessage <- function(df, include.aggregation.check = ADDITIONAL.REGIONAL.IAM.SCENARIO.DATA.AGGREGATION.CHECK) {
  #' Need to make sure that the `df` input does not have non-commercial biomass or
  #' other solid fuels for residential energy consumption.


  load_iam_region_mappings() # needed for regional aggregation

  vars.ue.industry <- c(
    "Useful Energy|Feedstocks", # industry
    "Useful Energy|Industrial Specific", # industry
    "Useful Energy|Industrial Thermal" # industry
  )
  vars.ue.rc <- c(
    "Useful Energy|RC Specific", # residential and commercial
    "Useful Energy|RC Thermal" # , # residential and commercial
    # "Useful Energy|Non-Commercial Biomass" # to a large degree used for cooking -- exclude because we also exclude it in the scenarios; which makes the improvement a bit faster in those regions that phase out non-commercial biomass
  )
  vars.ue.transport <- c(
    "Useful Energy|Shipping", # transportation
    "Useful Energy|Transport" # transportation
  )

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
    select(-region.message, -unit) %>%
    select(
      model, scenario, iso, agg.var, year, fe.to.ue.ratio
    ) %>%
    # align model / scenario naming with the downscaled data
    mutate(scenario = paste0(model, scenario)) %>%
    mutate(model = "MESSAGEix-GLOBIOM 1.0")


  # ggplot(df.out.temp.2,aes(x=year, y=fe.to.ue.ratio, colour=scenario, linetype=model)) +
  #   facet_grid(variable~region.message) +
  #   geom_line() +
  #   theme_bw()




  if (include.aggregation.check) { # perhaps should code this up as a separate
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

  return(df.out %>% rename(variable = agg.var))
}


#' format_fetoue_ratio_regional_shape <- function(df, project) {
#'   #' Need to make sure that the `df` input does not have non-commercial biomass or
#'   #' other solid fuels for residential energy consumption.
#'
#'
#'   load_iam_region_mappings() # needed for regional aggregation
#'
#'
#'   # if (IAM.OUTPUT.SHORT.FILENAME == "cdlinks_ssps"){
#'   #
#'   #   vars.ue.industry <- c(
#'   #     "Useful Energy|Feedstocks", # industry
#'   #     "Useful Energy|Industrial Specific", # industry
#'   #     "Useful Energy|Industrial Thermal" # industry
#'   #   )
#'   #   vars.ue.rc <- c(
#'   #     "Useful Energy|RC Specific", # residential and commercial
#'   #     "Useful Energy|RC Thermal" # , # residential and commercial
#'   #     # "Useful Energy|Non-Commercial Biomass" # to a large degree used for cooking -- exclude because we also exclude it in the scenarios; which makes the improvement a bit faster in those regions that phase out non-commercial biomass
#'   #   )
#'   #   vars.ue.transport <- c(
#'   #     "Useful Energy|Shipping", # transportation
#'   #     "Useful Energy|Transport" # transportation
#'   #   )
#'   #
#'   #   df.out.temp.1 <- df %>%
#'   #     rename(model = MODEL, scenario = SCENARIO, region.message = REGION, variable = VARIABLE, unit = UNIT) %>%
#'   #     pivot_longer(
#'   #       !!as.symbol(IAM.SCENARIO.DATA.STARTYEAR):!!as.symbol(IAM.SCENARIO.DATA.ENDYEAR),
#'   #       names_to = "year",
#'   #       values_to = "value"
#'   #     ) %>%
#'   #     select(model, scenario, region.message, variable, unit, year, value) %>% # in case there's years outside the range of interest
#'   #
#'   #     mutate(year = as.numeric(year))
#'   #
#'   #
#'   #   df.out.temp.2 <- df.out.temp.1 %>%
#'   #     mutate(
#'   #       agg.var =
#'   #         ifelse(
#'   #           variable %in% vars.ue.transport, "Useful Energy|Transportation",
#'   #           ifelse(
#'   #             variable %in% vars.ue.rc, "Useful Energy|Residential and Commercial",
#'   #             ifelse(
#'   #               variable %in% vars.ue.industry, "Useful Energy|Industry",
#'   #               ifelse(
#'   #                 grepl(x = variable, pattern = "Final Energy", fixed = T),
#'   #                 variable,
#'   #                 NA
#'   #               )
#'   #             )
#'   #           )
#'   #         )
#'   #     ) %>%
#'   #     # sum over useful energy sectors
#'   #     group_by(model, scenario, region.message, agg.var, unit, year) %>%
#'   #     summarise(value = sum(value)) %>%
#'   #     # calculate total useful energy (total final already reported, but not total useful)
#'   #     pivot_wider(names_from = agg.var, values_from = value) %>%
#'   #     mutate(
#'   #       `Useful Energy` = `Useful Energy|Transportation` + `Useful Energy|Residential and Commercial` + `Useful Energy|Industry`
#'   #     ) %>%
#'   #     # calculate ratios
#'   #     mutate(
#'   #       fe.to.ue = `Useful Energy` / `Final Energy`,
#'   #       fe.to.ue.transportation = `Useful Energy|Transportation` / `Final Energy|Transportation`,
#'   #       fe.to.ue.rescom = `Useful Energy|Residential and Commercial` / `Final Energy|Residential and Commercial`,
#'   #       fe.to.ue.industry = `Useful Energy|Industry` / `Final Energy|Industry`
#'   #     ) %>%
#'   #     select(model, scenario, region.message, unit, year, fe.to.ue, fe.to.ue.transportation, fe.to.ue.rescom, fe.to.ue.industry) %>%
#'   #     pivot_longer(cols = fe.to.ue:fe.to.ue.industry, names_to = "variable", values_to = "fe.to.ue.ratio") %>%
#'   #     mutate(
#'   #       variable =
#'   #         ifelse(
#'   #           variable == "fe.to.ue.transportation", "Final Energy|Transportation",
#'   #           ifelse(
#'   #             variable == "fe.to.ue.rescom", "Final Energy|Residential and Commercial",
#'   #             ifelse(
#'   #               variable == "fe.to.ue.industry", "Final Energy|Industry",
#'   #               ifelse(
#'   #                 variable == "fe.to.ue", "Final Energy",
#'   #                 NA
#'   #               )
#'   #             )
#'   #           )
#'   #         )
#'   #     ) %>%
#'   #     rename(agg.var = variable)
#'   #
#'   #   # assume same fe.to.ue.ratio for all countries in each messageix region
#'   #   df.out.temp.iso <- regions.message %>%
#'   #     left_join(
#'   #       df.out.temp.2
#'   #     ) %>%
#'   #     select(-region.message, -unit) %>%
#'   #     select(
#'   #       model, scenario, iso, agg.var, year, fe.to.ue.ratio
#'   #     ) %>%
#'   #     # align model / scenario naming with the downscaled data
#'   #     mutate(scenario = paste0(model, scenario)) %>%
#'   #     mutate(model = "MESSAGEix-GLOBIOM 1.0")
#'   # }
#'
#'   if (IAM.OUTPUT.SHORT.FILENAME == "SHAPE-final"){
#'
#'     # downscaled final energy quick cleaning
#'     fe.data <- df %>%
#'       rename(model = MODEL, scenario = SCENARIO, region = ISO, variable = VARIABLE, unit = UNIT) %>%
#'       filter(
#'         variable %in% c(
#'           "Final Energy",
#'           "Final Energy|Commercial",
#'           "Final Energy|Residential",
#'           "Final Energy|Residential and Commercial",
#'           "Final Energy|Industry",
#'           "Final Energy|Transportation"
#'         )
#'       )
#'
#'     fe.data %>% pull(variable) %>% unique()
#'
#'
#'     # useful energy mapping
#'     mapping.ue <- vroom(here(
#'       "data-raw",
#'       "scenario_data",
#'       "useful",
#'       "shape_mapping_useful.csv"
#'     ))
#'     mapping.ue %>% pull(file) %>% unique()
#'
#'
#'     # useful energy quick cleaning
#'     library(readxl)
#'     ue.data <-
#'       # image extra useful energy data
#'       vroom(here(
#'         "data-raw",
#'         "scenario_data",
#'         "useful",
#'         "image",
#'         "image_allscenarios_useful.csv"
#'       )) %>% rename(model = Model, scenario = Scenario, region = Region, variable = Variable, unit = Unit) %>%
#'       # image snapshot data (services)
#'       bind_rows(
#'
#'         c(
#'           here("data-raw","scenario_data","useful","image","snapshotdata","image-16nov-1668598686105-SDP_EI-1p5C.xlsx"),
#'           here("data-raw","scenario_data","useful","image","snapshotdata","image-16nov-1668598939539-SDP_MC-1p5C.xlsx"),
#'           here("data-raw","scenario_data","useful","image","snapshotdata","image-16nov-1668598959778-SDP_RC-1p5C.xlsx"),
#'           here("data-raw","scenario_data","useful","image","snapshotdata","image-16nov-1668612326097-SSP2.xlsx"),
#'           here("data-raw","scenario_data","useful","image","snapshotdata","image-16nov-1668612354818-SSP2-1p5C.xlsx")
#'         ) %>%
#'           map(~ read_excel(.)) %>%
#'           reduce(rbind) %>%
#'           select(-`...1`) %>%
#'           rename(model = Model, scenario = Scenario, region = Region, variable = Variable, unit = Unit)
#'
#'       ) %>%
#'
#'       # remind extra useful
#'       bind_rows(
#'
#'         vroom(
#'           here("data-raw","scenario_data","useful","remind","REMIND_SHAPE_UE_buildings_industry.csv")
#'         ) %>% rename(model = Model, scenario = Scenario, region = Region, variable = Variable, unit = Unit)
#'
#'       ) %>%
#'
#'       # remind snapshot data (services)
#'       bind_rows(
#'
#'         read_excel(
#'           here("data-raw","scenario_data","useful","remind","snapshotdata","remind-all-24oct-1666617443646-SusDev_alldata.xlsx")
#'         ) %>% rename(model = Model, scenario = Scenario, region = Region, variable = Variable, unit = Unit)
#'
#'       ) %>%
#'
#'
#'       # filter only the necessary variables
#'       filter(variable %in% c(
#'         mapping.ue %>% pull(fe.var) %>% unique(),
#'         mapping.ue %>% pull(ue.var) %>% unique()
#'       )) %>%
#'
#'       select(model:`2100`)
#'
#'
#'
#'     # # read in mapping
#'     # image.mapping.ue <- vroom(
#'     #   here(
#'     #     "data-raw",
#'     #     "scenario_data",
#'     #     "useful",
#'     #     "image",
#'     #     "image_mapping_useful.csv"
#'     #   )
#'     # )
#'     #
#'     # # variables image
#'     # vars.ue.industry <- c(
#'     #   "Energy Service|Industry|Steel", # steel service
#'     #   "Energy Service|Industry|Cement" # cement service
#'     # )
#'     # vars.ue.res <- c(
#'     #   "Useful Energy|Residential|Appliances",
#'     #   "Useful Energy|Residential|Cooking",
#'     #   "Useful Energy|Residential|Cooling",
#'     #   "Useful Energy|Residential|Heating",
#'     #   "Useful Energy|Residential|Lighting",
#'     #   "Useful Energy|Residential|Space Heating",
#'     #   "Useful Energy|Residential|Water Heating"
#'     #
#'     # )
#'     #
#'     # # should i split up both residential and commercial??????
#'     # vars.ue.comm <- c(
#'     #   "Useful Energy|Commercial|Cooling",
#'     #   "Useful Energy|Commercial|Heating"
#'     # )
#'     #
#'     # # vars.ue.rc <- vars.ue.res
#'     #
#'     # vars.ue.transport <- c(
#'     #   "Energy Service|Transportation|Passenger" # transportation service
#'     #
#'     # )
#'
#'     IAM.SCENARIO.DATA.STARTYEAR <- 2010
#'
#'     ue.data
#'     fe.data
#'
#'     mapping.ue
#'
#'
#'     df.out.temp.1 <-
#'       fe.data
#'
#'
#'       fe.data %>%
#'       rename(model = MODEL, scenario = SCENARIO, region.message = REGION, variable = VARIABLE, unit = UNIT) %>%
#'       pivot_longer(
#'         !!as.symbol(IAM.SCENARIO.DATA.STARTYEAR):!!as.symbol(IAM.SCENARIO.DATA.ENDYEAR),
#'         names_to = "year",
#'         values_to = "value"
#'       ) %>%
#'       select(model, scenario, region.message, variable, unit, year, value) %>% # in case there's years outside the range of interest
#'
#'       mutate(year = as.numeric(year))
#'
#'
#'     df.out.temp.2 <- df.out.temp.1 %>%
#'       mutate(
#'         agg.var =
#'           ifelse(
#'             variable %in% vars.ue.transport, "Useful Energy|Transportation",
#'             ifelse(
#'               variable %in% vars.ue.rc, "Useful Energy|Residential and Commercial",
#'               ifelse(
#'                 variable %in% vars.ue.industry, "Useful Energy|Industry",
#'                 ifelse(
#'                   grepl(x = variable, pattern = "Final Energy", fixed = T),
#'                   variable,
#'                   NA
#'                 )
#'               )
#'             )
#'           )
#'       ) %>%
#'       # sum over useful energy sectors
#'       group_by(model, scenario, region.message, agg.var, unit, year) %>%
#'       summarise(value = sum(value)) %>%
#'       # calculate total useful energy (total final already reported, but not total useful)
#'       pivot_wider(names_from = agg.var, values_from = value) %>%
#'       mutate(
#'         `Useful Energy` = `Useful Energy|Transportation` + `Useful Energy|Residential and Commercial` + `Useful Energy|Industry`
#'       ) %>%
#'       # calculate ratios
#'       mutate(
#'         fe.to.ue = `Useful Energy` / `Final Energy`,
#'         fe.to.ue.transportation = `Useful Energy|Transportation` / `Final Energy|Transportation`,
#'         fe.to.ue.rescom = `Useful Energy|Residential and Commercial` / `Final Energy|Residential and Commercial`,
#'         fe.to.ue.industry = `Useful Energy|Industry` / `Final Energy|Industry`
#'       ) %>%
#'       select(model, scenario, region.message, unit, year, fe.to.ue, fe.to.ue.transportation, fe.to.ue.rescom, fe.to.ue.industry) %>%
#'       pivot_longer(cols = fe.to.ue:fe.to.ue.industry, names_to = "variable", values_to = "fe.to.ue.ratio") %>%
#'       mutate(
#'         variable =
#'           ifelse(
#'             variable == "fe.to.ue.transportation", "Final Energy|Transportation",
#'             ifelse(
#'               variable == "fe.to.ue.rescom", "Final Energy|Residential and Commercial",
#'               ifelse(
#'                 variable == "fe.to.ue.industry", "Final Energy|Industry",
#'                 ifelse(
#'                   variable == "fe.to.ue", "Final Energy",
#'                   NA
#'                 )
#'               )
#'             )
#'           )
#'       ) %>%
#'       rename(agg.var = variable)
#'
#'     # assume same fe.to.ue.ratio for all countries in each messageix region
#'     df.out.temp.iso <- regions.image %>%
#'       left_join(
#'         df.out.temp.2
#'       ) %>%
#'       select(-region.message, -unit) %>%
#'       select(
#'         model, scenario, iso, agg.var, year, fe.to.ue.ratio
#'       ) %>%
#'       # align model / scenario naming with the downscaled data
#'       mutate(scenario = paste0(model, scenario)) %>%
#'       mutate(model = "IMAGE 3.3")
#'   }
#'
#'   if (model=="REMIND-MAgPIE 3.0-4.4"){
#'
#'     vars.ue.industry <- c(
#'       "UE|Industry"
#'     )
#'     vars.ue.rc <- c(
#'       "UE|Buildings"
#'     )
#'
#'     vars.ue.transport <- c(
#'       "tbd" # needs to be derived from the scenarios using the r script that sebastian sent to me
#'     )
#'
#'     df.out.temp.1 <- df %>%
#'       rename(model = MODEL, scenario = SCENARIO, region.message = REGION, variable = VARIABLE, unit = UNIT) %>%
#'       pivot_longer(
#'         !!as.symbol(IAM.SCENARIO.DATA.STARTYEAR):!!as.symbol(IAM.SCENARIO.DATA.ENDYEAR),
#'         names_to = "year",
#'         values_to = "value"
#'       ) %>%
#'       select(model, scenario, region.message, variable, unit, year, value) %>% # in case there's years outside the range of interest
#'
#'       mutate(year = as.numeric(year))
#'
#'
#'     df.out.temp.2 <- df.out.temp.1 %>%
#'       mutate(
#'         agg.var =
#'           ifelse(
#'             variable %in% vars.ue.transport, "Useful Energy|Transportation",
#'             ifelse(
#'               variable %in% vars.ue.rc, "Useful Energy|Residential and Commercial",
#'               ifelse(
#'                 variable %in% vars.ue.industry, "Useful Energy|Industry",
#'                 ifelse(
#'                   grepl(x = variable, pattern = "Final Energy", fixed = T),
#'                   variable,
#'                   NA
#'                 )
#'               )
#'             )
#'           )
#'       ) %>%
#'       # sum over useful energy sectors
#'       group_by(model, scenario, region.message, agg.var, unit, year) %>%
#'       summarise(value = sum(value)) %>%
#'       # calculate total useful energy (total final already reported, but not total useful)
#'       pivot_wider(names_from = agg.var, values_from = value) %>%
#'       mutate(
#'         `Useful Energy` = `Useful Energy|Transportation` + `Useful Energy|Residential and Commercial` + `Useful Energy|Industry`
#'       ) %>%
#'       # calculate ratios
#'       mutate(
#'         fe.to.ue = `Useful Energy` / `Final Energy`,
#'         fe.to.ue.transportation = `Useful Energy|Transportation` / `Final Energy|Transportation`,
#'         fe.to.ue.rescom = `Useful Energy|Residential and Commercial` / `Final Energy|Residential and Commercial`,
#'         fe.to.ue.industry = `Useful Energy|Industry` / `Final Energy|Industry`
#'       ) %>%
#'       select(model, scenario, region.message, unit, year, fe.to.ue, fe.to.ue.transportation, fe.to.ue.rescom, fe.to.ue.industry) %>%
#'       pivot_longer(cols = fe.to.ue:fe.to.ue.industry, names_to = "variable", values_to = "fe.to.ue.ratio") %>%
#'       mutate(
#'         variable =
#'           ifelse(
#'             variable == "fe.to.ue.transportation", "Final Energy|Transportation",
#'             ifelse(
#'               variable == "fe.to.ue.rescom", "Final Energy|Residential and Commercial",
#'               ifelse(
#'                 variable == "fe.to.ue.industry", "Final Energy|Industry",
#'                 ifelse(
#'                   variable == "fe.to.ue", "Final Energy",
#'                   NA
#'                 )
#'               )
#'             )
#'           )
#'       ) %>%
#'       rename(agg.var = variable)
#'
#'     # assume same fe.to.ue.ratio for all countries in each messageix region
#'     df.out.temp.iso <- regions.image %>%
#'       left_join(
#'         df.out.temp.2
#'       ) %>%
#'       select(-region.message, -unit) %>%
#'       select(
#'         model, scenario, iso, agg.var, year, fe.to.ue.ratio
#'       ) %>%
#'       # align model / scenario naming with the downscaled data
#'       mutate(scenario = paste0(model, scenario)) %>%
#'       mutate(model = "REMIND-MAgPIE 3.0-4.4")
#'   }
#'
#'
#'   return(df.out %>% rename(variable = agg.var))
#' }



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


# calculate weibull distribution parameters
# TODO



# project using deciles (or any number of percentiles?)
# TODO
