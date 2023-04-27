# Script calculating energy deprivation

log_info("DLE calculator: all variables have been prepared and projected, combining data now.")

# load data
scenario.assessment.data <-
  # final energy per capita (+population)
  readRDS(
    here(
      "data",
      paste0("scenario_FE", "_", IAM.OUTPUT.SHORT.FILENAME, ".RData")
    )
  ) %>% rename_remind_scenarios() %>%
  # energy inequality
  left_join(
    readRDS(
      here(
        "data",
        paste0("projected_energy_inequality", "_", IAM.OUTPUT.SHORT.FILENAME, ".RData")
      )
    ) %>% select(-scenario.mapping) %>% ungroup() %>% rename_remind_scenarios()
  ) %>%
  # dle
  left_join(
    readRDS(
      here(
        "data",
        paste0("projected_dle-total-and-sectoral-scaled", "_", IAM.OUTPUT.SHORT.FILENAME, ".RData")
      )
    )
  ) %>%
  # drop data where missing essential data
  drop_na(energy.per.capita, energy.gini, dle.threshold.curtech, dle.threshold.adjusted) %>%
  # reorder again
  arrange(model, scenario, iso, variable, unit, year)



log_info("DLE calculator: obtain share below threshold and depth of deficit - assuming lognormal distribution")

# calculate share of people below threshold and depth of deficit
scenario.assessment.calculated <- scenario.assessment.data %>%
  mutate(
    sig = GetInverseCDF_lognormal(energy.gini)
  ) %>%
  mutate(nu = log(energy.per.capita) - (sig^2) / 2) %>%
  mutate(share.below.projected.curtech = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.curtech, "share")) %>%
  mutate(depth.below.projected.curtech = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.curtech, "DoD")) %>%
  mutate(share.below.projected.adjusted = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.adjusted, "share")) %>%
  mutate(depth.below.projected.adjusted = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.adjusted, "DoD")) %>%
  select(-sig, -nu)



log_info(paste0("DLE calculator is done. Now the data should be saved in CSV and/or RData format."))
