# Creates a customized skimmer with my preferred descriptives
my_skim <- skimr::skim_with(base = skimr::sfl(n_complete = ~ sum(!is.na(.), 
                                                                 na.rm = TRUE),
                                              n_missing = ~sum(is.na(.), 
                                                               na.rm = TRUE)),
                     numeric = skimr::sfl(p25 = NULL, p75 = NULL, hist = NULL),
                     character = skimr::sfl(min = NULL, max = NULL),
                     factor = skimr::sfl(ordered = NULL))