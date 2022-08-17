library(tidyverse)

## -----Reading in the data-----

load("ict-2021-data.RData")
ict <- . 
ict <- ict %>%
  filter(!(country %in% c("NO", "IS")), agecls %in% 2:7)

# Some recoding
ict <- ict %>%
  mutate_at(.vars = vars(ticcsfoi, ticidis, ticnidis),
            .funs = parse_number) %>%
  mutate(i_tic2 = 100 * ticcsfoi + 10 * ticidis + ticnidis,
         i_tic2 = replace(i_tic2, i_tic2 %in% 1:111, 1),
         i_tic2 = replace(i_tic2, i_tic2 != 1 | is.na(i_tic2), 0)) 

## Information and data literacy -----

dsk_i <- ict %>%
  select(country, ind_seq_nr, ind_wght, iu, iuif, ihif, iunw1, ticxnd, i_tic2) %>%
  mutate_at(.vars = vars(iuif, ihif, iunw1, ticxnd),
            .funs = parse_number) %>%
  mutate_at(.vars = vars(iuif, ihif, iunw1, ticxnd, i_tic2),
            .funs = ~replace(., is.na(.), 0)) %>%
  mutate_at(.vars = vars(iuif, ihif, iunw1, ticxnd, i_tic2),
            .funs = ~replace(., iu != 1 | is.na(iu), NA)) 

dsk_i <- dsk_i %>%
  pivot_longer(cols = iuif:i_tic2, names_to = "item", values_to = "response") %>%
  group_by(country, ind_seq_nr, ind_wght, iu) %>%
  summarise(dsk_info = sum(I(response == 1)),
            .groups = "drop") %>%
  mutate(dsk_i = replace(dsk_info, dsk_info > 2, 2))

## Communication and collaboration -----

dsk_cc <- ict %>%
  select(country, ind_seq_nr, ind_wght, iu, iuem, iuph1, iuchat1, iusnet, iupol2, iuvote) %>%
  mutate_at(.vars = vars(iuem, iuph1, iuchat1, iusnet, iupol2, iuvote),
            .funs = parse_number) %>%
  mutate_at(.vars = vars(iuem, iuph1, iuchat1, iusnet, iupol2, iuvote),
            .funs = ~replace(., is.na(.), 0)) %>%
  mutate_at(.vars = vars(iuem, iuph1, iuchat1, iusnet, iupol2, iuvote),
            .funs = ~replace(., iu != 1 | is.na(iu), NA)) 

cc_cors <- dsk_cc %>%
  filter(iu == 1, !(country %in% c("NO", "IS"))) %>%
  select(iuem:iuvote) %>%
  cor(use = "pairwise")

cc_taus <- dsk_cc %>%
  filter(iu == 1, !(country %in% c("NO", "IS"))) %>%
  drop_na() %>%
  select(iuem:iuvote) %>% 
  GKtauDataframe()

pol1 <- xtabs(ind_wght ~ iupol2 + iuvote, data = dsk_cc, subset = iu == 1 & !(country %in% c("NO", "IS")))
pol2 <- xtabs(ind_wght ~ iupol2 + iuvote + country, data = dsk_cc, subset = iu == 1 & !(country %in% c("NO", "IS")))
save(list = c("pol1", "pol2"), file = "dsk-cc-civpart.RData")

jpeg(filename = "dsk-cc-correlations.jpeg", quality = 90)
corrplot.mixed(cc_cors, order = "hclust")
dev.off()

dsk_cc <- dsk_cc%>%
  pivot_longer(cols = iuem:iuvote, names_to = "item", values_to = "response") %>%
  group_by(country, ind_seq_nr, ind_wght, iu) %>%
  summarise(dsk_comm = sum(response == 1),
            .groups = "drop") %>%
  mutate(dsk_c = replace(dsk_comm, dsk_comm > 2, 2))

## Digital content creation -----

dsk_dcc <- ict %>%
  select(country, ind_seq_nr, ind_wght, iu, cwrd1, cepva1, cxfer1, cxls1, cpres2, cxlsadv1, cprg2) %>%
  mutate_at(.vars = vars(cwrd1, cepva1, cxfer1, cxls1, cpres2, cxlsadv1, cprg2),
            .funs = parse_number) %>%
  mutate_at(.vars = vars(cwrd1, cepva1, cxfer1, cxls1, cpres2, cxlsadv1, cprg2),
            .funs = ~replace(., is.na(.), 0)) %>%
  mutate_at(.vars = vars(cwrd1, cepva1, cxfer1, cxls1, cpres2, cxlsadv1, cprg2),
            .funs = ~replace(., iu != 1 | is.na(iu), NA)) 

dsk_dcc <- dsk_dcc %>%
  pivot_longer(cols = cwrd1:cprg2, names_to = "item", values_to = "response") %>%
  group_by(country, ind_seq_nr, ind_wght, iu) %>%
  summarise(dsk_dcc = sum(response == 1),
            .groups = "drop") %>%
  mutate(dsk_d = replace(dsk_dcc, dsk_dcc > 3, 3),
         dsk_d = replace(dsk_d, dsk_d == 1, 2))

## -----Safety-----

dsk_safe <- ict %>% 
  select(country, ind_seq_nr, ind_wght, iu, maps_rps, maps_rrgl, maps_lap, maps_raad, maps_cwsc, pcook1) %>%
  mutate_at(.vars = vars(maps_rps, maps_rrgl, maps_lap, maps_raad, maps_cwsc, pcook1),
            .funs = parse_number) %>%
  mutate_at(.vars = vars(maps_rps, maps_rrgl, maps_lap, maps_raad, maps_cwsc, pcook1),
            .funs = ~replace(., is.na(.), 0)) %>%
  mutate_at(.vars = vars(maps_rps, maps_rrgl, maps_lap, maps_raad, maps_cwsc, pcook1),
            .funs = ~replace(., iu != 1 | is.na(iu), NA))

safe_cors <- dsk_safe %>%
  filter(iu == 1, !(country %in% c("NO", "IS"))) %>%
  select(maps_rps:pcook1) %>%
  cor(use = "pairwise")

corrplot.mixed(safe_cors, addgrid.col = TRUE, order = "hclust")

dsk_safe <- dsk_safe %>%
  pivot_longer(cols = maps_rps:pcook1, names_to = "item", values_to = "response") %>%
  group_by(country, ind_seq_nr, ind_wght, iu) %>%
  summarise(dsk_safety = sum(response == 1),
            .groups = "drop") %>%
  mutate(dsk_safe = replace(dsk_safety, dsk_safety > 3, 3),
         dsk_safe = replace(dsk_safe, dsk_safe == 1, 2))

## Problem solving-----

dsk_ps <- ict %>% 
  select(country, ind_seq_nr, ind_wght, iu, cinsapp1, cconf1, ibuy, iusell, iubk, iuolc, iuolm, iujob) %>%
  mutate_at(.vars = vars(cinsapp1, cconf1, ibuy, iusell, iubk, iuolc, iuolm, iujob),
            .funs = parse_number) %>%
  mutate_at(.vars = vars(cinsapp1, cconf1, ibuy, iusell, iubk, iuolc, iuolm, iujob),
            .funs = ~replace(., is.na(.), 0)) 

dsk_ps <- dsk_ps %>%
  mutate(ibuy = ifelse(ibuy %in% 1:2, 1, 0),
         iuol = ifelse(iuolc == 1 | iuolm == 1, 1, 0)) %>%
  select(-iuolc, -iuolm) %>%
  mutate_at(.vars = vars(cinsapp1, cconf1, ibuy, iusell, iubk, iuol, iuol, iujob),
            .funs = ~replace(., iu != 1 | is.na(iu), NA))

dsk_ps <- dsk_ps %>%
  pivot_longer(cols = cinsapp1:iuol, names_to = "item", values_to = "response") %>%
  group_by(country, ind_seq_nr, ind_wght, iu) %>%
  summarise(dsk_prob = sum(response == 1),
            .groups = "drop") %>%
  mutate(dsk_ps = replace(dsk_prob, dsk_prob > 3, 3),
         dsk_ps = replace(dsk_ps, dsk_ps == 1, 2))

## The overall score ---- 

dsk <- left_join(dsk_i, dsk_cc) %>%
  left_join(., dsk_dcc) %>%
  left_join(., dsk_ps) %>%
  left_join(., dsk_safe) %>%
  mutate_at(.vars = vars(dsk_d, dsk_safe, dsk_ps),
            .funs = ~ifelse(. != 0, . - 1, .))

dsk <- dsk %>%
  mutate(overall = paste0(dsk_i, dsk_c, dsk_d, dsk_ps, dsk_safe),
         nos = str_count(string = overall, pattern = "0"),
         bas = str_count(string = overall, pattern = "1"),
         abv = str_count(string = overall, pattern = "2"),
         dsk_all = NA,
         dsk_all = replace(dsk_all, abv == 5, "Above basic"),
         dsk_all = replace(dsk_all, nos == 0 & abv < 5, "Basic"),
         dsk_all = replace(dsk_all, nos == 1, "Low"),
         dsk_all = replace(dsk_all, nos == 2, "Narrow"),
         dsk_all = replace(dsk_all, nos == 3, "Limited"),
         dsk_all = replace(dsk_all, nos >= 4, "None"))

dsk <- dsk %>%
  mutate(dsk_all = replace(dsk_all, iu != 1 | is.na(iu), "Not applicable"),
         dsk_all = factor(dsk_all, levels = c("Not applicable", "None", "Limited", "Narrow", "Low", "Basic", "Above basic"))) 

save(dsk, file = "dsi-2.0-all-areas.RData")

