pacman::p_load(dplyr, tidyr)
nwr18 <- readRDS("./output/nwr18_stats.rds")
nfh18 <- readRDS("./output/nfh18_stats.rds")

# Contest boundaries
start_date <- "2018-06-01"
end_date <- "2018-11-30"

contest_recs <- bind_rows(nwr18, nfh18) %>%
  mutate(orgname = itistools::Cap(orgname),
         orgname = gsub("National Wildlife Refuge", "NWR", orgname),
         orgname = gsub("National Fish Hatchery", "NFH", orgname),
         orgname = gsub("D'arbonne", "D'Arbonne", orgname)) %>%
  filter(date >= as.Date(start_date), date <= as.Date(end_date))

users_all <- contest_recs %>%
  group_by(orgname, user) %>%
  count(sort = TRUE)

users_5ref <- ungroup(users_all) %>%
  filter(n >= 5) %>%
  arrange(orgname, user)
users_5all <- ungroup(users_all) %>%
  group_by(user) %>%
  summarize(n = sum(n)) %>%
  filter(n >= 5)

users_oneorg <- pull(users_5ref, user) %>% unique()
users_allorgs <- pull(users_5all, user)
deserve <- users_allorgs[which(!users_allorgs %in% users_oneorg)]
users_5all <- ungroup(users_all) %>%
  filter(user %in% deserve) %>%
  group_by(user) %>%
  arrange(user, -n) %>%
  slice(1)

coins_to <- bind_rows(ungroup(users_5ref), ungroup(users_5all)) %>%
  arrange(orgname, user)
saveRDS(coins_to, file = "Output/coin_awardees.rds")

# # Retrieve mailing addresses
# # Manual editing occurred after creation so this is now commented out...
# official_orgs <- bind_rows(nwr18, nfh18) %>%
#   filter(date >= as.Date(start_date), date <= as.Date(end_date)) %>%
#   group_by(orgname, user) %>%
#   count() %>%
#   filter(n >= 5) %>%
#   pull(orgname) %>% unique()
# mailing <- readr::read_delim("R4_mailing_addresses.txt", delim = "~") %>%
#   mutate(ORGNAME =  toupper(ORGNAME)) %>%
#   select(Company = ORGNAME, 
#          Address1 = MAILADD1,
#          Address2 = MAILADD2,
#          City = MAILCITY,
#          State = MAILSTATEABBR,
#          Zip = MAILZIP) %>%
#   filter(Company %in% official_orgs) %>%
#   mutate(Company = gsub("NATIONAL WILDLIFE REFUGE", "NWR", Company),
#          Company = gsub("NATIONAL FISH HATCHERY", "NFH", Company))
# openxlsx::write.xlsx(mailing, file = "Output/challenge_coin_mailing_addresses.xlsx")
