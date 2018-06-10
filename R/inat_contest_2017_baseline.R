library(fwsinat)
library(dplyr)

nwr <- find_refuges(region = 4, ptype = "NWR")
nfh <- find_refuges(region = 4, ptype = "NFH")

# First, harvest observations to the USFWS NWRS iNaturalist project
# reap_nwr <- inat_harvest(nwr, user = Sys.getenv("user"), pw = Sys.getenv("pw"),
#                          interactive = FALSE)
# 
# Harvest new observations to the USFWS NFHS iNaturalist project
# reap_nfh <- inat_harvest(nfh,
#                          inat_proj = "usfws-national-fish-hatchery-system",
#                          user = Sys.getenv("user"), pw = Sys.getenv("pw"),
#                          interactive = FALSE)

# Retrieve R4 records until 20 November 2017 as baseline
# nwr_2017 <- inat_retrieve(nwr, d2 = "2017-11-30")
# nfh_2017 <- inat_retrieve(nfh, d2 = "2017-11-30",
#                           inat_proj = "usfws-national-fish-hatchery-system")
# saveRDS(nwr_2017, file = "./Output/nwr_2017.rds")
# saveRDS(nfh_2017, file = "./Output/nfh_2017.rds")

all_recs <- readRDS("./Output/nwr_2017.rds") %>%
  bind_rows(readRDS("./Output/nfh_2017.rds")) %>%
  mutate(orgname = itistools::Cap(orgname),
         orgname = gsub("National Wildlife Refuge", "NWR", orgname),
         orgname = gsub("National Fish Hatchery", "NFH", orgname),
         orgname = gsub(" And Conservation Area", "/CA", orgname),
         orgname = gsub("D'arbonne", "D'Arbonne", orgname))
spp_recs <- all_recs %>%
  filter(itis_taxon_rank %in% c("Genus", "Species", "Subspecies", "Variety"),
         grade == "research")

start_date <- "2017-06-01"

contest_recs <- all_recs %>%
  filter(date >= as.Date(start_date))

contest_recs_spp <- contest_recs %>%
  filter(itis_taxon_rank %in% c("Genus", "Species", "Subspecies", "Variety"),
         grade == "research")

# MOST SPECIES
most_spp <- spp_recs %>%
  filter(date >= as.Date(start_date)) %>%
  group_by(orgname) %>%
  summarize(n_spp = n_distinct(sci_name)) %>%
  mutate(rank = rank(-1 * n_spp, ties = "min")) %>%
  select(rank, orgname, n_spp) %>%
  arrange(rank, orgname)
names(most_spp) <- c("Rank", "Refuge or Hatchery", "# Total Species")

# Most new species
spp_before <- spp_recs %>%
  filter(date < as.Date(start_date)) %>%
  group_by(orgname) %>%
  summarize(before_spp = n_distinct(sci_name))

spp_after <-  spp_recs %>%
  group_by(orgname) %>%
  summarize(after_spp = n_distinct(sci_name))

most_new <- left_join(spp_after, spp_before, by = "orgname") %>%
  mutate(before_spp = ifelse(is.na(before_spp), 0, before_spp),
         n_new_spp = after_spp - before_spp,
         rank = rank(-1 * n_new_spp, ties = "min")) %>%
  select(rank, orgname, n_new_spp) %>%
  filter(n_new_spp > 0) %>%
  arrange(rank, orgname)
names(most_new) <- c("Rank", "Refuge or Hatchery", "# New Species")

# Most unique users
most_users <- contest_recs %>%
  group_by(orgname, user) %>%
  summarize(n_recs = n()) %>%
  filter(n_recs >= 5) %>%
  group_by(orgname) %>%
  summarize(n_users = n_distinct(user)) %>%
  mutate(rank = rank(-1 * n_users, ties = "min")) %>%
  select(rank, orgname, n_users) %>%
  arrange(rank, orgname)
names(most_users) <- c("Rank", "Refuge or Hatchery", "# Total Users")

### Unique taxa documented
n_distinct(contest_recs_spp$sci_name)

### Unique contributors
n_distinct(contest_recs$user)

### Refuges/Hatcheries with entries
n_distinct(contest_recs$orgname)
