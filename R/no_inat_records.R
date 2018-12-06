library(fwsinat)
nwr18 <- readRDS("./output/nwr18_stats.rds")
nfh18 <- readRDS("./output/nfh18_stats.rds")

# Contest boundaries
start_date <- "2018-06-01"
end_date <- "2018-11-30"

all_recs <- bind_rows(nwr18, nfh18) %>%
  mutate(orgname = itistools::Cap(orgname),
         orgname = gsub("National Wildlife Refuge", "NWR", orgname),
         orgname = gsub("National Fish Hatchery", "NFH", orgname),
         orgname = gsub(" And Conservation Area", "/CA", orgname),
         orgname = gsub("D'arbonne", "D'Arbonne", orgname)) %>%
  filter(date <= as.Date(end_date))

contest_recs <- filter(all_recs, date >= as.Date(start_date), date <= as.Date(end_date))
inat_nwrs <- pull(contest_recs, orgname) %>% unique() %>% sort()

# Get stations without records
all_nwrs <- find_refuges(ptype = c("NWR", "NFH"), region = 4) %>%
  itistools::Cap() %>%
  gsub("National Wildlife Refuge", "NWR", .) %>%
  gsub("National Fish Hatchery", "NFH", .) %>%
  gsub(" And Conservation Area", "/CA", .) %>%
  gsub("D'arbonne", "D'Arbonne", .)

no_iNat <- all_nwrs[-which(all_nwrs %in% inat_nwrs)]
data.frame(ORGNAME = no_iNat, stringsAsFactors = FALSE) %>% 
  openxlsx::write.xlsx(file = "Output/r4_no_iNat.xlsx", sheetName = "no_iNat")
