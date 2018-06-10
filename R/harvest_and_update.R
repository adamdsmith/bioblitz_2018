library(fwsinat)

# Specify NWRs and NFHs
nwr <- find_refuges(region = 4, ptype = "NWR")
nfh <- find_refuges(region = 4, ptype = "NFH")

### Initial retrieval
# Get project records
# nwr18 <- inat_retrieve(nwr)
# nfh18 <- inat_retrieve(nfh,
#                        inat_proj = "usfws-national-fish-hatchery-system")
# Save initial records
# saveRDS(nwr18, file = "./Output/nwr18_stats.rds")
# saveRDS(nfh18, file = "./Output/nfh18_stats.rds")

# Read in records in need of update
nwr18 <- readRDS("./output/nwr18_stats.rds")
nfh18 <- readRDS("./output/nfh18_stats.rds")

# Harvest new observations to the USFWS NWRS iNaturalist project
reap_nwr <- inat_harvest(nwr, user = Sys.getenv("user"), pw = Sys.getenv("pw"),
                        interactive = FALSE)

# Harvest new observations to the USFWS NFHS iNaturalist project
reap_nfh <- inat_harvest(nfh,
                        inat_proj = "usfws-national-fish-hatchery-system",
                        user = Sys.getenv("user"), pw = Sys.getenv("pw"),
                        interactive = FALSE)

# Update iNat histories after harvesting...
nwr18 <- inat_update(nwr18)
nfh18 <- inat_update(nfh18)
saveRDS(nwr18, file = "./Output/nwr18_stats.rds")
saveRDS(nfh18, file = "./Output/nfh18_stats.rds")
