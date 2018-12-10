pacman::p_load(dplyr, fwsinat)

# Need to tweak fwsinat::inat_retrieve function to get all records with media URLs...
get_inat <- function(refuge, inat_proj = "usfws-national-wildlife-refuge-system") {
  q_dt <- Sys.time()
  obs <- pbapply::pblapply(refuge, function(i) {
    r <- utils::read.csv(system.file("extdata", "fws_place_ids.csv", package = "fwsinat"),
                         stringsAsFactors = FALSE)
    ref_name <- r[r$orgname == i, "name"]
    place_id <- r[r$orgname == i, "inat_place_id"]
    
    # Retrieve observations for this request and inform of problem if too many
    n_recs <- fwsinat:::GET_inat(place_id, inat_proj, nrecs_only = TRUE, verbose = FALSE)
    
    # Now get observations
    obs <- fwsinat:::GET_inat(place_id, inat_proj, nrecs_only = FALSE, verbose = FALSE)
    if (is.null(obs)) return(obs)
    
    obs <- obs %>%
      filter(!itistools::is_missing(.data$latitude),
             !itistools::is_missing(.data$longitude)) %>%
      mutate(orgname = i,
             sci_name = fwsinat:::clean_sci_name(.data$scientific_name),
             date = as.Date(.data$observed_on, format = "%Y-%m-%d"),
             last_inat_update = as.Date(.data$updated_at, format = "%Y-%m-%d"),
             com_name = ifelse(itistools::is_missing(.data$common_name),
                               NA_character_, itistools::Cap(.data$common_name)),
             loc_obscured = as.logical(toupper(.data$coordinates_obscured)),
             notes = ifelse(itistools::is_missing(.data$description), NA_character_,
                            .data$description))
  })
  
  if (all(sapply(obs, is.null))) return(NULL)
  obs <- bind_rows(obs)
  n_dl <- nrow(obs)
  itis <- itistools::get_itis(obs$sci_name)
  if (identical(itis, tibble()))
    itis <- empty_itis()
  
  # Now join ITIS info to occurrence records
  obs <- left_join(ungroup(obs), itis, by = "sci_name") %>%
    mutate(sci_name = ifelse(is.na(.data$valid_sci_name), .data$sci_name, .data$valid_sci_name),
           iconic_taxon = fwsinat:::layman_iconic(.data$iconic_taxon_name)) %>%
    select(.data$orgname,
           .data$sci_name,
           .data$com_name,
           .data$itis_com_name,
           .data$iconic_taxon,
           .data$itis_taxon_rank,
           .data$date,
           .data$last_inat_update,
           lat = .data$latitude, lon = .data$longitude, .data$loc_obscured,
           .data$notes, grade = .data$quality_grade, .data$url,
           user = .data$user_login,
           everything()) %>%
    arrange(.data$orgname, .data$iconic_taxon, .data$sci_name, -as.numeric(.data$date))

  class(obs) <- c("fwsinat", class(obs))
  attr(obs, "inat_proj") <- ifelse(is.null(inat_proj), "all", inat_proj)
  attr(obs, "fws_props") <- refuge
  attr(obs, "query_dt") <- q_dt
  obs
}

# Now get the complete data set
nwr <- find_refuges(region = 4, ptype = "NWR")
nfh <- find_refuges(region = 4, ptype = "NFH")
nwr18 <- get_inat(nwr)
nfh18 <- get_inat(nfh, inat_proj = "usfws-national-fish-hatchery-system")

# Contest boundaries
start_date <- "2018-06-01"
end_date <- "2018-11-30"

# NOTE THIS IS NOT AN EXACT MATCH OF RECORDS USED TO CALCULATE CONTEST RESULTS
# In particular, it appears ~ 100 records have been deleted by users
# Additionally, about 25 "new" records are in the data set. 
drop_url_end <- function(x) sub("\\?.*", "", x)
contest_recs <- bind_rows(nwr18, nfh18) %>%
  mutate(orgname = itistools::Cap(orgname),
         orgname = gsub("National Wildlife Refuge", "NWR", orgname),
         orgname = gsub("National Fish Hatchery", "NFH", orgname),
         orgname = gsub("D'arbonne", "D'Arbonne", orgname),
         created_at = lubridate::ymd_hms(created_at)) %>%
  filter(date >= as.Date(start_date), date <= as.Date(end_date),
         # Drop any records created after midnight at BioBlitz end
         created_at <= lubridate::ymd_hms("2018-12-01 05:00:00 UTC")) %>%
  mutate(local_image = ifelse(itistools::is_missing(image_url), 
                              NA_character_, 
                              paste0("images/", id, ".", tools::file_ext(drop_url_end(image_url)))),
         local_sound = ifelse(itistools::is_missing(sound_url), 
                              NA_character_, 
                              paste0("sounds/", id, ".", tools::file_ext(drop_url_end(sound_url)))))
saveRDS(contest_recs, file = "Output/bioblitz18_complete_recs.rds")
# Text version
write.csv(contest_recs, file = "Output/2018_BioBlitz_records/bioblist_complete_recs.csv", row.names = FALSE)
# Excel version
openxlsx::write.xlsx(contest_recs, file = "Output/2018_BioBlitz_records/bioblitz18_complete_recs.xlsx",
                     firstActiveRow = 2, colWidths = "auto")

# Retrieve images and sounds... oof...
pbapply::pblapply(1:nrow(contest_recs), function(i) {
  iid <- contest_recs[i, "id"]
  iimage_url <- contest_recs[i, "image_url"]
  isound_url <- contest_recs[i, "sound_url"]
  # Image first, if necessary
  if (!itistools::is_missing(iimage_url)) {
    iimage_url <- sub("medium", "original", iimage_url)
    utils::download.file(iimage_url, quiet = TRUE, mode = "wb",
                         destfile = file.path(normalizePath("Output/2018_BioBlitz_records/images/"),
                                              paste(iid, tools::file_ext(drop_url_end(iimage_url)), sep = ".")))
  }
  # Sound next, if necessary
  if (!itistools::is_missing(isound_url))
    utils::download.file(isound_url, quiet = TRUE, mode = "wb",
                         destfile = file.path("Output/2018_BioBlitz_records/sounds",
                                              paste(iid, tools::file_ext(drop_url_end(isound_url)), sep = ".")))
})


