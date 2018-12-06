render_memo <- function(org) {
  out_file <- paste(org, "iNaturalist_participation_coins.pdf", sep = "_")
  rmarkdown::render("participation_memo.Rmd", output_dir = "Output/Memos",
                    output_file = paste0(org, ".pdf"),
                    params = list(orgname = org),
                    quiet = TRUE)
  message("Created iNaturalist participation memo for ", org)
}

coins_to <- readRDS("Output/coin_awardees.rds")
coins_orgs <- unique(coins_to$orgname)

for (org in coins_orgs) {
  render_memo(org)
}
