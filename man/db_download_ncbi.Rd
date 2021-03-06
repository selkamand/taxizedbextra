% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Taxid2Name.R
\name{db_download_ncbi}
\alias{db_download_ncbi}
\title{Download taxonomic databases}
\usage{
db_download_ncbi(overwrite = TRUE)
}
\arguments{
\item{overwrite}{(logical) If \code{TRUE} force an update by overwriting
previously downloaded data. Default: \code{FALSE}}
}
\value{
(character) path to the downloaded SQL database
}
\description{
Download taxonomic databases
}
\details{
Downloads sql database, cleans up unneeded files, returns path
to sql file
}
\examples{
\dontrun{
# ITIS
# db_download_itis()
# src_itis()

# Plantlist
# db_download_tpl()
# db_download_tpl(overwrite=TRUE) # overwrite - download again
# src_tpl()

# COL
# db_download_col()
# src_col()

# GBIF
# db_download_gbif()
# src_gbif()

# NCBI
# db_download_ncbi()
# src_ncbi()

# Wikidata
# db_download_wikidata()
# db_download_wikidata(overwrite=TRUE) # overwrite - download again
# src_wikidata()

# World Flora Online
# db_download_wfo()
# src_wfo()
}
}
\seealso{
\link[taxizedb]{tdb_cache}
}
