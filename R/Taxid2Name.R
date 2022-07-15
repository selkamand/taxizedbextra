#' NCBI Taxid to Scientific Name
#'
#' Faster taxid2name for long vectors with repeated taxids.
#' You may need to first run \code{taxizedb::db_download_ncbi()}
#'
#' @param taxids vector of taxids (numeric)
#' @param special_taxid_names give custom names for taxids <=0. Supply numeric vector where values correspond to taxids <=0 and names correspond to the custom names we want to use for each of these 'taxids'.
#' @return scientific names for each taxid, or NA if no taxanomic match could be found. for negative taxids, value of 'negative_taxid_name' will dictate the name returned
#' @export
taxid2name <- function(taxids, special_taxid_names = c("unclassified" = -1)){
  unique_taxids <- unique(taxids)

  assertthat::assert_that(is.numeric(special_taxid_names))
  assertthat::assert_that(all(special_taxid_names <= 0), msg = utilitybeltfmt::fmterror(
    "special_taxid_names must all be <= 0 since numbers > 0 might be real ncbi taxids!"
  ))

  assertthat::assert_that(is.numeric(unique_taxids))
  assertthat::assert_that(all(!is.na(unique_taxids)), msg = utilitybeltfmt::fmterror("taxids argument includes NA. This function expects no NA's in input. If NAs represent an inability to classify sequences to a taxid, replace it with -1. Its scientific name then return 'unclassified'"))

  unique_taxids_positive_only = unique_taxids[unique_taxids > 0]

  # Convert taxid to name
  ## --> Create taxid to scientific_name map
  scinames_unique = taxizedb::taxid2name(x = unique_taxids_positive_only,db = "ncbi", warn = FALSE)
  names(scinames_unique) <- unique_taxids_positive_only

  ## --> Add our custom taxids to the taxid-scientific_name map
  special_taxid_names_flipped <- names(special_taxid_names)
  names(special_taxid_names_flipped) <- special_taxid_names
  scinames_unique <- c(scinames_unique, special_taxid_names_flipped)

  ## --> convert our original taxids to their scientific names
  taxid_scinames <- scinames_unique[match(taxids, names(scinames_unique))]

  assertthat::assert_that(all(!is.na(taxid_scinames)),
                          msg = utilitybeltfmt::fmterror(
                            "taxid/s were not found in ncbi taxnomy database. Invalid taxids:\n",
                            paste0("\t", taxids[is.na(taxid_scinames)], collapse = "\n"),
                            "\n\nIf you're sure these are valid taxids, try updating the taxizedb ncbi taxonomy database by running taxizedb::db_download_ncbi(overwrite = TRUE)"
                          )
  )

  return(taxid_scinames)
}


#' Taxid to lineage
#'
#' Take an ncbi taxonomy id (e.g. 562 - E. coli) and return a taxanomic lineage string
#'
#' @param taxids ncbi taxonomy ids (numeric)
#' @param ranks_to_include which ranks to include in lineage string (character)
#' @param show_ranks Should ranks of each lineage level be added to (boolean)
#' @param ultimate_ancestor Text to add to (string)
#' @param special_taxid_names for taxids <= 0 you can define special names. (named numeric vector where names = names and values = taxids)
#'
#' @return '>' delimited lineage string for each taxid supplied (character)
#' @export
#'
#' @examples
#' \dontrun{
#' taxid2lineage(562, ranks_to_include=c("superkingdom","family", "genus", "species"))
#' # Bacteria>Enterobacteriaceae>Escherichia>Escherichia coli
#' }
taxid2lineage <- function(taxids, ranks_to_include = c("no rank", "superkingdom", "phylum", "class", "order", "family", "genus", "species", "strain"), show_ranks=FALSE, ultimate_ancestor = NULL, special_taxid_names = c("unclassified" = -1)) {

  valid_ranks = c("no rank", "superkingdom", "phylum", "class", "order", "family", "genus", "species", "strain")

  # Assertions
  assertthat::assert_that(all(ranks_to_include %in% valid_ranks),
                          msg = utilitybeltfmt::fmterror(
                            "Ranks must be one of ", paste0("[", valid_ranks, "]", collapse = " "),
                            "\n\nInvalid Ranks: \n", paste0(ranks_to_include[!ranks_to_include %in% valid_ranks])
                            ))

  unique_taxids <- unique(taxids)
  assertthat::assert_that(is.numeric(unique_taxids))
  assertthat::assert_that(all(!is.na(unique_taxids)), msg = utilitybeltfmt::fmterror("microbialsunburst::taxid2lineage\n\t taxids argument includes NA. This function expects no NA's in input. If NAs represent an inability to classify sequences to a taxid, replace it with -1. Its lineage will then return 'unclassified'"))

  # Grab only unique taxids > 0, and get taxid->classification information
  unique_taxids_gt_zero <- unique_taxids[unique_taxids > 0]
  unique_taxids_lt_or_equal_to_zero <- unique_taxids[unique_taxids <= 0]
  r=taxizedb::classification(x = unique_taxids_gt_zero, db = "ncbi", verbose = FALSE)

  assertthat::assert_that(all(!is.na(r)), msg = utilitybeltfmt::fmterror(
    "The following taxids were not found in the ncbi taxonomy database:\n",
    paste0("\t", unique_taxids_gt_zero[is.na(r)], collapse = "\n"),
    "\n\nIf you're sure these are valid NCBI taxids you can try updating ncbi taxonomy database by running `taxizedb::db_download_ncbi(overwrite = TRUE)`"
    ))

  # If there are taxids > 0 that aren't in our db - throw an error
  taxids_not_in_db <- names(r)[is.na(r) & names(r) > 0]

  if(length(taxids_not_in_db) > 0) {
    utilitybeltfmt::message_error(
      "The following taxids were not found in the ncbi taxonomy database:\n",
      paste0("\t", taxids_not_in_db, collapse = "\n")
    )
    utilitybeltfmt::message_error("\nIf you're sure these are valid NCBI taxids you can try updating ncbi taxonomy database by running `taxizedb::db_download_ncbi(overwrite = TRUE)`", symbol = FALSE)
    stop()
  }


  # For all unique taxids > 0, build a taxid -> lineage string vector map
  taxid_lineage = sapply(r, FUN = function(classification_df){
    #browser()
    if(any(is.na(classification_df))) { stop("cannot find ncbi taxonomy entry for one of the input taxids", names(classification_df)) }
    rows_of_interest = which(classification_df[["rank"]] %in% c(ranks_to_include))
    rows_of_interest = unique(vctrs::vec_c(rows_of_interest, nrow(classification_df), .ptype = numeric()))
    taxid_lineage_names = classification_df[["name"]][rows_of_interest]
    taxid_lineage_ranks = classification_df[["rank"]][rows_of_interest]


    # add rank in brackets next to each label
    if(show_ranks) taxid_lineage_names <- paste0(taxid_lineage_names, " (", taxid_lineage_ranks, ")")

    # add ultimate ancestor
    taxid_lineage_names = c(ultimate_ancestor, taxid_lineage_names)

    taxid_lineage_names_collapsed <- paste0(taxid_lineage_names, collapse = ">")
    return(taxid_lineage_names_collapsed)
  })

  #Make special taxids2lineage mapping list for taxids <=0
  special_taxid_mapping <- taxid2name(unique_taxids_lt_or_equal_to_zero, special_taxid_names = special_taxid_names)
  if(!is.null(ultimate_ancestor)) special_taxid_mapping <- paste0(ultimate_ancestor, ">", special_taxid_mapping)

  names(special_taxid_mapping) <- unique_taxids_lt_or_equal_to_zero


  # Merge special taxids2lineage with the one constructed for taxids > 0
  taxid_lineage <- vctrs::vec_c(taxid_lineage, special_taxid_mapping, .ptype = character())

  # extend our map to our special_taxid_names
  lineage = taxid_lineage[match(taxids, names(taxid_lineage))]

  return(lineage)
}
