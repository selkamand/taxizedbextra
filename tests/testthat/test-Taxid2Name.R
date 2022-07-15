test_that("taxid2name works", {

  # Runs without error
  expect_error(taxid2name(561), NA)
  expect_length(taxid2name(561), n = 1)
  expect_length(taxid2name(c(561, 562)), n = 2)


  expect_error(taxid2name(-1000))
  expect_error(taxid2name(-1000, special_taxid_names = c("minus_1000" = -1000)), NA)
  expect_equal(ignore_attr = TRUE, taxid2name(-1000, special_taxid_names = c("minus_1000" = -1000)), expected = "minus_1000")
  expect_equal(ignore_attr = TRUE, taxid2name(c(-1000, -20), special_taxid_names = c("minus_1000" = -1000, "minus_20" = -20)), expected = c("minus_1000", "minus_20"))
  expect_error(taxid2name(c(-1000, 561), special_taxid_names = c("minus_1000" = -1000)), NA)

  expect_error(taxid2lineage(Inf), "valid")

})

test_that("taxid2lineage works", {

  # Runs without error
  expect_error(taxid2lineage(561), NA)
  expect_error(taxid2lineage(561, show_ranks = FALSE), NA)
  expect_error(taxid2lineage(561, show_ranks = TRUE), NA)
  expect_error(taxid2lineage(Inf), "valid")

  expect_length(taxid2lineage(c(561, 562)), n = 2)
  expect_true(grepl(taxid2lineage(561, ultimate_ancestor = "UltimateAncestor"), pattern = "^UltimateAncestor>"))
  expect_true(all(grepl(taxid2lineage(c(561, 562), ultimate_ancestor = "UltimateAncestor"), pattern = "^UltimateAncestor>")))

  # Negative taxids
  expect_error(taxid2lineage(-1000))
  expect_error(taxid2lineage(-1000, special_taxid_names = c("Negative One Thousand"=-1000)), NA)
  expect_true(grepl(taxid2lineage(-1000, special_taxid_names = c("Negative One Thousand"=-1000)), pattern = "Negative One Thousand$"))


  # Using the show ranks flag should result in a longer lineage path
  length_without_ranks = nchar(taxid2lineage(561, show_ranks = FALSE))
  length_with_ranks = nchar(taxid2lineage(561, show_ranks = TRUE))
  expect_gt(length_with_ranks, length_without_ranks)


  # Supplying valid rank names throws no errors
  expect_error(taxid2lineage(561, ranks_to_include = c("superkingdom", "species"), show_ranks = TRUE), NA)

  # Supplying invalid rank names throws error
  expect_error(taxid2lineage(561, ranks_to_include = c("billy", "bobby"), show_ranks = TRUE))

  # Test specific taxids: These tests will eventually break if ncbi taxonomy changes drastically
  expect_equal(ignore_attr = TRUE, taxid2lineage(562, ranks_to_include = c("superkingdom", "genus", "species"), ultimate_ancestor = NULL, show_ranks = FALSE), "Bacteria>Escherichia>Escherichia coli")
})
