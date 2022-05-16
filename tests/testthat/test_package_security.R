# Scan current package DESCRIPTION
#nybem_vuln_pkgs <- oysteR::audit_description()

# Examine vulnerabilities
#vuln <- get_vulnerabilities(nybem_vuln_pkgs)



test_that("Test expect_secure", {
  skip_on_cran()
  ## Tests function and this package
  oysteR::expect_secure("nybem")
})
