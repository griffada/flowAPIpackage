.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Are you on the network/VPN? It won't call gateway otherwise.")
}


utils::globalVariables(c("Start", "End", "Discrete", "Significance", "year"))
