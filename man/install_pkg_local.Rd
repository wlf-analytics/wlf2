% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/install_pkg_local.R
\name{install_pkg_local}
\alias{install_pkg_local}
\title{install_pkg_local}
\usage{
install_pkg_local(
  pkg = NULL,
  ask = FALSE,
  upgrade = FALSE,
  on_exit_restart = TRUE
)
}
\arguments{
\item{pkg}{Character. Name or path of local package to install. If characters don't lead to a valid path, the function checks for that package in the set git folder.}

\item{ask}{Logical. Whether to ask for confirmation when installing a different version of a package that is already installed. Installations that only add new packages never require confirmation.}

\item{upgrade}{Logical. When FALSE, the default, pak does the minimum amount of work to give you the latest version(s) of pkg. It will only upgrade dependent packages if pkg, or one of their dependencies explicitly require a higher version than what you currently have. It will also prefer a binary package over to source package, even it the binary package is older.}

\item{on_exit_restart}{Logical. Restart R session of install.}
}
\description{
Install local R library
}
