% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_param_txt.R
\name{get_param_txt}
\alias{get_param_txt}
\alias{get_ini_txt}
\alias{get_general_txt}
\alias{get_tmp_txt}
\alias{get_plant_txt}
\alias{get_tec_txt}
\alias{get_soil_txt}
\alias{get_station_txt}
\alias{get_usm_txt}
\title{Read STICS input parameters from text files}
\usage{
get_param_txt(
  workspace,
  param = NULL,
  plant_id = NULL,
  variety = NULL,
  value_id = NULL,
  exact = FALSE,
  stics_version = "latest",
  ...
)

get_ini_txt(file = "ficini.txt", stics_version)

get_general_txt(file = "tempopar.sti")

get_tmp_txt(file = "tempoparv6.sti")

get_plant_txt(file = "ficplt1.txt", variety = NULL)

get_tec_txt(
  file = "fictec1.txt",
  stics_version = "latest",
  several_fert = NULL,
  several_thin = NULL,
  is_pasture = NULL,
  ...
)

get_soil_txt(file = "param.sol", stics_version)

get_station_txt(file = "station.txt")

get_usm_txt(file = "new_travail.usm", plant_id = NULL)
}
\arguments{
\item{workspace}{Path of the workspace containing the STICS (txt)
input files.}

\item{param}{Vector of parameter names. Optional, if not provided,
the function returns an object with all parameters.}

\item{plant_id}{plant index (1, 2), default(NULL) calculated from from
plant number in STICS initialization file}

\item{variety}{Integer. The plant variety to get the parameter from.}

\item{value_id}{index of technical interventions to be used to
retrieve parameter values, or layer index for soil parameters}

\item{exact}{Boolean indicating if the function must return results
only for exact match.}

\item{stics_version}{An optional version name as listed in
get_stics_versions_compat() return}

\item{...}{Further arguments to pass (for future-proofing only)}

\item{file}{File path}

\item{several_fert}{Is there several fertilization in the USM ? See details.}

\item{several_thin}{Is there several thinning in the USM ? See details.}

\item{is_pasture}{Is the plant a pasture ? See details.}
}
\value{
A list of parameters value(s),
or if \code{param = NULL} a list of all parameters:
\item{ini}{Initialization parameters}
\item{general}{General parameters}
\item{tec}{Technical parameters}
\item{plant}{Plant parameters}
\item{soil}{Soil parameters}
\item{station}{Station parameters}

A list of parameters, depending on the file/function:
\item{ini}{Initialization parameters}
\item{general}{General parameters}
\item{tec}{Technical parameters}
\item{plant}{Plant parameters}
\item{soil}{Soil parameters}
\item{station}{Station parameters}
\item{tmp}{Temporary parameters}
}
\description{
Read STICS model input parameters from a usm in text format
(STICS input)
Generally used after calling building a usm with \code{JavaSTICS}.

Read a specific STICS model input parameter file.
Users would generally use the wrapper \code{get_param_txt()} instead.
}
\details{
If the \code{variety} is not given and a \code{param} is asked,
the function will return the values for the variety that is simulated in
the USM by checking the \code{variete} parameter in the technical file.
If \code{param} is not provided by the user, the values from all varieties
will be returned unless the user ask for a given \code{variety}.

\code{several_fert}, \code{several_thin} and \code{is_pasture} are read from
the tmp file (\code{tempoparv6.sti}). \code{get_param_txt()} does it automatically.
If you absolutely need to use directly \code{get_tec_txt}, please see example.
}
\note{
Users would generally use \code{get_param_txt} to identify parameters
names and values and pass them to other functions.

The functions are compatible with intercrops. Users generally only use
\code{get_param_txt()}, which is a wrapper for all these functions.
}
\examples{
path <- get_examples_path(file_type = "txt")

# Getting the interrow distance parameter value
get_param_txt(path, param = "interrang")

# Getting varietal parameters values
# Get the leaf lifespan of the variety used in the usm:
get_param_txt(workspace = path, param = "durvieF")
# Get the leaf lifespan of another variety available in the plant file:
get_param_txt(workspace = path, param = "durvieF", variety = "Furio")
# To get the values for several (or all) varieties, either put all varieties:
varieties <- c("Pactol", "Cherif", "Furio", "Dunia", "Volga", "Cecilia")
get_param_txt(workspace = path, param = "durvieF", variety = varieties)
# Or get it from the output of the function returning all parameters:
get_param_txt(workspace = path)$plant$plant1$durvieF

# Get parameters for a specific plant
get_param_txt(workspace = path, plant_id = 1)
get_param_txt(workspace = path, param = "durvieF", plant_id = 1)
get_param_txt(
  workspace = path, param = "durvieF", plant_id = 1,
  variety = varieties
)

# Get parameters for specific interventions or soil layers
get_param_txt(workspace = path, param = "amount", value_id = c(1, 3))
get_param_txt(workspace = path, param = "Hinitf", value_id = c(1, 3))
get_param_txt(workspace = path, param = "epc", value_id = c(1, 3))

\dontrun{
# Read the initialization file (ficini.txt):
library(SticsRFiles)
path <- file.path(get_examples_path(file_type = "txt"), "ficini.txt")
get_ini_txt(path)

# Read the tec file directly:

# First, get the parameters from the tmp file:
tmp <- get_tmp_txt(file = file.path(
  get_examples_path(file_type = "txt"),
  "tempoparv6.sti"
))
several_fert <- ifelse(tmp$option_engrais_multiple == 1, TRUE, FALSE)
several_thin <- ifelse(tmp$option_thinning == 1, TRUE, FALSE)
is_pasture <- ifelse(tmp$option_pature == 1, TRUE, FALSE)

# Then, get the technical parameters:
get_tec_txt(
  file = file.path(get_examples_path(file_type = "txt"), "fictec1.txt"),
  several_fert = several_fert, several_thin = several_thin,
  is_pasture = is_pasture
)
}

}
\seealso{
\code{gen_varmod()},

\code{get_param_txt()}.
}
