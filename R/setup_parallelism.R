#' Sets up a parallelism cluster
#'
#' @description Read STICS observation or simulation files from a
#' JavaSTICS workspace and store data into a list per usm.
#' Used by `get_obs()` and `get_sim()`.
#'
#' @param inputs_number      Number of inputs
#' @param cores              Number of cores to use for parallel computation.
#'
#' @details The `.obs` files names should match USMs names, e.g. for a
#' usm called "banana", the `.obs` file should be named `banana.obs`.
#' For intercrops, the name should be suffixed by "p" for the principal
#' and "a" for the associated plant.
#'
#' @importFrom foreach %dopar% %do%
#' @importFrom parallel clusterCall makeCluster
#' @importFrom doParallel registerDoParallel
#'
#' @keywords internal
#'
#' @noRd
#'
setup_parallelism <- function(inputs_number, cores = NA) {
  # Managing parallel model simulations
  # Managing cores number to use
  cores_nb <- get_cores_nb(parallel = TRUE, required_nb = cores)

  # Do not allow more cores than number of inputs: waste of time
  cores_nb <- min(cores_nb, inputs_number)

  # Launching the cluster
  cl <- makeCluster(cores_nb)

  # Registering cluster
  registerDoParallel(cl)
  clusterCall(cl, function(x) .libPaths(x), .libPaths())
  cl
}
