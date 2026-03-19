# Compatibility checks between version and update to target_version
check_upgrade_versions <- function(stics_version, target_version) {
  version_num <- get_version_num(stics_version)
  target_version_num <- get_version_num(target_version)
  min_version_num <- get_min_version_num(
    version_num,
    get_version_num(target_version)
  )
  status <- TRUE
  supported <- TRUE
  upgradable <- TRUE
  higher <- FALSE
  major_version_num <- get_major_version(version_num)
  major_min_version_num <- get_major_version(min_version_num)
  major_target_version_num <- get_major_version(target_version_num)
  same_version <- major_version_num == major_target_version_num

  if (major_min_version_num > major_version_num) {
    status <- FALSE
    supported <- FALSE
    upgradable <- FALSE
  } else if (major_version_num < major_min_version_num) {
    # supported est TRUE car on peut le faire en
    # étapes 9 -> 10 et 10 -> 11
    status <- FALSE
    upgradable <- FALSE
  } else if (major_version_num > major_target_version_num) {
    status <- FALSE
    upgradable <- FALSE
    higher <- TRUE
  }
  warn_msg <- ""
  if (!status) {
    if (!supported) {
      warn_msg <- paste(
        "The starting version",
        stics_version,
        "is not supported for any upgrade!"
      )
    } else if (!upgradable && !supported) {
      warn_msg <- paste(
        "The starting version",
        stics_version,
        "cannot be upgraded directly from ",
        target_version,
        "but in several steps from one to the other"
      )
    } else if (same_version) {
      warn_msg <-
        paste("The initial and the target major versions are the same")
    } else if (higher) {
      warn_msg <-
        paste("The initial major version is higher than the target major")
    }
  }
  attr(status, "warn_msg") <- warn_msg
  status
}

get_min_version_num <- function(version_num, target_version_num) {
  if (version_num < get_version_num(9)) {
    return(get_version_num(9))
  }

  if (
    version_num < get_version_num(10) &&
      target_version_num == get_version_num(10)
  ) {
    return(get_version_num(9))
  }

  if (
    version_num < get_version_num(11) &&
      target_version_num == get_version_num(11)
  ) {
    return(get_version_num(10))
  }
  return(version_num)
}
