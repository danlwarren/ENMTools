# =============================================================================
# TabPFN model download/cache utilities and Python environment setup
# =============================================================================

# Internal registry of available finetuned models
.tabpfn_models <- list(
  "sdm-finetuned-nonspatial" = list(
    url = "https://placeholder.zenodo.org/sdm_finetuned_nonspatial.ckpt",
    sha256 = "placeholder",
    filename = "sdm_finetuned_nonspatial.ckpt",
    size_mb = 41
  ),
  "sdm-finetuned-spatial" = list(
    url = "https://placeholder.zenodo.org/sdm_finetuned_spatial.ckpt",
    sha256 = "placeholder",
    filename = "sdm_finetuned_spatial.ckpt",
    size_mb = 41
  )
)

#' Get the TabPFN model cache directory
#'
#' Returns the path to the ENMTools cache directory used for storing
#' downloaded TabPFN model checkpoints. Creates the directory if it
#' doesn't exist.
#'
#' @return Character string with the path to the cache directory.
#' @export
tabpfn_cache_dir <- function() {
  cache_dir <- tools::R_user_dir("ENMTools", "data")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  cache_dir
}

#' Get the local path for a named TabPFN model
#'
#' Returns the local file path for a named finetuned model. If the model
#' has not been downloaded yet, it will be downloaded automatically.
#'
#' @param model_name Character string. Name of a finetuned model
#'   (e.g., "sdm-finetuned-nonspatial", "sdm-finetuned-spatial").
#'
#' @return Character string with the path to the model checkpoint file.
#' @export
tabpfn_model_path <- function(model_name) {
  if (!model_name %in% names(.tabpfn_models)) {
    stop("Unknown TabPFN model: '", model_name, "'. ",
         "Available models: ", paste(names(.tabpfn_models), collapse = ", "),
         call. = FALSE)
  }

  model_info <- .tabpfn_models[[model_name]]
  local_path <- file.path(tabpfn_cache_dir(), model_info$filename)

  if (!file.exists(local_path)) {
    message("Model '", model_name, "' not found locally. Downloading (~",
            model_info$size_mb, " MB)...")
    tabpfn_download_model(model_name)
  }

  local_path
}

#' Download a TabPFN model checkpoint
#'
#' Downloads a finetuned TabPFN model checkpoint to the local cache directory.
#' Verifies file integrity using SHA256 checksum.
#'
#' @param model_name Character string. Name of the model to download.
#' @param url Optional URL to download from. If NULL, uses the internal registry.
#' @param force If TRUE, re-download even if the file already exists.
#'
#' @return The local file path (invisibly).
#' @export
tabpfn_download_model <- function(model_name, url = NULL, force = FALSE) {
  if (!model_name %in% names(.tabpfn_models)) {
    stop("Unknown TabPFN model: '", model_name, "'. ",
         "Available models: ", paste(names(.tabpfn_models), collapse = ", "),
         call. = FALSE)
  }

  model_info <- .tabpfn_models[[model_name]]
  local_path <- file.path(tabpfn_cache_dir(), model_info$filename)

  if (file.exists(local_path) && !force) {
    message("Model '", model_name, "' already cached at: ", local_path)
    return(invisible(local_path))
  }

  download_url <- url %||% model_info$url

  if (grepl("placeholder", download_url)) {
    stop("Model download URLs have not been configured yet. ",
         "Please check for package updates or provide a URL manually via the 'url' argument.",
         call. = FALSE)
  }

  utils::download.file(download_url, local_path, mode = "wb")

  # Verify checksum if available
  if (model_info$sha256 != "placeholder") {
    file_hash <- tools::md5sum(local_path)
    # Use sha256 via openssl or digest if available
    if (requireNamespace("openssl", quietly = TRUE)) {
      file_hash <- as.character(openssl::sha256(file(local_path)))
    } else {
      warning("Cannot verify SHA256 checksum: 'openssl' package not available.",
              call. = FALSE)
      file_hash <- NULL
    }
    if (!is.null(file_hash) && file_hash != model_info$sha256) {
      unlink(local_path)
      stop("SHA256 checksum verification failed for '", model_name, "'. ",
           "The downloaded file has been removed.", call. = FALSE)
    }
  }

  message("Model '", model_name, "' downloaded to: ", local_path)
  invisible(local_path)
}

#' List available TabPFN models
#'
#' Shows information about available finetuned TabPFN models, including
#' whether they are cached locally.
#'
#' @return A data.frame with model information (invisibly).
#' @export
tabpfn_list_models <- function() {
  cache <- tabpfn_cache_dir()

  info <- data.frame(
    name = names(.tabpfn_models),
    filename = vapply(.tabpfn_models, function(x) x$filename, character(1)),
    size_mb = vapply(.tabpfn_models, function(x) x$size_mb, numeric(1)),
    cached = vapply(.tabpfn_models, function(x) {
      file.exists(file.path(cache, x$filename))
    }, logical(1)),
    stringsAsFactors = FALSE
  )

  cat("Available TabPFN finetuned models:\n")
  cat("Cache directory:", cache, "\n\n")
  print(info, row.names = FALSE)

  invisible(info)
}

#' Clear the TabPFN model cache
#'
#' Removes all cached TabPFN model checkpoint files.
#'
#' @return NULL (invisibly).
#' @export
tabpfn_clear_cache <- function() {
  cache <- tabpfn_cache_dir()
  files <- list.files(cache, pattern = "\\.(pt|ckpt)$", full.names = TRUE)

  if (length(files) == 0) {
    message("No cached TabPFN models found.")
    return(invisible(NULL))
  }

  unlink(files)
  message("Removed ", length(files), " cached model file(s) from: ", cache)
  invisible(NULL)
}


# =============================================================================
# Python environment setup
# =============================================================================

#' Install TabPFN Python package
#'
#' Installs the TabPFN Python package using [reticulate::py_install()].
#' This sets up a Python environment suitable for running TabPFN models
#' from R.
#'
#' @param envname Name of the Python environment to install into.
#'   Default is "r-tabpfn".
#' @param method Installation method passed to [reticulate::py_install()].
#' @param gpu If TRUE, also installs CUDA-compatible PyTorch.
#' @param api If TRUE, also installs the `tabpfn-client` package for
#'   API backend access.
#' @param ... Additional arguments passed to [reticulate::py_install()].
#'
#' @export
install.tabpfn <- function(envname = "r-tabpfn", method = "auto",
                            gpu = FALSE, api = FALSE, ...) {
  packages <- "tabpfn"

  if (api) {
    packages <- c(packages, "tabpfn-client")
  }

  reticulate::py_install(packages, envname = envname, method = method, ...)

  if (gpu) {
    message("For GPU support, ensure CUDA-compatible PyTorch is installed. ",
            "See https://pytorch.org/get-started/locally/ for instructions.")
  }
}

#' Check if TabPFN Python package is available
#'
#' Returns TRUE if the TabPFN Python package is installed and available
#' via reticulate. Does not initialize Python or have any side effects.
#'
#' @return Logical.
#' @export
check.tabpfn <- function() {
  reticulate::py_module_available("tabpfn")
}
