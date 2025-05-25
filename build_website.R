# Script to build package website

library(httr)
library(jsonlite)
library(covr)
library(devtools)
library(pkgdown)
library(knitr)
library(rmarkdown)

# Set working directory to package root
setwd("/Users/janickweberpals/Documents/Projects/encore.analytics")

# Function to download files from GitHub
download_github_files <- function() {
  # GitHub API endpoint for the contents of the chapters directory
  api_url <- "https://api.github.com/repos/janickweberpals/imputation-ps-workflows/contents/chapters"
  
  # Get directory contents
  resp <- GET(api_url)
  if (status_code(resp) != 200) {
    stop("Failed to fetch directory contents from GitHub")
  }
  
  # Parse JSON response
  files <- fromJSON(rawToChar(resp$content))
  
  # Debug output
  print("API Response:")
  print(str(files))
  
  # Create articles directory
  articles_dir <- "vignettes/articles"
  dir.create(articles_dir, recursive = TRUE, showWarnings = FALSE)

  # Download and convert .qmd files
  qmd_files <- files[grepl("\\.qmd$", files$name), ]
  for (i in seq_len(nrow(qmd_files))) {
    download_url <- qmd_files$download_url[i]
    file_name <- qmd_files$name[i]
    base_name <- tools::file_path_sans_ext(basename(file_name))
    rmd_file <- file.path(articles_dir, paste0(base_name, ".Rmd"))
    
    # Download content
    temp <- tempfile()
    download.file(download_url, temp, quiet = TRUE)
    
    # Convert to R Markdown
    quarto_content <- xfun::read_utf8(temp)
    unlink(temp)
    
    # Process YAML header
    yaml_header <- c(
      "---",
      paste0("title: \"", base_name, "\""),
      "output:",
      "  rmarkdown::html_vignette:",
      "    toc: true",
      "vignette: >",
      paste0("  %\\VignetteIndexEntry{", base_name, "}"),
      "  %\\VignetteEngine{knitr::rmarkdown}",
      "  %\\VignetteEncoding{UTF-8}",
      "---",
      ""
    )
    
    # Convert Quarto code chunks to R Markdown
    body_content <- quarto_content
    if (length(body_content) > 0) {
      # Remove existing YAML if present
      if (body_content[1] == "---") {
        yaml_end <- which(body_content == "---")[2]
        body_content <- body_content[(yaml_end + 1):length(body_content)]
      }
      
      # Convert code chunks
      body_content <- gsub("```\\{([^}]+)\\}", "```{r \\1}", body_content)
    }
    
    # Write the R Markdown file
    writeLines(c(yaml_header, body_content), rmd_file)
    message("Converted: ", file_name, " -> ", basename(rmd_file))
  }
  
  # Create vignettes/articles directory if it doesn't exist
  articles_dir <- "vignettes/articles"
  dir.create(articles_dir, recursive = TRUE, showWarnings = FALSE)

  # Create _index.yml for articles
  index_yaml <- c(
    "---",
    "title: Articles",
    "---",
    "",
    "These articles demonstrate various features of encore.analytics:",
    ""
  )
  writeLines(index_yaml, file.path(articles_dir, "_index.yml"))

  # Download .qmd files
  qmd_files <- files[grepl("\\.qmd$", files$name), ]
  for (i in seq_len(nrow(qmd_files))) {
    download_url <- qmd_files$download_url[i]
    file_name <- qmd_files$name[i]
    base_name <- tools::file_path_sans_ext(basename(file_name))
    dest_file <- file.path(articles_dir, paste0(base_name, ".Rmd"))
    
    # Download and read content
    temp <- tempfile()
    download.file(download_url, temp, quiet = TRUE)
    qmd_content <- readChar(temp, file.info(temp)$size)
    unlink(temp)
    
    # Add YAML header for pkgdown if not present
    if (!grepl("^---\n", qmd_content)) {
      yaml_header <- paste0("---\ntitle: \"", base_name, "\"\ndescription: \"Documentation for ", base_name, "\"\n---\n\n")
      qmd_content <- paste0(yaml_header, qmd_content)
    }
    
    # Write processed content
    writeLines(qmd_content, dest_file, useBytes = TRUE)
    message("Downloaded and processed: ", file_name, " -> ", basename(dest_file))
  }
}

# Main execution
tryCatch({
  # Download and convert .qmd files
  download_github_files()
  
  # Create docs directory if it doesn't exist
  dir.create('docs', showWarnings = FALSE)
  
  # Generate coverage report
  tryCatch({
    covr::report(covr::package_coverage(), file = 'docs/coverage.html', browse = FALSE)
    message("Coverage report generated")
  }, error = function(e) {
    message("Warning: Failed to generate coverage report: ", e$message)
  })
  
  # Build website
  tryCatch({
    # Clean and recreate the docs directory
    if (dir.exists("docs")) {
      # Save coverage report if it exists
      coverage_exists <- file.exists("docs/coverage.html")
      if (coverage_exists) {
        file.copy("docs/coverage.html", "coverage.html.tmp")
      }
      
      # Remove and recreate docs
      unlink("docs", recursive = TRUE)
      dir.create("docs")
      
      # Restore coverage report if it existed
      if (coverage_exists) {
        file.copy("coverage.html.tmp", "docs/coverage.html")
        unlink("coverage.html.tmp")
      }
    }
    
    # Convert Quarto files to R Markdown
    qmd_files <- list.files("vignettes/articles", pattern = "\\.qmd$", full.names = TRUE)
    for (qmd_file in qmd_files) {
      rmd_file <- sub("\\.qmd$", ".Rmd", qmd_file)
      qmd_content <- readLines(qmd_file)
      
      # Check if YAML header exists and process it
      has_yaml <- length(qmd_content) >= 2 && qmd_content[1] == "---"
      if (has_yaml) {
        yaml_end <- which(qmd_content == "---")[2]
        yaml_content <- qmd_content[1:yaml_end]
        body_content <- qmd_content[(yaml_end + 1):length(qmd_content)]
      } else {
        yaml_content <- c("---",
                         paste0("title: \"", tools::file_path_sans_ext(basename(qmd_file)), "\""),
                         "output: rmarkdown::html_vignette",
                         "vignette: >",
                         "  %\\VignetteIndexEntry{", tools::file_path_sans_ext(basename(qmd_file)), "}",
                         "  %\\VignetteEngine{knitr::rmarkdown}",
                         "  %\\VignetteEncoding{UTF-8}",
                         "---")
        body_content <- qmd_content
      }
      
      # Convert code chunks
      body_content <- gsub("```\\{(.*?)\\}", "```{r \\1}", body_content)
      
      # Write the R Markdown file
      writeLines(c(yaml_content, "", body_content), rmd_file)
      message("Converted ", basename(qmd_file), " to ", basename(rmd_file))
      
      # Remove the original .qmd file after conversion
      unlink(qmd_file)
    }
    
    # Build site with debugging
    options(pkgdown.internet = FALSE) # Avoid external internet calls
    message("Building site with pkgdown...")
    
    # Initialize and build site
    pkgdown::clean_site()
    message("Site cleaned...")
    pkgdown::build_site(devel = FALSE, preview = FALSE, install = FALSE)
    message("Site built successfully")
  }, error = function(e) {
    message("Warning: Failed to build site: ", e$message)
  })
}, error = function(e) {
  message("Error occurred: ", e$message)
}, finally = {
  # Cleanup - remove downloaded files
  unlink("vignettes/articles/*.qmd")
  unlink("vignettes/articles/*.Rmd")
})
