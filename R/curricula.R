test_url <- function(url) {

   message(paste0("Testing: ", url))

   url_status <- try(httr::GET(url), silent = TRUE)

   # Fails if host can't be resolved

   status <- ifelse(suppressMessages(grepl("Could not resolve host", url_status)), "failed", "success")

   if (status == "success") {

     # Fails if 404'ed

     status <- ifelse(try(url_status$status_code, silent = TRUE) == 404, "failed", "success")

   }

   return(status)

}


keyword_count <- function(url, string) {

  attempted_url <- try(download.file(url, destfile = "site", method = "wget", extra = "--no-check-certificate"), silent = TRUE)

  if(class(attempted_url) != "try-error") {

    html <- read_html("site")

    text <- html_text(html)

    return(str_count(text, string))

  } else{

    error <- try(status_code(GET(url)))

    if(class(error) != "try-error") {

      return(paste("ERROR", error))

    } else{

      return(NA)

    }

    }

}
