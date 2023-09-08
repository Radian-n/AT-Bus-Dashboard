library(tidyverse)
library(jsonlite)
library(httr2)


req <- request("https://api.at.govt.nz/gtfs/v3/routes HTTP/1.1")


req <- request("https://api.at.govt.nz/gtfs/v3/routes")

req <- req %>%
  req_headers("Cache-Control" = "no-cache",
              "Ocp-Apim-Subscription-Key" = "374561ce70294e88a20a409969f1aac5")

req_dry_run(req)

resp <- req_perform(req)

t2 <- resp %>%
  resp_body_json() %>%
  as_tibble() %>%
  flatten()



read_json(rep2$data)

resp$body

rep2 <- resp%>% resp_body_json()
test <- data.frame(rep2$data)
