## Read data and make valid col names
file_name <- "Annalect_case_data_coding_and_simple_regression_2023.xlsx"
data_in <- readxl::read_excel(paste0("./data/raw/", file_name),
  skip = 12,
  col_names = TRUE
) %>%
  select(-1:-5) %>%
  janitor::clean_names()

data <- data_in %>%
  ## create column for week and year, fill missing year vals
  mutate(
    year = ifelse(grepl("-", year_week), substr(year_week, 1, 4), NA),
    week = ifelse(grepl("-", year_week), substr(year_week, 6, nchar(year_week)), year_week)
  ) %>%
  fill(year) %>%
  ## in case there are years with 53 weeks, let's filter them
  filter(week != 53) %>%
  ## Make data column
  mutate(date = as.Date(strptime(paste0(year, "-W", week, "-1"), format = "%Y-W%U-%u"))) %>%
  ## Fill NA of competitor 4 with 0
  mutate(competitor_4 = replace_na(competitor_4, 0)) %>% 
  ## Combine holiday, own and competitor campaign features into one
  mutate(
    holiday = rowSums(select(., christmas:pinse_pentecost)),
    own_campaigns = rowSums(select(., campaign_1:campaign_18)),
    campaigns_sum = rowSums(select(., campaign_1:competitor_7)),
    competitor_campaigns_mean = rowMeans(select(., competitor_1:competitor_7)),
    competitor_campaigns_sum = rowSums(select(., competitor_1:competitor_7))
  ) %>%
  ## assemble tidy df
  select(
    year,
    week,
    date,
    pizza,
    price_offer, 
    holiday, 
    temperature:sun, 
    own_campaigns, 
    campaigns_sum,
    competitor_campaigns_mean, 
    competitor_campaigns_sum, 
    competitor_1:competitor_7
  )

feather::write_feather(data, "./data/processed/data_clean.feather")
