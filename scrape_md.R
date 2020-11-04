easypackages::libraries("tidyverse", "rvest", "janitor")

reporting <- read_html("https://results.elections.maryland.gov/county_status_page_root.html") %>%
	html_text() %>%
	str_split("background-color", simplify = T)

reporting_split <- reporting[2] %>%
	str_split("\r\n")

reporting_table <- reporting_split[[1]][3:202]

reporting_df <- as.data.frame(matrix(reporting_table, ncol = 8, byrow = T), stringsAsFactors = F) %>%
	row_to_names(1) %>%
	clean_names() %>%
	separate(col = number_election_day_vote_center_scanners_reported1, into = c("reporting_centers", "total_centers"), sep = " of ") %>%
	mutate_at(vars(2:4), ~parse_number(.)) %>%
	mutate_all(~str_trim(.)) %>%
	mutate_at(vars(5:9), ~ifelse(. == "3Local", "Local results only", .))

pres <- read_html("https://results.elections.maryland.gov/elections/2020/results/General/gen_detail_results_2020_4_BOT001-.html") %>%
	html_node(xpath = '//*[@id="primary_right_col"]/div/div[2]/table') %>%
	html_table() %>%
	select(county = 1, trump = 2, biden = 3) %>%
	slice(-n()) %>%
	inner_join(select(reporting_df, county, reported = percent_election_day_results_reported2), by = "county")

boe_al <- read_html("https://results.elections.maryland.gov/elections/2020/results/general/gen_results_2020_4_by_county_160.html") %>%
	html_node(xpath = '//*[@id="primary_right_col"]/div/div[2]/table') %>%
	html_table() %>%
	select(-2) %>%
	clean_names() %>%
	slice(-n()) %>%
	mutate(total = as.numeric(total), percent = parse_number(percent))

boe_d2 <- read_html("https://results.elections.maryland.gov/elections/2020/results/general/gen_results_2020_4_by_county_160.html") %>%
	html_node(xpath = '//*[@id="primary_right_col"]/div/div[4]/table') %>%
	html_table() %>%
	select(-2) %>%
	clean_names() %>%
	slice(-n()) %>%
	mutate(total = as.numeric(total), percent = parse_number(percent))

boe_d4 <- read_html("https://results.elections.maryland.gov/elections/2020/results/general/gen_results_2020_4_by_county_160.html") %>%
	html_node(xpath = '//*[@id="primary_right_col"]/div/div[6]/table') %>%
	html_table() %>%
	select(-2) %>%
	clean_names() %>%
	slice(-n()) %>%
	mutate(total = as.numeric(total), percent = parse_number(percent))

ballot_a <- read_html("https://results.elections.maryland.gov/elections/2020/results/general/gen_qresults_2020_4_16_1.html") %>%
	html_node(xpath = '//*[@id="primary_right_col"]/div/div[2]/table') %>%
	html_table() %>%
	clean_names() %>%
	mutate(total = as.numeric(total), percentage = parse_number(percentage))

ballot_b <- read_html("https://results.elections.maryland.gov/elections/2020/results/general/gen_qresults_2020_4_16_1.html") %>%
	html_node(xpath = '//*[@id="primary_right_col"]/div/div[4]/table') %>%
	html_table() %>%
	clean_names() %>%
	mutate(total = as.numeric(total), percentage = parse_number(percentage))

ballot_c <- read_html("https://results.elections.maryland.gov/elections/2020/results/general/gen_qresults_2020_4_16_1.html") %>%
	html_node(xpath = '//*[@id="primary_right_col"]/div/div[6]/table') %>%
	html_table() %>%
	clean_names() %>%
	mutate(total = as.numeric(total), percentage = parse_number(percentage))

ballot_d <- read_html("https://results.elections.maryland.gov/elections/2020/results/general/gen_qresults_2020_4_16_1.html") %>%
	html_node(xpath = '//*[@id="primary_right_col"]/div/div[8]/table') %>%
	html_table() %>%
	clean_names() %>%
	mutate(total = as.numeric(total), percentage = parse_number(percentage))

write_csv(pres, "pres.csv")
write_csv(reporting_df, "reporting.csv")
write_csv(boe_al, "boe_al.csv")
write_csv(boe_d2, "boe_d2.csv")
write_csv(boe_d4, "boe_d4.csv")
write_csv(ballot_a, "ballot_a.csv")
write_csv(ballot_b, "ballot_b.csv")
write_csv(ballot_c, "ballot_c.csv")
write_csv(ballot_d, "ballot_d.csv")
