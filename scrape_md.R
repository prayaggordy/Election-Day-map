easypackages::libraries("tidyverse", "rvest", "janitor")

pres <- read_html("https://results.elections.maryland.gov/elections/2020/results/General/gen_detail_results_2020_4_BOT001-.html") %>%
	html_node(xpath = '//*[@id="primary_right_col"]/div/div[2]/table') %>%
	html_table() %>%
	select(county = 1, trump = 2, biden = 3) %>%
	slice(-n())

# reporting <- read_html("https://results.elections.maryland.gov/county_status_page_root.html") %>%
# 	html_node(xpath = '//*[@id="mdgov_globalSiteWrapper"]/table') %>%
# 	html_table() %>%
# 	select(county = 1, trump = 2, biden = 3) %>%
# 	slice(-n())

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
# write_csv(reporting, "reporting.csv")
write_csv(boe_al, "boe_al.csv")
write_csv(boe_d2, "boe_d2.csv")
write_csv(boe_d4, "boe_d4.csv")
write_csv(ballot_a, "ballot_a.csv")
write_csv(ballot_b, "ballot_b.csv")
write_csv(ballot_c, "ballot_c.csv")
write_csv(ballot_d, "ballot_d.csv")
