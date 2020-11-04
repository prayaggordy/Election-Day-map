easypackages::libraries("tidyverse", "rvest", "janitor", "knitr", "kableExtra", "rmarkdown")

# reporting <- read_html("https://results.elections.maryland.gov/county_status_page_root.html") %>%
# 	html_text() %>%
# 	str_remove_all("\\*") %>%
# 	str_split("background-color", simplify = T)
#
# reporting_split_init <- reporting[2] %>%
# 	str_split("\r\n")
#
# reporting_table <- reporting_split_init[[1]][3:177]
#
# reporting_df <- as.data.frame(matrix(reporting_table, ncol = 7, byrow = T), stringsAsFactors = F) %>%
# 	row_to_names(1) %>%
# 	clean_names() %>%
# 	separate(col = number_election_day_vote_center_scanners_reported1, into = c("reporting_centers", "total_centers"), sep = " of ") %>%
# 	mutate_at(vars(2:4), ~parse_number(.)) %>%
# 	mutate_all(~str_trim(.)) %>%
# 	mutate_at(vars(5:ncol(.)), ~ifelse(. == "3Local", "Local results only", .))

pres <- read_html("https://results.elections.maryland.gov/elections/2020/results/General/gen_detail_results_2020_4_BOT001-.html") %>%
	html_node(xpath = '//*[@id="primary_right_col"]/div/div[2]/table') %>%
	html_table() %>%
	select(county = 1, trump = 2, biden = 3) %>%
	slice(-n()) %>%
	mutate_at(vars(trump, biden), ~parse_number(.)) %>%
	mutate(total_votes = trump + biden,
				 trump_pct = trump/total_votes,
				 biden_pct = biden/total_votes) %>%
	# inner_join(select(reporting_df, county, reported = percent_election_day_results_reported2), by = "county") %>%
	select(-total_votes)# %>%
	# mutate(reported = as.numeric(reported)/100)

boe_al <- read_html("https://results.elections.maryland.gov/elections/2020/results/general/gen_results_2020_4_by_county_160.html") %>%
	html_node(xpath = '//*[@id="primary_right_col"]/div/div[2]/table') %>%
	html_table() %>%
	select(-2) %>%
	clean_names() %>%
	slice(-n()) %>%
	mutate(total = parse_number(total), percent = parse_number(percent)) %>%
	select(1, percent) %>%
	pivot_wider(names_from = 1, values_from = percent) %>%
	select(1, 2) %>%
	mutate_all(~ paste0(., "%"))

boe_d2 <- read_html("https://results.elections.maryland.gov/elections/2020/results/general/gen_results_2020_4_by_county_160.html") %>%
	html_node(xpath = '//*[@id="primary_right_col"]/div/div[4]/table') %>%
	html_table() %>%
	select(-2) %>%
	clean_names() %>%
	slice(-n()) %>%
	mutate(total = parse_number(total), percent = parse_number(percent)) %>%
	select(1, percent) %>%
	pivot_wider(names_from = 1, values_from = percent) %>%
	mutate(Race = "Board of Education, At-Large") %>%
	select(1, 2) %>%
	mutate_all(~ paste0(., "%"))

boe_d4 <- read_html("https://results.elections.maryland.gov/elections/2020/results/general/gen_results_2020_4_by_county_160.html") %>%
	html_node(xpath = '//*[@id="primary_right_col"]/div/div[6]/table') %>%
	html_table() %>%
	select(-2) %>%
	clean_names() %>%
	slice(-n()) %>%
	mutate(total = parse_number(total), percent = parse_number(percent)) %>%
	select(1, percent) %>%
	pivot_wider(names_from = 1, values_from = percent) %>%
	mutate(Race = "Board of Education, At-Large") %>%
	select(1, 2) %>%
	mutate_all(~ paste0(., "%"))

ballot_a <- read_html("https://results.elections.maryland.gov/elections/2020/results/general/gen_qresults_2020_4_16_1.html") %>%
	html_node(xpath = '//*[@id="primary_right_col"]/div/div[2]/table') %>%
	html_table() %>%
	clean_names() %>%
	mutate(total = parse_number(total), percentage = parse_number(percentage)) %>%
	select(1, percentage) %>%
	pivot_wider(names_from = 1, values_from = percentage) %>%
	mutate(Question = "A: Limit tax rate increases") %>%
	select(Question, For, Against)

ballot_b <- read_html("https://results.elections.maryland.gov/elections/2020/results/general/gen_qresults_2020_4_16_1.html") %>%
	html_node(xpath = '//*[@id="primary_right_col"]/div/div[4]/table') %>%
	html_table() %>%
	clean_names() %>%
	mutate(total = parse_number(total), percentage = parse_number(percentage)) %>%
	select(1, percentage) %>%
	pivot_wider(names_from = 1, values_from = percentage) %>%
	mutate(Question = "B: Cap property tax rate increases at inflation") %>%
	select(Question, For, Against)

ballot_c <- read_html("https://results.elections.maryland.gov/elections/2020/results/general/gen_qresults_2020_4_16_1.html") %>%
	html_node(xpath = '//*[@id="primary_right_col"]/div/div[6]/table') %>%
	html_table() %>%
	clean_names() %>%
	mutate(total = parse_number(total), percentage = parse_number(percentage)) %>%
	select(1, percentage) %>%
	pivot_wider(names_from = 1, values_from = percentage) %>%
	mutate(Question = "C: Expand County Council by two seats") %>%
	select(Question, For, Against)

ballot_d <- read_html("https://results.elections.maryland.gov/elections/2020/results/general/gen_qresults_2020_4_16_1.html") %>%
	html_node(xpath = '//*[@id="primary_right_col"]/div/div[8]/table') %>%
	html_table() %>%
	clean_names() %>%
	mutate(total = parse_number(total), percentage = parse_number(percentage)) %>%
	select(1, percentage) %>%
	pivot_wider(names_from = 1, values_from = percentage) %>%
	mutate(Question = "D: Convert all at-large Council seats to district seats") %>%
	select(Question, For, Against)

ballot_questions <- rbind(ballot_a, ballot_b, ballot_c, ballot_d) %>%
	mutate(For = paste0(For, "%"), Against = paste0(Against, "%"))

# reporting_table_df <- unite(reporting_df, `Election Day Vote Center Scanners Reported`, c("reporting_centers", "total_centers"), sep = " of ") %>%
# 	select(County = 1, 2, `Election day percent reported` = 3) %>%
# 	mutate_at(vars(3), ~ paste0(., "%"))

write_csv(pres, "pres.csv")
# write_csv(reporting_df, "reporting.csv")
# write_csv(reporting_table_df, "reporting_table.csv")
write_csv(boe_al, "boe_al.csv")
write_csv(boe_d2, "boe_d2.csv")
write_csv(boe_d4, "boe_d4.csv")
write_csv(ballot_questions, "ballot_questions.csv")

render("tables.Rmd", params = list(fn = "boe_al"), output_file = "boe_al.html")
render("tables.Rmd", params = list(fn = "boe_d2"), output_file = "boe_d2.html")
render("tables.Rmd", params = list(fn = "boe_d4"), output_file = "boe_d4.html")
render("tables.Rmd", params = list(fn = "ballot_questions"), output_file = "ballot_questions.html")
# render("tables.Rmd", params = list(fn = "reporting_table_df"), output_file = "reporting.html")
