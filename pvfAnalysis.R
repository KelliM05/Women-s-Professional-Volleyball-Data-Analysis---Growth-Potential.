library(dplyr)
library(ggplot2) # data vis
library(rvest) # data conversion
library(purrr) # map_df

## Teams
`Atlanta Vibe` <- read_html("Atlanta Vibe - Season 2025 - Pro Volleyball Federation.html") # atl <- "Atlanta Vibe"
`Columbus Fury` <- read_html("Columbus Fury - Season 2025 - Pro Volleyball Federation.html") # col <- "Columbus Fury"
`Grand Rapids Rise` <- read_html("Grand Rapids Rise - Season 2025 - Pro Volleyball Federation.html") # gr <- "Grand Rapids Rise"
`Indy Ignite` <- read_html("Indy Ignite - Season 2025 - Pro Volleyball Federation.html") # in <-"Indy Ignite"
`Omaha Supernovas` <- read_html("Omaha Supernovas - Season 2025 - Pro Volleyball Federation.html") # om <- "Omaha Supernovas"
`Orlando Valkyries` <- read_html("Orlando Valkyries - Season 2025 - Pro Volleyball Federation.html") # or <- "Orlando Valkyries"
`San Diego Mojo` <- read_html("San Diego Mojo - Season 2025 - Pro Volleyball Federation.html") # san <- "San Diego Mojo"
`Vegas Thrill` <- read_html("Vegas Thrill - Season 2025 - Pro Volleyball Federation.html") #vg <-"Vegas Thrill"

# Atlanta
`Taylor Head` <- read_html("Taylor Head - Season 2025 - Pro Volleyball Federation.html")
`Marlie Monserez` <- read_html("Marlie Monserez - Season 2025 - Pro Volleyball Federation.html")
`Taya Beller` <- read_html("Taya Beller - Season 2025 - Pro Volleyball Federation.html")
# Columbus
`Jill Gillen` <- read_html("Jill Gillen - Season 2025 - Pro Volleyball Federation.html")
`Mary Georgiades` <- read_html("Mary Kate Georgiades - Season 2025 - Pro Volleyball Federation.html")
`Abby Walker` <- read_html("Abby Walker - Season 2025 - Pro Volleyball Federation.html")
# Grand Rapids
`Alyssa Jenson` <- read_html("Alyssa Jensen - Season 2025 - Pro Volleyball Federation.html")
`Alison Bastianelli` <- read_html("Alison Bastianelli - Season 2025 - Pro Volleyball Federation.html")
`Naya Shime` <- read_html("Naya Shime - Season 2025 - Pro Volleyball Federation.html")
# Indy
`Caroline Crawford` <- read_html("Caroline Crawford - Season 2025 - Pro Volleyball Federation.html")
`Elena Scott` <- read_html("Elena Scott - Season 2025 - Pro Volleyball Federation.html")
`Nina Cajic` <- read_html("Nina Cajic - Season 2025 - Pro Volleyball Federation.html")
# Omaha
`Allison Whitten` <- read_html("Allison Whitten - Season 2025 - Pro Volleyball Federation.html")
`Emily Londot` <- read_html("Emily Londot - Season 2025 - Pro Volleyball Federation.html")
`Natalia Valentin Anderson` <- read_html("Natalia Valentin-Anderson - Season 2025 - Pro Volleyball Federation.html")
# Orlando
`Lydia Grote` <- read_html("Lydia Grote - Season 2025 - Pro Volleyball Federation.html")
`Shannon Scully` <- read_html("Shannon Scully - Season 2025 - Pro Volleyball Federation.html")
`Norah Sis` <- read_html("Norah Sis - Season 2025 - Pro Volleyball Federation.html")
# San Diego
`Jenaisya Moore` <- read_html("Jenaisya Moore - Season 2025 - Pro Volleyball Federation.html")
`Da Yeong Lee` <- read_html("Da-yeong Lee - Season 2025 - Pro Volleyball Federation.html")
`Lauren Page` <- read_html("Lauren Page - Season 2025 - Pro Volleyball Federation.html")
# Vegas 
`Carly Graham` <- read_html("Carly Graham - Season 2025 - Pro Volleyball Federation.html")
`Layne Van Buskirk` <- read_html("Layne Van Buskirk - Season 2025 - Pro Volleyball Federation.html")
`Mary Shroll` <- read_html("Mary Shroll - Season 2025 - Pro Volleyball Federation.html")

atl_match_rows <- `Atlanta Vibe` %>% html_nodes(".table.match.club .table-row")

atl_match_stats <- map_df(atl_match_rows, table_layout <- function(row) {
  sum_vals <- row %>% html_nodes(".columns-item.sum") %>% html_text(trim = TRUE)
  error_vals <- row %>% html_nodes(".columns-item.error") %>% html_text(trim = TRUE)
  pkt_vals <- row %>% html_nodes(".columns-item.pkt") %>% html_text(trim = TRUE)
  perfect_vals <- row %>% html_nodes(".columns-item.perfect-ratio") %>% html_text(trim = TRUE)
  
  data.frame(
    "Opponent" = row %>% html_node(".match") %>% html_text(trim = TRUE),
    "Sets" = row %>% html_node(".sets") %>% html_text(trim = TRUE),
	
	  #totals
    "Total Points" = sum_vals[1],
	"Points BP" = row %>% html_node(".columns-item.bp") %>% html_text(trim = TRUE),
	"Points Bal" = row %>% html_node(".columns-item.win-loss") %>% html_text(trim = TRUE),
	  
	  #Serve
    "Total Serves" = sum_vals[2],
    "Service Error(s)" = error_vals[1],
	"Service Ace(s)" = row %>% html_node(".columns-item.ace") %>% html_text(trim = TRUE),
	"Serve Eff%" = row %>% html_node(".columns-item.serve-eff") %>% html_text(trim = TRUE),  
	  
	  #Reception
	"Reception Total" = sum_vals[3],
	"Reception Errors" = error_vals[2],
	"Reception Pos%" = row %>% html_node(".columns-item.pos-ratio") %>% html_text(trim = TRUE),
	"Reception Perf%" = perfect_vals[1],	  
	  
	  #Spike
	"Spike Total" = sum_vals[4],
	"Spike Errors" = error_vals[3],
    "Spike Blocks" = row %>% html_node(".columns-item.block") %>% html_text(trim = TRUE),
    "Spike Points" = pkt_vals[1],
    "Hitting %" = perfect_vals[2],
	"Spike Eff%" = row %>% html_node(".columns-item.spike-eff") %>% html_text(trim = TRUE),    
	  
	  #Block
    "Block Points" = pkt_vals[2],
	"Block Touches" = row %>% html_node(".columns-item.block-plus") %>% html_text(trim = TRUE),

	  
    stringsAsFactors = FALSE
  )
})

atl_match_stats

#col_fury
col_match_rows <- `Columbus Fury` %>% html_nodes(".table.match.club .table-row")
col_match_stats <- map_df(col_match_rows, table_layout)
#col_match_stats

#grr_rise
grr_match_rows <- `Grand Rapids Rise` %>% html_nodes(".table.match.club .table-row")
grr_match_stats <- map_df(grr_match_rows, table_layout)
#grr_match_stats

#ind_ignite
ind_match_rows <- `Indy Ignite` %>% html_nodes(".table.match.club .table-row")
ind_match_stats <- map_df(ind_match_rows, table_layout)
#ind_match_stats

#oma_supernovas
oma_match_rows <- `Omaha Supernovas` %>% html_nodes(".table.match.club .table-row")
oma_match_stats <- map_df(oma_match_rows, table_layout)
#oma_match_stats

#orl_valkyries
orl_match_rows <- `Orlando Valkyries` %>% html_nodes(".table.match.club .table-row")
orl_match_stats <- map_df(orl_match_rows, table_layout)
#orl_match_stats

#sds_mojo
sds_match_rows <- `San Diego Mojo` %>% html_nodes(".table.match.club .table-row")
sds_match_stats <- map_df(sds_match_rows, table_layout)
#sds_match_stats

#lav_thrill
lav_match_rows <- `Vegas Thrill` %>% html_nodes(".table.match.club .table-row")
lav_match_stats <- map_df(lav_match_rows, table_layout)
#lav_match_stats

th_match_rows <- `Taylor Head` %>% html_nodes(".table.match.club .table-row")
th_match_stats <- map_df(th_match_rows, table_layout)
th_match_stats

mm_match_rows <- `Marlie Monserez` %>% html_nodes(".table.match.club .table-row")
mm_match_stats <- map_df(mm_match_rows, table_layout)
#mm_match_stats

tb_match_rows <- `Taya Beller` %>% html_nodes(".table.match.club .table-row")
tb_match_stats <- map_df(tb_match_rows, table_layout)
#tb_match_stats


jg_match_rows <- `Jill Gillen` %>% html_nodes(".table.match.club .table-row")
jg_match_stats <- map_df(jg_match_rows, table_layout)
#jg_match_stats

mkg_match_rows <- `Mary Georgiades` %>% html_nodes(".table.match.club .table-row")
mkg_match_stats <- map_df(mkg_match_rows, table_layout)
#mkg_match_stats

aw_match_rows <- `Abby Walker` %>% html_nodes(".table.match.club .table-row")
aw_match_stats <- map_df(aw_match_rows, table_layout)
#aw_match_stats


aj_match_rows <- `Alyssa Jenson` %>% html_nodes(".table.match.club .table-row")
aj_match_stats <- map_df(aj_match_rows, table_layout)
#aj_match_stats

ab_match_rows <- `Alison Bastianelli` %>% html_nodes(".table.match.club .table-row")
ab_match_stats <- map_df(ab_match_rows, table_layout)
#ab_match_stats

ns_match_rows <- `Naya Shime` %>% html_nodes(".table.match.club .table-row")
ns_match_stats <- map_df(ns_match_rows, table_layout)
#ns_match_stats


cc_match_rows <- `Caroline Crawford` %>% html_nodes(".table.match.club .table-row")
cc_match_stats <- map_df(cc_match_rows, table_layout)
#cc_match_stats

es_match_rows <- `Elena Scott` %>% html_nodes(".table.match.club .table-row")
es_match_stats <- map_df(es_match_rows, table_layout)
#es_match_stats

nc_match_rows <- `Nina Cajic` %>% html_nodes(".table.match.club .table-row")
nc_match_stats <- map_df(nc_match_rows, table_layout)
#nc_match_stats


alw_match_rows <- `Allison Whitten` %>% html_nodes(".table.match.club .table-row")
alw_match_stats <- map_df(alw_match_rows, table_layout)
#alw_match_stats

el_match_rows <- `Emily Londot` %>% html_nodes(".table.match.club .table-row")
el_match_stats <- map_df(el_match_rows, table_layout)
#el_match_stats

nva_match_rows <- `Natalia Valentin Anderson` %>% html_nodes(".table.match.club .table-row")
nva_match_stats <- map_df(nva_match_rows, table_layout)
#nva_match_stats


lg_match_rows <- `Lydia Grote` %>% html_nodes(".table.match.club .table-row")
lg_match_stats <- map_df(lg_match_rows, table_layout)
#lg_match_stats

ss_match_rows <- `Shannon Scully` %>% html_nodes(".table.match.club .table-row")
ss_match_stats <- map_df(ss_match_rows, table_layout)
#ss_match_stats

nos_match_rows <- `Norah Sis` %>% html_nodes(".table.match.club .table-row")
nos_match_stats <- map_df(nos_match_rows, table_layout)
#nos_match_stats


jm_match_rows <- `Jenaisya Moore` %>% html_nodes(".table.match.club .table-row")
jm_match_stats <- map_df(jm_match_rows, table_layout)
#jm_match_stats

dyl_match_rows <- `Da Yeong Lee` %>% html_nodes(".table.match.club .table-row")
dyl_match_stats <- map_df(dyl_match_rows, table_layout)
#dyl_match_stats

lp_match_rows <- `Lauren Page` %>% html_nodes(".table.match.club .table-row")
lp_match_stats <- map_df(lp_match_rows, table_layout)
#lp_match_stats


cg_match_rows <- `Carly Graham` %>% html_nodes(".table.match.club .table-row")
cg_match_stats <- map_df(cg_match_rows, table_layout)
#cg_match_stats

lvb_match_rows <- `Layne Van Buskirk` %>% html_nodes(".table.match.club .table-row")
lvb_match_stats <- map_df(lvb_match_rows, table_layout)
#lvb_match_stats

ms_match_rows <- `Mary Shroll` %>% html_nodes(".table.match.club .table-row")
ms_match_stats <- map_df(ms_match_rows, table_layout)
#ms_match_stats

#kills
spike_graph <- function(team, team_name){
	# Graph Labels
	op <- team$Opponent
	opp <- head(op, -2)
	opps <- tail(opp,-1)
	Opponents <- gsub("\\s+", " ",opps)
	Opponents <- trimws(Opponents) 
	
	# Sum
	kills <- team$Spike.Total
	yval <- as.numeric(kills)
	values <- yval[!is.na(yval)]
	count <- head(values,-2) 
	# Errors
	error <- team$Spike.Errors
	yval2 <- as.numeric(error)
	values2 <- yval2[!is.na(yval2)]
	count2 <- head(values2,-2)
	# Blocks
	blocks <- team$Spike.Blocks
	yval3 <- as.numeric(blocks)
	values3 <- yval3[!is.na(yval3)]
	count3 <- head(values3,-2)
	# Points
	points <- team$Spike.Points
	yval4 <- as.numeric(points)
	values4 <- yval4[!is.na(yval4)]
	count4 <- head(values4,-2)
	# Hitting%
	hit <- team$Hitting..
	yval5 <- as.numeric(gsub("%","",hit))
	values5 <- yval5[!is.na(yval5)]
	count5 <- head(values5,-2)
	
	counts <- rbind(count5, #hitting%
					count2, # errors
					count3, # blocks
					count4, # kills
					count) # total atttempts 

	#totals
	t <- barplot(abs(counts),
		names.arg = Opponents,
		las=2,
		cex.names= 1,
        col = c("orange",
				"red",
			   "yellow",
			   "skyblue",
				"green"),
        border = "white",
        main = paste(team_name," Hitting Statistics"),
	   horiz=TRUE)
t
	
#text above bar
	max_x <- max(counts)
	if ( max_x < 5) {
for (i in 1:nrow(counts)) {
	for (j in 1:ncol(counts)) { 
			text(x =  i - 0.5,
				 labels = counts[i, j], 
				 y = t[j],
				 cex = 1, pos = 4)    
		}}} else if ( max_x < 20) {
for (i in 1:nrow(counts)) {
	for (j in 1:ncol(counts)) { 
			text(x = 0 + i,
				 labels = counts[i, j], 
				 y = t[j],
				 cex = 1, pos = 4)
	}}}else if ( max_x < 26) {
for (i in 1:nrow(counts)) {
	for (j in 1:ncol(counts)) { 
			text(x = 0 + (i*2),
				 labels = counts[i, j], 
				 y = t[j],
				 cex = 1, pos = 4)
	}}}else if ( max_x < 55) {
for (i in 1:nrow(counts)) {
	for (j in 1:ncol(counts)) { 
			text(x = 0 + (2*i^2),
				 labels = counts[i, j], 
				 y = t[j],
				 cex = 1, pos = 4)
	}}}else if ( max_x < 101) {
for (i in 1:nrow(counts)) {
	for (j in 1:ncol(counts)) { 
			text(x = 0 + (3*i^2),
				 labels = counts[i, j], 
				 y = t[j],
				 cex = 1, pos = 4)
	}}}else {
for (i in 1:nrow(counts)) {
	#for (j in 1:ncol(counts)) {
  text(x = 0 + 8*i^2,       
       y = t,                
       labels = counts[i, ],   
       cex = 1, pos = 4)        
   
}}

	
# Legend
	legend("topright",
       legend = c("Hitting %",
				  "Total Errors",
				  "Blocks",
				  "Kills", 
				  "Total Attempts"),
       fill = c("orange", #Hitting%
				"red", # errors
			   "yellow", # blocks
			   "skyblue", # total kills
				"green"), # points
       bty = "n")
	
	}
suppressWarnings(spike_graph(atl_match_stats, "Atlanta Vibe"))

#serve
serve_graph <- function(team, team_name){
	# Graph Labels
	op <- team$Opponent
	opp <- head(op, -2)
	opps <- tail(opp,-1)
	Opponents <- gsub("\\s+", " ",opps)
	Opponents <- trimws(Opponents) 
	
	 
	# Errors
	error <- team$Service.Error.s.
	yval2 <- as.numeric(error)
	values2 <- yval2[!is.na(yval2)]
	count1 <- head(values2,-2)
	# Aces
	blocks <- team$Service.Ace.s.
	yval3 <- as.numeric(blocks)
	values3 <- yval3[!is.na(yval3)]
	count2 <- head(values3,-2)
	# eff
	eff <- team$Serve.Eff.
	yval <- as.numeric(gsub("%","",eff))
	values <- yval[!is.na(yval)]
	count3 <- head(values,-2)
	# total
	tot <- team$Total.Serves
	yval4 <- as.numeric(tot)
	values4 <- yval4[!is.na(yval4)]
	count4 <- head(values4,-2)
	
	counts <- rbind(count1, 
					count2, 
					count3,
				   count4)
	
	#totals
	t <- barplot(abs(counts),
		names.arg = Opponents,
		las=2,
		cex.names= 1,
        col = c("red",
				"orange",
			   "pink",
			   "green"),
        border = "white",
        main = paste(team_name,"Serving Statistics"),
	   horiz=TRUE)

t
	

#text above bar
	max_x <- max(counts)
	if ( max_x < 5) {
for (i in 1:nrow(counts)) {
	for (j in 1:ncol(counts)) { 
			text(x =  i - 0.5,
				 labels = counts[i, j], 
				 y = t[j],
				 cex = 1, pos = 4)    
		}}} else if ( max_x < 15) {
for (i in 1:nrow(counts)) {
	for (j in 1:ncol(counts)) { 
			text(x = 0 + i,
				 labels = counts[i, j], 
				 y = t[j],
				 cex = 1, pos = 4)
	}}}else if ( max_x < 55) {
for (i in 1:nrow(counts)) {
	for (j in 1:ncol(counts)) { 
			text(x = 0 + (i^2),
				 labels = counts[i, j], 
				 y = t[j],
				 cex = 1, pos = 4)
	}}}else {
for (i in 1:nrow(counts)) {
	#for (j in 1:ncol(counts)) {
  text(x = 0 + 6*i^2,       
       y = t,                
       labels = counts[i, ],   
       cex = 1, pos = 4)        
   
}}

	
# Legend
	legend("topright",
       legend = c("Errors",
				  "Aces",
				  "Eff %",
				 "Total"),
       fill = c("red", 
				"orange", 
			   "pink",
			   "green"),
       bty = "n")
	
	}
suppressWarnings(serve_graph(atl_match_stats, "Atlanta Vibe"))

#Reception
recieve_graph <- function(team, team_name){
	# Graph Labels
	op <- team$Opponent
	opp <- head(op, -2)
	opps <- tail(opp,-1)
	Opponents <- gsub("\\s+", " ",opps)
	Opponents <- trimws(Opponents) 
	
	 
	# Errors
	error <- team$Reception.Errors
	yval <- as.numeric(error)
	values <- yval[!is.na(yval)]
	count <- head(values,-2)
	# total
	total <- team$Reception.Total
	yval2 <- as.numeric(total)
	values2 <- yval2[!is.na(yval2)]
	count2 <- head(values2,-2)
	# pos%
	pos <- team$Reception.Pos.
	yval3 <- as.numeric(gsub("%","",pos))
	values3 <- yval3[!is.na(yval3)]
	count3 <- head(values3,-2)
	# perf%
	perf <- team$Reception.Perf.
	yval4 <- as.numeric(gsub("%","",perf))
	values4 <- yval4[!is.na(yval4)]
	count4 <- head(values4,-2)
	
	counts <- rbind(count, count4, count3, count2)
	
	#totals
	t <- barplot(abs(counts),
		names.arg = Opponents,
		las=2,
		cex.names= 1,
        col = c("red",
				"orange",
			   "yellow",
			   "green"),
        border = "white",
        main = paste(team_name,"Reception Statistics"),
	   horiz=TRUE)
t
	
#text above bar
	max_x <- max(counts)
	if ( max_x == 1) {
for (i in 1:nrow(counts)) {
	for (j in 1:ncol(counts)) { 
			text(x =  0.5,
				 labels = counts[i, j], 
				 y = t[j],
				 cex = 1, pos = 4)    
		}}}else if ( max_x < 2) {
for (i in 1:nrow(counts)) {
	for (j in 1:ncol(counts)) { 
			text(x =  0 - 0.5,
				 labels = counts[i, j], 
				 y = t[j],
				 cex = 1, pos = 4)    
		}}}else if ( max_x < 5) {
for (i in 1:nrow(counts)) {
	for (j in 1:ncol(counts)) { 
			text(x =  i - 0.5,
				 labels = counts[i, j], 
				 y = t[j],
				 cex = 1, pos = 4)    
		}}} else if ( max_x < 15) {
for (i in 1:nrow(counts)) {
	for (j in 1:ncol(counts)) { 
			text(x = 0 + i*2,
				 labels = counts[i, j], 
				 y = t[j],
				 cex = 1, pos = 4)
	}}}else {
for (i in 1:nrow(counts)) {
	#for (j in 1:ncol(counts)) {
  text(x = 0 + 6*i^2,       
       y = t,                
       labels = counts[i, ],   
       cex = 1, pos = 4)        
   
}}
	
# Legend
	legend("topright",
       legend = c("Errors", "Perf %", "Pos %", "Totals"),
       fill = c("red", 
				"orange", 
			   "yellow",
			   "green"),
       bty = "n")
	
	}
suppressWarnings(recieve_graph(atl_match_stats, "Atlanta Vibe"))

# Blocks
block_graph <- function(team, team_name){
	# Graph Labels
	op <- team$Opponent
	opp <- head(op, -2)
	opps <- tail(opp,-1)
	Opponents <- gsub("\\s+", " ",opps)
	Opponents <- trimws(Opponents) 
	
	# Points
	points <- team$Block.Points
	yval <- as.numeric(points)
	values <- yval[!is.na(yval)]
	count <- head(values,-2)
	# Touches
	touches <- team$Block.Touches
	yval2 <- as.numeric(touches)
	values2 <- yval2[!is.na(yval2)]
	count2 <- head(values2,-2)
	
	counts <- rbind(count2, count)
	
	#totals
	t <- barplot(counts,
		names.arg = Opponents,
		las=2,
		cex.names= 1,
        col = c("skyblue",
				"violet"),
        border = "white",
        main = paste(team_name,"Blocking Statistics"),
	   horiz=TRUE)
	
	
#text above bar
max_x <- max(counts)

if ( max_x < 5) {
for (i in 1:nrow(counts)) {
	for (j in 1:ncol(counts)) { 
			text(x =  i - 0.5,
				 labels = counts[i, j], 
				 y = t[j],
				 cex = 1, pos = 4)    
		}}} else if ( max_x < 15) {
for (i in 1:nrow(counts)) {
	for (j in 1:ncol(counts)) { 
			text(x = 0 + i,
				 labels = counts[i, j], 
				 y = t[j],
				 cex = 1, pos = 4)
	}}}else {
		for (i in 1:nrow(counts)) {
	for (j in 1:ncol(counts)) { 
			text(x = 0 + (2*(i^3)),       
				 y = t[j],                
				 labels = counts[i, j],   
				 cex = 1, pos = 4) 
		}}}
	
# Legend
	legend("topright",
       legend = c("Touches",
				  "Points"),
       fill = c("skyblue", 
				"violet"),
       bty = "n")
	
	}
suppressWarnings(block_graph(atl_match_stats, "Atlanta Vibe"))

# Totals
totals_graph <- function(team, team_name){
	# Graph Labels
	op <- team$Opponent
	opp <- head(op, -2)
	opps <- tail(opp,-1)
	Opponents <- gsub("\\s+", " ",opps)
	Opponents <- trimws(Opponents) 
	
	 
	# Sets
	sets <- team$Sets
	yval4 <- as.numeric(sets)
	values4 <- yval4[!is.na(yval4)]
	count4 <- head(values4,-2)
	# Points
	points <- team$Total.Points
	yval <- as.numeric(points)
	values <- yval[!is.na(yval)]
	count <- head(values,-2)
	# BP
	bp <- team$Points.BP
	yval2 <- as.numeric(bp)
	values2 <- yval2[!is.na(yval2)]
	count2 <- head(values2,-2)
	# Bal
	bal <- team$Points.Bal
	yval3 <- as.numeric(bal)
	values3 <- yval3[!is.na(yval3)]
	count3 <- head(values3,-2)
	
	counts <- rbind(count4, count2, count3, count)
	
	#totals
	t <- barplot(counts,
		names.arg = Opponents,
		las=2,
		cex.names= 1,
        col = c("pink",
				"yellow",
				"skyblue",
			   "green"),
        border = "white",
        main = paste(team_name,"Totals"),
	   horiz=TRUE)
	t
	

#text above bar
	max_x <- max(counts)
	if ( max_x < 15) {
for (i in 1:nrow(counts)) {
	for (j in 1:ncol(counts)) { 
			text(x =  i*2 ,
				 labels = counts[i, j], 
				 y = t[j],
				 cex = 1, pos = 4)    
		}}} else if ( max_x < 50) {
for (i in 1:nrow(counts)) {
	for (j in 1:ncol(counts)) { 
			text(x = 0 + (2*i^2),
				 labels = counts[i, j], 
				 y = t[j],
				 cex = 1, pos = 4)
	}}}else {
for (i in 1:nrow(counts)) {
	#for (j in 1:ncol(counts)) {
  text(x = 0 + 6*i^2,       
       y = t,                
       labels = counts[i, ],   
       cex = 1, pos = 4)        
   
}}
# Legend
	legend("topright",
       legend = c("Sets Played",
				  "BP",
				  "Bal",
				 "Points Scored"),
       fill = c("pink",
		   "yellow",
				"skyblue",
			   "green"),
       bty = "n")
	
	}
suppressWarnings(totals_graph(atl_match_stats, "Atlanta Vibe"))

#Col Fury
suppressWarnings(spike_graph(col_match_stats, "Columbus Fury"))
suppressWarnings(serve_graph(col_match_stats, "Columbus Fury"))
suppressWarnings(recieve_graph(col_match_stats, "Columbus Fury"))
suppressWarnings(block_graph(col_match_stats, "Columbus Fury"))
suppressWarnings(totals_graph(col_match_stats, "Columbus Fury"))

#GRR Rise
suppressWarnings(spike_graph(grr_match_stats, "Grand Rapids Rise"))
suppressWarnings(serve_graph(grr_match_stats, "Grand Rapids Rise"))
suppressWarnings(recieve_graph(grr_match_stats, "Grand Rapids Rise"))
suppressWarnings(block_graph(grr_match_stats, "Grand Rapids Rise"))
suppressWarnings(totals_graph(grr_match_stats, "Grand Rapids Rise"))

#Indy Ignite
suppressWarnings(spike_graph(ind_match_stats, "Indy Ignite"))
suppressWarnings(serve_graph(ind_match_stats, "Indy Ignite"))
suppressWarnings(recieve_graph(ind_match_stats, "Indy Ignite"))
suppressWarnings(block_graph(ind_match_stats, "Indy Ignite"))
suppressWarnings(totals_graph(ind_match_stats, "Indy Ignite"))

#Oma Supernovas
suppressWarnings(spike_graph(oma_match_stats, "Omaha Supernovas"))
suppressWarnings(serve_graph(oma_match_stats, "Omaha Supernovas"))
suppressWarnings(recieve_graph(oma_match_stats, "Omaha Supernovas"))
suppressWarnings(block_graph(oma_match_stats, "Omaha Supernovas"))
suppressWarnings(totals_graph(oma_match_stats, "Omaha Supernovas"))

#Orlando Valks
suppressWarnings(spike_graph(orl_match_stats, "Orlando Valkyries"))
suppressWarnings(serve_graph(orl_match_stats, "Orlando Valkyries"))
suppressWarnings(recieve_graph(orl_match_stats, "Orlando Valkyries"))
suppressWarnings(block_graph(orl_match_stats, "Orlando Valkyries"))
suppressWarnings(totals_graph(orl_match_stats, "Orlando Valkyries"))

#SD Mojo - RIP
suppressWarnings(spike_graph(sds_match_stats, "San Diego Mojo"))
suppressWarnings(serve_graph(sds_match_stats, "San Diego Mojo"))
suppressWarnings(recieve_graph(sds_match_stats, "San Diego Mojo"))
suppressWarnings(block_graph(sds_match_stats, "San Diego Mojo"))
suppressWarnings(totals_graph(sds_match_stats, "San Diego Mojo"))

#Vegas Thrill
suppressWarnings(spike_graph(lav_match_stats, "Vegas Thrill"))
suppressWarnings(serve_graph(lav_match_stats, "Vegas Thrill"))
suppressWarnings(recieve_graph(lav_match_stats, "Vegas Thrill"))
suppressWarnings(block_graph(lav_match_stats, "Vegas Thrill"))
suppressWarnings(totals_graph(lav_match_stats, "Vegas Thrill"))

#Player Stats

#Taylor Head-Smith
suppressWarnings(spike_graph(th_match_stats, "Taylor Head"))
suppressWarnings(serve_graph(th_match_stats, "Taylor Head"))
suppressWarnings(recieve_graph(th_match_stats, "Taylor Head"))
suppressWarnings(block_graph(th_match_stats, "Taylor Head"))
suppressWarnings(totals_graph(th_match_stats, "Taylor Head"))

#Marlie Monserez
suppressWarnings(spike_graph(mm_match_stats, "Marlie Monserez"))
suppressWarnings(serve_graph(mm_match_stats, "Marlie Monserez"))
suppressWarnings(recieve_graph(mm_match_stats, "Marlie Monserez"))
suppressWarnings(block_graph(mm_match_stats, "Marlie Monserez"))
suppressWarnings(totals_graph(mm_match_stats, "Marlie Monserez"))

#Taya Beller
suppressWarnings(spike_graph(tb_match_stats, "Taya Beller"))
suppressWarnings(serve_graph(tb_match_stats, "Taya Beller"))
suppressWarnings(recieve_graph(tb_match_stats, "Taya Beller"))
suppressWarnings(block_graph(tb_match_stats, "Taya Beller"))
suppressWarnings(totals_graph(tb_match_stats, "Taya Beller"))

#Jill Gillen
suppressWarnings(spike_graph(jg_match_stats, "Jill Gillen"))
suppressWarnings(serve_graph(jg_match_stats, "Jill Gillen"))
suppressWarnings(recieve_graph(jg_match_stats, "Jill Gillen"))
suppressWarnings(block_graph(jg_match_stats, "Jill Gillen"))
suppressWarnings(totals_graph(jg_match_stats, "Jill Gillen"))

#Mary Kate Georgiades
#suppressWarnings(spike_graph(mkg_match_stats, "Mary Kate Georgiades"))
#suppressWarnings(serve_graph(mkg_match_stats, "Mary Kate Georgiades"))
suppressWarnings(recieve_graph(mkg_match_stats, "Mary Kate Georgiades"))
#suppressWarnings(block_graph(mkg_match_stats, "Mary Kate Georgiades"))
suppressWarnings(totals_graph(mkg_match_stats, "Mary Kate Georgiades"))

#Abby Walker
suppressWarnings(spike_graph(aw_match_stats, "Abby Walker"))
suppressWarnings(serve_graph(aw_match_stats, "Abby Walker"))
suppressWarnings(recieve_graph(aw_match_stats, "Abby Walker"))
suppressWarnings(block_graph(aw_match_stats, "Abby Walker"))
suppressWarnings(totals_graph(aw_match_stats, "Abby Walker"))

#Alyssa Jenson
suppressWarnings(spike_graph(aj_match_stats, "Alyssa Jenson"))
suppressWarnings(serve_graph(aj_match_stats, "Alyssa Jenson"))
suppressWarnings(recieve_graph(aj_match_stats, "Alyssa Jenson"))
suppressWarnings(block_graph(aj_match_stats, "Alyssa Jenson"))
suppressWarnings(totals_graph(aj_match_stats, "Alyssa Jenson"))

#Alison Bastianelli
suppressWarnings(spike_graph(ab_match_stats, "Alison Bastianelli"))
suppressWarnings(serve_graph(ab_match_stats, "Alison Bastianelli"))
suppressWarnings(recieve_graph(ab_match_stats, "Alison Bastianelli"))
suppressWarnings(block_graph(ab_match_stats, "Alison Bastianelli"))
suppressWarnings(totals_graph(ab_match_stats, "Alison Bastianelli"))

#Naya Shime
suppressWarnings(spike_graph(ns_match_stats, "Naya Shime"))
suppressWarnings(serve_graph(ns_match_stats, "Naya Shime"))
#suppressWarnings(recieve_graph(ns_match_stats, "Naya Shime"))
suppressWarnings(block_graph(ns_match_stats, "Naya Shime"))
suppressWarnings(totals_graph(ns_match_stats, "Naya Shime"))

#Caroline Crawford
suppressWarnings(spike_graph(cc_match_stats, "Caroline Crawford"))
suppressWarnings(serve_graph(cc_match_stats, "Caroline Crawford"))
suppressWarnings(recieve_graph(cc_match_stats, "Caroline Crawford"))
suppressWarnings(block_graph(cc_match_stats, "Caroline Crawford"))
suppressWarnings(totals_graph(cc_match_stats, "Caroline Crawford"))

#Elena Scott
#suppressWarnings(spike_graph(es_match_stats, "Elena Scott"))
suppressWarnings(serve_graph(es_match_stats, "Elena Scott"))
suppressWarnings(recieve_graph(es_match_stats, "Elena Scott"))
#suppressWarnings(block_graph(es_match_stats, "Elena Scott"))
suppressWarnings(totals_graph(es_match_stats, "Elena Scott"))

#Nina Cajic
suppressWarnings(spike_graph(nc_match_stats, "Nina Cajic"))
suppressWarnings(serve_graph(nc_match_stats, "Nina Cajic"))
suppressWarnings(recieve_graph(nc_match_stats, "Nina Cajic"))
suppressWarnings(block_graph(nc_match_stats, "Nina Cajic"))
suppressWarnings(totals_graph(nc_match_stats, "Nina Cajic"))

#Allison Whitten
#suppressWarnings(spike_graph(alw_match_stats, "Allison Whitten"))
suppressWarnings(serve_graph(alw_match_stats, "Allison Whitten"))
suppressWarnings(recieve_graph(alw_match_stats, "Allison Whitten"))
#suppressWarnings(block_graph(alw_match_stats, "Allison Whitten"))
suppressWarnings(totals_graph(alw_match_stats, "Allison Whitten"))

#Emily Londot
suppressWarnings(spike_graph(el_match_stats, "Emily Londot"))
suppressWarnings(serve_graph(el_match_stats, "Emily Londot"))
suppressWarnings(recieve_graph(el_match_stats, "Emily Londot"))
suppressWarnings(block_graph(el_match_stats, "Emily Londot"))
suppressWarnings(totals_graph(el_match_stats, "Emily Londot"))

#Natalia Valentin-Anderson
suppressWarnings(spike_graph(nva_match_stats, "Natalia Valentin-Anderson"))
suppressWarnings(serve_graph(nva_match_stats, "Natalia Valentin-Anderson"))
suppressWarnings(recieve_graph(nva_match_stats, "Natalia Valentin-Anderson"))
suppressWarnings(block_graph(nva_match_stats, "Natalia Valentin-Anderson"))
suppressWarnings(totals_graph(nva_match_stats, "Natalia Valentin-Anderson"))

#Lydia Grote
suppressWarnings(spike_graph(lg_match_stats, "Lydia Grote"))
suppressWarnings(serve_graph(lg_match_stats, "Lydia Grote"))
#suppressWarnings(recieve_graph(lg_match_stats, "Lydia Grote"))
suppressWarnings(block_graph(lg_match_stats, "Lydia Grote"))
suppressWarnings(totals_graph(lg_match_stats, "Lydia Grote"))

#Shannon Scully
suppressWarnings(spike_graph(ss_match_stats, "Shannon Scully"))
suppressWarnings(serve_graph(ss_match_stats, "Shannon Scully"))
suppressWarnings(recieve_graph(ss_match_stats, "Shannon Scully"))
suppressWarnings(block_graph(ss_match_stats, "Shannon Scully"))
suppressWarnings(totals_graph(ss_match_stats, "Shannon Scully"))

#Norah Sis
suppressWarnings(spike_graph(nos_match_stats, "Norah Sis"))
suppressWarnings(serve_graph(nos_match_stats, "Norah Sis"))
suppressWarnings(recieve_graph(nos_match_stats, "Norah Sis"))
suppressWarnings(block_graph(nos_match_stats, "Norah Sis"))
suppressWarnings(totals_graph(nos_match_stats, "Norah Sis"))

#Jenaisya Moore
suppressWarnings(spike_graph(jm_match_stats, "Jenaisya Moore"))
suppressWarnings(serve_graph(jm_match_stats, "Jenaisya Moore"))
suppressWarnings(recieve_graph(jm_match_stats, "Jenaisya Moore"))
suppressWarnings(block_graph(jm_match_stats, "Jenaisya Moore"))
suppressWarnings(totals_graph(jm_match_stats, "Jenaisya Moore"))

#Da-Yeong Lee
suppressWarnings(spike_graph(dyl_match_stats, "Da-Yeong Lee"))
suppressWarnings(serve_graph(dyl_match_stats, "Da-Yeong Lee"))
suppressWarnings(recieve_graph(dyl_match_stats, "Da-Yeong Lee"))
suppressWarnings(block_graph(dyl_match_stats, "Da-Yeong Lee"))
suppressWarnings(totals_graph(dyl_match_stats, "Da-Yeong Lee"))

#Lauren Page
suppressWarnings(spike_graph(lp_match_stats, "Lauren Page"))
suppressWarnings(serve_graph(lp_match_stats, "Lauren Page"))
suppressWarnings(recieve_graph(lp_match_stats, "Lauren Page"))
suppressWarnings(block_graph(lp_match_stats, "Lauren Page"))
suppressWarnings(totals_graph(lp_match_stats, "Lauren Page"))

#Carly Graham 
suppressWarnings(spike_graph(cg_match_stats, "Carly Graham"))
suppressWarnings(serve_graph(cg_match_stats, "Carly Graham"))
#suppressWarnings(recieve_graph(cg_match_stats, "Carly Graham"))
suppressWarnings(block_graph(cg_match_stats, "Carly Graham"))
suppressWarnings(totals_graph(cg_match_stats, "Carly Graham"))

#Layne Van Burkirk
suppressWarnings(spike_graph(lvb_match_stats, "Layne Van Burkirk"))
suppressWarnings(serve_graph(lvb_match_stats, "Layne Van Burkirk"))
suppressWarnings(recieve_graph(lvb_match_stats, "Layne Van Burkirk"))
suppressWarnings(block_graph(lvb_match_stats, "Layne Van Burkirk"))
suppressWarnings(totals_graph(lvb_match_stats, "Layne Van Burkirk"))

#Mary Shroll
#suppressWarnings(spike_graph(ms_match_stats, "Mary Shroll"))
#suppressWarnings(serve_graph(ms_match_stats, "Mary Shroll"))
suppressWarnings(recieve_graph(ms_match_stats, "Mary Shroll"))
#suppressWarnings(block_graph(ms_match_stats, "Mary Shroll"))
suppressWarnings(totals_graph(ms_match_stats, "Mary Shroll"))

#2024-2025 SeasonComps

omh24 <- read_html("Omaha Supernovas - Season 2024 - Pro Volleyball Federation.html")
oma24_match_rows <- omh24 %>% html_nodes(".table.match.club .table-row")
oma24_match_stats <- map_df(oma24_match_rows, table_layout)
#oma24_match_stats

suppressWarnings(spike_graph(oma24_match_stats, "Omaha - 2024"))
suppressWarnings(serve_graph(oma24_match_stats, "Omaha - 2024"))
suppressWarnings(recieve_graph(oma24_match_stats, "Omaha - 2024"))
suppressWarnings(block_graph(oma24_match_stats, "Omaha - 2024"))
suppressWarnings(totals_graph(oma24_match_stats, "Omaha - 2024"))

ops <- head(oma_match_stats$Opponent, -2) %>% 
  tail(-1) %>% 
  gsub("\\s+", " ", .) %>% 
  trimws()

kp <- head(oma_match_stats$Spike.Points,-2) %>%
	tail(-1) %>%
	as.numeric()

oms <- data.frame(ops, kp)

ggplot(oms, aes(x =ops, y = kp)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1) +
  labs(title = "Kills by Opponent - 25'",
       x = "Opponent",
       y = "Kills") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


om24ops <- head(oma24_match_stats$Opponent,-2) %>% 
tail(-1) %>%
  gsub("\\s+", " ", .) %>% 
  trimws()

om24kp <- head(oma24_match_stats$Spike.Points,-2) %>%
tail(-1) %>%
	as.numeric()

oms24 <- data.frame(om24ops, om24kp)

ggplot(oms24, aes(x =om24ops, y = om24kp)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1) +
  labs(title = "Kills by Opponent - 24'",
       x = "Opponent",
       y = "Kills") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


jill_gill24 <- read_html("Jill Gillen - Season 2024 - Pro Volleyball Federation.html")
jg24_match_rows <- jill_gill24 %>% html_nodes(".table.match.club .table-row")
jg24_match_stats <- map_df(jg24_match_rows, table_layout)
#jg24_match_stats

suppressWarnings(spike_graph(jg24_match_stats, "Jill Gillen - 2024"))
suppressWarnings(serve_graph(jg24_match_stats, "Jill Gillen - 2024"))
suppressWarnings(recieve_graph(jg24_match_stats, "Jill Gillen - 2024"))
suppressWarnings(block_graph(jg24_match_stats, "Jill Gillen - 2024"))
suppressWarnings(totals_graph(jg24_match_stats, "Jill Gillen - 2024"))


jops <- head(jg_match_stats$Opponent,-2) %>% 
tail(-1) %>%
  gsub("\\s+", " ", .) %>% 
  trimws()

jkp <- head(jg_match_stats$Spike.Points,-2) %>%
tail(-1) %>%
	as.numeric()

jgms <- data.frame(jops, jkp)

ggplot(jgms, aes(x =jops, y = jkp)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1) +
  labs(title = "Kills by Opponent - 25'",
       x = "Opponent",
       y = "Kills") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


j24ops <- head(jg24_match_stats$Opponent,-2) %>% 
tail(-1) %>%
  gsub("\\s+", " ", .) %>% 
  trimws()

j24kp <- head(jg24_match_stats$Spike.Points,-2) %>%
tail(-1) %>%
	as.numeric()

j24gms <- data.frame(j24ops, j24kp)

ggplot(j24gms, aes(x =j24ops, y = j24kp)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1) +
  labs(title = "Kills by Opponent - 24'",
       x = "Opponent",
       y = "Kills") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))