

######################### Not Used at this stage, but this is how i look for key words/phrases and tag a column wiht them

#----------------------------
# Meta-analysis search

# search_climate <- grep(toupper("climate change"),toupper(data_bib$AB))
# data_bib$Screen3_meta[search_climate] <- "1"
# search_policy <- grep(toupper("policy"),toupper(data_bib$AB))
# data_bib$Screen3_meta[search_policy] <- "4"


#----------------------------
# Impact type search -- THIS NEEDS TO APPEND NOT OVERWRITE
# DOESN"T WORK IN MAIN CODE ANYWAY 
# search_impact_fatal <- grep(toupper("death"), toupper(data_bib$AB))
# data_bib$Screen3_impact[search_impact_fatal] <- "1"
# search_impact_comm <- grep(toupper("community"), toupper(data_bib$AB))
# data_bib$Screen3_impact[search_impact_comm] <- "5"
# search_impact_infra <- grep(toupper("infrastructure"), toupper(data_bib$AB))
# data_bib$Screen3_impact[search_impact_infra] <- "6"


######################### SAMPLE 
# search_climate <- grep(toupper("climate change"),toupper(data_bib$TI))
# search_vulnerability <- grep(toupper("vulnerability") ,toupper(data_bib$TI))
# data_bib$TEST <- NA
# # This finds the titles with "climate change" OR "vulnerability" then puts "OR" into those rows of the TEST column
# # You are playing with the same output column so it will overwrite.
# data_bib$TEST [unique(c(search_climate,search_vulnerability))] <- "OR"
# # This finds the titles with "climate change" AND "vulnerability" then puts "AND" into those rows of the TEST column
# data_bib$TEST [intersect(search_climate,search_vulnerability)]  <- "AND" ### AND
# # But maybe you want both tags in your output file, so this will do both separated by ;
# data_bib$TEST2 <- NA
# # This finds the titles with "climate change" AND "vulnerability" then puts "AND" into those rows of the TEST column
# data_bib$TEST2 [intersect(search_climate,search_vulnerability)]  <- "AND" ### AND
# data_bib$TEST2 [unique(c(search_climate,search_vulnerability))] <-
#   paste(data_bib$TEST2[unique(c(search_climate,search_vulnerability))],"OR",sep=";")
# #print out the results
# data_bib[,c("TI","TEST","TEST2")]
