library(scholar)
library(data.table)

rm(list = ls())

########## Load and clean faculty data ##########
faculty = read.csv("./dataraw/UMCES_FacultyList_20200226_VL.csv",
                   na.strings = "",
                   stringsAsFactors = FALSE)
faculty = setDT(faculty)

### Fix Google IDs that keep getting messed up in CSV
#Yantao Li's ID starts with "-" and gets lost in CSV
faculty$GoogleAuthorID[faculty$Name == "Li, Yantao"] = "-vcO4LsAAAAJ"

#Get PreferredInitial + FamilyName that are used in publications
faculty = faculty[, ':=' ( FamilyName = gsub(",.*$", "", Name),
                           FirstName = gsub(".*, ", "", Name) )]
faculty = faculty[, ':=' ( PreferredInitial = substring(FirstName, 1, 1) )]
#correct some initials:
faculty$PreferredInitial[faculty$FamilyName == "Lyubchich"] = "V"
#reassign the name variable
faculty$Name = paste(faculty$PreferredInitial, faculty$FamilyName)
length(unique(faculty$Name)) == nrow(faculty) #check uniqueness


########## Collect data from Google Scholar ##########
#select faculty with Google Scholar profiles
facultyGS = faculty[!is.na(GoogleAuthorID),]
GSpubs = lapply(facultyGS$GoogleAuthorID, function(x)
    get_publications(x, pagesize = 100, flush = TRUE) )
names(GSpubs) = facultyGS$Name
saveRDS(GSpubs, file = paste0("./dataderived/GSpubs_", Sys.Date(), ".rds"))

# The following doesn't run because of too many requests(?)
# GScite = lapply(facultyGS$GoogleAuthorID, function(x)
#     get_citation_history(x) )
# names(GScite) = facultyGS$Name
# saveRDS(GScite, file = paste0("./dataderived/GScite_", Sys.Date(), ".rds"))


#number of publications retrieved from each Google Scholar profile
faculty$npubs = NA
faculty$npubs[!is.na(faculty$GoogleAuthorID)] = as.integer(sapply(GSpubs, function(x) nrow(x)))
summary(faculty$npubs)

### NOTE: not all authors are listed in the "author" field -- see the "..." cases
# i = 2
# tmp = GSpubs[[i]]$author
# # Hence, sometimes even the Google Scholar account owner is not listed as a coauthor:
# grepl(pattern = facultyGS$FamilyName[i], x = tmp, ignore.case = FALSE)
# # The function of retrieving all authors doew not run because of too many requests
# tmp2 = sapply(GSpubs[[i]]$pubid, function(x) {
#     #Sys.sleep(runif(1, min = 30, max = 60))
#     get_complete_authors(facultyGS$GoogleAuthorID[i], x) })
# # Need to update and replace GSdata[[i]]$author below with all authors.


########## Create adjacency matrix ##########
A = matrix(NA, nrow = nrow(faculty), ncol = nrow(faculty),
           dimnames = list(faculty$Name, faculty$Name))
for(i in 1:length(GSpubs)) { # i = 2; j = 3
    authors = paste0(GSpubs[[i]]$author, collapse = ", ")
    authorssep = unlist(strsplit(authors, ", "))
    authorssep2 = tolower(unlist(strsplit(authorssep, " ")))
    for(j in 1:nrow(faculty)) {
        #if there is a j-th family name among Google coauthors & initial before it
        if(is.element(tolower(faculty$FamilyName[j]), authorssep2)) {
            #authors with matching family name
            tmp = authorssep[grepl(faculty$FamilyName[j], authorssep)]
            #Strip the family name to get initial(s)
            # tmp = gsub(faculty$FamilyName[j], "", tmp) #this works for long names, potential error for short family names
            #-starting character for the family name
            startfn = regexpr(faculty$FamilyName[j], tmp)
            tmp = substr(tmp, 1, startfn-2) #just the initials
            #check that the preferred initial is present
            A[facultyGS$Name[i], j] = any(grepl(faculty$PreferredInitial[j], tmp))
        } else {
            A[facultyGS$Name[i], j] = FALSE
        }
    }
}
#the matrix A is likely not symmetric, but the network construction function symmetrizes it.
save(A, faculty, file = paste0("./dataderived/image_getcoauthors_", Sys.Date(), ".RData"))

