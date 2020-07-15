library(scholar)

rm(list = ls())
faculty = read.csv("./data/UMCES_FacultyList_20200226_VL.csv", 
                   na.strings = "",
                   stringsAsFactors = FALSE)
#Yantao Li "-vcO4LsAAAAJ"
#create FacultyID for easy referencing
faculty$FamilyName = gsub(",.*$", "", faculty$Name)
faculty$FamilyName[faculty$FamilyName == "Li"] = paste0(substring(sapply(strsplit(faculty$Name[faculty$FamilyName == "Li"], ", "), "[", 2), 1, 1), " Li")
faculty$FamilyName[faculty$FamilyName == "Zhang"] = paste0(substring(sapply(strsplit(faculty$Name[faculty$FamilyName == "Zhang"], ", "), "[", 2), 1, 1), " Zhang")
length(unique(faculty$FamilyName)) == nrow(faculty) #check uniqueness

#select faculty with Google Scholar profiles
fGS = faculty[!is.na(faculty$GoogleAuthorID),]

#collect data from Google Scholar
GSdata = as.list(rep(NA, nrow(fGS)))
names(GSdata) = fGS$FamilyName
for(i in 1:nrow(fGS)) {
    GSdata[[i]] = get_publications(fGS$GoogleAuthorID[i], pagesize = 100, flush = TRUE)
}
saveRDS(GSdata, file = paste0("./data/GSdata_", Sys.Date(), ".rds"))

#number of publications retrieved from each Google Scholar profile 
fGS$npubs = sapply(GSdata, function(x) nrow(x))
summary(fGS$npubs)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 21.0    53.5    72.0   109.7   147.5   317.0 

# tmp = GSdata[[2]]$author
# grepl(pattern = faculty$FamilyName[3], x = tmp, ignore.case = FALSE)

#create (non-symmetric for now) adjacency matrix for all faculty
A = matrix(NA, nrow = nrow(faculty), ncol = nrow(faculty), 
           dimnames = list(faculty$FamilyName, faculty$FamilyName))
for(i in 1:length(GSdata)) { # i = 2
    authors = GSdata[[i]]$author
    iscoauthor = sapply(faculty$FamilyName, function(g)
                        grepl(pattern = g, x = authors, ignore.case = FALSE)
    )
    A[fGS$FamilyName[i],] = apply(iscoauthor, 2, any)
}
save.image(file = paste0("./dataderived/image_getcoauthors_", Sys.Date(), ".RData"))

