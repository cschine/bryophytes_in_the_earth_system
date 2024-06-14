# script to read in literature search results and check for duplicates

# read in csv for literature results
# loop through file list in target directory


# amend each search result table with a column that has the name of the search file and 
#   a 1 in the column for all of the records that are contained in that tibble
#   this will allow us to keep track of which searches turned up which records
#   and to add up the nuber of searches that returend a given record


# look for matches within each set of search results

# Write function to check for matches
## take in tibble
## separate out all titles
## loop through titles comparing each title to all other titles in turn
## set match threshold
## have any match that exceeds threshold sent to a command line prompt where the user
##     indicates whether it should be treated as a match or ignored



# still need to figure out how to deal with creating the next tibble
#   where all matches have been removed and all searches that have identified
#   a given record are recorded.
