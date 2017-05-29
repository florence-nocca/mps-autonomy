rm(list = ls(all = TRUE))

setwd("~/retrieve-tweets")

library("rtweet")
library(RCurl)
 
## Use exact value instead of scientific notation
options(scipen=999)

## Specify users' lists
user_lists = c("french_parties","french_cand","spanish_mps","spanish_parties","italian_mps","italian_parties")

rate = 0
lapply(user_lists, function(filename) {

    user_list = read.csv(file=paste0("datafiles/",filename,"_accounts.csv"),header=F)
    colnames(user_list) = c("name","account")
    Sys.sleep(sample(200, 1, replace=F))

    ## Inside each list, retrieve each account
    counter = 0
    lapply(user_list$account, function(x) {
        i = 0
        error = 0
        rate = rate + 1
        ## Every 200 accounts, sleep to avoid rate limit
        if(rate == 200)
            {
                Sys.sleep(sample(800:900, 1, replace=F))
                rate = 0
            }
        x = as.character(x)
        print(x)

        ## Return if account does not exist
        if(url.exists(paste0("https://twitter.com/",x)) == FALSE)
        {
            print("Does not exist")
            return
        }
        else
        {
            ## If no file exists for a list, create corresponding file
            if(file.exists(paste0("datafiles/",filename,".tweets.csv")) == FALSE)
            {
                data = get_timeline(x)
                save_as_csv(data, file_name=paste0("datafiles/",filename,".csv"))
                print("file does not exist")
            }

            else
            {
                ## Check if the tweets' file is opened
                if(counter == 0)
                {
                    old_tweets = read.csv(file=paste0("datafiles/",filename,".tweets.csv"), header=TRUE)
                    counter = 1
                }
                
                ## Find the most recent tweet retrieved for users previously retrieved (last_id)
                if(tolower(x) %in% tolower(old_tweets$screen_name))
                {
                    id_list = old_tweets[tolower(old_tweets$screen_name) == tolower(x),]$status_id
                    ## print(id_list)
                    last_id = max(id_list, na.rm=TRUE)
                    ## print("Printing last_id")
                    ## print(last_id)
                    ## If last_id was deleted on Twitter, find second most recent
                    if(url.exists(paste0("https://twitter.com/",x,"/status/",last_id)) == FALSE)
                    {
                        n = length(id_list[!is.na(id_list)])
                        last_id = sort(id_list,partial=n-1)[n-2]
                    }

                    ## If last_id is null, retrieve 200 last tweets
                    if(is.na(last_id))
                        to_write = get_timeline(x)
                    else
                    {
                        max_id = NULL
                        pages = NULL

                        ## Retrieve user's 200 last tweets and repeat until the list contains last_id
                        repeat{
                            last_page = get_timeline(x, max_id=max_id)
                            if(is.null(pages))
                                pages = last_page
                            ## Bind the newly retrieved tweets to the older iterations
                            else
                                pages = rbind(pages, last_page)
                            ## Retrieve 200 following tweets
                            max_id = min(pages$status_id)
                            ## print("Printing max_id")
                            ## print(max_id)
                            i = i + 1
                            ## Break repeat when i > 9
                            if (i > 9)
                            {
                                error = 1
                                break
                                
                            }
                            ## When the last_id is found, stop the iterations
                            if(sum(pages$status_id == last_id) != 0){
                                break
                            }
                        }

                        if (error == 0)
                        {
                            ## Keep the tweets newer than last_id
                            to_write = pages[pages$status_id > last_id,]
                        }
                        ## If last_id was not found after 9 repeats, keep the tweets saved from the repeat
                        else
                        {
                            print("Not able to find last tweet")
                            to_write = pages
                        }

                    }
                }
                ## If user was never retrieved before
                else
                {
                    print(paste0("First retrieve for ",x))
                    to_write = get_timeline(x)
                }

                ## Write new tweets in a temporary csv
                save_as_csv(to_write,file_name="datafiles/tmp.csv")
                ## Append old user's data with the new
                lapply(c("users","tweets"), function (y){
                    old_file = read.csv(file=paste0("datafiles/",filename,".",y,".csv"), header=TRUE)
                    tmp_file = read.csv(file=paste0("datafiles/tmp.",y,".csv"), header=TRUE)
                    write.csv(rbind(old_file,tmp_file), file=paste0("datafiles/",filename,".",y,".csv"), row.names=F)
                })
                
            }
        }
    })

})
