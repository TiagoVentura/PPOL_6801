library(openai)

Sys.setenv(OPENAI_API_KEY = 'GETCHER OWN KEY YA HOSER') # JB #2


create_prompt <- function(audit_data) {
  res <- list()
  for(i in 1:nrow(audit_data)) {
    age = audit_data$age[i]
    race = audit_data$race[i]
    gender = audit_data$gender[i]
    inc = audit_data$inc[i]
    educ = audit_data$educ[i]
    pid = audit_data$pid[i]
    res[[i]] <- list(
      list(
        "role" = "system",
        "content" = stringr::str_c(
          "You are a ",age," year old ",race," ",gender,
          " with a ",educ,", earning $",inc," per year. ",
          "You are a registered ",pid," living in the USA in 2019.")
      ),
      # Only provide the numeric answer, with no other justification or explanation.\n
      list(
        "role" = "user",
        "content" = stringr::str_c(
          "Provide responses from this person's perspective.\n
          Use only knowledge about politics that they would have.\n
          Format the output as a csv table with the following format:\n
          group,thermometer\n
          The following questions ask about individuals' feelings toward different groups.\n
          Responses should be given on a scale from 0 (meaning cold feelings) to 100 (meaning warm feelings).\n
          Ratings between 50 degrees and 100 degrees mean that\n
          you feel favorable and warm toward the group. Ratings between 0\n
          degrees and 50 degrees mean that you don't feel favorable toward\n
          the group and that you don't care too much for that group. You\n
          would rate the group at the 50 degree mark if you don't feel\n
          particularly warm or cold toward the group.\n
          How do you feel toward the following groups?\n",
          'The Democratic Party?\n',
          'The Republican Party?\n',
          'Democrats?\n',
          'Republicans?\n',
          'Black Americans?\n',
          'White Americans?\n',
          'Hispanic Americans?\n',
          'Asian Americans?\n',
          'Muslims?\n',
          'Christians?\n',
          'Immigrants?\n',
          'Gays and Lesbians?\n',
          'Jews?\n',
          'Liberals?\n',
          'Conservatives?\n',
          'Women?\n')
      )
    )
  }
  
  return(res)
}


audit_data <- expand.grid(age = c(20,35,50,65),
                          race = c('non-Hispanic white','non-Hispanic black','Hispanic'),
                          gender = c('male','female'),
                          inc = c('30,000','50,000','80,000','100,000','more than $150,000'),
                          educ = c('high school diploma',"some college, but no degree","bachelor's degree","postgraduate degree"),
                          pid = c('Republican','Democrat','Independent'),
                          stringsAsFactors = F) %>%
  as_tibble()


submit_openai <- function(prompt, temperature = 0.2, n = 1) {
  res <- openai::create_chat_completion(model = "gpt-3.5-turbo",
                                        messages = prompt,
                                        temperature = temperature,
                                        n = n)
  Sys.sleep(1)
  res
}

df <- read_csv('./results/therm_ANES.csv')

df <- df %>%
  mutate(index = as.numeric(gsub(',NA','',index)))

if(nrow(df) == 0) {
  start = 1
} else {
  start = max(df$index,na.rm=T) + 1
}

toSave <- NULL
TPM <- RPM <- NULL
zz <- zzz <- Sys.time()
for(i in start:nrow(audit_data)) {
  prompts <- create_prompt(audit_data[i,])
    openai_completions <- try(prompts |>
                                purrr::map(submit_openai,temperature = 1,n = 20))
    
    while(class(openai_completions) == 'try-error') {
      Sys.sleep(60)
      cat('issue on\n',
          'temp =',t,'\n',
          paste(audit_data[i,],collapse = ' / '),'\n')
      openai_completions <- try(prompts |>
                                  purrr::map(submit_openai,temperature = 1,n = 20))
      
    }

    tmp <- NULL
    for(j in 1:length(openai_completions[[1]]$choices$message.content)) {
      tmp <- bind_rows(tmp,
                       read.csv(text = gsub('\\\\','',openai_completions[[1]]$choices$message.content[j]),
                                col.names = c('group','thermometer')) %>%
        mutate(draw = j,
               thermometer = as.numeric(thermometer)))
    }
    
    toSave <- toSave %>%
      as_tibble() %>%
      bind_rows(data.frame(audit_data[i,]) %>%
                  cbind(tmp %>%
                          mutate(index = i)))
    
    TPM <- sum(TPM,openai_completions[[1]]$usage$total_tokens)
    RPM <- sum(RPM,1)
  # }
  
  if(difftime(Sys.time(),zzz,units = 'mins') < 1) {
    # cat('LT 1 minute\n')
    if(RPM > 3000 | TPM > 85000) {
      cat('RPM = ',RPM,'\nTPM = ',TPM,'\n')
      Sys.sleep(max(0,as.numeric(60 - difftime(Sys.time(),zzz,units = 'secs'))))
      RPM <- TPM <- NULL
      zzz <- Sys.time()
      cat('Approaching rate limit\n')
    }
  } else {
    # cat('Over 1 minute\n')
    RPM <- TPM <- NULL
    zzz <- Sys.time()
  }
  
  if(i %% 100 == 0) {
    write.table(toSave,file = './results/therm_ANES.csv',append = T,row.names = F,col.names = F,sep = ',')
    toSave <- NULL
    
    cat(i,'in',round(difftime(Sys.time(),zz,units = 'mins'),2),'minutes\n')
    zz <- Sys.time()
  }
}