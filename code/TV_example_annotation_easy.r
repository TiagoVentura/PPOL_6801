Sys.setenv(OPENAI_API_KEY = 'XXX') # Enter API Key Here

mods <- c('gpt-4.1','gpt-4o-mini')


instructionTrade = "You will be given a random sample of newsletters written by members of the 111th-118th U.S. Congresses. Please indicate whether the newsletter talks about international trade, including dimensions such as offshoring, imports, exports, or foreign competition.\n
                Please format your response as a tsv file, where the first column is the newsletter ID, the second column is a binary variable where 1 indicates the newsletter talks about international trade, and 0 if not, and the third column is your explanation.\n
                Please include the following column labels: 'newsletter_id', 'mentions_trade', 'explanation_trade'."

instructionJobs = "You will be given a random sample of newsletters written by members of the 111th-118th U.S. Congresses. Please indicate whether the newsletter talks about jobs, employment, or the labor market.\n
                Please format your response as a tsv file, where the first column is the newsletter ID, the second column is a binary variable where 1 indicates the newsletter talks about jobs, and 0 if not, and the third column is your explanation.\n
                Please include the following column labels: 'newsletter_id', 'mentions_jobs', 'explanation_jobs'."

instructionImmig = "You will be given a random sample of newsletters written by members of the 111th-118th U.S. Congresses. Please indicate whether the newsletter talks about immigration, migrants.\n
                Please format your response as a tsv file, where the first column is the newsletter ID, the second column is a binary variable where 1 indicates the newsletter talks about jobs, and 0 if not, and the third column is your explanation.\n
                Please include the following column labels: 'newsletter_id', 'mentions_immigration', 'explanation_immigration'."

chats <- list()
for(m in mods) {
  chats[[m]]$trade <- chat_openai(system_prompt = paste0(instructionTrade),model = m)
  chats[[m]]$jobs <- chat_openai(system_prompt = paste0(instructionJobs),model = m)
  chats[[m]]$imm <- chat_openai(system_prompt = paste0(instructionImmig),model = m)
}

resTrade <- chats$`gpt-4.1`$trade$chat(forChat$forChat)
resJobs <- chats$`gpt-4.1`$jobs$chat(forChat$forChat)
resImm <- chats$`gpt-4.1`$imm$chat(forChat$forChat)