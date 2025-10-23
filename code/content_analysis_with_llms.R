##############################################################################
# File-Name: tutorial_llms
# author: Tiago Ventura
# Purpose: showcase some different applications of LLMs uses for research
##############################################################################

# Packages ------------------------------------------------------------
library(httr)
library(base64enc)
library(jsonlite)
library(dplyr)
library(tools)
library(tidyverse)
library(janitor)

# Introduction ------------------------------

## Let's first make sure we can communicate with the OpenAI API

# to do so, you need to create an account. 
# You can either use your ChatGPT account if you have one, or 
# create one here: https://platform.openai.com/docs/overview


# when you are there, you can go on generate token and get your token. 
open_ai_token = ""

# mine is stored locally
api_key <- Sys.getenv("OPENAI_API_KEY")


## to communicate with the API, we need two steps: 
### a) create a payload to send the model a message
### b) use a post request (like clicking on a browser) to send via the internet the request

## Creating a payload
payload <- list(
  model = "gpt-4o-mini",
  temperature = 0,
  messages = list(
    list(role = "system",
         content ="You are a helpful assistant"),
    list(role = "user",
         content = "Tell me your thoughts about studying at Georgetown")
    ))

# Connecting with the API
response <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(
    Authorization = paste("Bearer", api_key),
    `Content-Type` = "application/json"
  ),
  body = toJSON(payload, auto_unbox = TRUE),
  encode = "json"
)

# Checking Output
str(response, 2)

# cleaning
raw_response <- jsonlite::fromJSON(content(response, "text", encoding = "UTF-8"))

# get the message
raw_response$choices$message$content

## Okay, so let's see some applications

# Task 1: Zero Shot Classification -----------------------------

## Let's see how to do simple sentiment classification with OPEN AI LLMs. Similar
## to the PNAS paper we saw in class. 

# Open a dataset of some social media data with labels from this paper here: https://journalqd.org/article/view/2573
## download the data here: https://www.dropbox.com/scl/fi/80f1vgrlgrllizpnwyhbu/incivility.csv?rlkey=gofwqwgneok1a4wgpi2hrulz5&dl=0

d <- read_csv("data/incivility.csv")

# Check this dataset:
d %>% View()

## first write your zero-shot prompt

prompt <- "As an expert annotator, you will be asked to annotate a sample of social media posts \n
I would like you to determine if the post can be considered uncivil, toxic or an attack to other people \n
Return a number with 1 when you consider the post uncivil, toxicy or an atack, 0 otherwise. Here is the posts"

# write a generic function to query the open ai api
payload <- list(
  model = "gpt-4o-mini",
  temperature = 0,
  messages = list(
    list(role = "system",
         content =
           "You are a helpful assistant that determines the sentiment of a list of social media posts"),
    list(role = "user",
         content = list(
           ## see here the prompt + a random text
           list(type = "text", text = paste0(prompt, "You idiot!"))
           )
         )
    ), 
  max_tokens = 4000)


# Connecting with the API
response <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(
    Authorization = paste("Bearer", api_key),
    `Content-Type` = "application/json"
  ),
  body = toJSON(payload, auto_unbox = TRUE),
  encode = "json"
)

# cleaning
raw_response <- jsonlite::fromJSON(content(response, "text", encoding = "UTF-8"))

# get the message
raw_response$choices$message$content



#  2 - Making API Calls More Robust ---------------------------------------

## to make these calls more robust, lets write a function, and define the output we are looking for. 
# write a generic function to query the open ai api

query_open_ai <- function(prompt, 
                          post, 
                          output_structure,
                          system_profile="You are a helpful assistant that determines the sentiment of a list of social media posts"){
  
# payload  
payload <- list(
  model = "gpt-4o-mini",
  temperature = 0,
  messages = list(
    list(role = "system",
         content =system_profile),
    list(role = "user",
         content = list(
           ## see here the prompt + a random text
           list(type = "text", 
                text = paste0(prompt,post))
         )
    )
  ),
  # notice here
  functions=list(output_structure),
  function_call = list(name = output_structure$name),
  max_tokens = 4000)


# Connecting with the API
response <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(
    Authorization = paste("Bearer", api_key),
    `Content-Type` = "application/json"
  ),
  body = toJSON(payload, auto_unbox = TRUE),
  encode = "json"
)

# cleaning
raw_response <- jsonlite::fromJSON(content(response, "text", encoding = "UTF-8"))

# get the message
as_tibble(jsonlite::fromJSON(raw_response$choices$message$function_call$arguments))

}


# prompt
prompt ="As an expert annotator, you will be asked to annotate a sample of social media posts \n
I would like you to determine if the post can be considered uncivil, toxic or an attack to other people \n

Return a json with: 

'text': for the text you took as input
'label': uncivil when you consider the post uncivil, civil otherwise
'reasoning': one sentence explanation of you reasoning

"
# structure
output_structure <- list(
  name = "code_social_media",
  description = "Annotates a social media posts", 
  parameters = list(
    type = "object",
    properties = list(
      text = list(
        type = "string", description = "The original post text."),
      label = list(
        type = "string",
        enum = c("civil", "uncivil"),
        description = "uncivil when you consider the post toconsidered uncivil, toxic or an attack to other people, civil otherwise "), 
    reasoning=list(
      type="string", 
      description="one sentence for your reasoning on the decision")
    ),
    required = c("text", "label", "reasoning")
  ))


# post
post="such a nice person! But you are lying to people!"

# Run one example
query_open_ai(prompt, post, output_structure)

# Let's run in our dataset now
output=list()
for(i in 1:10){
output[[i]] = query_open_ai(prompt, d$comment_message[[i]], output_structure)
print(i)  
}

# check results
output %>% bind_rows()



# Example 2: Another Annotation task  -------------------------------------------

## This is a similar example but with a more complicated prompt. Unfortunately, I cannot
## give you access to this data

# 2 - load dataset --------------------------------------------------------
links_df <- read_csv("~/Dropbox/artigos/VVW_LLMs_urls/data/data_nyhan/sample_voter_fraud.csv") %>% clean_names()

# 3 -  Classify ----------------------------------------------------------------

prompt <- "As an expert annotator, you will be asked to annotate a sample of news articles about U.S. politics
to determine (1) if they discuss election fraud or not and (2) if they include a statement questioning
or contradicting claims that election fraud is widespread or could change the outcome of one or
more elections. 

For each US politics news article, return JSON {q1:Yes/No, q2:Yes/No} with no explanation.

Q1: Yes if the article mentions election fraud in US elections (illegal interference with casting, tallying, certifying votes). Fraud includes in-person fraud, mail ballot fraud, blocking counting, or saying these processes are vulnerable. Includes general or indirect references like 'refuse to accept results', 'overturn/steal election', 'election denial', 'invalid election', or 'election denier'. Apply even if fraud is implied, not explicit. Ignore irrelevant scraped text, protests without fraud, satire, foreign election fraud, letters to the editor, generic 'election integrity' mentions, unrelated crimes (e.g., hacking emails), and hypothetical future-rule scenarios.

Q2: If Q1=No, code Q2=No. If Q1=Yes, code Yes if the article questions or contradicts claims that fraud is widespread or could change outcomes under current or past rules. This includes: calling claims false, baseless, unfounded, unsupported, absurd, or lies; noting courts rejected such claims; opposing interference with certification; rejecting legal arguments to do so. Also Yes if describing courts rejecting legal challenges to certification. No if Trump/allies question certification. 'Refuse to concede' ≠ Yes for Q2 unless it also questions fraud claims.

General: Only code the first distinct article if multiple are present; ignore reader comments. Do not use extraneous text (e.g., other headlines). Satire and non-US election fraud are excluded.

Here is the full body of the news:"

output_structure <- list(
  name = "code_article",
  description = "Annotates a US politics news article for mentions of election fraud and statements questioning fraud claims.",
  parameters = list(
    type = "object",
    properties = list(
      q1 = list(
        type = "string",
        enum = c("Yes", "No"),
        description = "Yes if the article references election fraud in US elections, including 2020, by promoting, discussing, or contradicting such claims. Fraud includes illegal interference with casting, counting, or certifying votes, e.g., in-person fraud, mail ballot fraud, preventing eligible votes from being cast/count, or describing these processes as vulnerable. Includes terms like 'refuse to accept results', 'overturn/steal election', 'election denial', 'invalid election', or similar. Ignore irrelevant text, protests without fraud mentions, satire, foreign elections, letters to the editor, and non-fraud election integrity topics."
      ),
      q2 = list(
        type = "string",
        enum = c("Yes", "No"),
        description = "If q1=No, code No. If q1=Yes, code Yes if article questions or contradicts claims that fraud is widespread or could change outcomes under current/past rules. This includes calling such claims false, baseless, unfounded, absurd, or lying; noting courts rejected such claims; opposing efforts to change/interfere with certification; or rejecting legal arguments to do so. Do NOT code Yes if Trump/allies question certification. Ignore references to 'refuse to concede' unless they also question fraud claims."
      )
    ),
    required = c("q1", "q2")
  )
)



# query api
response=list()

for(i in 1:10){
  # query    
  response[[i]] = query_open_ai(prompt, links_df$body_clean[[i]],output_structure)
  print(i)
}

response %>% 
  bind_rows() %>%
  bind_cols(links_df[1:10,"body_clean"])


# Example 3: Synthethic data with Large Language Models ---------------------------------------------------------

## LMMs can also be leveraged to generate synthetic data. 

## I will show two examples: 

### one from my ongoing research using LLMs to augment browsing data
### another from research using LLMs to respond to survey questions

### Augmenting Browsing data ---------

## prompt
prompt = " The url in triple quotations is a url that links to a news item.

Without going on the internet, produce the online news article based on this URL. 

Limit to three paragraphs, and return only the article.

When doing so, consider information in the URL, such as publication day, author, and queries in the full url 

Here is the URL:

"

## output structure
output_structure <- list(
  name = "generate_web_page_content",
  description = "Given a URL, write the full content of a web page",
  parameters = list(
    type = "object",
    properties = list(
      content = list(
        type = "string",
        description = "return the full content of a web page news based on a URL,without going on the internet. use only info in your training data"
      ),
      url = list(
        type = "string",
        description = "return the url back")
    ),
    required = c("content", "url")
  )
)

# query
output <- query_open_ai(prompt, links_df$simple_url[[1]],output_structure)

# Does it work well?
output$url

output$content

links_df$body_clean[[1]]

## really, not terrible!

### Using LLM agents in surveys ---------

# function to generate multiple prompts
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
          return a JSON with the group as the key, and your response as numerical value
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


# data with demographic profiles
audit_data <- expand.grid(age = c(20,35,50,65),
                          race = c('non-Hispanic white','non-Hispanic black','Hispanic'),
                          gender = c('male','female'),
                          inc = c('30,000','50,000','80,000','100,000','more than $150,000'),
                          educ = c('high school diploma',"some college, but no degree","bachelor's degree","postgraduate degree"),
                          pid = c('Republican','Democrat','Independent'),
                          stringsAsFactors = F) %>%
  as_tibble()

# output
function_info <- list(
  name = "feelings_thermometer",
  description = "Return 0–100 ratings toward groups",
  parameters = list(
    type = "object",
    properties = list(
      `The Democratic Party` = list(type = "integer", minimum = 0, maximum = 100),
      `The Republican Party` = list(type = "integer", minimum = 0, maximum = 100),
      Democrats = list(type = "integer", minimum = 0, maximum = 100),
      Republicans = list(type = "integer", minimum = 0, maximum = 100),
      `Black Americans` = list(type = "integer", minimum = 0, maximum = 100),
      `White Americans` = list(type = "integer", minimum = 0, maximum = 100),
      `Hispanic Americans` = list(type = "integer", minimum = 0, maximum = 100),
      `Asian Americans` = list(type = "integer", minimum = 0, maximum = 100),
      Muslims = list(type = "integer", minimum = 0, maximum = 100),
      Christians = list(type = "integer", minimum = 0, maximum = 100),
      Immigrants = list(type = "integer", minimum = 0, maximum = 100),
      `Gays and Lesbians` = list(type = "integer", minimum = 0, maximum = 100),
      Jews = list(type = "integer", minimum = 0, maximum = 100),
      Liberals = list(type = "integer", minimum = 0, maximum = 100),
      Conservatives = list(type = "integer", minimum = 0, maximum = 100),
      Women = list(type = "integer", minimum = 0, maximum = 100)
    ),
    required = c("The Democratic Party","The Republican Party","Democrats","Republicans",
                 "Black Americans","White Americans","Hispanic Americans","Asian Americans",
                 "Muslims","Christians","Immigrants","Gays and Lesbians","Jews",
                 "Liberals","Conservatives","Women")
  )
)

# create many prompts
prompts = create_prompt(audit_data)
samples = sample(length(prompts), 2)

# let check this
res=list()

for(i in 1:length(samples)){

payload <- list(
  model = "gpt-4o-mini",
  temperature = 0,
  messages = prompts[[samples[i]]],
  functions=list(function_info),
  function_call = list(name = function_info$name), 
  max_tokens = 4000)


# Connecting with the API
response <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(
    Authorization = paste("Bearer", api_key),
    `Content-Type` = "application/json"
  ),
  body = toJSON(payload, auto_unbox = TRUE),
  encode = "json"
)

response

# cleaning
raw_response <- jsonlite::fromJSON(content(response, "text", encoding = "UTF-8"))

# get the message
res[[i]] = as_tibble(jsonlite::fromJSON(raw_response$choices$message$function_call$arguments))
}


# lets see the results
bind_cols(audit_data %>% slice(samples), 
          bind_rows(res)) %>% View()
