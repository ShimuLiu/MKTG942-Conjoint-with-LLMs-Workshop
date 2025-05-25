Conjoint with LLMs
================
Shimu Liu
2025-05-24

Introduction: In this workshop, we’ll demonstrate how to simulate a
conjoint analysis based on Brand et al.(2024)’s paper using LLMs via API
in R. Here’s the overall guideline:

1.  Design product profiles (e.g., laptops)

2.  Generate prompts for LLM

3.  Send requests using API

4.  Parse responses to simulate consumer choices

Start Implementation:

1.a Load Libraries

``` r
# Load required packages
# httr and jsonlite are used for sending HTTP requests and handling JSON responses
library(httr)       # For API requests
library(purrr)      # For functional programming
library(dplyr)      # For data manipulation
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tibble)     # For tidy data frames
library(jsonlite)   # For JSON handling
```

    ## 
    ## Attaching package: 'jsonlite'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     flatten

``` r
library(tidyr)      # For data tidying
library(readr)      # For reading/writing CSV files
```

1.b Set API Key (<https://openrouter.ai/>) OpenRouter is a unified API
platform that provides streamlined access to a wide array of large
language models (LLMs)

We define our OpenRouter API key and specify which LLM to use. For this
example, we use the “deepseek/deepseek-chat-v3-0324:free” model. You can
substitute any other OpenRouter-compatible model here.

``` r
# Set your OpenRouter API key
#Sys.setenv(OPENROUTER_API_KEY = "Your API Key")

Sys.setenv(OPENROUTER_API_KEY =  "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")


# DeepSeek model specification
deepseek_model <- "deepseek/deepseek-chat-v3-0324:free"
```

2.  Design Conjoint Profiles

``` r
# Define product attributes
brands <- c("Microsoft Surface Laptop", "Apple Macbook Air")
prices <- c("$1000", "$1200", "$1400")
rams <- c("8GB", "16GB", "32GB")
storages <- c("256GB", "512GB", "1TB")
sizes <- c("13in", "15in")

# Create all possible product combinations
generate_product_profiles <- function() {
  expand.grid(
    Brand = brands,
    Price = prices,
    Size = sizes,
    RAM = rams,
    Storage = storages,
    stringsAsFactors = FALSE
  ) %>% 
    mutate(ProductID = paste0("P", row_number()))
}

products <- generate_product_profiles()
print(products)
```

    ##                        Brand Price Size  RAM Storage ProductID
    ## 1   Microsoft Surface Laptop $1000 13in  8GB   256GB        P1
    ## 2          Apple Macbook Air $1000 13in  8GB   256GB        P2
    ## 3   Microsoft Surface Laptop $1200 13in  8GB   256GB        P3
    ## 4          Apple Macbook Air $1200 13in  8GB   256GB        P4
    ## 5   Microsoft Surface Laptop $1400 13in  8GB   256GB        P5
    ## 6          Apple Macbook Air $1400 13in  8GB   256GB        P6
    ## 7   Microsoft Surface Laptop $1000 15in  8GB   256GB        P7
    ## 8          Apple Macbook Air $1000 15in  8GB   256GB        P8
    ## 9   Microsoft Surface Laptop $1200 15in  8GB   256GB        P9
    ## 10         Apple Macbook Air $1200 15in  8GB   256GB       P10
    ## 11  Microsoft Surface Laptop $1400 15in  8GB   256GB       P11
    ## 12         Apple Macbook Air $1400 15in  8GB   256GB       P12
    ## 13  Microsoft Surface Laptop $1000 13in 16GB   256GB       P13
    ## 14         Apple Macbook Air $1000 13in 16GB   256GB       P14
    ## 15  Microsoft Surface Laptop $1200 13in 16GB   256GB       P15
    ## 16         Apple Macbook Air $1200 13in 16GB   256GB       P16
    ## 17  Microsoft Surface Laptop $1400 13in 16GB   256GB       P17
    ## 18         Apple Macbook Air $1400 13in 16GB   256GB       P18
    ## 19  Microsoft Surface Laptop $1000 15in 16GB   256GB       P19
    ## 20         Apple Macbook Air $1000 15in 16GB   256GB       P20
    ## 21  Microsoft Surface Laptop $1200 15in 16GB   256GB       P21
    ## 22         Apple Macbook Air $1200 15in 16GB   256GB       P22
    ## 23  Microsoft Surface Laptop $1400 15in 16GB   256GB       P23
    ## 24         Apple Macbook Air $1400 15in 16GB   256GB       P24
    ## 25  Microsoft Surface Laptop $1000 13in 32GB   256GB       P25
    ## 26         Apple Macbook Air $1000 13in 32GB   256GB       P26
    ## 27  Microsoft Surface Laptop $1200 13in 32GB   256GB       P27
    ## 28         Apple Macbook Air $1200 13in 32GB   256GB       P28
    ## 29  Microsoft Surface Laptop $1400 13in 32GB   256GB       P29
    ## 30         Apple Macbook Air $1400 13in 32GB   256GB       P30
    ## 31  Microsoft Surface Laptop $1000 15in 32GB   256GB       P31
    ## 32         Apple Macbook Air $1000 15in 32GB   256GB       P32
    ## 33  Microsoft Surface Laptop $1200 15in 32GB   256GB       P33
    ## 34         Apple Macbook Air $1200 15in 32GB   256GB       P34
    ## 35  Microsoft Surface Laptop $1400 15in 32GB   256GB       P35
    ## 36         Apple Macbook Air $1400 15in 32GB   256GB       P36
    ## 37  Microsoft Surface Laptop $1000 13in  8GB   512GB       P37
    ## 38         Apple Macbook Air $1000 13in  8GB   512GB       P38
    ## 39  Microsoft Surface Laptop $1200 13in  8GB   512GB       P39
    ## 40         Apple Macbook Air $1200 13in  8GB   512GB       P40
    ## 41  Microsoft Surface Laptop $1400 13in  8GB   512GB       P41
    ## 42         Apple Macbook Air $1400 13in  8GB   512GB       P42
    ## 43  Microsoft Surface Laptop $1000 15in  8GB   512GB       P43
    ## 44         Apple Macbook Air $1000 15in  8GB   512GB       P44
    ## 45  Microsoft Surface Laptop $1200 15in  8GB   512GB       P45
    ## 46         Apple Macbook Air $1200 15in  8GB   512GB       P46
    ## 47  Microsoft Surface Laptop $1400 15in  8GB   512GB       P47
    ## 48         Apple Macbook Air $1400 15in  8GB   512GB       P48
    ## 49  Microsoft Surface Laptop $1000 13in 16GB   512GB       P49
    ## 50         Apple Macbook Air $1000 13in 16GB   512GB       P50
    ## 51  Microsoft Surface Laptop $1200 13in 16GB   512GB       P51
    ## 52         Apple Macbook Air $1200 13in 16GB   512GB       P52
    ## 53  Microsoft Surface Laptop $1400 13in 16GB   512GB       P53
    ## 54         Apple Macbook Air $1400 13in 16GB   512GB       P54
    ## 55  Microsoft Surface Laptop $1000 15in 16GB   512GB       P55
    ## 56         Apple Macbook Air $1000 15in 16GB   512GB       P56
    ## 57  Microsoft Surface Laptop $1200 15in 16GB   512GB       P57
    ## 58         Apple Macbook Air $1200 15in 16GB   512GB       P58
    ## 59  Microsoft Surface Laptop $1400 15in 16GB   512GB       P59
    ## 60         Apple Macbook Air $1400 15in 16GB   512GB       P60
    ## 61  Microsoft Surface Laptop $1000 13in 32GB   512GB       P61
    ## 62         Apple Macbook Air $1000 13in 32GB   512GB       P62
    ## 63  Microsoft Surface Laptop $1200 13in 32GB   512GB       P63
    ## 64         Apple Macbook Air $1200 13in 32GB   512GB       P64
    ## 65  Microsoft Surface Laptop $1400 13in 32GB   512GB       P65
    ## 66         Apple Macbook Air $1400 13in 32GB   512GB       P66
    ## 67  Microsoft Surface Laptop $1000 15in 32GB   512GB       P67
    ## 68         Apple Macbook Air $1000 15in 32GB   512GB       P68
    ## 69  Microsoft Surface Laptop $1200 15in 32GB   512GB       P69
    ## 70         Apple Macbook Air $1200 15in 32GB   512GB       P70
    ## 71  Microsoft Surface Laptop $1400 15in 32GB   512GB       P71
    ## 72         Apple Macbook Air $1400 15in 32GB   512GB       P72
    ## 73  Microsoft Surface Laptop $1000 13in  8GB     1TB       P73
    ## 74         Apple Macbook Air $1000 13in  8GB     1TB       P74
    ## 75  Microsoft Surface Laptop $1200 13in  8GB     1TB       P75
    ## 76         Apple Macbook Air $1200 13in  8GB     1TB       P76
    ## 77  Microsoft Surface Laptop $1400 13in  8GB     1TB       P77
    ## 78         Apple Macbook Air $1400 13in  8GB     1TB       P78
    ## 79  Microsoft Surface Laptop $1000 15in  8GB     1TB       P79
    ## 80         Apple Macbook Air $1000 15in  8GB     1TB       P80
    ## 81  Microsoft Surface Laptop $1200 15in  8GB     1TB       P81
    ## 82         Apple Macbook Air $1200 15in  8GB     1TB       P82
    ## 83  Microsoft Surface Laptop $1400 15in  8GB     1TB       P83
    ## 84         Apple Macbook Air $1400 15in  8GB     1TB       P84
    ## 85  Microsoft Surface Laptop $1000 13in 16GB     1TB       P85
    ## 86         Apple Macbook Air $1000 13in 16GB     1TB       P86
    ## 87  Microsoft Surface Laptop $1200 13in 16GB     1TB       P87
    ## 88         Apple Macbook Air $1200 13in 16GB     1TB       P88
    ## 89  Microsoft Surface Laptop $1400 13in 16GB     1TB       P89
    ## 90         Apple Macbook Air $1400 13in 16GB     1TB       P90
    ## 91  Microsoft Surface Laptop $1000 15in 16GB     1TB       P91
    ## 92         Apple Macbook Air $1000 15in 16GB     1TB       P92
    ## 93  Microsoft Surface Laptop $1200 15in 16GB     1TB       P93
    ## 94         Apple Macbook Air $1200 15in 16GB     1TB       P94
    ## 95  Microsoft Surface Laptop $1400 15in 16GB     1TB       P95
    ## 96         Apple Macbook Air $1400 15in 16GB     1TB       P96
    ## 97  Microsoft Surface Laptop $1000 13in 32GB     1TB       P97
    ## 98         Apple Macbook Air $1000 13in 32GB     1TB       P98
    ## 99  Microsoft Surface Laptop $1200 13in 32GB     1TB       P99
    ## 100        Apple Macbook Air $1200 13in 32GB     1TB      P100
    ## 101 Microsoft Surface Laptop $1400 13in 32GB     1TB      P101
    ## 102        Apple Macbook Air $1400 13in 32GB     1TB      P102
    ## 103 Microsoft Surface Laptop $1000 15in 32GB     1TB      P103
    ## 104        Apple Macbook Air $1000 15in 32GB     1TB      P104
    ## 105 Microsoft Surface Laptop $1200 15in 32GB     1TB      P105
    ## 106        Apple Macbook Air $1200 15in 32GB     1TB      P106
    ## 107 Microsoft Surface Laptop $1400 15in 32GB     1TB      P107
    ## 108        Apple Macbook Air $1400 15in 32GB     1TB      P108

3.  Generate Prompts

``` r
# Defines a function to generate comparison prompt
generate_prompt <- function(product1, product2) {
  #sprintf() is used to create a formatted string
  prompt1 <- sprintf("%s, Price: %s, RAM: %s, Storage: %s, Size: %s",
                    product1$Brand, 
                    product1$Price, 
                    product1$RAM, 
                    product1$Storage, 
                    product1$Size)
  
  prompt2 <- sprintf("%s, Price: %s, RAM: %s, Storage: %s, Size: %s",
                    product2$Brand, 
                    product2$Price, 
                    product2$RAM, 
                    product2$Storage, 
                    product2$Size)
  
  # Create the final prompt
  # 1. system message
  sys_msg <- list(
    role    = "system",
    content = paste(
      "You are a customer. You are selected at random while shopping",
      "for laptops to participate in a survey. The interviewer will describe the",
      "options you saw while shopping and ask you to report which option you chose",
      "to purchase. Whenever two options are shown, you can also choose a third",
      "option which is not to purchase anything that day."
    )
  )
  
  # 2. user message
  usr_msg <- list(
    role    = "user",
    content = sprintf(
      paste(
        "Option 1: %s\n",
        "Option 2: %s\n\n",
        "Which option do you choose? Respond ONLY with:\n",
        "- '1' for Option 1\n",
        "- '2' for Option 2\n",
        "- '0' if you choose not to purchase anything"
      ),
      prompt1, prompt2
    )
  )
  
  # 3. bundle them for the API call
  combined_content <- paste(sys_msg$content, usr_msg$content, sep = "\n\n")
  return(combined_content)
}


# Function to create all unique product pairs with prompts
generate_product_pairs <- function(products, iterations) {
  all_pairs <- combn(products$ProductID, 2, simplify = FALSE)
  sampled_pairs <- sample(all_pairs, min(iterations, length(all_pairs)))
  
  map_dfr(sampled_pairs, ~ {
    p1 <- products %>% filter(ProductID == .x[1])
    p2 <- products %>% filter(ProductID == .x[2])
    tibble(
      Product1_ID = p1$ProductID,
      Product2_ID = p2$ProductID,
      Prompt = generate_prompt(p1, p2)
    )
  })
}

pairs_df <- generate_product_pairs(products, iterations = 20)
#Inspect the first few rows of the generated prompts
head(pairs_df)
```

    ## # A tibble: 6 × 3
    ##   Product1_ID Product2_ID Prompt                                                
    ##   <chr>       <chr>       <chr>                                                 
    ## 1 P53         P74         "You are a customer. You are selected at random while…
    ## 2 P41         P53         "You are a customer. You are selected at random while…
    ## 3 P11         P26         "You are a customer. You are selected at random while…
    ## 4 P49         P62         "You are a customer. You are selected at random while…
    ## 5 P21         P85         "You are a customer. You are selected at random while…
    ## 6 P34         P84         "You are a customer. You are selected at random while…

``` r
pairs_df$Prompt[[1]]
```

    ## [1] "You are a customer. You are selected at random while shopping for laptops to participate in a survey. The interviewer will describe the options you saw while shopping and ask you to report which option you chose to purchase. Whenever two options are shown, you can also choose a third option which is not to purchase anything that day.\n\nOption 1: Microsoft Surface Laptop, Price: $1400, RAM: 16GB, Storage: 512GB, Size: 13in\n Option 2: Apple Macbook Air, Price: $1000, RAM: 8GB, Storage: 1TB, Size: 13in\n\n Which option do you choose? Respond ONLY with:\n - '1' for Option 1\n - '2' for Option 2\n - '0' if you choose not to purchase anything"

4.  Send Requests to API Check for more standard guidance:
    <https://openrouter.ai/deepseek/deepseek-chat-v3-0324:free/api>

``` r
query_llm <- function(prompt) {
  res <- POST(
    url = "https://openrouter.ai/api/v1/chat/completions",
    add_headers(
      "Authorization" = paste("Bearer", Sys.getenv("OPENROUTER_API_KEY")),
      "User-Agent" = "demo"
    ),
    encode = "json",
    body = toJSON(list(
      model = "deepseek/deepseek-chat-v3-0324:free",
      messages = list(list(role = "user", content = prompt)),
      max_tokens = 150
    ), auto_unbox = TRUE)
  )

  raw <- content(res, "text", encoding = "UTF-8")
  #print("=== Raw response ===")
  #print(raw)

  result <- fromJSON(raw, simplifyVector = FALSE)
  content_text <- result$choices[[1]]$message$content

  # Use regex to extract only the first 0, 1, or 2
  library(stringr)
  choice <- str_extract(content_text, "[012](?=\\s|$|\\b)")

  print(paste("Model replied (cleaned):", choice))
  return(choice)
}
```

5.a Collect Responses

``` r
# Run the study (comment out during testing)
#slice the first 2 rows
pairs_test <- pairs_df |> slice_head(n = 5)

rate_limit_pause <- 10  
print(pairs_test) 
```

    ## # A tibble: 5 × 3
    ##   Product1_ID Product2_ID Prompt                                                
    ##   <chr>       <chr>       <chr>                                                 
    ## 1 P53         P74         "You are a customer. You are selected at random while…
    ## 2 P41         P53         "You are a customer. You are selected at random while…
    ## 3 P11         P26         "You are a customer. You are selected at random while…
    ## 4 P49         P62         "You are a customer. You are selected at random while…
    ## 5 P21         P85         "You are a customer. You are selected at random while…

``` r
responses <- pairs_test %>%
  mutate(Response = map_chr(Prompt, ~ {
    Sys.sleep(rate_limit_pause)
    query_llm(.x)  # capture the choice directly
  }))
```

    ## [1] "Model replied (cleaned): 2"
    ## [1] "Model replied (cleaned): 2"
    ## [1] "Model replied (cleaned): 1"
    ## [1] "Model replied (cleaned): 1"
    ## [1] "Model replied (cleaned): 2"

``` r
head(responses)
```

    ## # A tibble: 5 × 4
    ##   Product1_ID Product2_ID Prompt                                        Response
    ##   <chr>       <chr>       <chr>                                         <chr>   
    ## 1 P53         P74         "You are a customer. You are selected at ran… 2       
    ## 2 P41         P53         "You are a customer. You are selected at ran… 2       
    ## 3 P11         P26         "You are a customer. You are selected at ran… 1       
    ## 4 P49         P62         "You are a customer. You are selected at ran… 1       
    ## 5 P21         P85         "You are a customer. You are selected at ran… 2

Reference: Brand, James, et al. “Using LLMs for Market Research.” SSRN
Electronic Journal, 2023, <https://doi.org/10.2139/ssrn.4395751>.
