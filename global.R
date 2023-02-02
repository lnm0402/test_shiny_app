base_cognito_url <- "https://YOUR_DOMAIN.YOUR_AMAZON_REGION.amazoncognito.com/"
app_client_id <- "YOUR_APP_CLIENT_ID"
app_client_secret <- "YOUR_APP_CLIENT_SECRET"
redirect_uri <- "https://YOUR_APP/redirect_uri"

library(httr)

app <- oauth_app(appname = "my_shiny_app",
                 key = app_client_id,
                 secret = app_client_secret,
                 redirect_uri = redirect_uri)
cognito <- oauth_endpoint(authorize = "authorize",
                          access = "token",
                          base_url = paste0(base_cognito_url, "oauth2"))


retrieve_user_data <- function(user_code){
  
  failed_token <- FALSE
  
  # get the token
  tryCatch({token_res <- oauth2.0_access_token(endpoint = cognito,
                                               app = app,
                                               code = user_code,
                                               user_params = list(client_id = app_client_id,
                                                                  grant_type = "authorization_code"),
                                               use_basic_auth = TRUE)},
           error = function(e){failed_token <<- TRUE})
  
  # check result status, make sure token is valid and that the process did not fail
  if (failed_token) {
    return(NULL)
  }
  
  # The token did not fail, go ahead and use the token to retrieve user information
  user_information <- GET(url = paste0(base_cognito_url, "oauth2/userInfo"), 
                          add_headers(Authorization = paste("Bearer", token_res$access_token)))
  
  return(content(user_information))
  
}