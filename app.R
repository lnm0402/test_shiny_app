library(shiny)
library(shinyjs)

# define a tibble of allwed users (this can also be read from a local file or from a database)
allowed_users <- tibble(
  user_email = c("lnm0402@@gmail.com",
                 "user2@example.com"))

server <- function(input, output, session){
  
  # initialize authenticated reactive values ----
  # In addition to these three (auth, name, email)
  # you can add additional reactive values here, if you want them to be based on the user which logged on, e.g. privileges.
  user <- reactiveValues(auth = FALSE, # is the user authenticated or not
                         name = NULL, # user's name as stored and returned by cognito
                         email = NULL)  # user's email as stored and returned by cognito
  
  # get the url variables ----
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!("code" %in% names(query))){
      # no code in the url variables means the user hasn't logged in yet
      showElement("login")
    } else {
      current_user <- retrieve_user_data(query$code)
      # if an error occurred during login
      if (is.null(current_user)){
        hideElement("login")
        showElement("login_error_aws_flow")
        showElement("submit_sign_out_div")
        user$auth <- FALSE
      } else {
        # check if user is in allowed user list
        # for more robustness, use stringr::str_to_lower to avoid case sensitivity
        # i.e., (str_to_lower(current_user$email) %in% str_to_lower(allowed_users$user_email))
        if (current_user$email %in% allowed_users$user_email){
          hideElement("login")
          showElement("login_confirmed")
          showElement("submit_sign_out_div")
          
          user$auth <- TRUE
          user$email <- current_user$email
          user$name <- current_user$name
          
          # ==== User is valid, continue prep ====
          
          # show the welcome box with user name
          output$confirmed_login_name <-
            renderText({
              paste0("Hi there!, ",
                     user$name)
            })
          
          # ==== Put additional login dependent steps here (e.g. db read from source) ====
          
          # ADD HERE YOUR REQUIRED LOGIC
          # I personally like to select the first tab for the user to see, i.e.:
          showTab("main_navigation", "content_tab_id", select = TRUE) 
          # (see the next chunk for how this tab is defined in terms of ui elements)
          
          # ==== Finish loading and go to tab ====
          
        } else {
          # user not allowed. Only show sign-out, perhaps also show a login error message.
          hideElement("login")
          showElement("login_error_user")
          showElement("submit_sign_out_div")
        }
      }
    }
  })
  
  # This is where you will put your actual elements (the server side that is) ----
  # For example:
  data <- read.csv('data.csv')
  output$distPlot <- renderPlot({
    # *** THIS IS EXTREMELY IMPORTANT!!! ***
    validate(need(user$auth, "No privileges to watch data. Please contact support."))
    # since shinyjs is not safe for hiding content, make sure that any information is covered
    # by the validate(...) expression as was specified. 
    # Rendered elements which were not preceded by a validate expression can be viewed in the html code (even if you use hideElement).
    
    barplot(Age ~ Name, data = data,main=input$title)
  })
}
  
  
  

ui <- fluidPage(
  useShinyjs(), # to enable the show/hide of elements such as login and buttons
  hidden( # this is how the logout button will like:
    div(
      id = "submit_sign_out_div",
      a(id = "submit_sign_out",
        "logout",
        href = aws_auth_logout,
        style = "color: black; 
              -webkit-appearance: button; 
              -moz-appearance: button; 
              appearance: button; 
              text-decoration: none; 
              background:#ff9999; 
              position: absolute; 
              top: 0px; left: 20px; 
              z-index: 10000;
              padding: 5px 10px 5px 10px;"
      )
    )
  ),
  navbarPage(
    "Cognito auth example",
    id = "main_navigation",
    tabPanel(
      "identification",
      value = "login_tab_id",
      h1("Login"),
      div(
        id = "login",
        p("To login you must identify with a username and password"),
        # This defines a login button which upon click will redirect to the AWS Cognito login page
        a(id = "login_link",
          "Click here to login",
          href = aws_auth_redirect,
          style = "color: black;
                  -webkit-appearance: button;
                  -moz-appearance: button;
                  appearance: button;
                  text-decoration: none;
                  background:#95c5ff;
                  padding: 5px 10px 5px 10px;")
      ),
      hidden(div(
        id = "login_error_aws_flow",
        p("An error has occurred."),
        p("Please contact support")
      )),
      hidden(
        div(
          id = "login_confirmed",
          h3("User confirmed"),
          fluidRow(
            textOutput("confirmed_login_name")),
          fluidRow(
            p("Use the menu bar to navigate."),
            p(
              "Don't forget to logout when you want to close the system."
            )
          )
        )
      ),
    ),
    tabPanel("Your actual content", 
             value = "content_tab_id",
             fluidRow(selectInput("title",'Input title of plot',c('Title!','Title.','The Title'))),
             fluidRow( plotOutput("distPlot")))
  )
)
# Run the application 
shinyApp(ui = ui, server = server)
