shinyUI(pageWithSidebar(
  headerPanel("Income Classification on Adult Dataset"),
  sidebarPanel(
    selectInput("workclass", "Select workclass:", 
                choices = c(" Federal-gov"," Local-gov"," Private"," Self-emp-inc"," Self-emp-not-inc"," State-gov","Unemployed")),
    selectInput("education", "Select education:", 
                choices = c(" Assoc-acdm"," Assoc-voc"," Bachelors"," HS-grad"," Prof-school"," Some-college","HigherEdu","SchoolLevel")),
    selectInput("MaritalStatus", "Select MaritalStatus:", 
                choices = c(" Never-married"," Widowed","Married","NotTogether")),
    selectInput("occupation", "Select occupation:", 
                choices = c(" Adm-clerical"," Exec-managerial"," Prof-specialty"," Protective-serv"," Sales"," Tech-support","BlueCollar","Other-Services")),
    selectInput("relationship", "Select relationship:", 
                choices = c(" Husband"," Not-in-family"," Other-relative"," Own-child"," Unmarried"," Wife")),
    selectInput("race", "Select race:", 
                choices = c(" Black"," White","Others")),
    selectInput("sex", "Select sex:", 
                choices = c(" Male"," Female")),
    numericInput("CapitalGain", "Enter the CapitalGain(No negative Values):",min=0,value=0),
    numericInput("CapitalLoss", "Enter the CapitalLoss(No Negative Values):",min=0,value=0),
    numericInput("HoursPerWeek", "Enter the HoursPerWeek(No Negative Values):",min=0,value=40),
    selectInput("NativeCountry", "Select NativeCountry:", 
                choices = c(" Canada"," Iran"," Mexico","Asia","Caribbean-Islands","Central-America","Eurasia","Great-Britain","South-America","South-East-Asia","United-States")),
    submitButton("Predict")
  ), mainPanel(tableOutput("results"))
))
