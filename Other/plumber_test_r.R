


#############################
##  Load needed libraries  ##
#############################
library("tidyverse")
library("h2o")


#################################
##  Set the working directory  ##
#################################
wd <- "/Users/mdturse/Desktop/Analytics/Sparkfund"


##############################
##  Load train & test data  ##
##############################
train_data <-
  read_rds(path = paste0(wd,
                         "/Data/Processed/",
                         "train_data.rds"
  )
  )

test_data <-
  read_rds(path = paste0(wd,
                         "/Data/Processed/",
                         "test_data.rds"
  )
  )


##########################
##  Load the H2O Model  ##
##########################
h2o.init()

model_path_h2o <-
  read_rds(path = paste0(wd,
                         "/Models/",
                         "model_path_h2o.rds"
  )
  )

# print(model_path_h2o)

automl_leader <- h2o.loadModel(path = model_path_h2o)


#############################
##  Take model prediction  ##
##  with Plumber           ##
#############################

# Test by just using one record
input_data <-
  test_data[1, ] %>% 
  select(-failure)

input_data_h2o <-
  input_data %>% 
  as.h2o()

# predict test case using the H2O model & Plumber
pred <-
  predict(automl_leader, input_data_h2o) %>% 
  as.data.frame() %>% 
  mutate(predict = case_when(predict == 1 ~ "failure",
                             predict == 0 ~ "non_failure",
                             TRUE ~ "unknown"
  )
  )

cat("----------------\nTest case predicted to be",
    as.character(pred$predict),
    "\n----------------"
)

# automl_leader



########################
##  Create Meta Data  ##
########################
str(train_data)

# show parameter definitions
var_names <- colnames(train_data)
for (i in 1:length(var_names)
) {
  var <- var_names[i]
  train_data_subs <-
    train_data[ , which(colnames(train_data) == var)] %>% 
    pull(1)
  type <- class(train_data_subs)
  
  if (type == "numeric") {
    min <- min(train_data_subs, na.rm = TRUE)
    max <- max(train_data_subs, na.rm = TRUE)
    
    cat("\n----------\n",
        "Variable:", var, "is of type:", type, "\n",
        "Min value in training data =", min, "\n",
        "Max value in training data =", max,
        "\n----------\n"
    )
  }
  
  if (type != "numeric") {
    cat("\n----------\n",
        "Variable:", var, "is of type:", type, "\n",
        # "Min value in training data =", min, "\n",
        # "Max value in training data =", max,
        "\n----------\n"
    )
  }
  
}


##################################
##  Create test data with JSON  ##
##################################
library("rjson")
test_case_json <- toJSON(input_data)
cat(test_case_json)


##  Use the call below from the terminal to test Plumber
# curl -H "Content-Type: application/json" -X GET -d '{"model2.MD04ABA400V":-0.261652798576857,"model2.MG07ACA14TA":-1.03242994929405,"model2.MQ01ABF050":-0.556545628617062,"model2.MQ01ABF050M":2.37859456307233,"smart_3_raw":-1.44829129454356,"smart_4_raw":-1.30176480120924,"smart_9_raw":1.55907395438345,"smart_12_raw":0.822450264989027,"smart_191_raw":-0.561869475724538,"smart_192_raw":-0.728452250447267,"smart_193_raw":-0.664158540978331,"smart_194_raw":-0.204562521841788,"smart_222_raw":-0.424904334200471,"smart_226_raw":-0.939211211047469,"weight_col":1}' "http://127.0.0.1:8000/predict"


