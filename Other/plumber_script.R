# script name:
# plumber_script.R

# set API title and description to show up in http://localhost:8000/__swagger__/

#* @apiTitle Run predictions for Hard Drive Failure with H2O's AutoML
#* @apiDescription This API takes as hard drive data and returns a prediction whether the hard drive
#* will fail or not.


# setup
library("tidyverse")
library("h2o")

wd <- "/Users/mdturse/Desktop/Analytics/Sparkfund"


# load the model
h2o.init()

model_path_h2o <-
  read_rds(path = paste0(wd,
                         "/Models/",
                         "model_path_h2o.rds"
  )
  )

automl_leader <- h2o.loadModel(path = model_path_h2o)


#* Log system time, request method, and HTTP user agent of the incoming request
#* @filter logger
function(req){
  cat("System time:", as.character(Sys.time()), "\n",
      "Request method:", req$REQUEST_METHOD, req$PATH_INFO, "\n",
      "HTTP user agent:", req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  
  plumber::forward()
}

# core function follows below:
# define parameters with type and description
# name endpoint
# return output as html/text
# specify 200 (okay) return

#* predict hard drive failure of a test case with an H2O AutoML model
#* @param model2.MD04ABA400V:numeric An indicator for this model (logged, centered, and scaled)
#* @param model2.MG07ACA14TA:numeric An indicator for this model (logged, centered, and scaled)
#* @param model2.MQ01ABF050:numeric An indicator for this model (logged, centered, and scaled)
#* @param model2.MQ01ABF050M:numeric An indicator for this model (logged, centered, and scaled)
#* @param smart_3_raw:numeric Representation of the variable was not provided (logged, centered, and scaled)
#* @param smart_4_raw:numeric Representation of the variable was not provided (logged, centered, and scaled)
#* @param smart_9_raw:numeric The number of hours a drive has been in service (logged, centered, and scaled)
#* @param smart_12_raw:numeric Representation of the variable was not provided (logged, centered, and scaled)
#* @param smart_191_raw:numeric Representation of the variable was not provided (logged, centered, and scaled)
#* @param smart_192_raw:numeric Representation of the variable was not provided (logged, centered, and scaled)
#* @param smart_193_raw:numeric Representation of the variable was not provided (logged, centered, and scaled)
#* @param smart_194_raw:numeric Representation of the variable was not provided (logged, centered, and scaled)
#* @param smart_222_raw:numeric Representation of the variable was not provided (logged, centered, and scaled)
#* @param smart_226_raw:numeric Representation of the variable was not provided (logged, centered, and scaled)
#* @param weight_col:numeric Representation of the weight (importance) given to each record (predicting "failure" is mor important than "non_failure")

#* @get /predict
#* @html

#* @response 200 Returns the class (failure or non_failure) prediction from the H2O AutoML model
calculate_prediction <-
  function(model2.MD04ABA400V,
           model2.MG07ACA14TA,
           model2.MQ01ABF050,
           model2.MQ01ABF050M,
           smart_3_raw,
           smart_4_raw,  
           smart_9_raw, 
           smart_12_raw, 
           smart_191_raw,
           smart_192_raw,
           smart_193_raw, 
           smart_194_raw,
           smart_222_raw,
           smart_226_raw,
           weight_col
  ) {
    
    # make data frame from numeric parameters
    input_data_num <<-
      data.frame(model2.MD04ABA400V,
                 model2.MG07ACA14TA,
                 model2.MQ01ABF050,
                 model2.MQ01ABF050M,
                 smart_3_raw,
                 smart_4_raw,  
                 smart_9_raw, 
                 smart_12_raw, 
                 smart_191_raw,
                 smart_192_raw,
                 smart_193_raw, 
                 smart_194_raw,
                 smart_222_raw,
                 smart_226_raw,
                 weight_col,
                 stringsAsFactors = FALSE
      )
    
    # and make sure they really are numeric
    input_data_num <<-
      as.data.frame(t(sapply(input_data_num,
                             as.numeric
      )
      )
      )
    
    # combine into one data frame
    # input_data <<- as.data.frame(cbind(input_data_num, input_data_int))
    input_data <<- as.data.frame(input_data_num)
    
    # validation for parameter
    if (any(is.na(input_data)
    )
    ) {
      res$status <- 400
      res$body <- "Parameters have to be numeric"
    }
    
    # predict and return result
    pred <<- predict(automl_leader,
                     as.h2o(input_data)
    ) %>% 
      as.data.frame() %>% 
      mutate(predict = case_when(predict == 1 ~ "failure",
                                 predict == 0 ~ "non_failure",
                                 TRUE ~ "unknown"
      )
      )
    paste("----------------\nTest case predicted to be", as.character(pred$predict), "\n----------------\n")
  }


