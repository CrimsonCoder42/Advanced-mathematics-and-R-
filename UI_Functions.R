
make_flashcard_buttons = function(df){
  all_buttons = tagList()
  for(i in 1:nrow(df)){
    #the elements to be added to the UI
    
    #code related to the outermost button (main flashcard button)
    button_id = paste0("btn_aa_q",i)
    button_text = paste0("Flash Card ", i)
    
    #code related to the front of the card
    flash_question = df[i,]$Question
    flash_id_front = paste0("flash_aa_q",i)
    flash_flip_button_id = paste0("flash_flip_q",i)
    flash_flip_button = actionButton(flash_flip_button_id, "Flip")
    
    #code related to the back of the card
    flash_answer = df[i,]$Answer
    flash_id_back = paste0("flash_aa_a",i)
    
    #the modal for the back of the card
    modal_back = bs_modal(id = flash_id_back, title = "", body = flash_answer)
    flash_flip_button_w_modal = flash_flip_button %>% bs_attach_modal(flash_id_back)
    
    #the modal for the front of the card
    modal_front = bs_modal(id = flash_id_front, title = "", body = flash_question,
                           footer = flash_flip_button_w_modal)
    
    #the final button / appending the button
    one_button = actionBttn(button_id,button_text) %>% bs_attach_modal(flash_id_front)
    all_buttons = tagList(all_buttons, one_button, br(), modal_front, modal_back)
  }
  return(all_buttons)
}

make_r_question_buttons = function(df){
 
  all_buttons = tagList()
  for(i in 1:nrow(df)){
    
    #code related to the outermost button (main flashcard button)
    button_id = paste0("btn_r_q",i)
    button_text = paste0("Question ", i)
    
    #code related to the front of the card
    question = df[i,]$Question
    modal_id = paste0("modal_r_q",i)
    
    #make a modal for a correct and incorrect answer
    modal_correct_id = paste0("correct_r_q",i)
    modal_incorrect_id = paste0("incorrect_r_q",i)
    modal_correct = bs_modal(id = modal_correct_id, title = "", body = "Correct")
    modal_incorrect = bs_modal(id = modal_incorrect_id, title = "", body = "Incorrect")
    
    #js code to change button colors
    js_code_correct = paste0("document.getElementById('", button_id, "').style.background = 'green'")
    js_code_incorrect = paste0("document.getElementById('", button_id, "').style.background = 'red'")
    
    if(ifelse(is.na(df[i,2]), "", df[i,2]) == df[i,]$Correct_Answer){
      button_1 = actionButton(paste0("btn_r_q",i,"_a1"), df[i,]$Answer_1, onClick = js_code_correct) %>%
        bs_attach_modal(modal_correct_id)
    } else{
      button_1 = actionButton(paste0("btn_r_q",i,"_a1"), df[i,]$Answer_1, onClick = js_code_incorrect) %>%
        bs_attach_modal(modal_incorrect_id)
    }
    
    if(ifelse(is.na(df[i,3]), "", df[i,3]) == df[i,]$Correct_Answer){
      button_2 = actionButton(paste0("btn_r_q",i,"_a2"), df[i,]$Answer_2, onClick = js_code_correct) %>% 
        bs_attach_modal(modal_correct_id)
    } else{
      button_2 = actionButton(paste0("btn_r_q",i,"_a2"), df[i,]$Answer_2, onClick = js_code_incorrect) %>% 
        bs_attach_modal(modal_incorrect_id)
    }
    
    if(ifelse(is.na(df[i,4]), "", df[i,4]) == df[i,]$Correct_Answer){
      button_3 = actionButton(paste0("btn_r_q",i,"_a3"), df[i,]$Answer_3, onClick = js_code_correct) %>% 
        bs_attach_modal(modal_correct_id)
    } else{
      button_3 = actionButton(paste0("btn_r_q",i,"_a3"), df[i,]$Answer_3, onClick = js_code_incorrect) %>%
        bs_attach_modal(modal_incorrect_id)
    }
    
    if(ifelse(is.na(df[i,5]), "", df[i,5]) == df[i,]$Correct_Answer){
      button_4 = actionButton(paste0("btn_r_q",i,"_a4"), df[i,]$Answer_4, onClick = js_code_correct) %>% 
        bs_attach_modal(modal_correct_id)
    } else{
      button_4 = actionButton(paste0("btn_r_q",i,"_a4"), df[i,]$Answer_4, onClick = js_code_incorrect) %>% 
        bs_attach_modal(modal_incorrect_id)
    }
    

    #making the body of the modal
    one_row = df[i,]
    modal_body = tagList(
      question,
      br(),
      splitLayout(
        cellWidths = "25%",
        button_1,
        button_2,
        button_3,
        button_4
      )
    )
    
    #the modal for the front of the card
    modal = bs_modal(id = modal_id, title = "", body = modal_body)
    
    #the final button / appending the button
    one_button = actionBttn(button_id,button_text) %>% bs_attach_modal(modal_id)
    all_buttons = tagList(all_buttons, one_button, br(), modal, modal_correct, modal_incorrect)
    
  }
  return(all_buttons)
}

make_linear_algebra_buttons = function(df){
  
  all_buttons = tagList()
  for(i in 1:nrow(df)){
    
    #code related to the outermost button (main flashcard button)
    button_id = paste0("btn_la_q",i)
    button_text = df[i,]$Button_Title
    
    #the question and answer image paths
    question_image_path = df[i,]$Question_Image 
    answer_image_path = df[i,]$Answer_Image %>% str_split(", ") %>% unlist()
    
    #the IDs for the initial question modal and the answer modal
    question_modal_id = paste0("modal_la_q",i)
    answer_modal_id = paste0("modal_la_a",i)
    
    
    #make a modal for a correct and incorrect answer
    modal_correct_id = paste0("correct_la_q",i)
    modal_incorrect_id = paste0("incorrect_la_q",i)
    modal_correct = bs_modal(id = modal_correct_id, title = "", body = "Correct")
    modal_incorrect = bs_modal(id = modal_incorrect_id, title = "", body = "Incorrect")
    
    js_code_correct = paste0("document.getElementById('", button_id, "').style.background = 'green'")
    js_code_incorrect = paste0("document.getElementById('", button_id, "').style.background = 'red'")

    correct_answer = df[i,]$Correct_Answer
    if(correct_answer == "a"){
      button_a = actionButton(paste0("btn_la_q",i,"_a"), "a", onClick = js_code_correct) %>% 
        bs_attach_modal(modal_correct_id)
      button_b = actionButton(paste0("btn_la_q",i,"_b"), "b", onClick = js_code_incorrect) %>% 
        bs_attach_modal(modal_incorrect_id)
      button_c = actionButton(paste0("btn_la_q",i,"_c"), "c", onClick = js_code_incorrect) %>% 
        bs_attach_modal(modal_incorrect_id)
      button_d = actionButton(paste0("btn_la_q",i,"_d"), "d", onClick = js_code_incorrect) %>% 
        bs_attach_modal(modal_incorrect_id)
    } 
    if(correct_answer == "b"){
      button_a = actionButton(paste0("btn_la_q",i,"_a"), "a", onClick = js_code_incorrect) %>% 
        bs_attach_modal(modal_incorrect_id)
      button_b = actionButton(paste0("btn_la_q",i,"_b"), "b", onClick = js_code_correct) %>% 
        bs_attach_modal(modal_correct_id)
      button_c = actionButton(paste0("btn_la_q",i,"_c"), "c", onClick = js_code_incorrect) %>% 
        bs_attach_modal(modal_incorrect_id)
      button_d = actionButton(paste0("btn_la_q",i,"_d"), "d", onClick = js_code_incorrect) %>% 
        bs_attach_modal(modal_incorrect_id)
    } 
    if(correct_answer == "c"){
      button_a = actionButton(paste0("btn_la_q",i,"_a"), "a", onClick = js_code_incorrect) %>% 
        bs_attach_modal(modal_incorrect_id)
      button_b = actionButton(paste0("btn_la_q",i,"_b"), "b", onClick = js_code_incorrect) %>% 
        bs_attach_modal(modal_incorrect_id)
      button_c = actionButton(paste0("btn_la_q",i,"_c"), "c", onClick = js_code_correct) %>% 
        bs_attach_modal(modal_correct_id)
      button_d = actionButton(paste0("btn_la_q",i,"_d"), "d", onClick = js_code_incorrect) %>% 
        bs_attach_modal(modal_incorrect_id)
    } 
    if(correct_answer == "d"){
      button_a = actionButton(paste0("btn_la_q",i,"_a"), "a", onClick = js_code_incorrect) %>% 
        bs_attach_modal(modal_incorrect_id)
      button_b = actionButton(paste0("btn_la_q",i,"_b"), "b", onClick = js_code_incorrect) %>% 
        bs_attach_modal(modal_incorrect_id)
      button_c = actionButton(paste0("btn_la_q",i,"_c"), "c", onClick = js_code_incorrect) %>% 
        bs_attach_modal(modal_incorrect_id)
      button_d = actionButton(paste0("btn_la_q",i,"_d"), "d", onClick = js_code_correct) %>% 
        bs_attach_modal(modal_correct_id)
    } 
    
    answer_images = tagList()
    for(path in answer_image_path){
      answer_images = tagList(answer_images, img(src = path, style = "width:50rem;") )
    }
    
    #the modal for the back of the card
    modal_answer = bs_modal(id = answer_modal_id, title = "", 
                            body = answer_images )
    answer_button = actionButton(paste0("answer_btn_q",i), "Show answer")
    answer_button_w_modal = answer_button %>% bs_attach_modal(answer_modal_id)
    
    modal_question_footer = splitLayout(
      cellWidths = c("15%","15%","15%","15%","20%"),
      button_a,
      button_b,
      button_c,
      button_d,
      answer_button_w_modal
    )
    
    #the modal for the front of the card
    modal_question = bs_modal(id = question_modal_id, title = "",
                              body = img(src = question_image_path, style = "width:50rem;") ,
                              footer = modal_question_footer)
    
    
    #the final button / appending the button
    one_button = actionBttn(button_id,button_text) %>% bs_attach_modal(question_modal_id)
    all_buttons = tagList(all_buttons, one_button, br(),
                          modal_question, modal_answer, modal_correct, modal_incorrect)
    
  }
  
  return(all_buttons)
  
}

#NOT FINISHED YET!!!
make_multiple_choice_buttons = function(df){
  
  all_buttons = tagList()
  for(i in 1:nrow(df)){
    #the elements to be added to the UI
    
    #code related to the outermost button (main flashcard button)
    button_id = paste0("btn_la_q",i)
    button_text = paste0("Question ", i)
    
    #code related to the front of the card
    flash_question = df[i,]$Question
    flash_id_front = paste0("flash_aa_q",i)
    flash_flip_button_id = paste0("flash_flip_q",i)
    flash_flip_button = actionButton(flash_flip_button_id, "Flip")
    
    #code related to the back of the card
    flash_answer = df[i,]$Answer
    flash_id_back = paste0("flash_aa_a",i)
    
    #the modal for the back of the card
    modal_back = bs_modal(id = flash_id_back, title = "", body = flash_answer)
    flash_flip_button_w_modal = flash_flip_button %>% bs_attach_modal(flash_id_back)
    
    #the modal for the front of the card
    modal_front = bs_modal(id = flash_id_front, title = "", body = flash_question,
                           footer = flash_flip_button_w_modal)
    
    #the final button / appending the button
    one_button = actionBttn(button_id,button_text) %>% bs_attach_modal(flash_id_front)
    all_buttons = tagList(all_buttons, one_button, br(), modal_front, modal_back)
  }
  return(all_buttons)
  
}

make_video_buttons = function(df){
  
  all_buttons = tagList()
  for(i in 1:nrow(df)){
    button_id = paste0("btn_vid_",i)
    button_text = df[i,]$Title
    button_onclick = paste0("window.open('", df[i,]$Link, "')")
    one_button = actionButton(button_id,button_text, onclick = button_onclick)
    all_buttons = tagList(all_buttons, one_button, br())
  }
  return(all_buttons)
}