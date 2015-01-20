WrdKill <-
function(){
  # Word might not alwasy quit and end the task
  # so killing the task is "ultima ratio"...
  
  shell('taskkill /F /IM WINWORD.EXE')
}
