XLKill <-
function(){
  # Excel would only quit, when all workbooks are closed before, someone said.
  # http://stackoverflow.com/questions/15697282/excel-application-not-quitting-after-calling-quit

  # We experience, that it Would not even then quit, when there's no workbook loaded at all. 
  # so killing the task is "ultima ratio"...
  
  shell('taskkill /F /IM EXCEL.EXE')
}
