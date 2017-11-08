corr <- function(directory, threshold = 0)
{
  setwd(file.path(getwd(), directory)) ## встановлення каталогу
  correlationVector = NULL ## ініціалізація кореляційної матриці
  for (i in 1:332) ##Перевірка через всі файли каталогу 
  {
    ## Due to the format of the filename, i.e 001, 010  instead of 1, 10. I became aware that the following method works but not efficient, 
    ## but at the time of the completion of this assignment, it was the only way I knew how to do it.           
    if (i <10)
    { 
      data <- read.csv(paste("0","0", as.character(i), ".csv", sep=""), ## Якщо 'id' = 7, то ми отримаємо 007.csv
                       header = T, 
                       na.strings=c("NA","NaN", " "))
    }
    else if (i>=10 & i<100)
    { 
      data <- read.csv(paste("0", as.character(i), ".csv", sep=""),
                       header = T, 
                       na.strings=c("NA","NaN", " "))
    }
    else
    { 
      data <- read.csv(paste(as.character(i), ".csv", sep=""),
                       header = T, 
                       na.strings=c("NA","NaN", " "))
    }
    data = na.omit(data) ## Позбавляємся усіх значень "NA" і всіх незавершених спостережень (ті, що мають щонайменше одну "NA" у рядку)
    if (nrow(data) > threshold) {correlationVector = c(correlationVector, cor(data[,2], data[,3]))} ## якщо кількість повних спостережуваних випадків відповідає квоті, знаходимо співвідношення між забруднювачами для даного монітора і зберігаємо результати в кореляційній матриці
  }
  setwd("..") ## скидання шляху до каталогу
  return (correlationVector)
}
