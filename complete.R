complete <- function(directory, id = 1:332)
{
  dataframe = NULL  ## ініціалізація кадрів даних, які ми хочемо отримати від цієї функції
  setwd(file.path(getwd(), directory)) ## встановлення каталогу
  for (i in id) ## Перевірка через файли каталогу, вказані в аргументі id
  {        
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
    data = as.matrix(data) ## робимо матрицею, щоб легко заповнити кожен послідовний ряд нашого кадру даних
    dataframe = rbind(dataframe, c(i,nrow(data))) ## заповнюємо кожен послідовний ряд нашого кадру даних. Кожен рядок містить ідентифікатор монітора, і його загальні повні випадки спостереження (без рядків, що входять до "NA")
  }
  setwd("..") ## скидання шляху до каталогу
  dataframe = data.frame(dataframe) ## від матриці до кадру даних
  names(dataframe) = c('id', 'nobs') ## встановлення назви стовпчиків кадру даних
  return (dataframe) 
}
