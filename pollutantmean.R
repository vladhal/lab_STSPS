pollutantmean <- function(directory, pollutant, id = 1:332)
{
  setwd(file.path(getwd(), directory)) ## встановлення каталогу
  total = 0 ## сума всіх спостережуваних значень забруднювачів (або сульфатів, або нітратів)
  observations = 0 ## загальна кількість спостережуваних значень забруднювача (сульфату або нітрату)
  for (i in id) ##Перевірка через файли каталогу, вказані в аргументі id
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
    observations = observations + nrow(data) ## сукупне додавання повних спостережень
    if (pollutant == "sulfate") {total = total + sum(data$sulfate)} ## залежно від забруднювача (сульфату чи нітрату), ми агрегуємо спостережувані значення
    else {total = total + sum(data$nitrate)}
  }
  setwd("..") ## скидання шляху до каталогу
  return (total/observations) ## повернення середнього значення забруднювача
}
