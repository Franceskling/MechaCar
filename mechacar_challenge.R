mechacar_data <- read.csv('MechaCar_mpg.csv') #import data set
head(mechacar_data)
lm(mpg~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD, data=mechacar_data) #generate multiple linear regression model
summary(lm(mpg~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD, data=mechacar_data)) #generate summary statistics

suspensionCoil_data <- read.csv('Suspension_Coil.csv') #import data suspension coil data set
suspensionCoil_data$VehicleID<- as.factor(suspensionCoil_data$VehicleID) #change VehicleID from character to factor 
suspensionCoil_data$Manufacturing_Lot<- as.factor(suspensionCoil_data$Manufacturing_Lot) #change manu lot from character to factor 
summary_suspensionCoil <- suspensionCoil_data %>% group_by(Manufacturing_Lot) %>% summarize(PSI_Mean=mean(PSI), PSI_MEdian=median(PSI), PSI_SD=sd(PSI), PSI_Variance=var(PSI)) #create summary table

sample_table <- suspensionCoil_data %>% sample_n(50) #randomly sample 50 data points
t.test(log10(sample_table$PSI),mu=mean(log10(suspensionCoil_data$PSI))) #compare sample versus population means