library(ggplot2)

# Create a variable for the path for the output files generated in the script
# Change path to directory for output files between the single quotes on line 5 using similar formatting
analyteOUTPUT <- 'C:/.../.../'

# Convert csv file with analyte data to an R data frame
# Change path to your data file between the single quotes on line 9 using similar formatting 
ANALYTE <- read.csv('P:/NBS/NSP Reporting/SCID/TrecData20171.csv')

# Calculate the mean and standard deviation for analyte data
mean(ANALYTE$VALUE)
sd(ANALYTE$VALUE)

# Calculate the number samples present in the analyte data
nrow(ANALYTE)

# Calculate percentiles for analyte data
# Numeric values on line 20 can be changed to calculate desired percentiles
quantile(ANALYTE$VALUE, c(.005, .01, .02, .025, .05))

# Generate a histogram graph for newborn age
# To change graph title, enter new title between the single quotes on line 28
ggplot(ANALYTE, aes(AGE)) +
  geom_histogram(breaks=seq(0, 5, by = 1), color = 'blue', fill = 'blue', alpha = 0.3) +
  scale_x_continuous(name = 'Age (Days)') +
  scale_y_continuous(name = 'Count') +
  ggtitle('Age Histogram')

# Save the histogram as an image file
# Change file name to save your image file between the single quotes on line 32 using similar formatting
ggsave(file.path(analyteOUTPUT, '....png'))

# Generate a density graph for analyte data
# To restrict the limit on the x axis, change the second number on line 41
# For x axis label, remove the hash/pound sign and enter label between the single quotes on line 42
# For y axis label, remove the hash/pound sign and enter label between the single quotes on line 43
# To change graph title, enter new title between the single quotes on line 44
ggplot(ANALYTE, aes(VALUE)) +
  geom_density(color = 'blue', fill = 'blue', alpha = 0.3) +
  xlim(0, 1500) +
#  xlab(label = '') +
#  scale_y_continuous(name = '') +
  ggtitle('Analyte Data')

# Save the density graph as an image file
# Change file name to save your image file between the single quotes on line 48 using similar formatting
ggsave(file.path(analyteOUTPUT, '....png'))



# Restrict data by removing outliers if needed
# Numeric values on line 54 can be changed assess data with outliers removed
ANALYTE_SEL <- ANALYTE[which(ANALYTE$VALUE <= 300),]

# Calculate the mean and standard deviation for analyte data
mean(ANALYTE_SEL$VALUE)
sd(ANALYTE_SEL$VALUE)

# Calculate percentiles for analyte data
# Numeric values on line 62 can be changed to calculate desired percentiles
quantile(ANALYTE_SEL$VALUE, c(.005, .01, .02, .025, .05))

# Generate a density graph for analyte data with outliers removed
# For x axis label, remove the hash/pound sign and enter label between the single quotes on line 70
# For y axis label, remove the hash/pound sign and enter label between the single quotes on line 71
# To change graph title, enter new title between the single quotes on line 72
ggplot(ANALYTE_SEL, aes(VALUE)) +
  geom_density(color = 'blue', fill = 'blue', alpha = 0.3) +
#  scale_x_continuous(name = '') +
#  scale_y_continuous(name = '') +
  ggtitle('Analyte Data')

# Save the density graph as an image file
# Change file name to save your image file between the single quotes on line 76 using similar formatting
ggsave(file.path(analyteOUTPUT, '....png'))



# Split analyte data by age
ANALYTE1Day <- ANALYTE[ANALYTE$AGE <= 1, ]
mean(ANALYTE1Day$VALUE)
sd(ANALYTE1Day$VALUE)
nrow(ANALYTE1Day)

ANALYTE2Day <- ANALYTE[ANALYTE$AGE == 2, ]
mean(ANALYTE2Day$VALUE)
sd(ANALYTE2Day$VALUE)
nrow(ANALYTE2Day)

ANALYTE3Day <- ANALYTE[ANALYTE$AGE == 3, ]
mean(ANALYTE3Day$VALUE)
sd(ANALYTE3Day$VALUE)
nrow(ANALYTE3Day)

ANALYTE4Day <- ANALYTE[ANALYTE$AGE == 4, ]
mean(ANALYTE4Day$VALUE)
sd(ANALYTE4Day$VALUE)
nrow(ANALYTE4Day)

ANALYTE5Day <- ANALYTE[ANALYTE$AGE == 5, ]
mean(ANALYTE5Day$VALUE)
sd(ANALYTE5Day$VALUE)
nrow(ANALYTE5Day)

# Generate density graphs for analyte data separated by newborn age
# To restrict the limit on the x axis, change the second number on line 118
# For x axis label, remove the hash/pound sign and enter label between the single quotes on line 119
# For y axis label, remove the hash/pound sign and enter label between the single quotes on line 120
# To change graph title, enter new title between the single quotes on line 121
ggplot(ANALYTE1Day, aes(VALUE)) +
  geom_density(aes(color = '0-1 Days')) +
  geom_density(data = ANALYTE2Day, aes(color = '1-2 Days')) +
  geom_density(data = ANALYTE3Day, aes(color = '2-3 Days')) +
  geom_density(data = ANALYTE4Day, aes(color = '3-4 Days')) +
  geom_density(data = ANALYTE5Day, aes(color = '4-5 Days')) +
  labs(color = 'Key') +
  xlim(0, 1500) +
#  xlab(label = '') +
#  scale_y_continuous(name = '') +
  ggtitle('Analyte Data by Age')  

# Save the density graph as an image file
# Change file name to save your image file between the single quotes on line 125 using similar formatting
ggsave(file.path(analyteOUTPUT, '....png'))



# Split analyte data by weight
ANALYTE_Very_Low <- ANALYTE[ANALYTE$BIRTH_WEIGHT <= 1000, ]
ANALYTE_Very_Low <- na.omit(ANALYTE_Very_Low)
mean(ANALYTE_Very_Low$VALUE)
sd(ANALYTE_Very_Low$VALUE)
nrow(ANALYTE_Very_Low)

ANALYTE_Low <- ANALYTE[ANALYTE$BIRTH_WEIGHT <= 1500, ]
ANALYTE_Low <- na.omit(ANALYTE_Low)
mean(ANALYTE_Low$VALUE)
sd(ANALYTE_Low$VALUE)
nrow(ANALYTE_Low)

ANALYTE_Med <- ANALYTE[ANALYTE$BIRTH_WEIGHT <= 2500 & ANALYTE$BIRTH_WEIGHT >= 1500, ]
ANALYTE_Med <- na.omit(ANALYTE_Med)
mean(ANALYTE_Med$VALUE)
sd(ANALYTE_Med$VALUE)
nrow(ANALYTE_Med)

ANALYTE_High <- ANALYTE[ANALYTE$BIRTH_WEIGHT >= 2500, ]
ANALYTE_High <- na.omit(ANALYTE_High)
mean(ANALYTE_High$VALUE)
sd(ANALYTE_High$VALUE)
nrow(ANALYTE_High)

# Generate density graphs for analyte data separated by weight
# To restrict the limit on the x axis, change the second number on line 158
# For x axis label, remove the hash/pound sign and enter label between the single quotes on line 159
# For y axis label, remove the hash/pound sign and enter label between the single quotes on line 160
# To change graph title, enter new title between the single quotes on line 161
ggplot(ANALYTE_Very_Low, aes(VALUE)) +
  geom_density(aes(color = 'Extremely Low Birth Weight (<1,000 g)')) +
  geom_density(data = ANALYTE_Low, aes(color = 'Very Low Birth Weight (<1,500 g)')) +
  geom_density(data = ANALYTE_Med, aes(color = 'Low Birth Weight (<2,500 g)')) +
  geom_density(data = ANALYTE_High, aes(color = 'Normal Birth Weight (>=2,500 g)')) +
  labs(color = 'Key') +
  scale_color_discrete(breaks=c('Extremely Low Birth Weight (<1,000 g)',
                                'Very Low Birth Weight (<1,500 g)',
                                'Low Birth Weight (<2,500 g)',
                                'Normal Birth Weight (>=2,500 g)')) +
  xlim(0, 1500) +
#  xlab(label = '') +
#  scale_y_continuous(name = '') +
  ggtitle('Analyte Data by Birth Weight')  

# Save the density graph as an image file
# Change file name to save your image file between the single quotes on line 165 using similar formatting
ggsave(file.path(analyteOUTPUT, '....png'))



# Split data by premie/sick status
ANALYTE_Sick <- ANALYTE[ANALYTE$PREMATURE_SICK == 'TRUE', ]
ANALYTE_Sick <- na.omit(ANALYTE_Sick)
mean(ANALYTE_Sick$VALUE)
sd(ANALYTE_Sick$VALUE)
nrow(ANALYTE_Sick)

ANALYTE_Term <- ANALYTE[ANALYTE$PREMATURE_SICK == 'FALSE', ]
ANALYTE_Term <- na.omit(ANALYTE_Term)
mean(ANALYTE_Term$VALUE)
sd(ANALYTE_Term$VALUE)
nrow(ANALYTE_Term)

# Generate density graphs for analyte data separated by premie/sick status
# To restrict the limit on the x axis, change the second number on line 191
# For x axis label, remove the hash/pound sign and enter label between the single quotes on line 192
# For y axis label, remove the hash/pound sign and enter label between the single quotes on line 193
# To change graph title, enter new title between the single quotes on line 194
ggplot(ANALYTE_Sick, aes(VALUE)) +
  geom_density(aes(color = 'Premature/Sick')) +
  geom_density(data = ANALYTE_Term, aes(color = 'Term')) +
  labs(color = 'Key') +
  xlim(0, 1500) +
#  xlab(label = '') +
#  scale_y_continuous(name = '') +
  ggtitle('Analyte Data by Premature/Sick Status')  

# Save the density graph as an image file
# Change file name to save your image file between the single quotes on line 198 using similar formatting
ggsave(file.path(analyteOUTPUT, '....png'))