# Read the Datasets 
# Read me, kindly select only one at a time because the data is save in to one variable
dataset <- read.csv('listings.csv')

#Remove the variables that are not relevant to this research
#Get only the amenities at column 59 from the full dataset.
# The other column is just to assist viewing (visualizing) the dataframe
sub_dataset <- dataset[,c(59,60)]


#Get all the list of avaliable amenities offered by hosts from airbnb website on amenities https://www.airbnb.co.uk/rooms/15896822 this is ust a random room
search = c('Pets Allowed', 'Buzzer wireless intercom','Elevator in Building','Breakfast','Free parking on premise','Cable TV',
           'Internet','Dryer','Family/Kid Friendly','Laptop friendly workspace','Pool','Hair dryer','Doorman','Iron','Suitable for events'
           ,'Essentials','Gym','Hangers','Wheelchair accessible','Heating','Smoking_Allowed','Washer','Air Conditioning',
           'TV','Kitchen', 'Private living room','Hot Tub','Private entrance','Indoor Fireplace','Ethernet connection',
           'Wireless Internet','Baby bath','Game console','High chair','Babysitter recommendations','Outlet covers',
           'Bathtub','Pack \'n Play/travel crib', 'Changing table','Room-darkening shades','Children\'s books and toys',
           'Stair gates','Children\'s dinnerware','Table corner guards','Crib','Window guards','Fireplace guards');

#Create a new dataset to store the amenities of each listing
new_set <- data.frame(matrix(ncol = 47, nrow = 0))

#This Package 'regexPipes' is a collection of functions used for regular expression to find and also remove patterns
#install.packages('regexPipes');
#library(regexPipes)

#using the gsub function found in regexPipes, here it is used to Remove the opening and closing curly brackets {} and the all the double quotes " 
sub_dataset$amenities = gsub(pattern = '[{}]|"', replacement = "", x = sub_dataset$amenities)

#Go through all the rows and filter out all the amenities offerred y each host
#This will be stored in the new dataset (new_set)
for(i in 1:nrow(sub_dataset)){
  words = strsplit(sub_dataset$amenities[i],',')[[1]]
  row = c();
  
  for(k in 1:ncol(new_set)){
    
    if(length(grep(pattern = search[k], x=words, fixed = TRUE))>0){
      #add to the new dataset
      new_set[i,k] = search[k]
    }
    
  }
  
  
}

#Reconstructing the dataset to be used in SAS

new_sas_set <- data.frame('Pets_Allowed'=integer(), 'Buzzer_wireless_intercom'=integer(),'Elevator_in_Building'=integer(),
                          'Breakfast'=integer(),'Free_parking_on_premise'=integer(),'Cable_TV'=integer(),'Internet'=integer(),
                          'Dryer'=integer(),'Family_Kid_Friendly'=integer(),'Laptop_friendly_workspace'=integer(),'Pool'=integer(),
                          'Hair_dryer'=integer(),'Doorman'=integer(),'Iron'=integer(),'Suitable_for_events'=integer(),
                          'Essentials'=integer(), 'Gym'=integer(),'Hangers'=integer(),'Wheelchair_accessible'=integer(),
                          'Heating'=integer(),'Smoking_Allowed'=integer(),'Washer'=integer(),'Air_Conditioning'=integer(),
                          'TV'=integer(),'Kitchen'=integer(),'Private_living_room'=integer(),'Hot_Tub'=integer(),
                          'Private_entrance'=integer(),'Indoor_Fireplace'=integer(),
                          'Ethernet_connection'=integer(),'Wireless_Internet'=integer(),'Baby_bath'=integer(),
                          'Game_console'=integer(),'High_chair'=integer(),'Babysitter_recommendations'=integer(),
                          'Outlet_covers'=integer(),'Bathtub'=integer(),'Pack_n_Play_travel_crib'=integer(),'Changing_table'=integer(),
                          'Room_darkening_shades'=integer(),'Childrens_books_and_toys'=integer(),'Stair_gates'=integer(),
                          'Childrens_dinnerware'=integer(),'Table_corner_guards'=integer(),'crib'=integer(),'Window_guards'=integer(),
                          'Fireplace_guards'=integer());

sas_set <- data.frame('Host'=integer(), 'Amenity'=character());

for(i in 1:nrow(sub_dataset)){
  
  words = strsplit(sub_dataset$amenities[i],',')[[1]]
  
  for(k in 1:ncol(new_sas_set)){
    if(length(grep(pattern = search[k], x=words, fixed = TRUE))>0){
      
      new_row = data.frame(Host=i,Amenity=search[k]);
      
      sas_set = rbind(sas_set,new_row);
      
    }
    
  }
  
  
}


#The new_set will be stored as a sparse matrix which will be used with apriori function in creating the model


#Save the preprocessed data to be used for further analysis------------------------------------------------------------------------------------- 

#Run this one one to store datasetfor R--------
write.csv(new_set, "listings_R.csv")
#----------------------------------

#Run this one to store dataset for sas--------
write.csv(sas_set, "listings_sas.csv")
#------------------------------------------------------------------------------------
