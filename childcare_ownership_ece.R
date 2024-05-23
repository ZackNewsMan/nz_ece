######### Import and set the stage #########

# I want to figure out who owns the highest percentage of licensed seats and how many are filled.
# This is a nearly complete build. I had to stop short because BusinessDesk funding ran out. 
  # https://docs.google.com/spreadsheets/d/1QI7_RDIaLxhYHoi-ka_pSIHMNyRf_8OPbSkrpwnUjIs/edit?usp=sharing 

install.packages("tidyverse")
library(tidyverse)

install.packages("janitor")
library(janitor)

library(readr)
childcare_ece_data_built <- read_csv("childcare_ece_data_built.csv")
View(childcare_ece_data_built)

# During fact-check, needed to build out one more row, Grace Christian Community Preschool, to see if they belonged with Learning Links. They did not. 

library(readr)
childcare_directory_manage_build_biggies_check_2 <- read_csv("childcare_directory - manage build - biggies check_2.csv")
View(childcare_directory_manage_build_biggies_check_2)

built <- childcare_ece_data_built

# Round 2: 

built <- childcare_directory_manage_build_biggies_check_2

# built is data with the ownership data and other tracing data. 

library(readr)
childcare_directory_in_prog_full_spreadsheet <- read_csv("childcare_directory - in prog - full spreadsheet.csv")
View(childcare_directory_in_prog_full_spreadsheet)

directory <- childcare_directory_in_prog_full_spreadsheet

# directory is data with all of the other assorted data. 

######### Cleaning ######


built %>% 
  clean_names() %>% 
  View()

# Worked great 

  built <- built %>% 
    clean_names()
  
  directory <- directory %>% 
    clean_names() 
  
# Going to export so I can do this simultaneously in SQL

  built %>% write_csv("built.csv", na = "")

  directory %>% write_csv("directory.csv", na = "")

###### Joining and additional cleaning time ######
  
  inner_join(directory, built, by = "service_number") %>% 
    View()
  
  # Appears to have worked. 
    # Mirrors SQL join:
      # select *
      #  from directory, built
      # WHERE directory.service_number = built.service_number
  
      joined <- inner_join(directory, built, by = "service_number")
  
  # Need to remove some of these columns to make the data easier to work with. 
    # For reference: https://dplyr.tidyverse.org/reference/select.html
      
      joined %>% 
        select(!c(authority, kme_peak_body, takiwa, territorial_authority, urban_rural, regional_council, general_electorate, maori_electorate, neighbourhood_sa2, neighbourhood_sa2_code, ward, community_of_learning_id, community_of_learning_name, count_service_number)) %>% 
        View()

      # Worked great.   
  
         joined <- joined %>% 
            select(!c(authority, kme_peak_body, takiwa, territorial_authority, urban_rural, regional_council, general_electorate, maori_electorate, neighbourhood_sa2, neighbourhood_sa2_code, ward, community_of_learning_id, community_of_learning_name, count_service_number)) 
            
      # Could not figure out a way to replicate a bulk deletion of columns in SQL outside of going into the "Modify Table" selection and manually deleting the columns in question.
         # Note for future Zack: I was able to remove one object in the RStudio environment by switching it from "List" to "Grid" mode. 
          # Then I checked the one box of the object I wanted to delete, and then hit the broom button. 
         
      
# I want to remove all of the rows with a "Charity" funder because we are just trying to identify ownership of for-profit spots. 

  # I want all rows that don't have a Charity funder:
         
    joined %>% 
      filter(!(funder_name == "Charity")) %>% 
      View()
    
      # 1,040 rows.
    
    # Echo'd in SQL:
    
        # SELECT *
        #  from joined
        # where funder_name != "Charity"
        
  # Just out of curiosity, how many were Charity:
    
    joined %>% 
      filter(funder_name == "Charity") %>% 
      View()
  
     # 1,548 rows that were just for charity... wooof I didn't need to do that much data entry for those. 
      # Maybe there was another indicator in the data somewhere along the way that it was a non-profit/charity row
      # Some lessons are learned painfully... this is one of them. Onward!
    
    # That pulled out way more rows than I needed to -- I did not put a funder for every line which caused R to eliminate every line that did not have the word "Charity," which included some NA's. 
      # Going to pull out just the Charity lines and do an anti-join to get back just the lines that don't have Charity as the funder. 
    
        charity <- joined %>% 
          filter(funder_name == "Charity")
    
    
    # Anti-join:
    
        anti_join(joined, charity, by = "service_number") %>% 
          View()
        
        # Worked great, New Shoots is back in there
    
  # Removing the charity rows
    
    for_profit <- anti_join(joined, charity, by = "service_number")
    
    # Export:
    
      for_profit %>% write_csv("for_profit.csv", na = "")
    
#### Analysis #######
    
# Need to eliminate blanks for ECE's that I did not have the time to track ownership to:
      
      for_profit %>% 
        filter(!(is.na(owner_name))) %>%
        View()
      
      for_profit_non_blank <- for_profit %>% 
        filter(!(is.na(owner_name)))
      
      # Export:
      
      for_profit_non_blank %>% write_csv("for_profit_non_blank.csv", na = "")
      
      
# Who owns the most number of licensed spots? 
    
      for_profit_non_blank %>% 
      group_by(owner_name) %>% 
      summarize(count = n(),
                license_total = sum(max_licenced_positions),
                enrollment_total = sum(enrollment_total)) %>% 
      arrange(desc(license_total)) %>% 
      View()
    
  # Worked great and really interesting. 
    # BestStart has the most number of licensed spots for kids - 18,110.
    
    # owner_name                             count       license_total    enrollment_total
    # 1 BestStart Educare Limited             260         18110            17807
    # 2 Evolve Education Group Limited         99          6717             5377
    # 3 Busy Bees                              84          6466             6139
    
    # Mirrored in SQL, which also verifies that the number of unique service numbers (aka licenses for an ECE) is the same as the owner count name:
    
      # select owner_name, sum(max_licenced_positions) as "license_total", sum(enrollment_total) as  "enrollment_total", count(owner_name), count( service_number) 
      # from for_profit_non_blank
      # group by owner_name
      # ORDER by license_total DESC
    
  # Want to add another column to identify how many report more enrolled students than they are licensed for.       
    
    for_profit_non_blank %>% 
      group_by(owner_name) %>% 
      summarize(count = n(),
                license_total = sum(max_licenced_positions),
                enrollment_total = sum(enrollment_total)) %>% 
       mutate(enrollment_difference = license_total - enrollment_total) %>% 
      arrange(desc(license_total)) %>% 
      View()
    
    # Worked swimmingly 
      # Here it is in SQL, although difference between enrolled and licensed is a bit funky:
         # select owner_name, sum(max_licenced_positions) as "license_total", sum(enrollment_total) as  "enrollment_total", count(owner_name), count( service_number),  max_licenced_positions - enrollment_total as "enrollment_difference"
         # from for_profit_non_blank
         # group by owner_name
         # ORDER by license_total DESC
    
    ece_totals <- for_profit_non_blank %>% 
      group_by(owner_name) %>% 
      summarize(count = n(),
                license_total = sum(max_licenced_positions),
                enrollment_total = sum(enrollment_total)) %>% 
      mutate(enrollment_difference = license_total - enrollment_total) %>% 
      arrange(desc(license_total)) 
    
    # Issues in SQL getting an accurate subtraction but it checked out when I compared it in Excel
  
      # Export it to a CSV:
          
          ece_totals %>% write_csv("ece_totals.csv", na = "")
        

# Do all of the schools take 20 hours free ECE?         
         
   for_profit_non_blank %>% 
   filter(x20_hours_ece == "Yes") %>% 
   View()
 
 # 1,088 rows, all but 1
  # SQL confirmed, 1,088 rows:
 
     # SELECT *
     #   from for_profit_non_blank
     # where x20_hours_ece = "Yes"
     
 # Who doesn't?

   for_profit_non_blank %>% 
   filter(x20_hours_ece == "No") %>% 
   View()

  # Only one place did not, a Learning Links
 
 

