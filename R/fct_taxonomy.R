# =================================
# Small function to return the taxo level and taxon name 
# =================================

taxo_selected <- function(supergroup, division, class, order, family, genus, species= "All"){

# Debug
# browser()  
  
  # If the lowest level is "All" then it is the one selected then goes up one level
  if ( !(species %in% c("All", "")) ) {
    taxo_level = "species"
    taxo_name = species
  } else {
    if(!(genus %in% c("All", "")) ) {
      taxo_level = "genus"
      taxo_name = genus
    } else {
      if(!(family %in% c("All", "")) ) {
        taxo_level = "family"
        taxo_name = family
      } else {
        if(!(order %in% c("All", "")) ) {
          taxo_level = "order"
          taxo_name = order
        } else {
          if(!(class %in% c("All", "")) ) {
            taxo_level = "class"
            taxo_name = class
          } else {
            if(!(division %in% c("All", "")) ) {
              taxo_level = "division"
              taxo_name = division
            } else {
              if(!(supergroup %in% c("All", "")) ) {
              taxo_level = "supergroup"
              taxo_name = supergroup
              } else {
                taxo_level = "kingdom"
                taxo_name = "Eukaryota" 
                }
            }
          }
        }
      }  
    }  
  } 

# Debug
# browser()
  
  return( list(level = taxo_level, name = taxo_name))      
}
