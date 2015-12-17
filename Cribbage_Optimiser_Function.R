#########################################################################################################
#
# Cribbage Hand Optimiser - written by Dean Findlay 15/12/15
#
# version.string R version 3.2.2 (2015-08-14)
# platform       x86_64-w64-mingw32 
#
# 
#
#


# initialise the pack and remove the player hand from it
# input is a vector of cards e.g c("1h","2c","3d","4s","5c","6d")
#  or NULL for when a random hand is to be assigned
create_cards <- function(start_cards = NULL){
  
  # initialise the pack with all 52 cards
  pack <- data.table( value = rep(1:13, each = 4),
                      suit = rep(c("c","d","h","s"), 13),
                      cut = FALSE)
  
  # function to split start_card vector characters and find index No in pack
  find_index <- function(start_card){
    c_val <- substr(start_card,1,1)
    c_suit <- substr(start_card,2,2)
    # return index No of card in the pack
    return(pack[ , .I[value == c_val & suit == c_suit]])
  }
  
  # if no input cards given then create a random hand
  if(is.null(start_cards)){
    # create a random sample of 6 cards to represent the hand
    card_positions <- sample(1:52, 6)
  }
  else if(length(start_cards)== 6){
    # find the positions of the given cards in the pack
    card_positions <- sapply(start_cards, "find_index")
  }
  # incorrect number of cards
  else{
    stop("6 cards or NULL call are required")
  }
  
  # create the players hand from cars in the pack
  hand <- pack[card_positions,]
  # remove the chosen cards from the pack
  pack <- pack[-card_positions,]
  
  # return the pack and the hand
  return(list(pack, hand))
}













