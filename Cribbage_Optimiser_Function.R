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


require(data.table)

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
    card_positions <- sort(sample(1:52, 6))
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




# takes input of a list containing the pack and player hand
# returns a table of all combination of hands and cut cards with points
optimise_hand <- function(cards){
  # obtain the pack from the cards
  pack <- cards[[1]]
  # obtain full_hand from cards
  full_hand <- cards[[2]]
  
  count15s<- function(hand){
    
    # define recursive function to keep checking against the rest of the hand
    # vect = vector of the values in the hand with face cards = 10
    # i = current iteration in the loop
    # cntr = current No of cards from from i that is being used
    # total = the current total of the values of cards currently in use
    # points = current amount of points scored by the hand
    # really not happy with this function but it seems to be working for now:$
    moreCards<- function(vect, i, cntr, total, points, startCntr)
    {
      # check to see if trying to look at a 6th card
      if((cntr+i) > 5)
      {
        # check to see if start counter at end
        # yes means leave function as all cards for this i looked at
        if((startCntr+i) > 5)
        {
          return(points)
        }
        # no means add one to startCntr and start carry on with function 
        else
        {
          startCntr<- startCntr + 1
          # make total = i.value
          total<- vect[i]
          # make cntr = startCntr
          cntr = startCntr
          # call morecards 
          moreCards(vect, i, cntr, total, points, startCntr)
        }
      }
      # ok to carry on with function
      else
      {
        # add the next card to the total
        total<- total + vect[i+cntr]
        
        # check to see if total greater than 15 
        if(total > 15)
        {
          # check to see if there is a gap between the startCntr and cntr > 1
          # this will dictate whether previous cards need to be removed and later
          # ones added to ensure all combinations checked
          # only interested in finding combination using i & startCntr in them as other will be taken care
          # of later on in the recursion
          if(cntr - startCntr > 1)
          {
            
            # if diff = 3
            if(cntr - startCntr == 3)
            {
              # just check the 3 combinations available for = 15
              if(vect[i]+vect[i+startCntr]+vect[i+cntr] == 15)
              {
                points<- points + 2
              }
              
              if(vect[i]+vect[i+startCntr]+ vect[i+cntr-2]+vect[i+cntr] == 15)
              {
                points<- points + 2
              }
              
              if(vect[i]+vect[i+startCntr]+ vect[i+cntr-1]+vect[i+cntr] == 15)
              {
                points<- points + 2
              }  
            }
            # else diff = 2
            else
            {
              # remove the value of the previous card from the total
              total<- total - vect[i+cntr-1]
              # check to see if the total is 15 and add points if true
              if(total == 15)
              {
                points<- points + 2
              }
              # check to see if there is a card further in the hand that needs adding in
              # i.e a hand of vals 4,4,4,4,7 would be > 15 by card 4
              # thisd can only happen when i=1 startCntr=1 & cntr = 3
              if(i+cntr < 5)
              {
                # just check the 3 combinations available for = 15
                if(vect[i]+vect[i+startCntr]+vect[i+cntr+1] == 15)
                {
                  points<- points + 2
                }
                
                if(vect[i]+vect[i+startCntr]+ vect[i+cntr-1]+vect[i+cntr+1] == 15)
                {
                  points<- points + 2
                }
                
                if(vect[i]+vect[i+startCntr]+ vect[i+cntr]+vect[i+cntr+1] == 15)
                {
                  points<- points + 2
                }
              }
            }
          }
          
          # move startCntr onto the next card to check further combinations
          startCntr<- startCntr + 1
          # make total = i.value
          total<- vect[i]
          # make cntr = startCntr
          cntr = startCntr
          # call morecards 
          moreCards(vect, i, cntr, total, points, startCntr)
          
          
        }
        #else if check to see if total = 15 if true then points need adding and
        # the last card needs removing from the total and the next card adding
        # in case it is of the same value therfore needing more ponts
        else if(total == 15)
        {
          
          # check for diff in startCntr and cntr to see if any other combos are needed to be checked
          # if startCntr = cntr then further combos will be taken care of later on in the recursion
          if(startCntr == cntr)
          {
            points<- points + 2
            # move startCntr onto the next card to check further combinations
            startCntr<- startCntr + 1
            # make total = i.value
            total<- vect[i]
            # make cntr = startCntr
            cntr = startCntr
            # call morecards 
            moreCards(vect, i, cntr, total, points, startCntr)
          }
          else
          {
            points<- points + 2
            
            # while next card = current card & not greater than 5 cards
            # cntr++ 
            while(i+cntr+1 < 6 & vect[i+cntr] == vect[i+cntr+1])
            {
              points<- points + 2
              cntr<- cntr+1
            }
            
            # check to see if there is another card behind which didn't match which needs looking at
            # i.e 2,3,5,5,10 - the 10 would make 15 with the 2&3 but StartCntr gets moved on before this
            if(i + cntr + 1 < 6)
            {
              # check if this combo = 15
              if(vect[i] + vect[i+startCntr] + vect[i+cntr+1] == 15)
              {
                points<- points + 2
              }
            }
            
            startCntr<- startCntr + 1
            # make total = i.value
            total<- vect[i]
            # make cntr = startCntr
            cntr<- startCntr
            # call morecards 
            moreCards(vect, i, cntr, total, points, startCntr)
            
          }
        }
        #else the total is less than 15 and more cards need adding
        else
        {
          cntr<- cntr + 1
          moreCards(vect, i, cntr, total, points, startCntr)
        }
      }
    }
    
    # initialise points
    points <- 0
    
    #obtain values from hand
    vect<- hand[,value]
    # change the values of any 'face' cards from 11, 12 or 13 to 10
    vect[vect>10]<-10
    
    for(i in 1:4)
    {
      startCntr <- 1
      cntr<- 1
      total <- vect[i]
      
      #    # if combo of 2 cards greater than 15 exit the loop
      #    if(total + vect[i+a] > 15)
      #    {
      #      return(points)
      #    }
      
      points<- moreCards(vect, i, cntr, total, points, startCntr)
    }
    
    return(points)
  }
  
  pair <- function(hand){
    points<- 0
    
    kind2 <- hand[duplicated(hand[,value]),value]
    points = points + length(kind2) * 2
    kind3 <- kind2[duplicated(kind2)]
    points = points + length(kind3) * 2
    kind4 <- kind3[duplicated(kind3)]
    points = points + length(kind4) * 2
    
    return(points)
  }
  
  flushScorer <- function(hand){
    # reduce the hand to unique suits, if there is only 1 suit then add 4 to points
    if (length(unique(hand[hand$cut == FALSE,suit])) == 1){
      points <- 4
      # if the suit of the cut card matches the suit of the cards in the hand
      # make points = 5
      if(unique(hand[hand$cut == TRUE,suit]) == unique(hand[hand$cut == FALSE,suit])){
        points <- 5
      }
      # if there is more than one unique suit in the pack then points = 0
    } else { 
      points <- 0}
    
    return(points)
  }
  
  countRuns<- function(hand){
    # vect is a vector of the values in the hand
    # cntr is the current position being examined - initially set at 2
    # currentVal sent in initially as the first value in the hand
    # runLength states how many cards the run has been going on for
    # multipleLength states how many preceding cards have had the same value - initially set as 0
    # endMultiple is a bool that states whether multiple cards have alreay been present in hand on the run
    cycleRuns<- function(vect, cntr, currentVal, runLength, multipleLength, onMultiple)
    {
      
      # check that cntr is not greater than 5 so its not looking for a 6th card
      if(cntr < 6)
      {
        # is the value of cntr within 1 of currentVal continue function if not exit as no other
        if(vect[cntr] < currentVal+2)
        {
          # is the value the same as current val
          # if yes then set onMultiple to TRUE and multipleLength to 1
          if(vect[cntr] == currentVal)
          {
            # check to see if this is 3rd or 4th card in a row of same val
            if(onMultiple)
            {
              multipleLength<- multipleLength + 1
              currentVal<- vect[cntr]
              cntr<- cntr + 1
              cycleRuns(vect, cntr, currentVal, runLength, multipleLength, onMultiple)
            }
            # otherwise it is a new multiple and the 2 cards need adding to multiple length
            else
            {
              multipleLength<- multipleLength + 2
              onMultiple<- TRUE
              currentVal<- vect[cntr]
              cntr<- cntr + 1
              cycleRuns(vect, cntr, currentVal, runLength, multipleLength, onMultiple)
            }
          }
          # the card is on a potential run
          else
          {
            runLength<- runLength +1
            onMultiple<- FALSE
            currentVal<- vect[cntr]
            cntr<- cntr + 1
            cycleRuns(vect, cntr, currentVal, runLength, multipleLength, onMultiple)
          }
        }
        # else not part of a run
        # check that a run of 3 or greater has not ended as their will be no more runs after this
        #   and the points will need to be calculated
        else if(runLength > 2)
        {
          #if multipleLenght = 0 change it to 1 so it can be multiplied with runLength
          if(multipleLength == 0)
          {
            multipleLength<- 1
          }
          
          return(runLength * multipleLength)
        }
        # reset counters and carry on
        else
        {
          onMultiple<- FALSE
          runLength<- 1
          multipleLength<- 0
          currentVal<- vect[cntr]
          cntr<- cntr + 1
          cycleRuns(vect, cntr, currentVal, runLength, multipleLength, onMultiple)
        }
      }
      # check to see if the hand ended on a run
      else if(runLength > 2)
      {
        #if multipleLenght = 0 change it to 1 so it can be multiplied with runLength
        if(multipleLength == 0)
        {
          multipleLength<- 1
        }
        
        return(runLength * multipleLength)
      }
      else
      {
        return(0)
      }
    }
    
    # get values of the hand
    vect<- hand[,value]
    
    # initiate cycleRuns()
    points<- cycleRuns(vect, 2, vect[1],1, 0,FALSE)
    
    return(points)
  }
  
  nobs<- function(hand){
    points <- 0
    # returns the suit of any cards from hand (not cut) that are jacks
    jacks <- hand[hand$value==11 & hand$cut==FALSE,suit]
    
    # if length of jacks is greater than zero get the suit of the cut card
    # join jacks and cut card together and sum the check for duplicates
    
    if (length(jacks) > 0){
      # returns the suit of the cut card
      cut <- hand[hand$cut==TRUE,suit]
      suits <- c(jacks,cut)
      points <- sum(duplicated(suits))
    }
    
    return(points)
    
  }
  
  calculateScore<- function(hand){
    points<- count15s(hand)
    points<- points + countRuns(hand)
    points<- points + flushScorer(hand)
    points<- points + pair(hand)
    points<- points + nobs(hand)
    return(points)

  }
  
  calculateOneHand<- function(chosenHand, discardedCards=""){
    #initialise points
    points<- 0
    #create the data table with 46 rows filling in the first column with the hand
    handPoints<- data.table(playerHand= paste(paste(c(chosenHand[1,value],chosenHand[1,suit]),collapse="")
                                              ,paste(c(chosenHand[2,value],chosenHand[2,suit]),collapse="")
                                              ,paste(c(chosenHand[3,value],chosenHand[3,suit]),collapse="")
                                              ,paste(c(chosenHand[4,value],chosenHand[4,suit]),collapse=""),sep=","), # a better way of doing this?
                            discarded= discardedCards,
                            cutcard= "",
                            score= rep(0,46)) # allocate 0 to the table 46 times to ensure that there are enough rows to insert into
    
    
    # loop for 46 times getting the remaining cards in the pack
    for(i in 1:46)
    {
      # append cut card to hand
      hand<- rbind(chosenHand, pack[i,])
          
      # set cut card 'cut' to TRUE
      hand[5,cut:= TRUE]
          
      # order the hand by value and suit
      hand <- hand[order(hand[,value], hand[,suit]),]
          
      #pass hand to calculateScore
      points<- calculateScore(hand)
      
      # use i to place at row level and add the cut card and points to the data.table
      handPoints[i,score:=points]
      handPoints[i,cutcard:=paste(c(pack[i,value],pack[i,suit]),collapse="")]
    }
    
    return(handPoints)
  }
  
  calculateAllHands<- function(full_hand){
    allCombos<- data.table(playerHand=character(0), discarded=character(0), cutcard=character(0), score=integer(0))
    
    # loop through different combinations of discards
    for(i in 1:5)
    {
      for(j in i+1:(6-i))
      {
        
        # use index i and j to discard cards and create hand
        chosenHand<- full_hand[c(-i,-j),]
        
        # create string discards from the discarded cards
        discards<- paste(c(
          paste(c(full_hand[i,value],full_hand[i,suit]),collapse=""),
          paste(c(full_hand[j,value],full_hand[j,suit]),collapse="")  
        ),collapse=",")
        
        # use rbind to populate data.table using calculateOneHand
        allCombos<- rbind(allCombos,calculateOneHand(chosenHand, discards))
        
      }
    }
    
    return(allCombos)
  }
  
  return(calculateAllHands(full_hand))
  
}




# takes input of a table containing all combinations of hands, cut cards and points
# returns a summarised table combined on the playerHand
summarise_hand <- function(full_table){

  # summarise the data based on the playerHand
  sumHand <- full_table[ , .(sumPoints = sum(score),
                             minVal = min(score),
                             maxVal = max(score),
                             meanVal = round(mean(score)),
                             stdev = round(sd(score)),
                             lowerQuantile = round(quantile(score, .25)),
                             median = round(quantile(score, .50)),
                             upperQuantile = round(quantile(score, .75))) ,
                         by = playerHand]
  
  return(sumHand[order(-rank(sumPoints))])
}





