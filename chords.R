
#notes  <- c("C","C#","D","D#","E","F","F#","G","G#","A","A#","B")
#notesb <- c("C","Db","D","Eb","E","F","Gb","G","Ab","A","Bb","B")
#notes2 <- c("B#C","C#Db","D","D#Eb","EFb","E#F","F#Gb","G","G#Ab","A","A#Bb","BCb")
notes2 <- c("B#·C·","C#·Db·","D·","D#·Eb·","E·Fb·","E#·F·","F#·Gb·","G·","G#·Ab·","A·","A#·Bb·","B·Cb·")

modes    <- c("______Lydian","______Ionian","__Mixolydian","______Dorian","_____Aeolian","____Phrygian","_____Locrian")
modesmel <- c("__Locrian_b4","_Melodic_min","___Dorian_b2","__Lydian_aug","__Lydian_dom","__Mixolyd_b6","__Aeolian_b5")

#scale <- c(0,2,4,5,7,9,11) #major scale
regularscale <- function(t,d,n=7) {
  #t tonic 0<=t<12
  #d shift 0<=d<7 (modes)
  ((0:11)[(n*(0:11)+d)%%12<n]+t)%%12 #regular pattern STTSTTT
}

#scale <- c(0,2,3,5,7,9,11) #melodic minor
melodicdegrees <- function(t,d,n=7) {
  #t tonic 0<=t<12
  #d shift 0<=d<7 (modes)
  scale0 <- c(0,1,3,4,6,8,10)
  if (d !=0) scale0 <- c(scale0[(d+1):7],scale0[1:d])-scale0[d+1]
  (scale0+t)%%12 #STSTTTT pattern
}


harmonicfield <- function(scale){

  field <- list()
  fielddiff <- list()
  fieldnames <- rep("",length(scale))
  
  for (i in 1:7){
 
    field[[i]] <- c(scale[(i-1+0)%%7+1], scale[(i-1+2)%%7+1],
                    scale[(i-1+4)%%7+1], scale[(i-1+6)%%7+1])
  
    fielddiff[[i]] <- c(field[[i]][1],
                       (field[[i]][2]-field[[i]][1])%%12,
                       (field[[i]][3]-field[[i]][1])%%12,
                       (field[[i]][4]-field[[i]][1])%%12)
  
    #we should use the bemols scale when is needed
    #this is when there is not E or B in the scale because then
    #some other letter ir repeated
    #if ((max('E'==notes[scale+1])==0 | 
    #    max('B'==notes[scale+1])==0)==0)
    #fieldnames[i] <- notesb[field[[i]][1]+1]
    #else fieldnames[i] <- notes[field[[i]][1]+1]
    
    #enharmonic names for notes
    fieldnames[i] <- notes2[field[[i]][1]+1]
    
    if (fielddiff[[i]][2]==3)  fieldnames[i] <- paste0(fieldnames[i],"m")
    if (fielddiff[[i]][4]==11) fieldnames[i] <- paste0(fieldnames[i],"maj7") 
    else if (fielddiff[[i]][4]==10) fieldnames[i] <- paste0(fieldnames[i],"7")
    if (fielddiff[[i]][3]==6)  fieldnames[i] <- paste0(fieldnames[i],"b5")
    else if (fielddiff[[i]][3]==8)  fieldnames[i] <- paste0(fieldnames[i],"#5")
  
  }
  
  return(fieldnames)

}

harmonictableprint <- function(t){

  harmonictable <- list()

  for (d in 0:6){
  
    scale <- regularscale(t,d)
    harmonictable[[d+1]] <- harmonicfield(scale)
  
  }

  harmonictable <- as.data.frame(harmonictable)
  names(harmonictable) <- modes
  print(harmonictable)

}

harmonictableprintmel <- function(t){
  
  harmonictable <- list()
  
  for (d in 0:6){
    
    scale <- melodicdegrees(t,d)
    harmonictable[[d+1]] <- harmonicfield(scale)
    
  }
  
  harmonictable <- as.data.frame(harmonictable)
  names(harmonictable) <- modesmel
  print(harmonictable)
  
}

#major scale d=1 minor scale d=4
scale <- regularscale(0,1)

for (t in 0:11) harmonictableprint(t)

#minor scale d=1 minor scale d=1
scale <- melodicdegrees(0,1)

for (t in 0:11) harmonictableprintmel(t)




