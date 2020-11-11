# Hope Baldwin
# Exercise 11

library(ggplot2)

# initially 99 normal cells and 1 mutant
N0 = 99
M0 = 1

# r for the normal cells when treated is -.1, but is .05 for the mutant ones
rN = -.1
rM = .05

# carrying capacity is 1,000,000
K=1000000

# looking over 50 timesteps
timesteps=100

# make a data frame to store information
tumor=data.frame(time=1:timesteps, tumorNorm=rep(0,timesteps), tumorMut=rep(0,timesteps))

tumor$tumorNorm[1]=N0;
tumor$tumorMut[1]=M0;

# simulate the growth with the given models
for(t in 2:timesteps){
  tumor$tumorNorm[t] <- tumor$tumorNorm[t-1]+rN*tumor$tumorNorm[t-1]*(1-(tumor$tumorNorm[t-1]+tumor$tumorMut[t-1])/K)  
  tumor$tumorMut[t] <- tumor$tumorMut[t-1]+rM*tumor$tumorMut[t-1]*(1-(tumor$tumorMut[t-1]+tumor$tumorNorm[t-1])/K) 
}

# put data into long form 
tumor2=data.frame(time=c(tumor$time,tumor$time), 
                    TumorSize=c(tumor$tumorNorm, tumor$tumorMut),
                    CellType=rep(c("Normal Tumor Cells", "Mutant Tumor Cells"),
                    each=timesteps))
                    
# plot
ggplot(data=tumor2,aes(x=time, y=TumorSize, color = CellType))+
  geom_line() + theme_classic()
                    
                    

                                        
