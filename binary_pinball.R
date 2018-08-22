## Project Pinball
## Goofing around with Decimal and Binary with kids
 
library(R.utils) # for the "intToBin" function
 
# simple test of loop
# for (i in 1:100){print(i)}

# MAIN LOOP
i = 1
w = 0.25 # fraction of second to wait on loop 
while (i<128)
{
  #intToBin(i) - this is just the Decimal to Binary - we add 2^16 above to ensure we have leading zeros for formatting
  temp = (intToBin(i+65536)) # 2^16 is  "10000000000000000 " - we ignore the left most bit, just pick right hand bits
  temp = gsub("([0-Z])", "\\1   \\2", temp)
  temp = (paste("         ",temp,i)) # 2^16 is  "10000000000000000 " - we ignore the left most bit, just pick right hand bits
  
  print("BIT:                                    256 128  64  32  16   8   4   2   1")
 
  print(temp)
  gsub("([0-Z])", "\\1 \\2", temp)

  i = i+1
  Sys.sleep(w)
}
 
