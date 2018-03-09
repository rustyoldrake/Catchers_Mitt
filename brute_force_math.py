# brute_force_math.py
#
# L&J - this is a sample program of how to leverage computing power
# to "brute force" a problem.  In this case - we are going to use a
# Loop to try EVERY possible combination answer to your question
# while at time inelegant - it can be effective for solving problems
# black hat hackers also use this to try to crack passwords
# 3,872,649,A0B

# MATH PROBLEM TO BRUTE FORCE:
# 10 digit number 3,872,649,A0B is divisible by 36
# The letters A and B each represent single digit EVEN numbers  (0,2,4,6,8)
# find sum of A and B

# STEP 1
print ("STEP 1 - let's do a simple loop showing counters")
# http://www.pythonforbeginners.com/loops/for-while-and-nested-loops-in-python
# Prints out the numbers

for A in range(0,5):
    for B in range(0,5):
        print("A=",A*2,"B=",B*2)
print ("notice how we double 0,1,2,3,4 to get 0,2,4,6,8")
print ("and try all combinaitons - with no deduplication however")

##   STEP 2
print("\n \n \n ")
print ("STEP 2 - now let's merve these with the BIG number - 3,872,649,A0B ")
# http://www.pythonforbeginners.com/concatenation/string-concatenation-and-formatting-in-python
N = 3872649000.0 # we can solve with Number Variable, or String Concatenation then convert string to number - im gonna try former
print ("the .0 decimal forces a float, rather than integer - this helps later to see if we have remainder (non .000 after result)")
print ("note A is our 'hundreds' column - so to ADD A to number we ADD 'A-hundred' and B-ones")
for A in range(0,5):
    for B in range(0,5):
        N1 = N + ((A*2)*100+(B*2))
        print("When A is:",(A*2),"and B is",(B*2), "then", N1,"/36 is", N1/36 )
        #print("A=",A*2,"B=",B*2)
