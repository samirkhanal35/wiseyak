import sys
class leftposition:
    def __init__(self):
        self.missionary = 0
        self.cannibals = 0
        self.boat = 0

    def displayleft(self):
        print("Left Bank")
        print("missionaries:",self.missionary,"cannibals:",self.cannibals)

class rightposition:
    def __init__(self, a):
        self.missionary = int(a[0])
        self.cannibals =  int(a[1])
        self.boat = int(a[2])

    def displayright(self):
        print("Right Bank")
        print("missionaries:",self.missionary,"cannibals:",self.cannibals)

def displaybankvalues(left, right):
    left.displayleft()
    right.displayright()

def moveBoatRightToLeft(left, right):
    right.boat = 0
    left.boat = 1

def moveBoatLeftToRight(left, right):
    left.boat = 0
    right.boat = 1

def problemsolution(left, right):
    while (right.missionary != 0) or (right.cannibals !=0):       
        if right.boat == 0 :
            # do the solution for left bank            
            if (left.cannibals > left.missionary and left.missionary == 0) or (left.missionary>left.cannibals and left.cannibals>0):
                left.cannibals -= 1
                right.cannibals += 1
                moveBoatLeftToRight(left, right)
                displaybankvalues(left, right) 
            elif left.cannibals == left.missionary:
                left.cannibals -= 1
                right.cannibals +=1
                left.missionary -=1
                right.missionary +=1
                moveBoatLeftToRight(left, right)
                displaybankvalues(left, right)            
            else:
                break             
        else:
            # do the solution for right bank                     
            if (right.missionary>2 and right.cannibals>2 and right.boat>0) or (right.missionary>right.cannibals and right.cannibals>1):
                displaybankvalues(left, right)
                left.cannibals += 2
                right.cannibals -= 2
                moveBoatRightToLeft(left, right)
                displaybankvalues(left, right)            
            elif (right.missionary>2 and right.cannibals!=0 and right.cannibals<2) or (right.missionary == right.cannibals and right.missionary ==2):
                right.missionary -= 2
                left.missionary +=2
                moveBoatRightToLeft(left, right) 
                displaybankvalues(left, right)            
            elif right.cannibals > right.missionary and right.missionary ==0 and right.cannibals>1:
                right.cannibals -= 2
                left.cannibals +=2
                moveBoatRightToLeft(left, right)
                displaybankvalues(left, right)
            else: 
                break

def missionaryCannibal(a):
    "Solve the missionary and Cannibal problem"
    left = leftposition() # left bank initiation
    right = rightposition(a) # right bank initiation with given missionaries and cannibals
    problemsolution(left, right)

if __name__ ==  '__main__':
    # a = list(sys.argv[1:])
    a = [3,3,1]
    missionaryCannibal(a)
