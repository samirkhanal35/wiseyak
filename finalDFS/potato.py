import sys

class PotatoProblem:
    def __init__(self, neededPotato, AvailablePotato):
        self.InitialState = [0, 0, 0]
        self.NeededPotato = neededPotato
        self.AvailablePotato = AvailablePotato
        self.Possiblemoves = [5.0, 0.5, -5.0, -0.5]

    def getMinimumPossibleMove(self):
        Moves = [ abs(i) for i in self.Possiblemoves]
        return min(Moves) 
    
    def  CheckLegality(self, NextState, problemSolution):       
        lastState = problemSolution.getLastState()
        minimumPossibleMove = self.getMinimumPossibleMove()
        if NextState[0] <= self.AvailablePotato  and NextState[0]>0 and (abs(NextState[0]-self.NeededPotato)<=(abs(lastState[0]-self.NeededPotato)+minimumPossibleMove)) :
            return True
        else:
            return False

    def nextMove(self, CurrentState, index, problemSolution):               
        newState = []
        NumMoves = len(self.Possiblemoves)    
        if index+1 == NumMoves:
            return False
        else:
            for i in range(index+1, NumMoves+1):            
                Move = self.Possiblemoves[i-1]
                newState = [CurrentState[0]+Move,  CurrentState[-1], i]
                newStateValues = [newState[0]]            
                if (self.CheckLegality(newState, problemSolution)):                                       
                    return newState            
            return False

    def CheckFinalState(self, CurrentState):
        if CurrentState[0]== self.NeededPotato:
            return True
        else:
            return False

def potato():
    try:
        AvailablePotato = int(input("Enter the amount of available potato in whole number : "))
        NeededPotato = float(input("Enter the amount of Potatoes needed (upto one decimal place with the multiple of 0.5): "))    
        if (((NeededPotato*100)%10)>0):
            print("Sorry !! Please enter the amount upto decimal place with the multiple of 0.5 !!")
            sys.exit()                  
        if NeededPotato>AvailablePotato:
            print("Sorry !! Not enough potato available !!")
            sys.exit() 
        Potato = PotatoProblem(NeededPotato, AvailablePotato)
        return Potato
    except:
        print("Sorry !! Please enter the amount of available potato in whole number like 50 !!")
        print("Sorry !! Please enter the amount of needed Potatoes upto decimal place with the multiple of 0.5 !!")
        sys.exit()

