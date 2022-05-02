import sys

class PotatoProblem:
    def __init__(self):
        self.InitialState = [0]
        self.NeededPotato = 0
        self.AvailablePotato = 0
        self.Possiblemoves = [5.0, 0.5, -5.0, -0.5]
        self.maxMoves = 4
        self.InitialIndex = -1
    
    def  CheckLegality(self, State):        
        return (State[0] <= self.AvailablePotato  and State[0]>0) 

    def nextMove(self, CurrentState, PrevIndex):       
        for i in range(PrevIndex+1, self.maxMoves):            
            Move = self.Possiblemoves[i]
            newState = [CurrentState[0]+Move]
            newStateIndex = [newState, i]              
            if (self.CheckLegality(newState)):                                   
                return newStateIndex            
        return False

    def CheckFinalState(self, CurrentState):
        return (CurrentState[0]== self.NeededPotato)
  
    def dialog(self):
        try:
            self.AvailablePotato = int(input("Enter the amount of available potato in whole number : "))
            self.NeededPotato = float(input("Enter the amount of Potatoes needed (upto one decimal place with the multiple of 0.5): "))    
            if (((self.NeededPotato*100)%10)>0):
                print("Sorry !! Please enter the amount upto decimal place with the multiple of 0.5 !!")
                sys.exit()                  
            if self.NeededPotato>self.AvailablePotato:
                print("Sorry !! Not enough potato available !!")
                sys.exit()             
        except:
            print("Sorry !! Please enter the amount of available potato in whole number like 50 !!")
            print("Sorry !! Please enter the amount of needed Potatoes upto decimal place with the multiple of 0.5 !!")
            sys.exit()