import sys

class PotatoProblem:
    def __init__(self):
        self.InitialState = [0]
        self.NeededPotato = 0
        self.AvailablePotato = 0
        self.Possiblemoves = [5.0, 0.5, -5.0, -0.5]
        self.maxMoves = 4
    
    def  CheckLegality(self, State):        
        if State[0] <= self.AvailablePotato  and State[0]>0  :
            return True
        else:
            return False

    def nextMove(self, CurrentState, PrevIndex):   
        print("potato Previndex in nextMove", PrevIndex)       
        for i in range(PrevIndex+1, self.maxMoves+1):            
            Move = self.Possiblemoves[i-1]
            newState = [CurrentState[0]+Move]
            newStateIndex = [newState, i]   
            # print("potato newStateIndex", newStateIndex)               
            if (self.CheckLegality(newState)):  
                # print("passed potato newstateindex:", newStateIndex)                                     
                return newStateIndex            
        return False

    def CheckFinalState(self, CurrentState):
        if CurrentState[0]== self.NeededPotato:
            return True
        else:
            return False
    
    def InitializeVariables(self, neededPotato, AvailablePotato):
        self.NeededPotato = neededPotato
        self.AvailablePotato = AvailablePotato
       
    def dialog(self):
        try:
            AvailablePotato = int(input("Enter the amount of available potato in whole number : "))
            NeededPotato = float(input("Enter the amount of Potatoes needed (upto one decimal place with the multiple of 0.5): "))    
            if (((NeededPotato*100)%10)>0):
                print("Sorry !! Please enter the amount upto decimal place with the multiple of 0.5 !!")
                sys.exit()                  
            if NeededPotato>AvailablePotato:
                print("Sorry !! Not enough potato available !!")
                sys.exit() 
            self.InitializeVariables(NeededPotato, AvailablePotato)
            
        except:
            print("Sorry !! Please enter the amount of available potato in whole number like 50 !!")
            print("Sorry !! Please enter the amount of needed Potatoes upto decimal place with the multiple of 0.5 !!")
            sys.exit()