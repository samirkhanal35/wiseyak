import sys
class missionaryProblem:
    def __init__(self):
        self.InitialState =[0,0,1]
        self.Possiblemoves = [[0,1],[1,0],[1,1],[0,2],[2,0]]
        self.maxMoves = 5     
        self.InitialIndex = -1

    def CheckLegality(self, State):   
        rCannibals = State[1]
        rMissionaries = State[0]
        lCannibals = self.InitialState[1]-State[1]
        lMissionaries = self.InitialState[0]-State[0]
        return( (((rCannibals<=rMissionaries) or (rMissionaries==0)) and ((lCannibals<=lMissionaries) or (lMissionaries==0))))
      
    def nextMove( self, CurrentState, PrevIndex):     
        BoatPosition = CurrentState[2]
        for i in range(PrevIndex+1, self.maxMoves):            
            Move = self.Possiblemoves[i]
            newState = [CurrentState[0]+(-1*BoatPosition*Move[0]), CurrentState[1]+(-1*BoatPosition*Move[1]), -1*BoatPosition] 
            newStateIndex = [newState, i]
            if (self.CheckLegality(newState))  :       
                return newStateIndex            
        return False 

    def CheckFinalState(self, CurrentState):
        return(CurrentState[0]==0 and CurrentState[1]==0)
    
    def dialog(self):
        try:
            inputNumber = input("Enter the number of missionaries and cannibals(comma separated values like: 3,3):").split(",")
            formattedInputValues = [ int(i) for i in inputNumber]
            if not (len(formattedInputValues)==2): sys.exit()
            self.InitialState[0], self.InitialState[1] =  formattedInputValues[0], formattedInputValues[1]       
        except:
            print("!!! Please enter the number of missionaries and cannibals as comma separated values as 3,3")
            sys.exit()