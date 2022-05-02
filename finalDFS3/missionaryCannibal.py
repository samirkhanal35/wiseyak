import sys
class missionaryProblem:
    def __init__(self):
        self.InitialState =[0,0,1]
        self.Possiblemoves = [[0,1],[1,0],[1,1],[0,2],[2,0]]
        self.maxMoves = 5     
        self.InitialIndex = -1  

    def CheckLegality(self, State):   
        # print("inside check legality, missionary cannibal problem") 
        if (State[0]==0) or ((State[0]>=0) and (State[0]>=State[1]) ):
            if  State[0]>=0 and State[1]>=0 and State[0]<=self.InitialState[0] and State[1]<=self.InitialState[1] :
                if not ((self.InitialState[0]-State[0]) == 0):
                    if ((self.InitialState[0]-State[0])>=(self.InitialState[1]-State[1])):
                        return True
                    else:
                        return False
                else: 
                    return True
            else:
                return False

    def nextMove( self, CurrentState, PrevIndex): 
        # print("inside missionary cannibal nextMove")
        # print("currentState:", CurrentState, "prevIndex:", PrevIndex)        
        BoatPosition = CurrentState[2]
        for i in range(PrevIndex+1, self.maxMoves+1):            
            Move = self.Possiblemoves[i-1]
            newState = [CurrentState[0]+(-1*BoatPosition*Move[0]), CurrentState[1]+(-1*BoatPosition*Move[1]), -1*BoatPosition] 
            newStateIndex = [newState, i]
            # print("missionarycannibal next state:", newState)
            if (self.CheckLegality(newState))  :       
                return newStateIndex            
        return False 

    def CheckFinalState(self, CurrentState):
        if CurrentState[0]==0 and CurrentState[1]==0:
            return True
        else:
            return False
    
    def dialog(self):
        try:
            inputNumber = input("Enter the number of missionaries and cannibals(comma separated values like: 3,3):").split(",")
            formattedInputValues = [ int(i) for i in inputNumber]
            if len(formattedInputValues)<2 or len(formattedInputValues)>2:
                print("!!! Please enter the number of missionaries and cannibals as comma separated values as 3,3")
                sys.exit()
            self.InitialState[0] =  formattedInputValues[0]  
            self.InitialState[1] =  formattedInputValues[1]         
        except:
            print("!!! Please enter the number of missionaries and cannibals as comma separated values as 3,3")
            sys.exit()