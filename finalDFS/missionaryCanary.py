import sys
class missionaryProblem:
    def __init__(self, initialState):
        self.InitialState = [initialState[0], initialState[1], 1, 0, 0]
        self.defaultBoatPosition = -1
        self.Possiblemoves = [[0,1],[1,0],[1,1],[0,2],[2,0]]

    def CheckLegality(self, NextState):    
        if (NextState[0]==0) or ((NextState[0]>0) and (NextState[0]>=NextState[1])):
            if  NextState[0]>=0 and NextState[1]>=0 and NextState[0]<=self.InitialState[0] and NextState[1]<=self.InitialState[1] :
                if not ((self.InitialState[0]-NextState[0]) == 0):
                    if ((self.InitialState[0]-NextState[0])>=(self.InitialState[1]-NextState[1])):
                        return True
                    else:
                        return False
                else: 
                    return True
            else:
                return False

    def nextMove( self, CurrentState, index, problemSolution):        
        BoatPosition = self.defaultBoatPosition*CurrentState[2]
        newState = []
        NumMoves = len(self.Possiblemoves)    
        if index+1 == NumMoves:
            return False
        else:
            for i in range(index+1, NumMoves+1):            
                Move = self.Possiblemoves[i-1]
                newState = [CurrentState[0]+(BoatPosition*Move[0]), CurrentState[1]+(BoatPosition*Move[1]), BoatPosition, CurrentState[-1], i]            
                newStateValues = [newState[0], newState[1], newState[2]]                       
                if (self.CheckLegality(newState)) and  (newStateValues not in problemSolution.VisitedStates):
                    problemSolution.VisitedStates.append(newStateValues)                
                    return newState            
            return False 

    def CheckFinalState(self, CurrentState):
        if CurrentState[0]==0 and CurrentState[1]==0:
            return True
        else:
            return False

def missionary():
    try:
        inputNumber = input("Enter the number of missionaries and cannibals(comma separated values like: 3,3):").split(",")
        formattedInputValues = [ int(i) for i in inputNumber]
        if len(formattedInputValues)<2 or len(formattedInputValues)>2:
            print("!!! Please enter the number of missionaries and cannibals as comma separated values as 3,3")
            sys.exit()
        Missionary = missionaryProblem(formattedInputValues)
        return Missionary
    except:
        print("!!! Please enter the number of missionaries and cannibals as comma separated values as 3,3")
        sys.exit()