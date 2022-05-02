Path = []
InitialState = [3,3,1,0,0]
defaultBoatPosition = -1
VisitedStates = []
Possiblemoves = [[0,1],[1,0],[1,1],[0,2],[2,0]]

def CheckLegality(NextState):
    global Path
    InitialState = Path[0]
    if (NextState[0]==0) or ((NextState[0]>0) and (NextState[0]>=NextState[1])):
        if  NextState[0]>=0 and NextState[1]>=0 and NextState[0]<=InitialState[0] and NextState[1]<=InitialState[1] :
            if not ((InitialState[0]-NextState[0]) == 0):
                if ((InitialState[0]-NextState[0])>=(InitialState[1]-NextState[1])):
                    return True
                else:
                    return False
            else: 
                return True
        else:
            return False

def nextMove(CurrentState, index):        
    global Path
    global VisitedStates
    InitialState = Path[0]
    BoatPosition = defaultBoatPosition*CurrentState[2]
    newState = []
    NumMoves = len(Possiblemoves)    
    if index+1 == NumMoves:
        return False
    else:
        for i in range(index+1, NumMoves+1):            
            Move = Possiblemoves[i-1]
            newState = [CurrentState[0]+(BoatPosition*Move[0]), CurrentState[1]+(BoatPosition*Move[1]), BoatPosition, CurrentState[-1], i]            
            newStateValues = [newState[0], newState[1], newState[2]]                       
            if (CheckLegality(newState)) and  (newStateValues not in VisitedStates):
                VisitedStates.append(newStateValues)                
                return newState            
        return False
            
def InitializeState():
    global Path
    Path = []
    Path.append(InitialState)

def CheckFinalState(CurrentState):
    if CurrentState[0]==0 and CurrentState[1]==0:
        return True
    else:
        return False

def missionaryCannibal():
    global Path
    global VisitedStates
    MaxDepth = 100
    Solved = False 
    Depth_in_CurIteration = 1
    Branch_Pruned = True
    while Depth_in_CurIteration < MaxDepth:
        Path = []
        Path.append(InitialState)
        InitialValues = [InitialState[0], InitialState[1], InitialState[2]]
        VisitedStates = []
        VisitedStates.append(InitialValues)
        stateIndex = 0  
        Branch_Pruned = False
        while True:                       
            lastStateIndex = len(Path)-1
            lastState = Path[lastStateIndex]           
            NextState = nextMove(lastState, stateIndex)            
            if  NextState:
                Path.append(NextState)                
                stateIndex = 0             
                if NextState[0]==0 and NextState[1]==0:
                    Solved = True 
                    break                
                if (len(Path)-1)>=Depth_in_CurIteration:
                    removedState = Path.pop()
                    stateIndex = removedState[4]
                    Branch_Pruned = True
                    continue                    
            else:
                if len(Path)>1:
                    removedState = Path.pop()
                    stateIndex = removedState[4]               
                    continue
                else:                    
                    break
        Depth_in_CurIteration +=1     
        if Solved:
            break
        
    if Solved:
        FinalSteps = [[i[0],i[1],i[2]] for i in Path]
        print("Optimal Solution in", Depth_in_CurIteration,"Depth: (-1 -> represents right bank side, 1-> represents left bank side) \n", FinalSteps)
    else:
        print("Sorry !! The Problem cannot be solved.")

if __name__ ==  '__main__':    
    missionaryCannibal()

# for the initialState=[M,C], with M and C greater than 3 are not solvable