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

def InitializeVisitedStates():
    global VisitedStates
    VisitedStates = []
    InitialValues = [InitialState[0], InitialState[1], InitialState[2]]
    VisitedStates.append(InitialValues)

def CheckFinalState(CurrentState):
    if CurrentState[0]==0 and CurrentState[1]==0:
        return True
    else:
        return False

def getLastState():
    global Path
    lastStateIndex = len(Path)-1
    lastState = Path[lastStateIndex]   
    return lastState

def DepthFirstSearch(MaxDepth):
    global Path
    InitializeState()    
    InitializeVisitedStates()    
    stateIndex = 0  
    Branch_Pruned = False
    while True:                    
        lastState =  getLastState()               
        NextState = nextMove(lastState, stateIndex)            
        if  NextState:
            Path.append(NextState)                
            stateIndex = 0             
            if CheckFinalState(NextState):
                Solved = True 
                return [Path, Branch_Pruned]                
            if (len(Path)-1)>=MaxDepth:
                removedState = Path.pop()
                stateIndex = removedState[-1]
                Branch_Pruned = True
                continue                    
        else:
            if len(Path)>1:
                removedState = Path.pop()
                stateIndex = removedState[-1]               
                continue
            else:                    
                return [[], Branch_Pruned]
    
def missionaryCannibal():
    global Path
    global VisitedStates
    MaxDepth = 1
    while True:
        [solution, solutionExists] = DepthFirstSearch(MaxDepth)
        if solution:
            FinalSteps = [[i[0],i[1],i[2]] for i in Path]
            print("Optimal Solution in", MaxDepth,"Depth: (-1 -> represents right bank side, 1-> represents left bank side) \n", FinalSteps)
            break
        else:
            if solutionExists:
                MaxDepth += 1
                continue
            else:
                print("Sorry !! The Problem cannot be solved.")
                break

if __name__ ==  '__main__':    
    missionaryCannibal()