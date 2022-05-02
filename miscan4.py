Path = []
OptimumPath = []
defaultBoatPosition = -1
VisitedStates = []
Possiblemoves = [[0,1],[1,0],[1,1],[0,2],[2,0]]

def CheckLegality(NextState):
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

def shortestPath():
    global Path
    global OptimumPath
    CurrentState = Path[0]
    count = 0
    while True:
        OptimumPath.append(CurrentState)
        count += 1
        indexes = []        
        BoatPosition = defaultBoatPosition*CurrentState[2]
        for Move in Possiblemoves:
            newState = [CurrentState[0]+(BoatPosition*Move[0]), CurrentState[1]+(BoatPosition*Move[1]), BoatPosition]
            if  (newState in Path):
                indexes.append(Path.index(newState))
        maxIndex = max(indexes)
        CurrentState = Path[maxIndex]
        if maxIndex == (len(Path)-1):
            OptimumPath.append(CurrentState)            
            break
        
def generateMove(CurrentState, index):    
    global Path
    InitialState = Path[0]
    BoatPosition = defaultBoatPosition*CurrentState[2]
    newState = []
    NumMoves = len(Possiblemoves)
    if index+1 == NumMoves:
        return False
    else:
        for i in range(index+1, NumMoves+1):
            Move = Possiblemoves[i-1]
            newState = [CurrentState[0]+(BoatPosition*Move[0]), CurrentState[1]+(BoatPosition*Move[1]), BoatPosition, CurrentState[4], i]
            newStateValues = [newState[0], newState[1], newState[2]]           
            if (CheckLegality(newState)) and  (newStateValues not in VisitedStates):
                VisitedStates.append(newStateValues)
                return newState            
        return False
            
def missionaryCannibal(InitialState):
    global Path
    Path.append(InitialState)
    InitialValues = [InitialState[0], InitialState[1], InitialState[2]]
    VisitedStates.append(InitialValues)
    stateIndex = 0
    canBeSolved = True
    while True:    
        lastStateIndex = len(Path)-1
        lastState = Path[lastStateIndex]
        NextState = generateMove(lastState, stateIndex)
        if  NextState:
            Path.append(NextState)    
            stateIndex = 0             
            if NextState[0]==0 and NextState[1]==0:
                break                    
        else:
            if len(Path)>1:
                removedState = Path.pop()
                stateIndex = removedState[4]               
                continue
            else:
                canBeSolved = False
                break
    # print("Path:", Path)
    if canBeSolved:
        FinalSteps = [[i[0],i[1],i[2]] for i in Path]
        Path = FinalSteps
        shortestPath()
        print("Optimum Solution:", OptimumPath)
        # print("Solution Steps: (-1 -> represents right bank side, 1-> represents left bank side) \n", FinalSteps)
    else:
        print("Sorry !! The Problem cannot be solved.")

if __name__ ==  '__main__':
    InitialState = [3,3,1,0,0]
    missionaryCannibal(InitialState)

# for the initialState=[M,C], with M and C greater than 3 are not solvable