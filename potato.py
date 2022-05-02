Path = []
InitialState = [0, 0, 0]
NeededPotato = 0
AvailablePotato = 50
VisitedStates = []
Possiblemoves = [5.0, 0.5, -5.0, -0.5]
TotalSolutions = []

def getLastState():
    global Path
    lastStateIndex = len(Path)-1
    lastState = Path[lastStateIndex]   
    return lastState

def getMinimumPossibleMove():
    Moves = [ abs(i) for i in Possiblemoves]
    return min(Moves)   

def CheckLegality(NextState):
    global AvailablePotato
    global Possiblemoves   
    lastState = getLastState()
    minimumPossibleMove = getMinimumPossibleMove()
    if NextState[0] <= AvailablePotato  and NextState[0]>0 and (abs(NextState[0]-NeededPotato)<=(abs(lastState[0]-NeededPotato)+minimumPossibleMove)) :
        return True
    else:
        return False
 
def nextMove(CurrentState, index):        
    global Path
    global VisitedStates
    newState = []
    NumMoves = len(Possiblemoves)    
    if index+1 == NumMoves:
        return False
    else:
        for i in range(index+1, NumMoves+1):            
            Move = Possiblemoves[i-1]

            newState = [CurrentState[0]+Move,  CurrentState[-1], i]    

            newStateValues = [newState[0]] 
            # print("Path: ", Path)
            # print("Next State:", newState)      
                     
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
    InitialValues = [InitialState[0]]
    VisitedStates.append(InitialValues)

def CheckFinalState(CurrentState):
    if CurrentState[0]== NeededPotato:
        return True
    else:
        return False

def DepthFirstSearch(MaxDepth):
    global Path      
    stateIndex = 0  
    Branch_Pruned = False
    while True:       
        lastState =  getLastState()                       
        NextState = nextMove(lastState, stateIndex)            
        if  NextState:
            Path.append(NextState)                
            stateIndex = 0         
            # print("Path:", Path)
            InitializeVisitedStates()    
            
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
                InitializeVisitedStates()                               
                continue
            else:                    
                return [[], Branch_Pruned]
    
def weightPotato(neededPotato):
    global Path
    global VisitedStates
    global NeededPotato 
    global TotalSolutions
    NeededPotato = neededPotato
    if NeededPotato>AvailablePotato:
        print("Sorry !! Not enough potato available !!")
        return 0
    MaxDepth = 1
    InitializeState()
    InitializeVisitedStates() 
    while True:       
        # print("Depth:", MaxDepth)
        [solution, solutionExists] = DepthFirstSearch(MaxDepth)
        if solution:
            FinalSteps = [[i[0]] for i in solution]
            # print("solutions:", solution)     
            InitializeVisitedStates()             
            if not(FinalSteps in TotalSolutions) :                
                TotalSolutions.append(FinalSteps)      
                continue                             
        else:
            if solutionExists and not(TotalSolutions):
                MaxDepth += 1
                InitializeState()
                InitializeVisitedStates() 
                continue
            else:
                if TotalSolutions:
                    print("Possible Optimum Solutions:")
                    for FinalSolution in TotalSolutions:
                        print(FinalSolution)
                    break
                else:
                    print("Sorry !! Not enough potato available !!")
                    break

if __name__ ==  '__main__': 
    NeededPotato = float(input("Enter the amount of potato required (upto one decimal point): "))   
    weightPotato(NeededPotato)