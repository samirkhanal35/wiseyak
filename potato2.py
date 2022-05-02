Path = []
InitialState = [0, 0, 0]
NeededPotato = 0
AvailablePotato = 50
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
    newState = []
    NumMoves = len(Possiblemoves)    
    if index+1 == NumMoves:
        return False
    else:
        for i in range(index+1, NumMoves+1):            
            Move = Possiblemoves[i-1]
            newState = [CurrentState[0]+Move,  CurrentState[-1], i]
            newStateValues = [newState[0]]            
            if (CheckLegality(newState)):                                       
                return newState            
        return False
            
def InitializeState():
    global Path
    Path = []
    Path.append(InitialState)

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
    
def weightPotato(neededPotato):
    global Path    
    global NeededPotato 
    global TotalSolutions
    NeededPotato = neededPotato
    if NeededPotato>AvailablePotato:
        print("Sorry !! Not enough potato available !!")
        return 0
    MaxDepth = 1
    InitializeState()    
    while True:       
        [solution, solutionExists] = DepthFirstSearch(MaxDepth)
        if solution:
            FinalSteps = [[i[0]] for i in solution]                       
                          
            TotalSolutions.append(FinalSteps)      
            continue                             
        else:
            if solutionExists and not(TotalSolutions):
                MaxDepth += 1
                InitializeState()                
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