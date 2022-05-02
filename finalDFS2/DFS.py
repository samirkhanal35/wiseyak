def DepthFirstSearch(ProblemInstance, Path, MaxDepth):
    stateIndex = 0  
    Branch_Pruned = False
    while True:  
        lastStateIndex = len(Path)-1
        lastState = Path[lastStateIndex]              
        NextState = ProblemInstance.nextMove(lastState, stateIndex, Path)        
        if  NextState:
            Path.append(NextState)             
            stateIndex = 0         
            if ProblemInstance.CheckFinalState(NextState):
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