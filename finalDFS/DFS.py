def DepthFirstSearch(ProblemInstance, problemSolution, MaxDepth):
    stateIndex = 0  
    Branch_Pruned = False
    while True:                    
        lastState =  problemSolution.getLastState()       
        NextState = ProblemInstance.nextMove(lastState, stateIndex, problemSolution)        
        if  NextState:
            problemSolution.Path.append(NextState)                
            stateIndex = 0         
            if ProblemInstance.CheckFinalState(NextState,):
                Solved = True 
                return [problemSolution.Path, Branch_Pruned]           
            if (len(problemSolution.Path)-1)>=MaxDepth:
                removedState = problemSolution.Path.pop()
                stateIndex = removedState[-1]
                Branch_Pruned = True
                continue                    
        else:
            if len(problemSolution.Path)>1:
                removedState = problemSolution.Path.pop()
                stateIndex = removedState[-1]               
                continue
            else:                    
                return [[], Branch_Pruned]