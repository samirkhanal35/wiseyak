def DepthFirstSearch(ProblemInstance, PathIndex, MaxDepth):
    [_, PrevIndex] = PathIndex[0]
    Branch_Pruned = False
    while True:  
        [LastState, _] = PathIndex[-1]             
        NextState = ProblemInstance.nextMove(LastState, PrevIndex)      
        if  NextState:
            PathIndex.append(NextState)             
            [_, PrevIndex] = PathIndex[0]         
            if ProblemInstance.CheckFinalState(NextState[0]):
                return [PathIndex, Branch_Pruned]           
            if (len(PathIndex)-1)>=MaxDepth:
                [_, PrevIndex] = PathIndex.pop() 
                Branch_Pruned = True
                continue                    
        else:
            if len(PathIndex)>1:
                [_, PrevIndex] = PathIndex.pop()    
                continue
            else:                    
                return [PathIndex, Branch_Pruned]