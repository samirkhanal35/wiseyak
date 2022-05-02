def DepthFirstSearch(ProblemInstance, PathIndex, MaxDepth):
    # print("inside DFS, Path:", PathIndex)
    FirstStateIndex = PathIndex[0]
    PrevIndex = FirstStateIndex[1]
    Branch_Pruned = False
    while True:  
        print("updated Path:", PathIndex)
        LastStateIndex = PathIndex[-1]  
        LastState = LastStateIndex[0]        
        # print("before nextmove")          
        NextState = ProblemInstance.nextMove(LastState, PrevIndex)    
        # print("inside DFS nextState:", NextState)    
        if  NextState:
            PathIndex.append(NextState)             
            PrevIndex = FirstStateIndex[1]         
            if ProblemInstance.CheckFinalState(NextState[0]):
                Solved = True 
                return [PathIndex, Branch_Pruned]           
            if (len(PathIndex)-1)>=MaxDepth:
                PrevStateIndex = PathIndex.pop()
                PrevIndex = PrevStateIndex[1]
                Branch_Pruned = True
                continue                    
        else:
            if len(PathIndex)>1:
                PrevStateIndex = PathIndex.pop() 
                PrevIndex = PrevStateIndex[1]       
                continue
            else:                    
                return [[], Branch_Pruned]