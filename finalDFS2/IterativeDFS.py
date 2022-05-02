from DFS import DepthFirstSearch

def IterativeDFS(ProblemInstance):
    MaxDepth = 1 
    TotalSolutions = []   
    Path = []
    Path.append(ProblemInstance.InitialState)
    while True:       
        [solution, solutionExists] = DepthFirstSearch(ProblemInstance, Path, MaxDepth)
        Path = solution
        if solution and (solution not in TotalSolutions):
            FinalSteps = [[i[:-2]] for i in solution]                          
            TotalSolutions.append(FinalSteps)      
            continue                             
        else:
            if solutionExists and not(TotalSolutions):
                MaxDepth += 1
                Path = []                
                Path.append(ProblemInstance.InitialState)           
                continue
            else:
                if TotalSolutions:
                    return TotalSolutions
                else:
                    return []