from DFS import DepthFirstSearch

def IterativeDFS(ProblemInstance, problemSolution):
    MaxDepth = 1    
    problemSolution.InitializeState(ProblemInstance.InitialState)  
    problemSolution.InitializeVisitedStates(ProblemInstance.InitialState)
    while True:       
        [solution, solutionExists] = DepthFirstSearch(ProblemInstance, problemSolution, MaxDepth)
        if solution:
            FinalSteps = [[i[:-2]] for i in solution]                          
            problemSolution.TotalSolutions.append(FinalSteps)      
            continue                             
        else:
            if solutionExists and not(problemSolution.TotalSolutions):
                MaxDepth += 1
                problemSolution.InitializeState(ProblemInstance.InitialState)  
                problemSolution.InitializeVisitedStates(ProblemInstance.InitialState)            
                continue
            else:
                if problemSolution.TotalSolutions:
                    return problemSolution.TotalSolutions
                else:
                    # print("Sorry !! Problem cannot be solved !!")
                    return []