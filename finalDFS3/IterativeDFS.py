from DFS import DepthFirstSearch

def IterativeDFS(ProblemInstance,  Path, MaxDepth = 1):        
    while True:     
        print("Depth inside ITDFS", MaxDepth)  
        [solution, solutionExists] = DepthFirstSearch(ProblemInstance, Path, MaxDepth)
        # print("Iterative dfs returned solution:",solution,"solutionExists:", solutionExists)
        if solution :
            return solution, MaxDepth
        else:
            if solutionExists:
                MaxDepth += 1
                InitialStateIndex = Path[0]
                Path = []                
                Path.append(InitialStateIndex)           
                continue
            else:
                return [], MaxDepth