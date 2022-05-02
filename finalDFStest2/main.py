import sys
from missionaryCannibal import missionaryProblem as missionary
from potato import PotatoProblem as potato
from pacman import PacmanProblem as pacman
from DFS import DepthFirstSearch

ProblemList = {'potato': potato, 'missionary': missionary, 'pacman': pacman}

def SolutionProvider(ProblemInstance, solutionPath, MaxDepth, CurrentDepth = 1):
    for CurDepth in range(CurrentDepth, MaxDepth+1):
        solutionPath, solutionExists = DepthFirstSearch(ProblemInstance, solutionPath, CurDepth)
        if len(solutionPath)>1:
            return solutionPath, CurDepth, solutionExists
    return solutionPath, CurDepth, solutionExists

def Solution(Problem):
    solutionPath = []
    MaxDepth = int(input("Enter the MaxDepth:"))
    CurDepth = 1
    # Initializing the problem instance    
    ProblemInstance = Problem()       
    ProblemInstance.dialog()
    # Solving The Problem  
    solutionPath.append([ProblemInstance.InitialState, ProblemInstance.InitialIndex]) 
    while True:
        solutionPath, CurDepth, solutionExists = SolutionProvider(ProblemInstance, solutionPath, MaxDepth, CurDepth)
        if len(solutionPath)>1:
            FinalSolutionPath = [[StateIndex[0]] for StateIndex in solutionPath] 
            print("Solution:", FinalSolutionPath)
        if not solutionExists:
            print("Sorry!! No more solution exists")
            break    
        if solutionExists and CurDepth==MaxDepth and not(len(solutionPath)>1):
            print("Solution doesn't exists for given depth: ", MaxDepth)
            UserOpinion = input("To continue with new MaxDepth press 'y' or press Enter").lower()
            if UserOpinion == 'y':
                MaxDepth =  int(input("Enter new MaxDepth: "))
                continue
            else:
                break
        NeedNextSolution = input("Enter 'y' for next solution: " ).lower()
        if NeedNextSolution == 'y':
            continue
        break

if __name__ ==  '__main__':
    try:
        ProblemName = sys.argv[1].lower()
        ProblemNameList = dict.keys(ProblemList)
        if (ProblemName not in ProblemNameList):
            print("Invalid input!!!!")   
            print("Enter 'missionary' for missionary and cannibal problem.\nEnter 'potato' for Potato problem.\nEnter 'pacman' for Pacman Problem")
            sys.exit()
        Solution(ProblemList[ProblemName])            
    except:
        print("Run the program using : python3 <file.py> <problemType>")
        print("Problem Types: potato, missionary, pacman")
    sys.exit()