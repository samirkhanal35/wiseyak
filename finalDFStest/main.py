import sys
from missionaryCannibal import missionaryProblem as missionary
from potato import PotatoProblem as potato
from pacman import PacmanProblem as pacman
from DFS import DepthFirstSearch

ProblemList = {'potato': potato, 'missionary': missionary, 'pacman': pacman}

def Solution(Problem):
    solutionPath = []
    MaxDepth = int(input("Enter the MaxDepth:"))
    # Initializing the problem instance    
    ProblemInstance = Problem()       
    ProblemInstance.dialog()
    # Solving The Problem  
    solutionPath.append([ProblemInstance.InitialState, ProblemInstance.InitialIndex]) 
    for CurDepth in range(1,MaxDepth+1):
        solutionPath, solutionExists = DepthFirstSearch(ProblemInstance, solutionPath, CurDepth)                                                                                                                
        # Checking and displaying solutions     
        if len(solutionPath)>1:
            FinalSolution = [[StateIndex[0]] for StateIndex in solutionPath] 
            print("Solution in",CurDepth,"Steps\n", FinalSolution)
            break
    if solutionExists :
        print("You can increase the depth for solutions in further depth!")      
    else:
         print("Sorry !! Problem cannot be solved !!")      

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