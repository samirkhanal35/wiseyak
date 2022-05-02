import sys
from missionaryCannibal import missionaryProblem as missionary
from potato import PotatoProblem as potato
from IterativeDFS import IterativeDFS
from pacman import PacmanProblem as pacman

ProblemList = {'potato': potato, 'missionary': missionary, 'pacman': pacman}

def Solution(Problem):
    solutionPath = []
    CurDepth = 1
    # Initializing the problem instance    
    ProblemInstance = Problem()       
    ProblemInstance.dialog()
    # Solving The Problem  n
    InitialIndex = ProblemInstance.InitialIndex
    InitialStateIndex = [ProblemInstance.InitialState, InitialIndex]
    solutionPath.append(InitialStateIndex) 
    while True:
        solution, CurDepth = IterativeDFS(ProblemInstance, solutionPath, CurDepth)                                                                                                             
        # Checking and displaying solutions
        if solution:
            solutionPath = solution.copy()
            FinalSolution = [[i[0]] for i in solutionPath] 
            print("Solution in",CurDepth,"Steps\n", FinalSolution)
            NextSolutionChoice = input("Do you want next solution? Press 'y' for Yes and any other key to exit.").lower()
            if NextSolutionChoice == 'y':
                continue
            else:
                break            
        else:
            print("Sorry !! Problem cannot be solved !!")
            break
    sys.exit()            

if __name__ ==  '__main__':
    try:
        ProblemName = sys.argv[1].lower()
        Problem = dict.keys(ProblemList)
        if ProblemName in Problem:
            Solution(ProblemList[ProblemName])
        else:
            print("Invalid input!!!!")   
            print("Enter 'missionary' for missionary and cannibal problem.\nEnter 'potato' for Potato problem")
        sys.exit()
    except:
        print("Run the program using : python3 <file.py> <problemType>")
        print("Problem Types: potato, missionary, pacman")
        sys.exit()