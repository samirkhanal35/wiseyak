import sys
from missionaryCanary import missionaryProblem as missionary
from potato import PotatoProblem as potato
from IterativeDFS import IterativeDFS
from pacman import PacmanProblem as pacman

ProblemList = {'potato': potato, 'missionary': missionary, 'pacman': pacman}

def Solution(Problem):
    # Initializing the problem instance    
    ProblemInstance = Problem()       
    ProblemInstance.dialog()
    # Solving The Problem    
    solutions = IterativeDFS(ProblemInstance)    
    # Checking and displaying solutions
    if solutions:
        print("Possible Optimum Solution:")
        for FinalSolution in solutions:
            print(FinalSolution)
        sys.exit()
    else:
        print("Sorry !! Problem cannot be solved !!")
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