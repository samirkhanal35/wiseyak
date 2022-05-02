import sys
from missionaryCanary import missionary
from potato import potato
from IterativeDFS import IterativeDFS
from pacman import pacman

class ProblemSolution:
    def __init__(self):
        self.Path = []
        self.TotalSolutions = []
        self.VisitedStates = []

    def InitializeState(self, InitialState):
        self.Path = []
        self.Path.append(InitialState)

    def InitializeVisitedStates(self, InitialState):
        self.VisitedStates = []
        InitialValues = [ i for i in InitialState[:-2]]
        self.VisitedStates.append(InitialValues)

    def getLastState(self):
        lastStateIndex = len(self.Path)-1
        lastState = self.Path[lastStateIndex]   
        return lastState

def ProblemIdentifier(ProblemName):
    if (ProblemName == 'potato'):
         return potato
    elif (ProblemName == 'missionary'):
        return missionary 
    elif(ProblemName =='pacman'):
        return pacman            
    else:
        print("Invalid input!!!!")   
        print("Enter 'missionary' for missionary and cannibal problem.\nEnter 'potato' for Potato problem")
        sys.exit()

def Solution(Problem):
    # identifying the type of problem 
    ProblemFunction = ProblemIdentifier(Problem)
    # Initializing the problem instance
    ProblemInstance = ProblemFunction()   
    # Solving The Problem
    problemSolution = ProblemSolution()
    solutions = IterativeDFS(ProblemInstance, problemSolution)    
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
    ProblemName = sys.argv[1].lower()
    Solution(ProblemName.lower())
   