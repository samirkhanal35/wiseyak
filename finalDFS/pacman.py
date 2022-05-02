class PacmanProblem:
    def __init__(self, PacManPosition, foodPosition, gridWall, gridSize):
        self.InitialState = [PacManPosition[0], PacManPosition[1], 0, 0]
        self.FinalState = foodPosition
        self.WallPositions = gridWall
        self.gridSize = gridSize
        self.Possiblemoves = [[-1,0], [0,-1],[0,1], [1,0]]

    def CheckLegality(self, NextState):    
        # check for WallPositions and for the GridBoundary through GridSize
        if  (NextState not in self.WallPositions) and ((NextState[0]<self.gridSize[0] and NextState[0]>=0) and (NextState[1]<self.gridSize[1] and NextState[1]>=0)):
            return True
        else:
            return False

    def nextMove(self, CurrentState, index, problemSolution):               
        newState = []
        NumMoves = len(self.Possiblemoves)    
        if index+1 == NumMoves:
            return False
        else:
            # print("path:", problemSolution.Path)
            for i in range(index+1, NumMoves+1):            
                Move = self.Possiblemoves[i-1]
                newState = [CurrentState[0]+Move[0],  CurrentState[1]+Move[1], CurrentState[-1], i]
                newStateValues = [newState[0], newState[1]]   
                # print("new state:", newState)         
                if (self.CheckLegality(newStateValues)) and  (newStateValues not in problemSolution.VisitedStates):     
                    problemSolution.VisitedStates.append(newStateValues)                                  
                    return newState            
            return False 

    def CheckFinalState(self, CurrentState):
        if CurrentState[0]==self.FinalState[0] and CurrentState[1]==self.FinalState[1]:
            return True
        else:
            return False

def pacman():
    PacManPosition = [3, 9]
    foodPosition = [5, 1]
    gridSize = [7, 20]
    """A wall is represented by the character '%' ( ascii value 37 ), 
    PacMan is represented by UpperCase alphabet 'P' ( ascii value 80 ), 
    empty spaces which can be used by PacMan for movement is represented by the character '-' ( ascii value 45 ) 
    and food is represented by the character '.' ( ascii value 46 )  """

    grid = [['%','%','%','%','%','%','%','%','%','%','%','%','%','%','%','%','%','%','%','%'],\
        ['%','-','-','-','-','-','-','-','-','-','-','-','-','-','-','%','-','-','-','%'],\
            ['%','-','%','%','-','%','%','-','%','%','-','%','%','-','%','%','-','%','-','%'],\
                ['%','-','-','-','-','-','-','-','-','P','-','-','-','-','-','-','-','%','-','%'],\
                    ['%','%','%','%','%','%','%','%','%','%','%','%','%','%','%','%','%','%','-','%'],\
                        ['%','.','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','%'],\
                            ['%','%','%','%','%','%','%','%','%','%','%','%','%','%','%','%','%','%','%','%']]
    
    Walls = []
    for i in range(0, gridSize[0]):
        for j in range(0, gridSize[1]):
            if grid[i][j] == '%':
                Walls.append([i,j])
    Pacman = PacmanProblem(PacManPosition, foodPosition, Walls, gridSize)
    return Pacman