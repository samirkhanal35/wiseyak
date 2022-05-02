class PacmanProblem:
    def __init__(self):
        self.InitialState = [0,0]
        self.FinalState = 0
        self.WallPositions = 0
        self.gridSize = 0
        self.Possiblemoves = [[-1,0], [0,-1],[0,1], [1,0]]
        self.maxMoves = 4       

    def CheckLegality(self, State):    
        if  ([State[0],State[1]] not in self.WallPositions) and ((State[0]<self.gridSize[0] and State[0]>=0) and (State[1]<self.gridSize[1] and State[1]>=0)):
            return True
        else:
            return False

    def nextMove(self, CurrentState, PrevIndex):                 
        for i in range(PrevIndex+1, self.maxMoves+1):            
            Move = self.Possiblemoves[i-1]
            newState = [CurrentState[0]+Move[0],  CurrentState[1]+Move[1]]
            newStateIndex = [newState, i]
            # print("pacman newState:", newState)
            if (self.CheckLegality(newState) )  : 
                return newStateIndex            
        return False 

    def CheckFinalState(self, CurrentState):
        if CurrentState[0]==self.FinalState[0] and CurrentState[1]==self.FinalState[1]:
            return True
        else:
            return False

    def InitializeVariables(self, PacManPosition, foodPosition, gridWall, gridSize):
        self.InitialState[0] = PacManPosition[0]
        self.InitialState[1] = PacManPosition[1]
        self.FinalState = foodPosition
        self.WallPositions = gridWall
        self.gridSize = gridSize

    def dialog(self):
        # print("inside pacman dialog")
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
        # print("before initializing pacman variables")
        # print("packman walls:", Walls)
        self.InitializeVariables(PacManPosition, foodPosition, Walls, gridSize)  
        # print("after initializing pacman variables")     