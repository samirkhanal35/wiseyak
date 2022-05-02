class PacmanProblem:
    def __init__(self):
        self.InitialState = [0,0]
        self.FinalState = 0
        self.WallPositions = 0
        self.gridSize = 0
        self.Possiblemoves = [[-1,0], [0,-1],[0,1], [1,0]]
        self.maxMoves = 4   
        self.InitialIndex = -1  

    def CheckLegality(self, State):    
        return ( ([State[0],State[1]] not in self.WallPositions) and (State[0]<=self.gridSize[0] or State[0]>=0) and (State[1]<=self.gridSize[1] or State[1]>=0))

    def nextMove(self, CurrentState, PrevIndex):              
        for i in range(PrevIndex+1, self.maxMoves):            
            Move = self.Possiblemoves[i]
            newState = [CurrentState[0]+Move[0],  CurrentState[1]+Move[1]]
            newStateIndex = [newState, i]
            if (self.CheckLegality(newState) )  : 
                return newStateIndex            
        return False 

    def CheckFinalState(self, CurrentState):
        return (CurrentState[0]==self.FinalState[0] and CurrentState[1]==self.FinalState[1])

    def dialog(self):
        self.InitialState = [3, 9]
        self.FinalState = [5, 1]
        self.gridSize = [7, 20]
        """A wall is represented by the character '%' ( ascii value 37 ), 
        PacMan is represented by UpperCase alphabet 'P' ( ascii value 80 ), 
        empty spaces which can be used by PacMan for movement is represented by the character '-' ( ascii value 45 ) 
        and food is represented by the character '.' ( ascii value 46 )  """

        grid = [['%','%','%','%','%','%','%','%','%','%','%','%','%','%','%','%','%','%','%','%'],\
            ['%','-','-','-','-','-','-','-','-','-','-','-','-','-','-','%','-','-','-','%'],\
                ['%','-','%','%','-','%','%','-','%','%','-','%','%','-','%','%','-','%','-','%'],\
                    ['%','-','-','-','-','-','-','-','-','P','-','-','-','-','-','-','-','%','-','%'],\
                        ['%','%','%','-','%','%','%','%','-','%','%','%','%','%','%','%','%','%','-','%'],\
                            ['%','.','-','-','-','-','-','-','-','-','%','-','-','-','-','-','-','-','-','%'],\
                                ['%','%','%','%','%','%','%','%','%','%','%','%','%','%','%','%','%','%','%','%']]
        
        Walls = []
        for i in range(0, self.gridSize[0]):
            for j in range(0, self.gridSize[1]):
                if grid[i][j] == '%':
                    Walls.append([i,j])
        self.WallPositions = Walls