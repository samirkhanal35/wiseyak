import sys

def missionaryCannibal(InitialState):
    totalStates = []
    possibleMoves = [[2,0],[1,1],[0,1],[1,0],[0,2]]
    curStates = []
    curStates.append(InitialState)
    totalStates.extend(curStates)
    visitedStates = []
    visitedStates.append(InitialState[:2])
    foundFinalState = False
    boatPosition = -1
    while True:
        tempStates = []       
        for curState in curStates:
            for move in possibleMoves:                       
                newState = [curState[0] +(boatPosition*move[0]), curState[1] +(boatPosition*move[1])]                
                print(curState,  move, "=", newState)
                if newState[0]==0 and newState[1]==0:
                    tempStates.append(newState)
                    foundFinalState = True
                    break
                if (newState[0]>=newState[1])  and (newState[0]>0) and (newState[1]>0) and not(newState in visitedStates):
                    # if ((not ((InitialState[0]-newState[0])==0) and ((InitialState[0]-newState[0])>=(InitialState[1]-newState[1]))) or (InitialState[0]-newState[0])==0):
                    tempStates.append(newState)        
                    visitedStates.append(newState)            
                else: 
                    continue
            if foundFinalState: 
                break        
        
        curStates = []
        curStates.extend(tempStates)
        totalStates.append(curStates)
        # if boatPosition>0:
        #     boatPosition = -1
        # else:
        #     boatPosition = 1
        if foundFinalState: 
            break
    print(totalStates)   

if __name__ ==  '__main__':
    # a = list(sys.argv[1:])
    InitialState = [3,3,1]
    missionaryCannibal(InitialState)