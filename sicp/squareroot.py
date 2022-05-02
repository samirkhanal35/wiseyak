def average(x,y):
    return (x+y)/2

def square(x):
    return x*x

def improve(guess, x):
    return average(guess, (x/guess))

def goodEnough(guess, x):
    return (abs(square(guess)-x)<0.001)

def sqrtIter(guess, x):
    if goodEnough(guess, x):
        print("guess:", guess)        
    else:
        # print("guess:", guess)
        sqrtIter(improve(guess, x) , x)

def sqrt(x):
    a = sqrtIter(1.0, x)
   

sqrt(25)