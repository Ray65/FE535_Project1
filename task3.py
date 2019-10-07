def hello():
    print("Hello World")

def dieGame():
    outcomes = range(1, 7)
    allCombns = list()
    for i1 in outcomes:
        for i2 in outcomes:
            for i3 in outcomes:
                for i4 in outcomes:
                    for i5 in outcomes:
                        for i6 in outcomes:
                            comb = list()
                            comb.append(i1)
                            comb.append(i2)
                            comb.append(i3)
                            comb.append(i4)
                            comb.append(i5)
                            comb.append(i6)
                            allCombns.append(comb)
    print("Length: " + str(len(allCombns)))
    return  allCombns

def diffLessThan3(allCombns):
    count = 0
    for comb in allCombns:
        maximum = max(comb)
        minimum = min(comb)
        if((maximum - minimum) < 3):
            count = count + 1
    prob = count / (len(allCombns))
    return prob



if __name__=="__main__":
    #hello()
    allCombns = dieGame()
    print(diffLessThan3(allCombns))