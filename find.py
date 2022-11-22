def findMutation(mutationString, dnaFile):
    with open(dnaFile) as f:
        if mutationString in f.read():
            print('\nPateient has it\n')  
        else:
            print('\nPateient clear\n')

def main():
    text_file = open("jack.txt", "r")
    data = text_file.read()
    text_file.close()

    str = 'CAACGATCGGCC'
    findMutation(str, "jack.txt")

    n = 5
    chunks = [data[i:i+n] for i in range(0, len(data), n)]

    print(chunks)

    print('\nNumber of occurrence of AATGA:', data.count('AATGA'))

    
if __name__ == "__main__":
    main()