from enum import Enum

class Choice(Enum):
    ROCK = 1
    PAPER = 2
    SCISSORS = 3

column_a_mappings = {
    "A": Choice.ROCK,
    "B": Choice.PAPER,
    "C": Choice.SCISSORS,
}

column_b_mappings = {
    "X": Choice.ROCK,
    "Y": Choice.PAPER,
    "Z": Choice.SCISSORS,
}

scores = {
    (Choice.ROCK, Choice.SCISSORS): 6,
    (Choice.ROCK, Choice.ROCK): 3,
    (Choice.ROCK, Choice.PAPER): 0,
    (Choice.PAPER, Choice.ROCK): 6,
    (Choice.PAPER, Choice.PAPER): 3,
    (Choice.PAPER, Choice.SCISSORS): 0,
    (Choice.SCISSORS, Choice.PAPER): 6,
    (Choice.SCISSORS, Choice.SCISSORS): 3,
    (Choice.SCISSORS, Choice.ROCK): 0
}
        
def process(line):
    tokens = line.split(" ")
    
    them = column_a_mappings[tokens[0]]
    me = column_b_mappings[tokens[1]]
    
    return me.value + scores[(me, them)]


if __name__ == "__main__":
    
    with open("inputs/day2/input_a.txt", "r") as f:
        input = f.read()
        
    result = sum(map(process, input.split("\n")))
        
    print(result) # 11475