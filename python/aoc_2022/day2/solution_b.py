from enum import Enum

class Choice(Enum):
    ROCK = 1
    PAPER = 2
    SCISSORS = 3

class Outcome(Enum):
    LOSE = 1
    DRAW = 2
    WIN = 3

column_a_mappings = {
    "A": Choice.ROCK,
    "B": Choice.PAPER,
    "C": Choice.SCISSORS,
}

column_b_mappings = {
    "X": Outcome.LOSE,
    "Y": Outcome.DRAW,
    "Z": Outcome.WIN,
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
        
plays = {
    (Choice.ROCK, Outcome.LOSE): Choice.SCISSORS,
    (Choice.ROCK, Outcome.WIN): Choice.PAPER,
    (Choice.ROCK, Outcome.DRAW): Choice.ROCK,
    (Choice.PAPER, Outcome.LOSE): Choice.ROCK,
    (Choice.PAPER, Outcome.WIN): Choice.SCISSORS,
    (Choice.PAPER, Outcome.DRAW): Choice.PAPER,
    (Choice.SCISSORS, Outcome.LOSE): Choice.PAPER,
    (Choice.SCISSORS, Outcome.WIN): Choice.ROCK,
    (Choice.SCISSORS, Outcome.DRAW): Choice.SCISSORS
}

def process(line):
    tokens = line.split(" ")
    
    them = column_a_mappings[tokens[0]]
    desired_outcome = column_b_mappings[tokens[1]]
    
    me = plays[(them, desired_outcome)]
    
    return me.value + scores[(me, them)]


if __name__ == "__main__":
    
    with open("inputs/day2/input_a.txt", "r") as f:
        input = f.read()
        
    result = sum(map(process, input.split("\n")))
        
    print(result) # 16862