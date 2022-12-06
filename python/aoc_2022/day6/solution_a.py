if __name__ == "__main__":
    
    with open("inputs/day6/input_a.txt", "r") as f:
        input = [*f.read()]
        
    window_size = 4
    
    windows = map(lambda i: input[i: i + window_size], range(len(input) - window_size + 1))
    index, window = next(filter(lambda pair: len(pair[1]) == len(set(pair[1])), enumerate(windows)))
    
    result = index + window_size
    print(result)
    