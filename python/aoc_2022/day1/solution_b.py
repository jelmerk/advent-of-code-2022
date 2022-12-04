if __name__ == "__main__":
    
    with open("inputs/day1/input_a.txt", "r") as f:
        input = f.read()
        
    groups = map(lambda group: map(lambda line: int(line), group), map(lambda group: group.split("\n"), input.split("\n\n")))    
    group_sums = map(lambda group: sum(group), groups)
    result = sum(sorted(group_sums, reverse=True)[:3])
    print(result) # 207456
    