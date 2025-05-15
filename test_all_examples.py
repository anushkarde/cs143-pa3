import subprocess
import os

# Define paths
examples_dir = "/afs/ir/class/cs143/examples/"
local_semant = "./semant"
reference_semant = "/afs/ir/class/cs143/bin/semant"
lexer = "/afs/ir/class/cs143/bin/lexer"
parser = "/afs/ir/class/cs143/bin/parser"

# List of test files
test_files = [
    "arith.cl", "atoi.cl", "atoi_test.cl", "book_list.cl", "cells.cl", "complex.cl", "cool.cl",
    "echo.cl", "graph.cl", "hairyscary.cl", "hello_world.cl", "io.cl", "lam.cl",
    "life.cl", "list.cl", "new_complex.cl", "palindrome.cl", "primes.cl", "snowman.cl", "sort_list.cl"
]

def run_pipeline(semant_path, filename):
    try:
        lexer_proc = subprocess.Popen([lexer, filename], stdout=subprocess.PIPE)
        parser_proc = subprocess.Popen([parser], stdin=lexer_proc.stdout, stdout=subprocess.PIPE)
        semant_proc = subprocess.Popen([semant_path], stdin=parser_proc.stdout, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        lexer_proc.stdout.close()
        parser_proc.stdout.close()
        output, _ = semant_proc.communicate()
        return output.decode("utf-8", errors="replace").splitlines()
    except Exception as e:
        return [f"Error running pipeline: {e}"]

def find_first_mismatch(lines1, lines2):
    for i, (line1, line2) in enumerate(zip(lines1, lines2), 1):
        if line1 != line2:
            return i, line1, line2
    if len(lines1) != len(lines2):
        return min(len(lines1), len(lines2)) + 1, (
            lines1[len(lines2)] if len(lines1) > len(lines2) else "<no line>"), (lines2[len(lines1)] if len(lines2) > len(lines1) else "<no line>")
    return None, None, None

def main():
    mismatches = []

    for test_file in test_files:
        full_path = os.path.join(examples_dir, test_file)
        print(f"Testing {test_file}...")
        output_local = run_pipeline(local_semant, full_path)
        output_reference = run_pipeline(reference_semant, full_path)

        line_num, line_local, line_ref = find_first_mismatch(output_local, output_reference)

        if line_num is not None:
            mismatches.append(test_file)
            print(f"❌ Mismatch at line {line_num} in {test_file}:")
            print(f"  Your semant:     {line_local}")
            print(f"  Reference semant:{line_ref}")
        else:
            print(f"✅ Output match for {test_file}")

    print("\nSummary:")
    if mismatches:
        print("The following files had mismatched output:")
        for f in mismatches:
            print(f" - {f}")
    else:
        print("All files matched!")

if __name__ == "__main__":
    main()
