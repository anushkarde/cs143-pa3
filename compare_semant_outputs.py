import subprocess
import sys

def run_pipeline(filename, semant_cmd):
    """Run lexer | parser | semant and return combined stdout and stderr as lines."""
    try:
        lexer = subprocess.Popen(
            ["/afs/ir/class/cs143/bin/lexer", filename],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE
        )
        parser = subprocess.Popen(
            ["/afs/ir/class/cs143/bin/parser"],
            stdin=lexer.stdout,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE
        )
        lexer.stdout.close()
        semant = subprocess.Popen(
            [semant_cmd],
            stdin=parser.stdout,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE
        )
        parser.stdout.close()

        out, err = semant.communicate()
        combined_output = out + err
        return combined_output.decode("utf-8").splitlines()

    except Exception as e:
        return [f"Error running pipeline: {e}"]

def compare_outputs(ref_lines, your_lines):
    min_len = min(len(ref_lines), len(your_lines))
    for i in range(min_len):
        if ref_lines[i] != your_lines[i]:
            return i + 1, ref_lines[i], your_lines[i]

    if len(ref_lines) != len(your_lines):
        return min_len + 1, (
            ref_lines[min_len] if len(ref_lines) > min_len else "<no line>",
            your_lines[min_len] if len(your_lines) > min_len else "<no line>"
        )

    return None  # No differences

def main():
    if len(sys.argv) != 2:
        print("Usage: python3 compare_semant_outputs.py <filename>")
        sys.exit(1)

    filename = sys.argv[1]

    ref_lines = run_pipeline(filename, "/afs/ir/class/cs143/bin/semant")
    your_lines = run_pipeline(filename, "./semant")

    result = compare_outputs(ref_lines, your_lines)

    if result is None:
        print("✅ Outputs match exactly.")
    else:
        line_num, ref_line, your_line = result
        print("❌ Outputs differ.")
        print(f"First mismatch at line {line_num}:")
        print(f"Reference: {ref_line}")
        print(f"Your Output: {your_line}")

if __name__ == "__main__":
    main()
