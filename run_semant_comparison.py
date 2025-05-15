import subprocess
import sys

def run_pipeline(filename, semant_cmd):
    """Run lexer | parser | semant and return combined stdout and stderr output."""
    try:
        # Start lexer
        lexer = subprocess.Popen(
            ["/afs/ir/class/cs143/bin/lexer", filename],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE
        )
        # Pipe to parser
        parser = subprocess.Popen(
            ["/afs/ir/class/cs143/bin/parser"],
            stdin=lexer.stdout,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE
        )
        lexer.stdout.close()
        # Pipe to semant
        semant = subprocess.Popen(
            [semant_cmd],
            stdin=parser.stdout,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE
        )
        parser.stdout.close()

        # Capture both stdout and stderr
        out, err = semant.communicate()
        combined_output = out + err

        return combined_output.decode("utf-8").strip()

    except Exception as e:
        return f"Error running pipeline: {e}"

def main():
    if len(sys.argv) != 2:
        print("Usage: python3 run_semant_comparison.py <filename>")
        sys.exit(1)

    filename = sys.argv[1]

    ref_output = run_pipeline(filename, "/afs/ir/class/cs143/bin/semant")
    your_output = run_pipeline(filename, "./semant")

    print("Reference Semant Output:")
    print("----------------------------")
    print(ref_output)
    print("\nYour Output:")
    print("----------------------------")
    print(your_output)

if __name__ == "__main__":
    main()
