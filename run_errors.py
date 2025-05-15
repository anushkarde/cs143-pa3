import subprocess
import sys

def run_pipeline(cmd):
    result = subprocess.run(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    return (result.stdout + result.stderr).decode("utf-8")

def main():
    if len(sys.argv) < 2:
        print("Usage: python3 raw_compare_semant.py <file1.cl> [file2.cl ...]")
        sys.exit(1)

    for filename in sys.argv[1:]:
        print(f"\n=== {filename}: Reference Output ===")
        ref_cmd = f"/afs/ir/class/cs143/bin/lexer {filename} | /afs/ir/class/cs143/bin/parser | /afs/ir/class/cs143/bin/semant"
        print(run_pipeline(ref_cmd))

        print(f"\n=== {filename}: Your Output ===")
        your_cmd = f"/afs/ir/class/cs143/bin/lexer {filename} | /afs/ir/class/cs143/bin/parser | ./semant"
        print(run_pipeline(your_cmd))

if __name__ == "__main__":
    main()