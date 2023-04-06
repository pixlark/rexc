# Lesen is the front-to-back testing framework for Rexc.
#
# It finds all tests in a directory and runs them, where is a test is
# a source file and an expected output. That output can be something on
# stdout, an expected status on stderr, etc.
#
# It can also test against the emitted C output, instead of the output
# from running the program.
#
# Lesen is our main defense against regression. Unit tests via `cargo test`
# are written where we can, but they tend to be extremely verbose, because
# they require one to recreate by hand the huge data structures that result
# from parsing into an AST.

import argparse
import colorama
import difflib
import json
import os
import os.path
import subprocess
import sys
from termcolor import colored, cprint

colorama.just_fix_windows_console()

def print_at_depth(depth, s):
    print('\n'.join(map(lambda c: "  "*depth + c, s.split('\n'))))

def print_verbose(args, s):
    if args["verbose"]:
        print(s)

def validate_run_args(args):
    if not os.path.isdir(args["directory"]):
        raise Exception(f"{args['directory']} is not a directory.")

def validate_generate_args(args):
    if not os.path.isdir(args["directory"]):
        raise Exception(f"{args['directory']} is not a directory.")
    if not os.path.isfile(args["source"]):
        raise Exception(f"{args['source']} is not a file.")
    valid_test_kinds = ["emit", "exitcode", "stdout"]
    if args["kind"] not in valid_test_kinds:
        raise Exception(f"{args['kind']} is not a generate-able test kind (one of: {valid_test_kinds})")

def validate_clean_args(args):
    if not os.path.isdir(args["directory"]):
        raise Exception(f"{args['directory']} is not a directory.")

def get_manifest(args):
    manifest_path = os.path.join(args["directory"], "manifest.json")
    if not os.path.isfile(manifest_path):
        raise Exception(f"No `manifest.json` in test directory.")
    with open(manifest_path, "r") as f:
        manifest = json.load(f)
    return manifest

def validate_manifest(manifest):
    def expect(key, type_):
        if key not in manifest or type(manifest[key]) != type_:
            raise Exception(f"Must have \"{key}\" defined in `manifest.json` ({type_} type).")
    expect("language-extension", str)
    expect("emit-extension", str)
    expect("compile-command", str)

def validate_metadata(manifest, metadata, test):
    def expect(key, type_):
        if key not in metadata or type(metadata[key]) != type_:
            raise Exception(f"Must have \"{key}\" defined in `{test}` metadata ({type_} type).")
    expect("test-kind", str)
    valid_test_kinds = ["emit", "exitcode", "stdout"]
    if metadata["test-kind"] not in valid_test_kinds:
        raise Exception(f"\"test-kind\" in `{test}` metadata must be one of {valid_test_kinds}.")
    if metadata["test-kind"] == "emit" and "compile-command-emit" not in manifest:
        raise Exception(f"(`{test}`) Cannot define an \"emit\"-type test without defining \"compile-command-emit\" in `manifest.json`")

def compare_text(expected, got, expected_filename, got_filename):
    assert('\r' not in expected)
    assert('\r' not in got)
    diff = difflib.context_diff(
        [f"{s}\n" for s in expected.split('\n')],
        [f"{s}\n" for s in got.split('\n')],
        fromfile=expected_filename,
        tofile=got_filename
    )
    return expected == got, ''.join(diff)

def run_test(args, manifest, path, depth):
    test_source_fullname = f"{path}.{manifest['language-extension']}"
    test_source_basename = f"{os.path.basename(path)}.{manifest['language-extension']}"

    expect_file_basename = f"{os.path.basename(path)}.expect"

    with open(path + ".expect", "r") as f:
        expect_source = f.read()

    # Split expect file into metadata and expected output
    delimiter = expect_source.find("---\n")
    expect_metadata = json.loads(expect_source[:delimiter])
    expected_output = expect_source[delimiter + 4:]

    validate_metadata(manifest, expect_metadata, expect_file_basename)

    verbose_information = []
    verbose_information.append(f"test-kind: {expect_metadata['test-kind']}")

    def happy():
        print_at_depth(depth, colored(f"* {test_source_basename}...", "green"))
        print_verbose(args, '\n'.join(verbose_information))
    def sad():
        print_at_depth(depth, colored(f"* {test_source_basename}!", "red"))
        print_verbose(args, '\n'.join(verbose_information))

    # Generate emitted code
    if expect_metadata["test-kind"] == "emit":
        compile_command = manifest["compile-command-emit"]
    else:
        compile_command = manifest["compile-command"]
    compile_command = compile_command.replace("$", test_source_fullname)
    verbose_information.append(f"compiling with: `{compile_command}`")

    compile_result = subprocess.run(compile_command, stdout = subprocess.PIPE, stderr = subprocess.PIPE, shell=True)

    if compile_result.returncode != 0:
        sad()
        print_at_depth(depth, colored(f"  Test failed to compile!", "red"))
        print(compile_result.stdout.decode('utf-8'))
        print(compile_result.stderr.decode('utf-8'))
        return

    # For emit tests, we stop here and compare
    if expect_metadata["test-kind"] == "emit":
        generated_source_basename = f"{os.path.basename(path)}.{manifest['emit-extension']}"
        generated_source_fullname = f"{path}.{manifest['emit-extension']}"
        with open(generated_source_fullname, "r") as f:
            generated_source = f.read()
        same, diff = compare_text(expected_output, generated_source, expect_file_basename, generated_source_basename)
        if same:
            happy()
            return
        else:
            sad()
            print_at_depth(depth, colored(f"  Test did not match expected output (emit)!"))
            print(diff)
            return

    # Otherwise, we run the executable
    executable_fullname = f"{path}.exe"
    executable_result = subprocess.run(executable_fullname, stdout = subprocess.PIPE, stderr = subprocess.PIPE, shell=True)

    if expect_metadata["test-kind"] == "exitcode":
        try:
            expected_exitcode = int(expected_output)
        except ValueError:
            raise Exception(f"Test type \"exitcode\" defined in `{expect_file_basename}` requires that the expected output be simply an integer.")
        if executable_result.returncode == expected_exitcode:
            happy()
            return
        else:
            sad()
            print_at_depth(depth, colored(f"  Test exited with code {executable_result.returncode}, but code {expected_exitcode} was expected."))
            return

    if expect_metadata["test-kind"] == "stdout":
        stdout = executable_result.stdout.replace(b'\r\n', b'\n').decode('utf-8')
        same, diff = compare_text(expected_output, stdout, expect_file_basename, "<stdout>")
        if same:
            happy()
            return
        else:
            sad()
            print_at_depth(depth, colored(f"  Test did not match expected output (stdout)!"))
            print(diff)
            return

    raise Exception("Unreachable")

def crawl_directory(args, manifest, directory, directory_callback, file_callback, depth = 0):
    directory_callback(directory, depth)

    subdirs = []
    source_files = []
    expect_files = []
    for entry in os.listdir(directory):
        path = os.path.join(directory, entry)
        if os.path.isdir(path):
            subdirs.append(path)
        elif os.path.isfile(path):
            (base, ext) = os.path.splitext(path)
            if ext == f".{manifest['language-extension']}":
                source_files.append(base)
            elif ext == ".expect":
                expect_files.append(base)

    for source_file in source_files:
        if source_file not in expect_files:
            print(colored("WARNING:", "yellow"), f"Source file {source_file}.{manifest['language-extension']} has no companion .expect file")
    for expect_file in expect_files:
        if expect_file not in source_files:
            print(colored("WARNING:", "yellow"), f"Expect file {expect_file}.expect has no companion source file")

    # First traverse every subdirectory
    subdirs.sort()
    for subdir in subdirs:
        crawl_directory(args, manifest, subdir, directory_callback, file_callback, depth = depth + 1)

    # Now execute on files in this directory
    good_files = list(set(source_files).intersection(set(expect_files)))
    for file in good_files:
        file_callback(file, depth)

def crawl_directory_and_run_tests(args, manifest):
    crawl_directory(
        args, manifest, args["directory"],
        lambda directory, depth: print_at_depth(depth, colored(f"[{os.path.basename(directory)}]", "blue")),
        lambda file, depth: run_test(args, manifest, file, depth = depth)
    )

def crawl_directory_and_clean(args, manifest):
    def clean(file_bare, _depth):
        generated_source_fullname = f"{file_bare}.{manifest['emit-extension']}"
        executable_fullname = f"{file_bare}.exe"
        try:
            print(f"Deleting {generated_source_fullname}")
            os.remove(generated_source_fullname)
        except FileNotFoundError:
            print(f"  Didn't exist!")
        try:
            print(f"Deleting {executable_fullname}")
            os.remove(executable_fullname)
        except FileNotFoundError:
            print(f"  Didn't exist!")
    crawl_directory(
        args, manifest, args["directory"],
        lambda _dir, _dep: (), clean
    )

def generate_expect_file_from_source(args, manifest):
    (source_bare, source_ext) = os.path.splitext(args["source"])
    if source_ext != f".{manifest['language-extension']}":
        raise Exception(f"Source file {args['source']} does not have the right extension.")

    def make(output):
        header = {
            "test-kind": args["kind"]
        }
        generated_expect_fullname = f"{source_bare}.expect"
        with open(generated_expect_fullname, "w") as f:
            json.dump(header, f, indent = 4)
            f.write('\n---\n')
            # Python adds \r to every \n automatically, so if we have \r's
            # coming from stdout or anywhere else, let's not double them up.
            f.write(''.join(output.split('\r')))

    verbose_information = []

    # Generate emitted code
    if args["kind"] == "emit":
        compile_command = manifest["compile-command-emit"]
    else:
        compile_command = manifest["compile-command"]
    compile_command = compile_command.replace("$", args["source"])
    verbose_information.append(f"compiling with: `{compile_command}`")

    compile_result = subprocess.run(compile_command, stdout = subprocess.PIPE, stderr = subprocess.PIPE, shell=True)

    if compile_result.returncode != 0:
        print(colored(f"Test failed to generate!", "red"))
        print('\n'.join(verbose_information))
        print(compile_result.stdout.decode('utf-8'))
        print(compile_result.stderr.decode('utf-8'))
        return

    # For generating emit tests, we stop here
    if args["kind"] == "emit":
        generated_source_fullname = f"{source_bare}.{manifest['emit-extension']}"
        with open(generated_source_fullname, "r") as f:
            generated_source = f.read()
        make(generated_source)
        return

    # Otherwise, we run the executable
    executable_fullname = f"{source_bare}.exe"
    executable_result = subprocess.run(executable_fullname, stdout = subprocess.PIPE, stderr = subprocess.PIPE, shell=True)

    if args["kind"] == "exitcode":
        make(f"{executable_result.returncode}\n")
        return

    if args["kind"] == "stdout":
        make(executable_result.stdout.decode('utf-8'))
        return

    raise Exception("Unreachable")


def main():
    parser = argparse.ArgumentParser(
        description = "Front-to-back testing framework for programming languages",
    )
    subparsers = parser.add_subparsers(dest='subparser', required=True)

    run_parser = subparsers.add_parser('run', help='Run tests from a directory')
    run_parser.add_argument("--directory", help="Directory containing tests", required=True)
    run_parser.add_argument("--verbose", help="Add debug information for lesen development", action='store_true')

    generate_parser = subparsers.add_parser('generate', help='Generate `.expect` files from source')
    generate_parser.add_argument("--directory", help="Directory containing tests", required=True)
    generate_parser.add_argument("--source", help="Test source file", required=True)
    generate_parser.add_argument("--kind", help="Which test kind to generate", required=True)
    generate_parser.add_argument("--verbose", help="Add debug information for lesen development", action='store_true')

    generate_parser = subparsers.add_parser('clean', help='Deletes all executables and emitted source files')
    generate_parser.add_argument("--directory", help="Directory containing tests", required=True)

    args = vars(parser.parse_args())

    if args['subparser'] == 'run':
        validate_run_args(args)

        manifest = get_manifest(args)
        validate_manifest(manifest)

        crawl_directory_and_run_tests(args, manifest)
        return

    if args['subparser'] == 'generate':
        validate_generate_args(args)

        manifest = get_manifest(args)
        validate_manifest(manifest)

        generate_expect_file_from_source(args, manifest)
        return

    if args['subparser'] == 'clean':
        validate_clean_args(args)

        manifest = get_manifest(args)
        validate_manifest(manifest)

        crawl_directory_and_clean(args, manifest)
        return

    raise Exception('Unreachable')

if __name__ == '__main__':
    main()
