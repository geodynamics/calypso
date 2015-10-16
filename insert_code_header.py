#! /usr/bin/env python3

import argparse
import os
import tempfile


def line_comment(header, token):
    return token + (os.linesep + token).join(
        header.strip().splitlines()) + os.linesep


def block_comment(header, start_token, end_token):
    return os.linesep.join(
        [start_token, header.strip(), end_token]) + os.linesep


def check_header_for_block_tokens(header, start_token, end_token):
    if end_token in header:
        raise ValueError(
            "Header string contains a token that end block comments.")


def interactive_interface(header, file):
    def print_help():
        print("Possible Input:")
        print("  ?,h,help    - Display this help.")
        print("  q,quit      - Quit this program.")
        print("  y,yes       - Remove displayed lines and add header to file.")
        print("  n,no        - Add header to file without removing any lines.")
        print("  %N          - Remove %N lines from file and add header.")
        print("  s,show %N   - Show %N lines from the current file and\n"
              "                 reprompt for input.")
        print("  a,all y,yes - Remove displayed lines from this file and\n"
              "                 the same number of lines from all remaining\n"
              "                 files while adding the header.")
        print("  a,all n,no  - Add header to this file and all remaining\n"
              "                 files without removing any lines.")
        print("  a,all %N    - Remove %N line from this file and all\n"
              "                 remaining files while adding the header.")

    num_lines = len(header.splitlines())

    while True:
        print(file.name + ":")

        for n in range(num_lines):
            line = file.readline()
            if line:
                print(("{:0" + str(len(str(num_lines))) + "} ").format(n + 1)
                      + line.strip("\n"))
            else:
                print()
                print("EOF")
                num_lines = n
                break
        else:
            if file.readline():
                print("...")

        file.seek(0)

        choice = input("-> ").lower().split()

        if len(choice) == 1:
            if choice[0] == "y" or choice[0] == "yes":
                pass
            elif choice[0] == "n" or choice[0] == "no":
                num_lines = 0
            elif choice[0] == "?" or choice[0] == "h" or choice[0] == "help":
                print_help()
                continue
            elif choice[0] == "q" or choice[0] == "quit":
                exit(0)
            else:
                try:
                    num_lines = int(choice[0])
                except ValueError:
                    print("ERROR: unrecognized input.")
                    print_help()
                    continue

            for x in range(num_lines):
                file.readline()
            return None

        elif len(choice) == 2:
            if choice[0] == "a" or choice[0] == "all":
                if choice[1] == "y" or choice[1] == "yes":
                    pass
                elif choice[1] == "n" or choice[1] == "no":
                    num_lines = 0
                else:
                    try:
                        num_lines = int(choice[1])
                    except ValueError:
                        print("ERROR: unrecognized input.")
                        print_help()
                        continue

                for x in range(num_lines):
                    file.readline()
                return num_lines

            elif choice[0] == "s" or choice[0] == "show":
                try:
                    num_lines = int(choice[1])
                except ValueError:
                    print("ERROR: unrecognized input.")
                    print_help()
                    continue


def create_main_parser():
    parser = argparse.ArgumentParser(
        description=("Insert header text taken from a file into one or more"
                     " files."))

    parser.add_argument(
        type=argparse.FileType("rt"),
        metavar="header-file",
        dest="header_file",
        help=("Path to header text file. - means read from stdin."),
    )

    previous_header_group = parser.add_argument_group(
        title="header replacement",
        description=("Choose if a header should replace lines at the top of"
                     " each file. Defaults to no-removal.")
    ).add_mutually_exclusive_group()

    previous_header_group.add_argument(
        "-d",
        "--no-removal",
        action="store_true",
        dest="no_removal",
        help=("Don't remove any lines when adding the header."),
    )
    previous_header_group.add_argument(
        "-r",
        "--replace-lines",
        metavar="N",
        type=int,
        dest="num_lines",
        help=("Remove the first N lines in each file when adding the header."),
    )
    previous_header_group.add_argument(
        "-o",
        "--overwrite",
        action="store_true",
        dest="overwrite",
        help=("Remove an amount of lines equal to the amount of lines in the"
              " provided header."),
    )
    previous_header_group.add_argument(
        "-i",
        "--interactive",
        action="store_true",
        dest="interactive_removal",
        help=("For each file, display the first few lines and ask how many"
              " lines should be removed when adding the header."),
    )

    code_comment_group = parser.add_argument_group(
        title="code comment formatting",
        description=("Provides very basic support for formatting a header as"
                     " either single line comments or a block comment."
                     " Defaults to no-commenting."),
    ).add_mutually_exclusive_group()

    code_comment_group.add_argument(
        "-n",
        "--no-commenting",
        action="store_true",
        dest="no_commenting",
        help=("Insert the header as is. This choice is for headers that are"
              " already formatted for insertion into the provided files."),
    )

    code_comment_group.add_argument(
        "-l",
        "--line-comments",
        nargs=1,
        metavar="SYMBOL",
        dest="line_token",
        help=("Insert the header as a series of single line comments. SYMBOL"
              " is the string token used to start a single line comment."),
    )

    code_comment_group.add_argument(
        "-b",
        "--block-comment",
        nargs=2,
        metavar=("START-SYMBOL", "END-SYMBOL"),
        dest="block_tokens",
        help=("Insert the header as a single block comment. START-SYMBOL is"
              " the string token used to start a block comment. END-SYMBOL is"
              " the token used to end a block comment."),
    )

    parser.add_argument(
        "-f",
        "--force-block-comment",
        action="store_true",
        dest="ignore_block_warning",
        help=("Ignore warning when formatting a block comment about the header"
              " text containing an END-SYMBOL."),
    )

    parser.add_argument(
        nargs="+",
        metavar="file",
        dest="files",
        help=("Files to be modified by adding the header. Be sure not to mix"
              " file types if using the code comment formatting."),
    )

    return parser


def main(args):
    with args.header_file as header_file:
        header = ""
        for line in header_file:
            header += line

    if args.block_tokens:
        try:
            check_header_for_block_tokens(header, *args.block_tokens)
        except ValueError:
            if not args.ignore_block_warning:
                print(("ERROR: Header text contains the token, {}, for ending"
                       " a block comment. Please use the --force-block-comment"
                       " flag to ignore this error.").format(
                    args.block_tokens[1]))
                exit(1)

    for filename in args.files:
        with tempfile.NamedTemporaryFile('wt', delete=False) as tmp_file:
            with open(filename, "rt") as file:
                if args.line_token:
                    header = line_comment(header, *args.line_token)
                elif args.block_tokens:
                    header = block_comment(header, *args.block_tokens)
                else:  # args.no_commenting
                    pass

                if args.overwrite:
                    args.num_lines = len(header.splitlines())
                    args.overwrite = None

                if args.num_lines:
                    for x in range(args.num_lines):
                        file.readline()
                elif args.interactive_removal:
                    num_lines = interactive_interface(header, file)
                    if num_lines is not None:
                        args.num_lines = num_lines
                        args.interactive_removal = None
                else:  # args.no_removal
                    pass

                tmp_file.write(header)

                for line in file:
                    tmp_file.write(line)

        os.rename(tmp_file.name, file.name)


if __name__ == "__main__":
    main(create_main_parser().parse_args())
