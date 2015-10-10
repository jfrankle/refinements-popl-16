#!/usr/bin/python

from __future__ import print_function

import difflib
import os
from os.path import splitext, join
import subprocess
import sys

TEST_EXT = '.ml'
FLAGS = ['--time data --enum_strategy raw --input']

def find_tests(root):
    tests = []
    for path, dirs, files in os.walk(root):
        files = [(f[0], f[1]) for f in [splitext(f) for f in files]]
        tests.extend([(path, f[0]) for f in files if f[1] == TEST_EXT])
    return tests

def run_test(prog, path):
    output_string = subprocess.check_output([prog] + FLAGS + [path], stderr=subprocess.STDOUT)
    return output_string

def check_test(prog, path, base):
    test_path     = join(path, base + TEST_EXT)
    try:
        output = run_test(prog, test_path)
    except subprocess.CalledProcessError as err:
        return
    sys.stdout.write(output)

def check_tests(prog, root):
    for path, base in sorted(find_tests(root)):
        check_test(prog, path, base)

def print_usage(args):
    print("Usage: {0} <prog> <test|testdir>".format(args[0]))

def main(args):
    if len(args) == 3:
        prog = args[1]
        path = args[2]
        if not os.path.exists(prog):
            print_usage(args)
        elif os.path.exists(path) and os.path.isdir(path):
            check_tests(prog, path)
        else:
            path, filename = os.path.split(path)
            base, ext = splitext(filename)
            if ext != TEST_EXT:
                print_usage(args)
            else:
                check_test(prog, path, base)
    else:
        print_usage(args)

if __name__ == '__main__':
    main(sys.argv)
