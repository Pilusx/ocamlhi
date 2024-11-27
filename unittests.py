'''
Testing framework.
'''

import argparse
import re
import os
from multiprocessing import Process, Queue

OUT = os.path.join("tests", "out")

class Colors:
    # pylint: disable=too-few-public-methods
    '''
    Enables colorful output.
    '''
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

    @staticmethod
    def success(condition, string):
        '''
        Returns colorful output. Color depends on the condition.
        '''
        if condition:
            return Colors.OKGREEN + string + Colors.ENDC
        return Colors.FAIL + string + Colors.ENDC

class Statistics:
    '''
    Class containing aggregated statistics.
    '''

    categories = ["interpreted", "parsed", "timelimit", "accepted"]

    def __init__(self):
        self.total = {c: 0 for c in self.categories}
        self.score = self.total.copy()

    def print(self, message, name):
        '''
        Prints one of the statistics in the specified format.
        '''
        score = self.score[name]
        total = self.total[name]
        print(Colors.success(score == total, "{}: {}/{}".format(message, score, total)))

    def summarize(self):
        '''
        Prints all the statistics in the specified format.
        '''
        self.print("Parsed", "parsed")
        self.print("Interpreted", "interpreted")
        self.print("In time limit", "timelimit")
        self.print("Passed", "accepted")

class OcamlTest:
    '''
    A single test, usually a single interpretable file in one of the directories.
    '''
    def __init__(self, subdirectory, name):
        self.subdirectory = subdirectory
        self.file = name

    @staticmethod
    def default_result():
        '''
        Returns a default (usually) failing result.
        '''
        d_1 = {c: 0 for c in Statistics.categories}
        d_2 = {'issue': "Timeout. Process killed."}
        return {**d_1, **d_2}

    @staticmethod
    def file_contains_string(lines, string):
        '''
        Checks whether exactly one of lines contains occurence of @string.
        '''
        pattern = re.compile(string)
        match = [1 for line in lines if re.search(pattern, line)]
        return sum(match) == 1

    @staticmethod
    def file_contains_pattern(lines, regex):
        '''
        Finds all issues annotated in the given file.
        '''
        pattern = re.compile(regex)
        results = []
        for line in lines:
            match = re.findall(pattern, line)
            if match:
                results = results + match
        return results

    def run(self, queue):
        '''
        Executes one of the files and checks its output.
        '''
        result = self.default_result()
        src = os.path.join("tests", self.subdirectory, self.file)
        out = os.path.join(OUT, self.file.replace(".ml", ""))
        os.system("./ocamlhi -s {} > {} 2>&1".format(src, out))
        with open(src, "r") as file:
            lines = file.readlines()
            issues = self.file_contains_pattern(lines, r'-- \[ISSUE\] (.*)')
            result["issue"] = issues[0] + " (1 of {})".format(len(issues)) if issues else ""
        with open(out, "r") as file:
            lines = file.readlines()
            result["parsed"] = self.file_contains_string(lines, "Parse Successful!")
            result["interpreted"] = self.file_contains_string(lines, "Interpreter Successful")

        queue.put(result)

    def run_test(self):
        '''
        Runs the test with timeout.
        '''
        queue = Queue()
        process = Process(target=OcamlTest.run, args=(self, queue))
        process.start()
        process.join(timeout=1)

        if process.is_alive():
            process.terminate()
            os.system("pkill -9 ocamlhi")
            process.join()
            result = self.default_result()
        else:
            result = queue.get()
            result["timelimit"] = True

        return result

def over_directory(directory):
    '''
    Executes all possible tests in the given @directory.
    '''
    for file in sorted(os.listdir(os.path.join("tests", directory))):
        yield file, OcamlTest(directory, file).run_test()

class SuiteRunner:
    '''
    A test suite template. It is usually corresponding to one directory.
    '''
    fmt = "{:^3}|{:^3}|{:^8}| {:40} | {:}"

    def __init__(self):
        self.results = []
        self.stats = Statistics()

    def merge(self, judge, result):
        '''
        Updates the statistics based on the category.
        '''
        for category in self.stats.categories:
            if judge.countable(category):
                self.stats.total[category] += 1
                self.stats.score[category] += result[category]

    def run(self, judge, print_filter=None):
        '''
        Aggregates the results of tests from the judged directories.
        '''
        for directory in judge.directories:
            for (file, result) in over_directory(directory):
                result["accepted"] = judge.accept(result)
                self.merge(judge, result)
                entry = (directory, file, result)
                self.results.append(entry)

                name = file + ("" if result['timelimit'] else " (timed out) ")

                if (print_filter is None) or print_filter(result):
                    print(self.fmt.format(
                        "+" if result["parsed"] else "-",
                        "+" if result["interpreted"] else "-",
                        directory,
                        Colors.success(result["accepted"], name),
                        result["issue"]
                    ))

    def start(self):
        '''
        Header of the suite's log.
        '''
        print(self.fmt.format("Parsed", "Interpreted", "Suite", "Filename", "FIXME"))

    def end(self):
        '''
        Epilogue of the suite's log.
        '''
        self.stats.summarize()

class IJudge:
    '''
    The interface of evaluating tests.
    '''
    @staticmethod
    def directories():
        '''
        Returns the directories that contain test files.
        '''

    @staticmethod
    def accept(result):
        '''
        Returns True, when the test has been successful.
        '''

    @staticmethod
    def countable(category):
        '''
        Returns True, when the test should be included in statistics of this category.
        '''

class JudgeGood(IJudge):
    '''
    The way of evaluating 'good' tests.
    '''
    directories = ["good", "stdlib"]

    @staticmethod
    def accept(result):
        return result['parsed'] and result['interpreted'] and result['timelimit']

    @staticmethod
    def countable(_):
        return True

class JudgeBad(IJudge):
    '''
    The way of evaluating 'bad' tests.
    '''
    directories = ["bad"]
    @staticmethod
    def accept(result):
        return result['parsed'] and not result['interpreted'] and result['timelimit']

    @staticmethod
    def countable(category):
        return category in ['timelimit', 'accepted'] # ['parsed']?

if __name__ == '__main__':
    if not os.path.exists(OUT):
        os.mkdir(OUT)

    PARSER = argparse.ArgumentParser()
    PARSER.add_argument('--print_failed', action="store_true", help="show only failed tests")
    ARGS = PARSER.parse_args()
    KWARGS = {
        'print_filter': (lambda x: not x["accepted"]) if ARGS.print_failed else None
    }

    SUITES = SuiteRunner()
    SUITES.start()
    SUITES.run(JudgeGood, **KWARGS)
    SUITES.run(JudgeBad, **KWARGS)
    SUITES.end()
