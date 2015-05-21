from subprocess import call
import os

call(["erl", "-make"])
os.chdir("./ebin")