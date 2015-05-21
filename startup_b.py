from subprocess import call
import os

call(["erl", "-make"])
call(["erl", "-sname", "b", "-config", "config/b.config", "-pa", "ebin/"])