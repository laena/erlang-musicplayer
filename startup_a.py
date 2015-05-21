from subprocess import call
import os

call(["erl", "-make"])
call(["erl", "-sname", "a", "-config", "config/a.config", "-pa", "ebin/"])