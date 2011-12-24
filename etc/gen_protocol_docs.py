import sys,os

def print_verbatim(s):
    print "\\begin{mylisting}"
    print "\\begin{verbatim}"
    print s
    print "\\end{verbatim}"
    print "\\end{mylisting}"

def print_bold(s):
    print "\\textbf{" + s + "}"

def print_nl():
    print "\\\\"

def print_vspace(height):
    print "\\vspace{" + height + "}"

def print_indent():
    print "\\indent"

class FileReader:

    def __init__(self, fin):
        self.fin = fin

    def lines(self):
        for line in self.fin:
            yield line
        yield None


def read_until(fin,stop,offset):
    result = ""
    line = next(fin)
    while line.find(stop) == -1:
        result = result + line[offset:]
        line = next(fin)
    return result.lstrip().rstrip()
    


class DataStructure:

    def __init__(self, offset):
        self.offset = offset
    
    def read(self, fin):
        off = self.offset + 2
        self.name = read_until(fin,"Summary:",off)
        self.summary = read_until(fin,"Structure:",off)
        self.structure = read_until(fin,"*/",off)

    def print_latex(self):
        print_bold(self.name)
        print_nl()
        print self.summary
        print_nl()
        print_verbatim(self.structure)
        print_vspace("5 mm")
        print "\n"

class RPCCall:

    def __init__(self, offset):
        self.offset = offset
    
    def read(self, fin):
        off = self.offset + 2
        self.name = read_until(fin,"Summary:",off)
        self.summary = read_until(fin,"Arguments:",off)
        self.arguments = read_until(fin,"Return:",off)
        self.return_struct = read_until(fin,"Example call:",off)
        self.example_call = read_until(fin,"Example return:",off)
        self.example_return = read_until(fin,"*/",off)

    def print_latex(self):
        print_bold(self.name)
        print_nl()
        print self.summary + "\\\\\\\\"

        print_bold("Arguments:")
        if self.arguments == "None":
            print " None\\\\\\\\"
        else:
            print_verbatim(self.arguments)

        print_bold("Return:")
        if self.return_struct == "None":
            print " None\\\\\\\\"
        else:
            print_verbatim(self.return_struct)

        print_bold("Example Call:")
        print_verbatim(self.example_call)

        print_bold("Example Return:")
        print_verbatim(self.example_return)
        print_vspace("5 mm")
        print "\n"


assert sys.argv[1] == "data" or sys.argv[1] == "rpc"

fin = (FileReader(open("../src/main/scala/org/ensime/protocol/SwankProtocol.scala")).lines())
line = next(fin)
while line:

    if sys.argv[1] == "data":
        i = line.find("Doc DataStructure:")
        if i > -1:
            handler = DataStructure(i)
            handler.read(fin)
            handler.print_latex()

    if sys.argv[1] == "rpc":
        i = line.find("Doc RPC:")
        if i > -1:
            handler = RPCCall(i)
            handler.read(fin)
            handler.print_latex()

    line = next(fin)


