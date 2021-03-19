namespace ANSI
def reset  : String := "\u0001B[0m"
def black  : String := "\u0001B[30m"
def red    : String := "\u0001B[31m"
def green  : String := "\u0001B[32m"
def yellow : String := "\u0001B[33m"
def blue   : String := "\u0001B[34m"
def purple : String := "\u0001B[35m"
def cyan   : String := "\u0001B[36m"
def white  : String := "\u0001B[37m"
namespace background
def black  : String := "\u0001B[40m"
def red    : String := "\u0001B[41m"
def green  : String := "\u0001B[42m"
def yellow : String := "\u0001B[43m"
def blue   : String := "\u0001B[44m"
def purple : String := "\u0001B[45m"
def cyan   : String := "\u0001B[46m"
def white  : String := "\u0001B[47m"
end background
end ANSI
