import sys

def do(ARGV):
    """Allow to check whether the exception handlers are all in place.
    """
    if       len(ARGV) != 3: return False
    elif     ARGV[1] != "<<TEST:Exceptions/function>>" \
         and ARGV[1] != "<<TEST:Exceptions/on-import>>":   return False
   
    if len(ARGV) < 3: return False
    exception = ARGV[2]
    if   exception == "KeyboardInterrupt": raise KeyboardInterrupt()
    elif exception == "AssertionError":    raise AssertionError()
    elif exception == "Exception":         raise Exception()
    # If we did not raise an exception here, we didn't do anything
    print "No exception was triggered."
    return False

def do_on_import(ARGV):
    # Double check whether exception handlers are in place on module-import
    if len(ARGV) == 3 and ARGV[1] == "<<TEST:Exceptions/on-import>>":
        do(ARGV)

def handle(TheException):

    if "--debug-exception" in sys.argv:
        import traceback 
        print traceback.format_exc()
        return

    if   isinstance(TheException, AssertionError):
        print "Assertion error -- please report a bug under\n" + \
              " https://sourceforge.net/tracker/?group_id=168259&atid=846112"

    elif isinstance(TheException, KeyboardInterrupt): 
        print
        print "#\n# Keyboard Interrupt -- Processing unfinished.\n#"

    elif isinstance(TheException, SystemExit):
        pass

    elif isinstance(TheException, Exception):
        print "Exception occurred -- please, report a bug under\n" + \
              " https://sourceforge.net/tracker/?group_id=168259&atid=846112"
    
    # Indicate Error For 'make'-procedures, etc.
    sys.exit(-1)

