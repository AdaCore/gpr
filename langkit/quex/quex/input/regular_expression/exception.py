class RegularExpressionException(Exception):
    def _get_message(self): return self._message
    def _set_message(self, message): self._message

    message = property(_get_message, _set_message)

    def __init__(self, InfoStr):
        self._message = InfoStr

    def __repr__(self):
        txt = "QuexException:\n"
        return txt + self._message

