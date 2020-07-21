from gpr2.capi import LibGPR2


class ProjectView:
    def __init__(self, message):
        self.message = message
 
    @property
    def level(self):
        return self.message["level"]

    @property
    def message(self):
        return self.message["message"]

    @property
    def formatted_message(self):
        return self.message["formatted_message"]

    @property
    def filename(self):
        return self.message["filename"]

    @property
    def has_source_reference(self):
        return self.message["line"] is not None

    @property
    def line(self):
        return self.message["line"]

    @property
    def column(self):
        return self.message["column"]

    def __del__(self):
        pass
