from gpr2.capi import LibGPR2


class ProjectView:
    def __init__(self, id: str):
        self.id = id
        self.info = LibGPR2.gpr2_prj_view_information({"view_id": self.id})

    @property
    def name(self):
        return self.info["name"]

    @property
    def path_name(self):
        return self.info["path_name"]

    def __del__(self):
        pass
