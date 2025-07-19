import gdb
import gdb.printing
from gnatdbg.tagged import reinterpret_tagged

# Use the tag class if it is available.
if hasattr(gdb, "ValuePrinter"):
    base = gdb.ValuePrinter
else:
    base = object


class Virtual_String_Printer(base):
    def __init__(self, val):
        self._val = val

    def to_string(self):
        unit_type = gdb.lookup_type("vss.unicode.utf8_code_unit").pointer()
        storage = self._val["data"]["storage_address"].cast(unit_type)
        size = self._val["data"]["size"]
        if storage.address == 0:
            # "null" string
            return ""
        return storage.lazy_string(encoding="utf-8", length=size)

    def display_hint(self):
        return "string"


class VSSPrinter(gdb.printing.PrettyPrinter):
    """Pretty-print VSS strings."""

    def __init__(self):
        super().__init__("VSS")

    def __call__(self, val):
        if str(val.type) == "vss.strings.virtual_string":
            return Virtual_String_Printer(val)


gdb.printing.register_pretty_printer(None, VSSPrinter())
