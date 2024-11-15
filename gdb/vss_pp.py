import gdb
import gdb.printing
from gnatdbg.tagged import reinterpret_tagged

# Use the tag class if it is available.
if hasattr(gdb, "ValuePrinter"):
    base = gdb.ValuePrinter
else:
    base = object


def decode_utf8(bytes, size):
    result = ""
    i = 0

    while i < size:
        if bytes[i] <= 0x7F:
            c = bytes[i]
            i = i + 1

        elif bytes[i] <= 0xDF:
            u1 = (bytes[i] & 0x1F) << 6
            u2 = bytes[i + 1] & 0x3F
            c = u1 | u2
            i = i + 2

        elif bytes[i] <= 0xEF:
            u1 = (bytes[i] & 0x1F) << 12
            u2 = (bytes[i + 1] & 0x3F) << 6
            u3 = bytes[i + 2] & 0x3F
            c = u1 | u2 | u3
            i = i + 3

        elif bytes[i] <= 0xF4:
            u1 = (bytes[i] & 0x1F) << 18
            u2 = (bytes[i + 1] & 0x3F) << 12
            u3 = (bytes[i + 2] & 0x3F) << 6
            u4 = bytes[i + 3] & 0x3F
            c = u1 | u2 | u3 | u4
            i = i + 4

        result += chr(c)

    return result


class Virtual_String_Printer(base):
    def __init__(self, val):
        self._val = val

    def to_string(self):
        text_type = gdb.lookup_type(
            "vss.implementation.text_handlers.abstract_text_handler"
        )
        utf8static_type = gdb.lookup_type(
            "vss.implementation.text_handlers.utf8.static.static_utf8_handler"
        )
        utf8dynamic_type = gdb.lookup_type(
            "vss.implementation.text_handlers.utf8.dynamic.dynamic_utf8_handler"
        )

        storage = self._val["data"]["storage"]

        if all(byte == 0 for byte in storage.bytes):
            # "null" string

            return None

        text = reinterpret_tagged(storage.cast(text_type))

        if text.type == utf8static_type:
            # GDB is unable to resolve "storage" component of the record,
            # so skip first implicit component of System.Address and decode.
            return decode_utf8(
                text.bytes[gdb.lookup_type("system.address").sizeof :], text["size"]
            )

        elif text.type == utf8dynamic_type:
            data = text["pointer"].dereference()
            return decode_utf8(data["storage"].bytes, data["size"])

        else:
            print("<UNKNOWN TYPE>")

        return None


#    def display_hint(self):
#        return "string"


class VSSPrinter(gdb.printing.PrettyPrinter):
    """Pretty-print VSS strings."""

    def __init__(self):
        super().__init__("VSS")

    def __call__(self, val):
        if str(val.type) == "vss.strings.virtual_string":
            return Virtual_String_Printer(val)


gdb.printing.register_pretty_printer(None, VSSPrinter())
