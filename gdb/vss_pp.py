
def decode_utf8(val):
    result = ''
    i = 0

    while i < val["size"]:
        if val["storage"][i] <= 0x7F:
            c = val["storage"][i]
            i = i + 1

        elif val["storage"][i] <= 0xDF:
            u1 = (val["storage"][i] & 0x1F) << 6
            u2 = val["storage"][i + 1] & 0x3F
            c = u1 | u2
            i = i + 2

        elif val["storage"][i] <= 0xEF:
            u1 = (val["storage"][i] & 0x1F) << 12
            u2 = (val["storage"][i + 1] & 0x3F) << 6
            u3 = val["storage"][i + 2] & 0x3F
            c = u1 | u2 | u3
            i = i + 3

        elif val["storage"][i] <= 0xF4:
            u1 = (val["storage"][i] & 0x1F) << 18
            u2 = (val["storage"][i + 1] & 0x3F) << 12
            u3 = (val["storage"][i + 2] & 0x3F) << 6
            u4 = val["storage"][i + 3] & 0x3F
            c = u1 | u2 | u3 | u4
            i = i + 4

        result += chr(c)

    return result

def utf8_pp(val):
    data_type = gdb.lookup_type("vss.implementation.utf8_string_handlers.utf8_string_data").pointer()
    data = val.cast(data_type).dereference()

    return decode_utf8(data)

def utf8ip_pp(val):
    data_type = gdb.lookup_type("vss.implementation.utf8_string_handlers.utf8_in_place_data")
    data = val.cast(data_type)

    return decode_utf8(data)

class Virtual_String_Printer:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        data = self.val["data"]

        if data["in_place"]:
            return utf8ip_pp(data["storage"])

        else:
            if data["handler"] == 0:
                return None

            else:
                if data["handler"] == gdb.parse_and_eval("vss.implementation.utf8_string_handlers.global_utf8_string_handler'address"):
                    return utf8_pp(data["pointer"])
        return None

#    def display_hint(self):
#        return 'string'

def vss_pp_func(val):
    if str(val.type) == 'vss.strings.virtual_string':
        return Virtual_String_Printer(val)

gdb.pretty_printers.append(vss_pp_func)
