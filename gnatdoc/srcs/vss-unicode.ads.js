GNATdoc.SourceFile = {
  "kind": "code",
  "children": [
    {
      "kind": "line",
      "number": 1,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "------------------------------------------------------------------------------"
        }
      ]
    },
    {
      "kind": "line",
      "number": 2,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--                        M A G I C   R U N T I M E                         --"
        }
      ]
    },
    {
      "kind": "line",
      "number": 3,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--                                                                          --"
        }
      ]
    },
    {
      "kind": "line",
      "number": 4,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--                       Copyright (C) 2020, AdaCore                        --"
        }
      ]
    },
    {
      "kind": "line",
      "number": 5,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--                                                                          --"
        }
      ]
    },
    {
      "kind": "line",
      "number": 6,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- This library is free software;  you can redistribute it and/or modify it --"
        }
      ]
    },
    {
      "kind": "line",
      "number": 7,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- under terms of the  GNU General Public License  as published by the Free --"
        }
      ]
    },
    {
      "kind": "line",
      "number": 8,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- Software  Foundation;  either version 3,  or (at your  option) any later --"
        }
      ]
    },
    {
      "kind": "line",
      "number": 9,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- version. This library is distributed in the hope that it will be useful, --"
        }
      ]
    },
    {
      "kind": "line",
      "number": 10,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --"
        }
      ]
    },
    {
      "kind": "line",
      "number": 11,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --"
        }
      ]
    },
    {
      "kind": "line",
      "number": 12,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--                                                                          --"
        }
      ]
    },
    {
      "kind": "line",
      "number": 13,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- As a special exception under Section 7 of GPL version 3, you are granted --"
        }
      ]
    },
    {
      "kind": "line",
      "number": 14,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- additional permissions described in the GCC Runtime Library Exception,   --"
        }
      ]
    },
    {
      "kind": "line",
      "number": 15,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- version 3.1, as published by the Free Software Foundation.               --"
        }
      ]
    },
    {
      "kind": "line",
      "number": 16,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--                                                                          --"
        }
      ]
    },
    {
      "kind": "line",
      "number": 17,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- You should have received a copy of the GNU General Public License and    --"
        }
      ]
    },
    {
      "kind": "line",
      "number": 18,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- a copy of the GCC Runtime Library Exception along with this program;     --"
        }
      ]
    },
    {
      "kind": "line",
      "number": 19,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --"
        }
      ]
    },
    {
      "kind": "line",
      "number": 20,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- <http://www.gnu.org/licenses/>.                                          --"
        }
      ]
    },
    {
      "kind": "line",
      "number": 21,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--                                                                          --"
        }
      ]
    },
    {
      "kind": "line",
      "number": 22,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "------------------------------------------------------------------------------"
        }
      ]
    },
    {
      "kind": "line",
      "number": 23,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  Utilities to handle strings as code units in UTF-family encodings."
        }
      ]
    },
    {
      "kind": "line",
      "number": 24,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--"
        }
      ]
    },
    {
      "kind": "line",
      "number": 25,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  This package is intended to be used by relatively low level code."
        }
      ]
    },
    {
      "kind": "line",
      "number": 26,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--"
        }
      ]
    },
    {
      "kind": "line",
      "number": 27,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  Opposite to general Ada convention to use '1'-based indexing of the"
        }
      ]
    },
    {
      "kind": "line",
      "number": 28,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  characters in the strings, here '0'-based indexing is used as more"
        }
      ]
    },
    {
      "kind": "line",
      "number": 29,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  appropriate for low level applications and interoparability."
        }
      ]
    },
    {
      "kind": "line",
      "number": 30,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 31,
      "children": [
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "with"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Interfaces"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 32,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 33,
      "children": [
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "package"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "VSS.Unicode",
          "href": "docs/vss__unicode___spec.html#L33C13"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "is"
        }
      ]
    },
    {
      "kind": "line",
      "number": 34,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 35,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "pragma"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Pure"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 36,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 37,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "type"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Code_Point",
          "href": "docs/vss__unicode___spec.html#L37C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "is"
        }
      ]
    },
    {
      "kind": "line",
      "number": 38,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "new"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Interfaces.Unsigned_32"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "range"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "16"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "#"
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "00_0000"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "#"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ".."
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "16"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "#"
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "10_FFFF"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "#"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 39,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 40,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "type"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "UTF8_Code_Unit",
          "href": "docs/vss__unicode___spec.html#L40C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "is"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "mod"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "2"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "**"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "8"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 41,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "type"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "UTF8_Code_Unit_Offset",
          "href": "docs/vss__unicode___spec.html#L41C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "is"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "new"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Interfaces.Integer_32"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 42,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "subtype"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "UTF8_Code_Unit_Count",
          "href": "docs/vss__unicode___spec.html#L42C12"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "is"
        }
      ]
    },
    {
      "kind": "line",
      "number": 43,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "UTF8_Code_Unit_Offset",
          "href": "docs/vss__unicode___spec.html#L41C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "range"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "0"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ".."
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "UTF8_Code_Unit_Offset",
          "href": "docs/vss__unicode___spec.html#L41C9"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "'"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Last"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 44,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "subtype"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "UTF8_Code_Unit_Index",
          "href": "docs/vss__unicode___spec.html#L44C12"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "is"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "UTF8_Code_Unit_Count",
          "href": "docs/vss__unicode___spec.html#L42C12"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 45,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 46,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "type"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "UTF16_Code_Unit",
          "href": "docs/vss__unicode___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "is"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "mod"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "2"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "**"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "16"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 47,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "type"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "UTF16_Code_Unit_Offset",
          "href": "docs/vss__unicode___spec.html#L47C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "is"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "new"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Interfaces.Integer_32"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 48,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "subtype"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "UTF16_Code_Unit_Count",
          "href": "docs/vss__unicode___spec.html#L48C12"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "is"
        }
      ]
    },
    {
      "kind": "line",
      "number": 49,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "UTF16_Code_Unit_Offset",
          "href": "docs/vss__unicode___spec.html#L47C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "range"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "0"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ".."
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "UTF16_Code_Unit_Offset",
          "href": "docs/vss__unicode___spec.html#L47C9"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "'"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Last"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 50,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "subtype"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "UTF16_Code_Unit_Index",
          "href": "docs/vss__unicode___spec.html#L50C12"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "is"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "UTF16_Code_Unit_Count",
          "href": "docs/vss__unicode___spec.html#L48C12"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 51,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 52,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "type"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "UTF32_Code_Unit",
          "href": "docs/vss__unicode___spec.html#L52C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "is"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "mod"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "2"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "**"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "32"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- range 0 .. 16#10_FFFF#;"
        }
      ]
    },
    {
      "kind": "line",
      "number": 53,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "type"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "UTF32_Code_Unit_Offset",
          "href": "docs/vss__unicode___spec.html#L53C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "is"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "new"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Interfaces.Integer_32"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 54,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "subtype"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "UTF32_Code_Unit_Count",
          "href": "docs/vss__unicode___spec.html#L54C12"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "is"
        }
      ]
    },
    {
      "kind": "line",
      "number": 55,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "UTF32_Code_Unit_Offset",
          "href": "docs/vss__unicode___spec.html#L53C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "range"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "0"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ".."
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "UTF32_Code_Unit_Offset",
          "href": "docs/vss__unicode___spec.html#L53C9"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "'"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Last"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 56,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "subtype"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "UTF32_Code_Unit_Index",
          "href": "docs/vss__unicode___spec.html#L56C12"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "is"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "UTF32_Code_Unit_Count",
          "href": "docs/vss__unicode___spec.html#L54C12"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 57,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 58,
      "children": [
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "end"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "VSS.Unicode"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    }
  ],
  "label": "vss-unicode.ads"
};