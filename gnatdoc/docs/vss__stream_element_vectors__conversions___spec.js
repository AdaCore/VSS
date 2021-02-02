GNATdoc.Documentation = {
  "label": "VSS.Stream_Element_Vectors.Conversions",
  "qualifier": "",
  "summary": [
  ],
  "description": [
  ],
  "entities": [
    {
      "entities": [
        {
          "label": "To_Stream_Element_Vector",
          "qualifier": "",
          "line": 40,
          "column": 13,
          "src": "srcs/vss-stream_element_vectors-conversions.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "function"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "To_Stream_Element_Vector",
                      "href": "docs/vss__stream_element_vectors__conversions___spec.html#L40C13"
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
                      "text": "     "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Item",
                      "href": "docs/vss__stream_element_vectors__conversions___spec.html#L41C7"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Ada.Streams.Stream_Element_Array"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "return"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Stream_Element_Vector",
                      "href": "docs/vss__stream_element_vectors___spec.html#L127C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Convert Stream_Element_Array into Stream_Element_Buffer.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Item",
              "line": 41,
              "column": 7,
              "type": {
                "label": "Stream_Element_Array"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Unchecked_From_Unbounded_String",
          "qualifier": "",
          "line": 35,
          "column": 13,
          "src": "srcs/vss-stream_element_vectors-conversions.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "function"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Unchecked_From_Unbounded_String",
                      "href": "docs/vss__stream_element_vectors__conversions___spec.html#L35C13"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 36,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "     "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Item",
                      "href": "docs/vss__stream_element_vectors__conversions___spec.html#L36C7"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Ada.Strings.Unbounded.Unbounded_String"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 37,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "return"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Stream_Element_Vector",
                      "href": "docs/vss__stream_element_vectors___spec.html#L127C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Converts Unbounded_String into Stream_Element_Buffer without any checks.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Item",
              "line": 36,
              "column": 7,
              "type": {
                "label": "Unbounded_String"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Unchecked_To_String",
          "qualifier": "",
          "line": 30,
          "column": 13,
          "src": "srcs/vss-stream_element_vectors-conversions.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 30,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "function"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Unchecked_To_String",
                      "href": "docs/vss__stream_element_vectors__conversions___spec.html#L30C13"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 31,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "     "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Item",
                      "href": "docs/vss__stream_element_vectors__conversions___spec.html#L31C7"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Stream_Element_Vector",
                      "href": "docs/vss__stream_element_vectors___spec.html#L127C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "'"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Class"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "return"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "String"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Convert content of the buffer to standard string. It do binary\n"
                },
                {
                  "kind": "span",
                  "text": "conversion without any assumptions or checks.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Item",
              "line": 31,
              "column": 7,
              "type": {
                "label": "VSS.Stream_Element_Vectors.Stream_Element_Vector",
                "docHref": "docs/vss__stream_element_vectors___spec.html#L33C9"
              },
              "description": [
              ]
            }
          ]
        }
      ],
      "label": "Subprograms"
    }
  ]
};