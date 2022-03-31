
with VSS.Stream_Element_Vectors;

package Blog_Utilities is

   function Decode
     (Buffer : VSS.Stream_Element_Vectors.Stream_Element_Vector)
      return Wide_Wide_String;

   function Encode
     (Text : Wide_Wide_String)
      return VSS.Stream_Element_Vectors.Stream_Element_Vector;

end Blog_Utilities;
