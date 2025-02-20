# Tips & Tricks


## Replace arbitrary line terminator by particular one

Sometimes source text might have different kinds of line terminator sequences
(VSS supports LF, CR, CRLF, NEL, VT, FF, LS, PS as line terminator sequences),
but application might expected to use only one of them. It can be done by
sequence of calls of `Split_Lines`.`Join_Lines` subprograms.


```ada
with VSS.Strings.Line_Iterators;
with VSS.String_Vectors;

function Normalize_Line_Terminators
  (Source : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String
is
   Terminators : constant VSS.Strings.Line_Terminator_Set :=
     (VSS.Strings.LF | VSS.Strings.CR | VSS.Strings.CRLF => True,
      others => False);
   --  Line terminator sequences that might be present in source text.
   
begin
   return
     Source.Split_Lines (Terminators).Join_Lines
       (VSS.Strings.LF, Source.At_Last_Line.Has_Line_Terminator);
   --  Result text use LF as line terminator sequence.
end Normalize_Line_Terminators;
```
