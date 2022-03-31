
with VSS.Strings;

package Messages is

   type LSP_Position is record
      Line      : Natural;
      Character : Natural;
   end record;

   type LSP_Range is record
      Range_Start : LSP_Position;
      Range_End   : LSP_Position;
   end record;

   type LSP_Text_Edit is record
      Text_Range : LSP_Range;
      New_Text   : VSS.Strings.Virtual_String;
   end record;

end Messages;
