
with Ada.Wide_Wide_Text_IO;

with VSS.JSON.Push_Writers;
with VSS.Text_Streams.Memory_UTF8_Output;

with Blog_Utilities;
with Messages;
with Output;

procedure Write_Message is

   Stream  :
     aliased VSS.Text_Streams.Memory_UTF8_Output.Memory_UTF8_Output_Stream;
   Writer  : VSS.JSON.Push_Writers.JSON_Simple_Push_Writer;
   Message : Messages.LSP_Text_Edit :=
     (Text_Range => ((5, 23), (6, 0)),
      New_Text   => "text to insert");
   Success : Boolean := True;

begin
   Writer.Set_Stream (Stream'Unchecked_Access);

   Writer.Start_Document (Success);
   Output.Write (Writer, Message, Success);
   Writer.End_Document (Success);

   Ada.Wide_Wide_Text_IO.Put_Line (Blog_Utilities.Decode (Stream.Buffer));
end Write_Message;
