
with Ada.Wide_Wide_Text_IO;

with VSS.JSON.Pull_Readers.Simple;
with VSS.Strings.Conversions;
with VSS.Text_Streams.Memory_UTF8_Input;

with Blog_Utilities;
with Messages;
with Input;

procedure Read_Message is
   use type VSS.JSON.Pull_Readers.JSON_Event_Kind;

   Text    : constant Wide_Wide_String :=
     "{""range"":{""start"":{""line"":5,""character"":23},"
     & """end"":{""line"":6,""character"":0}},""newText"":""text to insert""}";

   Stream  :
     aliased VSS.Text_Streams.Memory_UTF8_Input.Memory_UTF8_Input_Stream;
   Reader  : VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
   Message : Messages.LSP_Text_Edit;
   Success : Boolean := True;

begin
   Stream.Set_Data (Blog_Utilities.Encode (Text));
   Reader.Set_Stream (Stream'Unchecked_Access);

   if Reader.Read_Next /= VSS.JSON.Pull_Readers.Start_Document then
      Success := False;
   end if;

   Input.Read (Reader, Message, Success);

   if Success
     and then Reader.Read_Next /= VSS.JSON.Pull_Readers.End_Document
   then
      Success := False;
   end if;

   if Success then
      Ada.Wide_Wide_Text_IO.Put_Line
        ("Start line:     "
         & Natural'Wide_Wide_Image (Message.Text_Range.Range_Start.Line));
      Ada.Wide_Wide_Text_IO.Put_Line
        ("Start character:"
         & Natural'Wide_Wide_Image (Message.Text_Range.Range_Start.Character));
      Ada.Wide_Wide_Text_IO.Put_Line
        ("End line:       "
         & Natural'Wide_Wide_Image (Message.Text_Range.Range_End.Line));
      Ada.Wide_Wide_Text_IO.Put_Line
        ("End character:  "
         & Natural'Wide_Wide_Image (Message.Text_Range.Range_End.Character));
      Ada.Wide_Wide_Text_IO.Put_Line
        ("New text:        """
         & VSS.Strings.Conversions.To_Wide_Wide_String (Message.New_Text) & '"');
   end if;
end Read_Message;
