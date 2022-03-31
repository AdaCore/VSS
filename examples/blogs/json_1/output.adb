
with Interfaces;

package body Output is

   procedure Write
     (Writer  : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Item    : Messages.LSP_Position;
      Success : in out Boolean);

   procedure Write
     (Writer  : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Item    : Messages.LSP_Range;
      Success : in out Boolean);

   -----------
   -- Write --
   -----------

   procedure Write
     (Writer  : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Item    : Messages.LSP_Position;
      Success : in out Boolean) is
   begin
      Writer.Start_Object (Success);

      Writer.Key_Name ("line", Success);
      Writer.Integer_Value (Interfaces.Integer_64 (Item.Line), Success);

      Writer.Key_Name ("character", Success);
      Writer.Integer_Value (Interfaces.Integer_64 (Item.Character), Success);

      Writer.End_Object (Success);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (Writer  : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Item    : Messages.LSP_Range;
      Success : in out Boolean) is
   begin
      Writer.Start_Object (Success);

      Writer.Key_Name ("start", Success);
      Write (Writer, Item.Range_Start, Success);

      Writer.Key_Name ("end", Success);
      Write (Writer, Item.Range_End, Success);

      Writer.End_Object (Success);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (Writer  : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Item    : Messages.LSP_Text_Edit;
      Success : in out Boolean) is
   begin
      Writer.Start_Object (Success);

      Writer.Key_Name ("range", Success);
      Write (Writer, Item.Text_Range, Success);

      Writer.Key_Name ("newText", Success);
      Writer.String_Value (Item.New_Text, Success);

      Writer.End_Object (Success);
   end Write;

end Output;
