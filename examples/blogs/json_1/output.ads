
with VSS.JSON.Content_Handlers;

with Messages;

package Output is

   procedure Write
     (Writer  : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Item    : Messages.LSP_Text_Edit;
      Success : in out Boolean);

end Output;
