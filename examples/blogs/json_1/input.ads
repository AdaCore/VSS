
with VSS.JSON.Pull_Readers;

with Messages;

package Input is

   procedure Read
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Item    : out Messages.LSP_Text_Edit;
      Success : in out Boolean);

end Input;
