
with VSS.Strings;

package body Input is

   use type VSS.JSON.Pull_Readers.JSON_Event_Kind;
   use type VSS.Strings.Virtual_String;

   procedure Read
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Item    : out Messages.LSP_Position;
      Success : in out Boolean);

   procedure Read
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Item    : out Messages.LSP_Range;
      Success : in out Boolean);

   ----------
   -- Read --
   ----------

   procedure Read
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Item    : out Messages.LSP_Position;
      Success : in out Boolean) is
   begin
      while Success loop
         case Reader.Read_Next is
            when VSS.JSON.Pull_Readers.Key_Name =>
               if Reader.Key_Name = "line" then
                  case Reader.Read_Next is
                     when VSS.JSON.Pull_Readers.Number_Value =>
                        Item.Line :=
                          Natural (VSS.JSON.As_Integer (Reader.Number_Value));

                     when others =>
                        Success := False;
                  end case;

               elsif Reader.Key_Name = "character" then
                  case Reader.Read_Next is
                     when VSS.JSON.Pull_Readers.Number_Value =>
                        Item.Character :=
                          Natural (VSS.JSON.As_Integer (Reader.Number_Value));

                     when others =>
                        Success := False;
                  end case;

               else
                  Success := False;
               end if;

            when VSS.JSON.Pull_Readers.Start_Object =>
               null;

            when VSS.JSON.Pull_Readers.End_Object =>
               exit;

            when others =>
               Success := False;
         end case;
      end loop;
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Item    : out Messages.LSP_Range;
      Success : in out Boolean) is
   begin
      while Success loop
         case Reader.Read_Next is
            when VSS.JSON.Pull_Readers.Key_Name =>
               if Reader.Key_Name = "start" then
                  Read (Reader, Item.Range_Start, Success);

               elsif Reader.Key_Name = "end" then
                  Read (Reader, Item.Range_End, Success);

               else
                  Success := False;
               end if;

            when VSS.JSON.Pull_Readers.Start_Object =>
               null;

            when VSS.JSON.Pull_Readers.End_Object =>
               exit;

            when others =>
               Success := False;
         end case;
      end loop;
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Item    : out Messages.LSP_Text_Edit;
      Success : in out Boolean) is
   begin
      while Success loop
         case Reader.Read_Next is
            when VSS.JSON.Pull_Readers.Key_Name =>
               if Reader.Key_Name = "range" then
                  Read (Reader, Item.Text_Range, Success);

               elsif Reader.Key_Name = "newText" then
                  case Reader.Read_Next is
                     when VSS.JSON.Pull_Readers.String_Value =>
                        Item.New_Text := Reader.String_Value;

                     when others =>
                        Success := False;
                  end case;

               else
                  Success := False;
               end if;

            when VSS.JSON.Pull_Readers.Start_Object =>
               null;

            when VSS.JSON.Pull_Readers.End_Object =>
               exit;

            when others =>
               Success := False;
         end case;
      end loop;
   end Read;

end Input;
