--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

separate (VSS.Command_Line)
package body Platform is

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Message
     (Message  : VSS.Strings.Virtual_String;
      Is_Error : Boolean) is
   begin
      if Is_Error then
         Put_Line_Error (Message);

      else
         Put_Line_Output (Message);
      end if;
   end Report_Message;

   --------------------
   -- Report_Message --
   --------------------

   procedure Report_Message
     (Message  : VSS.String_Vectors.Virtual_String_Vector;
      Is_Error : Boolean) is
   begin
      for Line of Message loop
         Report_Message (Line, Is_Error);
      end loop;
   end Report_Message;

end Platform;
