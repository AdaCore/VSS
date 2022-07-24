--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Interfaces.C;

with VSS.Implementation.Windows.Kernel32;
with VSS.Implementation.Windows.String_Utilities;
with VSS.Implementation.Windows.User32;

separate (VSS.Command_Line)
package body Platform is

   function Use_Message_Box return Boolean;
   --  Return True then message box should be used instead of standard error
   --  stream to report errors.

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Error (Message : VSS.Strings.Virtual_String) is
      use type VSS.Implementation.Windows.UINT;

      Title  : VSS.Implementation.Windows.String_Utilities.char16_array_access;
      Text   : VSS.Implementation.Windows.String_Utilities.char16_array_access;
      Result : Interfaces.C.int with Unreferenced;

   begin
      if Use_Message_Box then
         Text :=
           VSS.Implementation.Windows.String_Utilities.To_New_Native_String
             (Message);
         Title :=
           VSS.Implementation.Windows.String_Utilities.To_New_Native_String
             (VSS.Application.Application_File);
         Result :=
           VSS.Implementation.Windows.User32.MessageBox
             (0,
              Text (Text'First)'Access,
              Title (Title'First)'Access,
              VSS.Implementation.Windows.User32.MB_OK
                or VSS.Implementation.Windows.User32.MB_TOPMOST
                or VSS.Implementation.Windows.User32.MB_SETFOREGROUND
                or VSS.Implementation.Windows.User32.MB_ICONERROR);
         VSS.Implementation.Windows.String_Utilities.Free (Text);
         VSS.Implementation.Windows.String_Utilities.Free (Title);

      else
         Output_Error (Message);
      end if;
   end Report_Error;

   ---------------------
   -- Use_Message_Box --
   ---------------------

   function Use_Message_Box return Boolean is
      use type VSS.Implementation.Windows.DWORD;
      use type VSS.Implementation.Windows.HANDLE;
      use type VSS.Implementation.Windows.HWND;

      Info : aliased VSS.Implementation.Windows.Kernel32.STARTUPINFO;

   begin
      if VSS.Implementation.Windows.Kernel32.GetConsoleWindow /= 0 then
         --  Application has associated console window.

         return False;
      end if;

      --  Initialize STARTUPINFO record

      Info.cb :=
        VSS.Implementation.Windows.Kernel32
          .STARTUPINFO'Max_Size_In_Storage_Elements;

      --  Obtain startup information

      VSS.Implementation.Windows.Kernel32.GetStartupInfo
        (Info'Unchecked_Access);

      if (Info.dwFlags
            and VSS.Implementation.Windows.Kernel32.STARTF_USESTDHANDLES)
        /= 0
      then
         --  Standard IO streams was specified for the application.

         return False;
      end if;

      return True;
   end Use_Message_Box;

end Platform;
