------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.C;

with VSS.Implementation.Environment_Utilities;
with VSS.Implementation.Windows.String_Utilities;
with VSS.Implementation.Windows.Advapi32;
with VSS.Implementation.Windows.Kernel32;
with VSS.Implementation.Windows.Userenv;

package body VSS.Standard_Paths is

   -----------------------
   -- Writable_Location --
   -----------------------

   function Writable_Location
     (Location : Standard_Location) return VSS.Strings.Virtual_String
   is
      pragma Unreferenced (Location);

      use type VSS.Implementation.Windows.BOOL;
      use type VSS.Implementation.Windows.DWORD;

      Result         : VSS.Strings.Virtual_String;

      Process_Handle : VSS.Implementation.Windows.HANDLE;
      Token_Handle   : VSS.Implementation.Windows.HANDLE;
      Success        : VSS.Implementation.Windows.BOOL;
      Buffer_Size    : VSS.Implementation.Windows.DWORD := 0;

   begin
      --  Attempt to obtain user profile directory via WinAPI call.

      Process_Handle := VSS.Implementation.Windows.Kernel32.GetCurrentProcess;
      --  GetCurrentProcess returns pseudo handle, it not need to be closed
      --  after use.

      Success :=
        VSS.Implementation.Windows.Advapi32.OpenProcessToken
          (Process_Handle,
           VSS.Implementation.Windows.TOKEN_QUERY,
           Token_Handle);

      if Success /= VSS.Implementation.Windows.FALSE then
         Success :=
           VSS.Implementation.Windows.Userenv.GetUserProfileDirectory
             (Token_Handle, null, Buffer_Size);
         --  Request minimum buffer size, operation fails, however, minimum
         --  buffer size is written.

         if Success = VSS.Implementation.Windows.FALSE
           and VSS.Implementation.Windows.Kernel32.GetLastError
                 = VSS.Implementation.Windows.ERROR_INSUFFICIENT_BUFFER
           and Buffer_Size /= 0
         then
            declare
               C_Buffer : Interfaces.C.char16_array
                           (0 .. Interfaces.C.size_t (Buffer_Size));

            begin
               Success :=
                 VSS.Implementation.Windows.Userenv.GetUserProfileDirectory
                   (Token_Handle,
                    C_Buffer (C_Buffer'First)'Unchecked_Access,
                    Buffer_Size);

               if Success /= VSS.Implementation.Windows.FALSE then
                  Result :=
                    VSS.Implementation.Windows.String_Utilities
                      .From_Native_String (C_Buffer);
               end if;
            end;
         end if;

         Success :=
           VSS.Implementation.Windows.Kernel32.CloseHandle (Token_Handle);
         pragma Assert (Success /= VSS.Implementation.Windows.FALSE);
      end if;

      --  XXX Checks that directory exists and writable are not implemented

      if Result.Is_Empty then
         --  Construct value using environment variables.

         Result :=
           VSS.Implementation.Environment_Utilities.Get_Env ("USERPROFILE");
      end if;

      if Result.Is_Empty then
         Result :=
           VSS.Implementation.Environment_Utilities.Get_Env ("HOMEDRIVE");
         Result.Append
           (VSS.Implementation.Environment_Utilities.Get_Env ("HOMEPATH"));
      end if;

      if Result.Is_Empty then
         Result :=
           VSS.Implementation.Environment_Utilities.Get_Env ("HOME");
      end if;

      if Result.Is_Empty then
         Result :=
           VSS.Implementation.Environment_Utilities.Get_Env ("SystemDrive");
         Result.Append ('/');
      end if;

      return Result;
   end Writable_Location;

end VSS.Standard_Paths;
