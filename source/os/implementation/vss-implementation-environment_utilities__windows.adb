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

with VSS.Implementation.Windows.Kernel32;
with VSS.Implementation.Windows.String_Utilities;

package body VSS.Implementation.Environment_Utilities is

   -------------
   -- Get_Env --
   -------------

   function Get_Env
     (Name    : VSS.Strings.Virtual_String;
      Default : VSS.Strings.Virtual_String := VSS.Strings.Empty_Virtual_String)
      return VSS.Strings.Virtual_String
   is
      use type VSS.Implementation.Windows.DWORD;
      use type VSS.Implementation.Windows.String_Utilities.char16_array_access;

      Result   : VSS.Strings.Virtual_String := Default;
      C_Name   :
        VSS.Implementation.Windows.String_Utilities.char16_array_access :=
          VSS.Implementation.Windows.String_Utilities.To_New_Native_String
            (Name);
      C_Buffer :
        VSS.Implementation.Windows.String_Utilities.char16_array_access;
      Size     : VSS.Implementation.Windows.DWORD;

   begin
      if C_Name = null or Name.Is_Empty then
         Result.Clear;

      else
         --  Request minimun size of the buffer to store result.
         Size :=
           VSS.Implementation.Windows.Kernel32.GetEnvironmentVariable
             (C_Name (C_Name'First)'Unchecked_Access, null, 0);

         if Size = 0 then
            if VSS.Implementation.Windows.Kernel32.GetLastError
                 = VSS.Implementation.Windows.ERROR_ENVVAR_NOT_FOUND
            then
               Result.Clear;

            else
               Result := "";
            end if;

         else
            C_Buffer :=
              VSS.Implementation.Windows.String_Utilities
                .New_Native_String_Buffer (Interfaces.C.size_t (Size));

            Size :=
              VSS.Implementation.Windows.Kernel32.GetEnvironmentVariable
                (C_Name (C_Name'First)'Unchecked_Access,
                 C_Buffer (C_Buffer'First)'Unchecked_Access,
                 C_Buffer'Length);
            pragma Assert (Size /= 0);

            Result :=
              VSS.Implementation.Windows.String_Utilities.From_Native_String
                (C_Buffer.all);

            VSS.Implementation.Windows.String_Utilities.Free (C_Buffer);
         end if;
      end if;

      VSS.Implementation.Windows.String_Utilities.Free (C_Name);

      return Result;
   end Get_Env;

end VSS.Implementation.Environment_Utilities;
