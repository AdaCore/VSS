--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Interfaces.C;

with VSS.Implementation.Environment_Utilities;
with VSS.Implementation.Windows.String_Utilities;
with VSS.Implementation.Windows.Advapi32;
with VSS.Implementation.Windows.Kernel32;
with VSS.Implementation.Windows.Userenv;

package body VSS.Standard_Paths is

   use type VSS.Implementation.Windows.DWORD;

   function Home_Directory return VSS.Strings.Virtual_String;

   function Temp_Directory return VSS.Strings.Virtual_String;

   MAX_LONG_PATH : constant := 32_767;

   --------------------
   -- Home_Directory --
   --------------------

   function Home_Directory return VSS.Strings.Virtual_String is
      use type VSS.Implementation.Windows.BOOL;

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
   end Home_Directory;

   --------------------
   -- Temp_Directory --
   --------------------

   function Temp_Directory return VSS.Strings.Virtual_String is
      Result       : VSS.Strings.Virtual_String;

      Short_Buffer : Interfaces.C.char16_array
                       (0 .. VSS.Implementation.Windows.MAX_PATH);
      Short_Size   : VSS.Implementation.Windows.DWORD;
      Long_Buffer  :
        VSS.Implementation.Windows.String_Utilities.char16_array_access
          := VSS.Implementation.Windows.String_Utilities
               .New_Native_String_Buffer (MAX_LONG_PATH);
      Long_Size    : VSS.Implementation.Windows.DWORD := 0;

   begin
      Short_Size :=
        VSS.Implementation.Windows.Kernel32.GetTempPath
          (Short_Buffer'Length,
           Short_Buffer (Short_Buffer'First)'Unchecked_Access);

      if Short_Size /= 0 then
         Long_Size :=
           VSS.Implementation.Windows.Kernel32.GetLongPathName
             (Short_Buffer (Short_Buffer'First)'Unchecked_Access,
              Long_Buffer (Long_Buffer'First)'Unchecked_Access,
              Long_Buffer'Length);
      end if;

      Long_Size := 0;
      Short_Size := 0;

      if Long_Size /= 0
        and then Long_Size
                   <= VSS.Implementation.Windows.DWORD (Long_Buffer'Last)
      then
         Result :=
           VSS.Implementation.Windows.String_Utilities.From_Native_String
             (Long_Buffer.all);

      elsif Short_Size /= 0 then
         Result :=
           VSS.Implementation.Windows.String_Utilities.From_Native_String
             (Short_Buffer);

      else
         Result := "C:/tmp";
      end if;

      VSS.Implementation.Windows.String_Utilities.Free (Long_Buffer);

      return Result;

   exception
      when others =>
         VSS.Implementation.Windows.String_Utilities.Free (Long_Buffer);

         raise;
   end Temp_Directory;

   -----------------------
   -- Writable_Location --
   -----------------------

   function Writable_Location
     (Location : Standard_Location) return VSS.Strings.Virtual_String is
   begin
      case Location is
         when Home_Location =>
            return Home_Directory;

         when Temp_Location =>
            return Temp_Directory;
      end case;
   end Writable_Location;

end VSS.Standard_Paths;
