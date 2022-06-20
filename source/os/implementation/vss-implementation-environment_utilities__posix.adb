--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This is implementation of the package for POSIX systems.
--
--  There are known limitations of the current implementation:
--
--   - only UTF-8 locales are supported
--   - Mac OS X use text in NFD form for paths, conversion is not implemented

with Interfaces.C.Strings;

with VSS.Strings.Conversions;

package body VSS.Implementation.Environment_Utilities is

   function Decode_Native_Path
     (Item : Interfaces.C.Strings.chars_ptr)
      return VSS.Strings.Virtual_String;
   --  Convert path in native format to Virtual_String.

   function getenv
     (Name : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr
      with Import, Convention => C;

   ------------------------
   -- Decode_Native_Path --
   ------------------------

   function Decode_Native_Path
     (Item : Interfaces.C.Strings.chars_ptr)
      return VSS.Strings.Virtual_String is
   begin
      --  On Mac OS X it may normalize string to NFC, because native paths
      --  are always decomposed to NFD.

      return
        VSS.Strings.Conversions.To_Virtual_String
          (Interfaces.C.Strings.Value (Item));
   end Decode_Native_Path;

   -------------
   -- Get_Env --
   -------------

   function Get_Env
     (Name    : VSS.Strings.Virtual_String;
      Default : VSS.Strings.Virtual_String := VSS.Strings.Empty_Virtual_String)
      return VSS.Strings.Virtual_String
   is
      use type Interfaces.C.Strings.chars_ptr;

      Ada_Name : constant String :=
        VSS.Strings.Conversions.To_UTF_8_String (Name);
      C_Name   : aliased Interfaces.C.char_array :=
        Interfaces.C.To_C (Ada_Name);
      Value    : constant Interfaces.C.Strings.chars_ptr :=
        getenv (Interfaces.C.Strings.To_Chars_Ptr (C_Name'Unchecked_Access));

   begin
      if Value = Interfaces.C.Strings.Null_Ptr then
         return Default;

      else
         return Decode_Native_Path (Value);
      end if;
   end Get_Env;

end VSS.Implementation.Environment_Utilities;
