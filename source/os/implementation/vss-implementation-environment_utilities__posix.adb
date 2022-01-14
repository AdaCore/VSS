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
