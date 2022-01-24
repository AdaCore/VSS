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

--  XXX Right now only simple wrappers are implemented.

with VSS.Characters;
with VSS.Implementation.Environment_Utilities;

package body VSS.Environments is

   package Platform is

      function Native_Path_Separator return VSS.Characters.Virtual_Character;
      --  Return path separator of underlying platform.

   end Platform;

   package body Platform is separate;

   --------------
   -- Contains --
   --------------

   function Contains
     (Self : Process_Environment'Class;
      Name : VSS.Strings.Virtual_String) return Boolean
   is
      pragma Unreferenced (Self);

      Value : constant VSS.Strings.Virtual_String :=
        VSS.Implementation.Environment_Utilities.Get_Env (Name);

   begin
      return not Value.Is_Null;
   end Contains;

   -----------
   -- Value --
   -----------

   function Value
     (Self    : Process_Environment'Class;
      Name    : VSS.Strings.Virtual_String;
      Default : VSS.Strings.Virtual_String := VSS.Strings.Empty_Virtual_String)
      return VSS.Strings.Virtual_String
   is
      pragma Unreferenced (Self);

   begin
      return VSS.Implementation.Environment_Utilities.Get_Env (Name, Default);
   end Value;

   -----------------
   -- Value_Paths --
   -----------------

   function Value_Paths
     (Self             : Process_Environment'Class;
      Name             : VSS.Strings.Virtual_String;
      Keep_Empty_Paths : Boolean                                  := True;
      Default          : VSS.String_Vectors.Virtual_String_Vector :=
        VSS.String_Vectors.Empty_Virtual_String_Vector)
      return VSS.String_Vectors.Virtual_String_Vector
   is
      pragma Unreferenced (Self);

      Value : constant VSS.Strings.Virtual_String :=
        VSS.Implementation.Environment_Utilities.Get_Env (Name);

   begin
      if Value.Is_Null then
         return Default;

      else
         return Value.Split (Platform.Native_Path_Separator, Keep_Empty_Paths);
      end if;
   end Value_Paths;

end VSS.Environments;
