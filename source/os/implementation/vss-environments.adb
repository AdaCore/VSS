--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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
