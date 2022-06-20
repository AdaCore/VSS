--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.String_Vectors;
with VSS.Strings;

package VSS.Environments is

   type Process_Environment is tagged private;

   function Contains
     (Self : Process_Environment'Class;
      Name : VSS.Strings.Virtual_String) return Boolean;
   --  Return True when environment variable with the given name is set in
   --  the environment.

   function Value
     (Self    : Process_Environment'Class;
      Name    : VSS.Strings.Virtual_String;
      Default : VSS.Strings.Virtual_String := VSS.Strings.Empty_Virtual_String)
      return VSS.Strings.Virtual_String;
   --  Return value of the environment variable with the given name or given
   --  default value otherwise.

   function Value_Paths
     (Self             : Process_Environment'Class;
      Name             : VSS.Strings.Virtual_String;
      Keep_Empty_Paths : Boolean                                  := True;
      Default          : VSS.String_Vectors.Virtual_String_Vector :=
        VSS.String_Vectors.Empty_Virtual_String_Vector)
      return VSS.String_Vectors.Virtual_String_Vector;
   --  Return value of the environment variable with the given name if
   --  defined, otherwise return given default value. Value of the environment
   --  variable is split into the vector with platform specific separator
   --  (':' on POSIX and ';' on Windows).

private

   type Process_Environment is tagged record
      null;
   end record;

end VSS.Environments;
