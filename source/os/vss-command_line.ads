--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This package provides access to command line arguments as parsed options.

with Ada.Containers.Vectors;

with VSS.Strings;
with VSS.String_Vectors;

package VSS.Command_Line is

   type Name_Value is record
      Name  : VSS.Strings.Virtual_String;
      Value : VSS.Strings.Virtual_String;
   end record;

   package Name_Value_Vectors is
      new Ada.Containers.Vectors (Positive, Name_Value);

   type Abstract_Option is abstract tagged record
      Description : VSS.Strings.Virtual_String;
   end record;

   type Positional_Option is new Abstract_Option with record
      Name : VSS.Strings.Virtual_String;
   end record;

   type Named_Option is abstract new Abstract_Option with record
      Short_Name : VSS.Strings.Virtual_String;
      Long_Name  : VSS.Strings.Virtual_String;
   end record;

   type Binary_Option is new Named_Option with null record;

   type Value_Option is new Named_Option with record
      Value_Name : VSS.Strings.Virtual_String;
   end record;

   type Name_Value_Option is new Named_Option with record
      Name_Name  : VSS.Strings.Virtual_String;
      Value_Name : VSS.Strings.Virtual_String;
   end record;

   procedure Add_Option (Option : Abstract_Option'Class);

   procedure Process;
   --  Parse command line. If error is found then report it and terminate
   --  application.

   function Is_Specified (Option : Abstract_Option'Class) return Boolean;
   --  Return True when given option has been specified the command line.

   function Value
     (Option : Value_Option'Class) return VSS.Strings.Virtual_String;
   --  Return value of the given option.

   function Values
     (Option : Name_Value_Option'Class) return Name_Value_Vectors.Vector;
   --  Return all name=value pairs of the given option specified in the
   --  command line.

   function Positional_Arguments
     return VSS.String_Vectors.Virtual_String_Vector;
   --  Return list of positional arguments.

   procedure Report_Error (Message : VSS.Strings.Virtual_String)
     with No_Return;
   --  Report error and terminate application.

private

   function Unique_Name
     (Self : Named_Option'Class) return VSS.Strings.Virtual_String;

end VSS.Command_Line;
