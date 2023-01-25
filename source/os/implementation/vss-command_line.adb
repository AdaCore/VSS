--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with GNAT.OS_Lib;

with VSS.Application;
with VSS.Command_Line.Parsers;
with VSS.Text_Streams.Standadrs;

package body VSS.Command_Line is

   Parser : VSS.Command_Line.Parsers.Command_Line_Parser;

   procedure Output_Error (Message : VSS.Strings.Virtual_String);
   --  Outputs error message to the standard error.

   package Platform is

      procedure Report_Error (Message : VSS.Strings.Virtual_String);
      --  Report error in platform specific way. On POSIX systems, error is
      --  output to standard error stream; on Windows application console is
      --  used when available, otherwise, if standard error stream is defined
      --  the message is output to it, and otherwise message box with the
      --  messase text is displayed.

   end Platform;

   ----------------
   -- Add_Option --
   ----------------

   procedure Add_Option (Option : Abstract_Option'Class) is
   begin
      Parser.Add_Option (Option);
   end Add_Option;

   ------------------
   -- Is_Specified --
   ------------------

   function Is_Specified (Option : Abstract_Option'Class) return Boolean is
   begin
      return Parser.Is_Specified (Option);
   end Is_Specified;

   ------------------
   -- Output_Error --
   ------------------

   procedure Output_Error (Message : VSS.Strings.Virtual_String) is
      Error   : VSS.Text_Streams.Output_Text_Stream'Class :=
        VSS.Text_Streams.Standadrs.Standard_Error;
      Success : Boolean := True;

   begin
      Error.Put_Line (Message, Success);
   end Output_Error;

   --------------
   -- Platform --
   --------------

   package body Platform is separate;

   --------------------------
   -- Positional_Arguments --
   --------------------------

   function Positional_Arguments
     return VSS.String_Vectors.Virtual_String_Vector is
   begin
      return Parser.Positional_Arguments;
   end Positional_Arguments;

   -------------
   -- Process --
   -------------

   procedure Process is
   begin
      if not Parser.Parse (VSS.Application.Arguments) then
         Report_Error (Parser.Error_Message);
      end if;

      if not Parser.Unknown_Option_Arguments.Is_Empty then
         Report_Error ("unknown option");
      end if;
   end Process;

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Error (Message : VSS.Strings.Virtual_String) is
   begin
      Platform.Report_Error (Message);
      GNAT.OS_Lib.OS_Exit (1);
   end Report_Error;

   -----------------
   -- Unique_Name --
   -----------------

   function Unique_Name
     (Self : Named_Option'Class) return VSS.Strings.Virtual_String is
   begin
      if Self.Short_Name.Is_Empty then
         return Self.Long_Name;

      else
         return Self.Short_Name;
      end if;
   end Unique_Name;

   -----------
   -- Value --
   -----------

   function Value
     (Option : Positional_Option'Class) return VSS.Strings.Virtual_String is
   begin
      return Parser.Value (Option);
   end Value;

   -----------
   -- Value --
   -----------

   function Value
     (Option : Value_Option'Class) return VSS.Strings.Virtual_String is
   begin
      return Parser.Value (Option);
   end Value;

   ------------
   -- Values --
   ------------

   function Values
     (Option : Value_Option'Class)
      return VSS.String_Vectors.Virtual_String_Vector is
   begin
      return Parser.Values (Option);
   end Values;

   ------------
   -- Values --
   ------------

   function Values
     (Option : Name_Value_Option'Class) return Name_Value_Vectors.Vector is
   begin
      return Parser.Values (Option);
   end Values;

end VSS.Command_Line;
