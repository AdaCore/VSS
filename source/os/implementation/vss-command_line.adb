--
--  Copyright (C) 2022-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with GNAT.OS_Lib;

with VSS.Application;
with VSS.Command_Line.Parsers;
with VSS.Text_Streams.Standards;

package body VSS.Command_Line is

   Parser              : VSS.Command_Line.Parsers.Command_Line_Parser;
   Process_Help_Option : Boolean := False;
   Help_Option         : constant Binary_Option :=
     (Short_Name  => "h",
      Long_Name   => "help",
      Description => "Display help information");

   procedure Put_Line_Error (Message : VSS.Strings.Virtual_String);
   --  Outputs message's line to the standard error stream.

   procedure Put_Line_Output (Message : VSS.Strings.Virtual_String);
   --  Outputs message's line to the standard output stream.

   package Platform is

      procedure Report_Message
        (Message  : VSS.Strings.Virtual_String;
         Is_Error : Boolean);
      procedure Report_Message
        (Message  : VSS.String_Vectors.Virtual_String_Vector;
         Is_Error : Boolean);
      --  Report message in the platform specific way. On POSIX systems,
      --  message is output to the standard output/error stream; on Windows
      --  application's console is used when available, otherwise, if standard
      --  output/error stream is defined the message is output to it, and
      --  otherwise message box with the messase text is displayed.

   end Platform;

   ---------------------
   -- Add_Help_Option --
   ---------------------

   procedure Add_Help_Option is
   begin
      Parser.Add_Option (Help_Option);
      Process_Help_Option := True;
   end Add_Help_Option;

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

      if Process_Help_Option and then Parser.Is_Specified (Help_Option) then
         Platform.Report_Message
           (Message  => Parser.Help_Text,
            Is_Error => False);
         GNAT.OS_Lib.OS_Exit (0);
      end if;

      if not Parser.Unknown_Option_Arguments.Is_Empty then
         Report_Error ("unknown option");
      end if;
   end Process;

   --------------------
   -- Put_Line_Error --
   --------------------

   procedure Put_Line_Error (Message : VSS.Strings.Virtual_String) is
      Stream  : VSS.Text_Streams.Output_Text_Stream'Class :=
        VSS.Text_Streams.Standards.Standard_Error;
      Success : Boolean := True;

   begin
      Stream.Put_Line (Message, Success);
   end Put_Line_Error;

   ---------------------
   -- Put_Line_Output --
   ---------------------

   procedure Put_Line_Output (Message : VSS.Strings.Virtual_String) is
      Stream  : VSS.Text_Streams.Output_Text_Stream'Class :=
        VSS.Text_Streams.Standards.Standard_Output;
      Success : Boolean := True;

   begin
      Stream.Put_Line (Message, Success);
   end Put_Line_Output;

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Error (Message : VSS.Strings.Virtual_String) is
   begin
      Platform.Report_Message
        (Message  => Message,
         Is_Error => True);
      GNAT.OS_Lib.OS_Exit (1);
   end Report_Error;

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Error
     (Message : VSS.String_Vectors.Virtual_String_Vector) is
   begin
      Platform.Report_Message
        (Message  => Message,
         Is_Error => True);
      GNAT.OS_Lib.OS_Exit (1);
   end Report_Error;

   --------------------
   -- Report_Message --
   --------------------

   procedure Report_Message
     (Message : VSS.Strings.Virtual_String) is
   begin
      Platform.Report_Message
        (Message  => Message,
         Is_Error => False);
      GNAT.OS_Lib.OS_Exit (0);
   end Report_Message;

   --------------------
   -- Report_Message --
   --------------------

   procedure Report_Message
     (Message : VSS.String_Vectors.Virtual_String_Vector) is
   begin
      Platform.Report_Message
        (Message  => Message,
         Is_Error => False);
      GNAT.OS_Lib.OS_Exit (0);
   end Report_Message;

   --------------------------------
   -- Require_Positional_Options --
   --------------------------------

   procedure Require_Positional_Options (Count : Natural) is
   begin
      Parser.Require_Positional_Options (Count);
   end Require_Positional_Options;

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
     (Option : Multivalue_Positional_Option'Class)
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
