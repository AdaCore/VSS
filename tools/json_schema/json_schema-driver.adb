--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Streams.Stream_IO;
with Ada.Wide_Wide_Text_IO;

with VSS.Application;
with VSS.JSON.Pull_Readers.Simple;
with VSS.Stream_Element_Vectors;
with VSS.Strings.Conversions;
with VSS.Text_Streams.Memory_UTF8_Input;
with VSS.String_Vectors;

with JSON_Schema.Readers;
with JSON_Schema.Writers.Types;

procedure JSON_Schema.Driver is

   function Parse_Command_Line return Boolean;
   --  Parse application arguments and return True if success.

   procedure Read_File
     (File_Name : VSS.Strings.Virtual_String;
      Value     : out VSS.String_Vectors.Virtual_String_Vector);
   --  Read lines from given file into a string vector

   Arg          : VSS.Strings.Virtual_String;
   --  JSON Schema input file
   Root_Package : VSS.Strings.Virtual_String := "Types";
   --  Name of a root package
   Enum_Package : VSS.Strings.Virtual_String;
   --  Name of dedicated package for enumeration types, could be useful to
   --  avoid name clashes.
   Header_File  : VSS.Strings.Virtual_String;
   --  Header file - copyright/license to put on top of each file
   Holders      : VSS.String_Vectors.Virtual_String_Vector;
   --  Array of "holder" fields in form of "type:field" where a holder type
   --  should be used instead of type.

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   function Parse_Command_Line return Boolean is
      use type VSS.Strings.Virtual_String;
      Is_Root_Package : Boolean := False;
      Is_Enum_Package : Boolean := False;
      Is_Header_File  : Boolean := False;
      Is_Holder       : Boolean := False;
   begin
      for Item of VSS.Application.Arguments loop
         if Is_Root_Package then
            Is_Root_Package := False;
            Root_Package := Item;
         elsif Is_Enum_Package then
            Is_Enum_Package := False;
            Enum_Package := Item;
         elsif Is_Header_File then
            Is_Header_File := False;
            Header_File := Item;
         elsif Is_Holder then
            Is_Holder := False;
            Holders.Append (Item);
         elsif Item = "--root-package" then
            Is_Root_Package := True;
         elsif Item = "--enum-package" then
            Is_Enum_Package := True;
         elsif Item = "--header-file" then
            Is_Header_File := True;
         elsif Item = "--holder" then
            Is_Holder := True;
         else
            Arg := Item;
         end if;
      end loop;

      return not Arg.Is_Empty
        and not Is_Header_File
        and not Is_Holder
        and not Is_Root_Package
        and not Is_Enum_Package;
   end Parse_Command_Line;

   procedure Read_File
     (File_Name : VSS.Strings.Virtual_String;
      Value     : out VSS.String_Vectors.Virtual_String_Vector)
   is
      Input : Ada.Wide_Wide_Text_IO.File_Type;
   begin
      Ada.Wide_Wide_Text_IO.Open
       (Input,
        Ada.Wide_Wide_Text_IO.In_File,
        VSS.Strings.Conversions.To_UTF_8_String (File_Name));

      while not Ada.Wide_Wide_Text_IO.End_Of_File (Input) loop
         declare
            Line : constant Wide_Wide_String :=
              Ada.Wide_Wide_Text_IO.Get_Line (Input);
         begin
            Value.Append (VSS.Strings.To_Virtual_String (Line));
         end;
      end loop;

      Ada.Wide_Wide_Text_IO.Close (Input);
   end Read_File;

   File  : Ada.Streams.Stream_IO.File_Type;
   Raw   : VSS.Stream_Element_Vectors.Stream_Element_Vector;
   Input : aliased VSS.Text_Streams.Memory_UTF8_Input.Memory_UTF8_Input_Stream;

   Header : VSS.String_Vectors.Virtual_String_Vector;
   Reader : VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
   Schema : JSON_Schema.Schema_Access;
   Other  : JSON_Schema.Readers.Schema_Map;
begin
   if not Parse_Command_Line then
      Ada.Wide_Wide_Text_IO.Put_Line
       ("Usage: gen_json [options] <json_schema>.json");
      Ada.Wide_Wide_Text_IO.New_Line;
      Ada.Wide_Wide_Text_IO.Put_Line ("Where options:");
      Ada.Wide_Wide_Text_IO.Put_Line
        ("  --root-package <package> - A package for generated types");
      Ada.Wide_Wide_Text_IO.Put_Line
        ("  --enum-package <package> - A package for enumeration types");
      Ada.Wide_Wide_Text_IO.Put_Line
        ("  --header-file  <file>    - A copyright header file");
      Ada.Wide_Wide_Text_IO.Put_Line
        ("  --holder <type:field>    - Use holder to break circular " &
         "dependency");
      return;
   end if;

   if not Header_File.Is_Empty then
      Read_File (Header_File, Header);
      Header.Append ("");
   end if;

   Ada.Streams.Stream_IO.Open
     (File,
      Ada.Streams.Stream_IO.In_File,
      VSS.Strings.Conversions.To_UTF_8_String (Arg));

   while not Ada.Streams.Stream_IO.End_Of_File (File) loop
      declare
         Data : Ada.Streams.Stream_Element_Array (1 .. 256);
         Last : Ada.Streams.Stream_Element_Offset;
      begin
         Ada.Streams.Stream_IO.Read (File, Data, Last);
         for X of Data (1 .. Last) loop
            Raw.Append (X);
         end loop;
      end;
   end loop;

   Input.Set_Data (Raw);
   Reader.Set_Stream (Input'Unchecked_Access);
   Reader.Read_Next;
   pragma Assert (Reader.Is_Start_Document);
   Reader.Read_Next;

   JSON_Schema.Readers.Read (Reader, Schema, Other);
   JSON_Schema.Writers.Types.Write
     (Other, Root_Package, Enum_Package, Header, Holders);
end JSON_Schema.Driver;
