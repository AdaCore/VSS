--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

private with Ada.Finalization;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
private with Ada.Wide_Wide_Text_IO;

package UCD.Data_File_Loaders is

   type Field_Index is range 0 .. 16;

   type File_Loader is tagged limited private;

   procedure Open
     (Self      : in out File_Loader;
      UCD_Root  : Wide_Wide_String;
      File_Name : Wide_Wide_String);

   procedure Close (Self : in out File_Loader);

   function End_Of_File (Self : File_Loader) return Boolean;

   procedure Skip_Line (Self : in out File_Loader);

   function Get_Field
     (Self : File_Loader; Index : Field_Index) return Wide_Wide_String;

   function Get_Field
     (Self  : File_Loader;
      Index : Field_Index) return UCD.Code_Point_Vectors.Vector;

   function Has_Field (Self : File_Loader; Index : Field_Index) return Boolean;

   procedure Get_Code_Point_Range
     (Self       : in out File_Loader;
      First_Code : out UCD.Code_Point;
      Last_Code  : out UCD.Code_Point);
   --  Get range of code points current line applied. It parse zero field of
   --  the line and supports both ordinary XXXX..YYYY format and special
   --  UnicodeData.txt when two lines used to define range.

   procedure Get_Field
     (Self  : File_Loader;
      Index : Field_Index;
      Tag   : out Unbounded_Wide_Wide_String;
      Data  : out UCD.Code_Point_Vectors.Vector);
   --  Parse filed according to format of decomposition type & mapping of
   --  UnicodeData.txt

private

   use Ada.Wide_Wide_Text_IO;

   type Field is record
      First : Positive;
      Last  : Natural;
   end record;

   type Field_Array is array (Field_Index) of Field;

   type File_Loader is new Ada.Finalization.Limited_Controlled with record
      File      : File_Type;
      Buffer    : Wide_Wide_String (1 .. 2048);
      Line_Last : Natural;
      Fields    : Field_Array;
   end record;

   overriding procedure Finalize (Self : in out File_Loader);

end UCD.Data_File_Loaders;
