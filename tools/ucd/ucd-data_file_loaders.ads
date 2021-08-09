------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

private with Ada.Finalization;
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

private

   use Ada.Wide_Wide_Text_IO;

   type Field is record
      First : Positive;
      Last  : Natural;
   end record;

   type Field_Array is array (Field_Index) of Field;

   type File_Loader is new Ada.Finalization.Limited_Controlled with record
      File   : File_Type;
      Buffer : Wide_Wide_String (1 .. 2048);
      Line_Last   : Natural;
      Fields : Field_Array;
   end record;

   overriding procedure Finalize (Self : in out File_Loader);

end UCD.Data_File_Loaders;
