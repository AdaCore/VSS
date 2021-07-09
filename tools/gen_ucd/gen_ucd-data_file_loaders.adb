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

with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

package body Gen_UCD.Data_File_Loaders is

   use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

   procedure Scan_Next_Line (Self : in out File_Loader'Class);

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (Self : File_Loader) return Boolean is
   begin
      return not Is_Open (Self.File) or Self.Buffer'First > Self.Line_Last;
   end End_Of_File;

   ---------------
   -- Get_Field --
   ---------------

   function Get_Field
     (Self  : in out File_Loader;
      Index : Field_Index) return Wide_Wide_String is
   begin
      return
        Self.Buffer (Self.Fields (Index).First .. Self.Fields (Index).Last);
   end Get_Field;

   ---------------
   -- Has_Field --
   ---------------

   function Has_Field
     (Self : File_Loader; Index : Field_Index) return Boolean is
   begin
      return
        Self.Fields (Index).First /= Self.Buffer'First
          or else Self.Fields (Index).First <= Self.Fields (Index).Last;
   end Has_Field;

   ----------
   -- Open --
   ----------

   procedure Open
     (Self      : in out File_Loader;
      UCD_Root  : Wide_Wide_String;
      File_Name : Wide_Wide_String)
   is
   begin
      Put_Line ("Loading " & File_Name & "...");
      Open (Self.File, In_File, Encode (UCD_Root & '/' & File_Name), "wcem=8");
      Self.Scan_Next_Line;
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (Self : in out File_Loader) is
   begin
      if Is_Open (Self.File) then
         Close (Self.File);
      end if;
   end Close;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out File_Loader) is
   begin
      Self.Close;
   end Finalize;

   --------------------
   -- Scan_Next_Line --
   --------------------

   procedure Scan_Next_Line (Self : in out File_Loader'Class) is
      Current_Field : Field_Index := Field_Index'First;
      Field_First   : Positive;
      Field_Last    : Natural;
      Current       : Positive;
      Has_Separator : Boolean;

   begin
      Self.Fields := (others => (1, 0));

      loop
         Self.Line_Last := 0;

         exit when End_Of_File (Self.File);

         Get_Line (Self.File, Self.Buffer, Self.Line_Last);

         --  Remove comments

         for J in Self.Buffer'First .. Self.Line_Last loop
            if Self.Buffer (J) = '#' then
               Self.Line_Last := J - 1;

               exit;
            end if;
         end loop;

         if Self.Buffer'First <= Self.Line_Last then
            --  Put_Line (Self.Buffer (Self.Buffer'First .. Self.Line_Last));

            Current := Self.Buffer'First;

            loop
               Field_First := Current;
               Field_Last  := Self.Line_Last;
               Has_Separator := False;

               for J in Field_First .. Self.Line_Last loop
                  if Self.Buffer (J) = ';' then
                     Field_Last := J - 1;
                     Current    := J + 1;
                     Has_Separator := True;

                     exit;
                  end if;
               end loop;

               --  Remove leading spaces

               for J in Field_First .. Field_Last loop
                  Field_First := J;

                  exit when Self.Buffer (J) /= ' ';
               end loop;

               --  Remove trailing spaces

               for J in reverse Field_First .. Field_Last loop
                  Field_Last := J;

                  exit when Self.Buffer (J) /= ' ';
               end loop;

               Self.Fields (Current_Field) := (Field_First, Field_Last);
               Current_Field := Current_Field + 1;

               --  Put_Line
               --    (''' & Self.Buffer (Field_First .. Field_Last) & ''');

               exit when not Has_Separator;
            end loop;

            exit;
         end if;
      end loop;
   end Scan_Next_Line;

   ---------------
   -- Skip_Line --
   ---------------

   procedure Skip_Line (Self : in out File_Loader) is
   begin
      Self.Scan_Next_Line;
   end Skip_Line;

end Gen_UCD.Data_File_Loaders;
