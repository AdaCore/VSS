--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

package body UCD.Data_File_Loaders is

   use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

   procedure Scan_Next_Line (Self : in out File_Loader'Class);

   function To_Code_Point (Item : Wide_Wide_String) return UCD.Code_Point;

   function Parse_Sequence_Of_Code_Unit
     (Buffer : Wide_Wide_String) return UCD.Code_Point_Vectors.Vector;

   -----------
   -- Close --
   -----------

   procedure Close (Self : in out File_Loader) is
   begin
      if Is_Open (Self.File) then
         Close (Self.File);
      end if;
   end Close;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (Self : File_Loader) return Boolean is
   begin
      return not Is_Open (Self.File) or Self.Buffer'First > Self.Line_Last;
   end End_Of_File;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out File_Loader) is
   begin
      Self.Close;
   end Finalize;

   --------------------------
   -- Get_Code_Point_Range --
   --------------------------

   procedure Get_Code_Point_Range
     (Self       : in out File_Loader;
      First_Code : out UCD.Code_Point;
      Last_Code  : out UCD.Code_Point)
   is
      function Is_Start_Of_Unicode_Data_Range return Boolean;
      --  Returns True when current line opens range in UnicodeData.txt format,
      --  thus first field ends with ", First>" string.

      ------------------------------------
      -- Is_Start_Of_Unicode_Data_Range --
      ------------------------------------

      function Is_Start_Of_Unicode_Data_Range return Boolean is
         Buffer : constant Wide_Wide_String := Self.Get_Field (1);
         Suffix : constant Wide_Wide_String := ", First>";

      begin
         return
           Buffer'Length > Suffix'Length
             and then Buffer (Buffer'First) = '<'
             and then Buffer (Buffer'Last - Suffix'Length + 1 .. Buffer'Last)
                        = Suffix;
      end Is_Start_Of_Unicode_Data_Range;

   begin
      if Is_Start_Of_Unicode_Data_Range then
         First_Code := To_Code_Point (Self.Get_Field (0));
         Self.Skip_Line;
         Last_Code := To_Code_Point (Self.Get_Field (0));

      else
         declare
            Buffer  : constant Wide_Wide_String := Self.Get_Field (0);
            Current : Positive;
            First   : Positive;
            Last    : Natural;

         begin
            First   := Buffer'First;
            Last    := Buffer'Last;
            Current := First;

            while Current <= Buffer'Last loop
               if Buffer (Current) not in '0' .. '9' | 'A' .. 'F' then
                  Last := Current - 1;

                  exit;
               end if;

               Current := Current + 1;
            end loop;

            First_Code := To_Code_Point (Buffer (First .. Last));
            Last_Code  := First_Code;

            if Last = Buffer'Last then
               return;
            end if;

            if Last + 3 < Buffer'Last
              and then Buffer (Last + 1) /= '.'
              and then Buffer (Last + 2) /= '.'
            then
               raise Program_Error;
            end if;

            First   := Last + 3;
            Last    := Buffer'Last;
            Current := First;

            while Current <= Buffer'Last loop
               if Buffer (Current) not in '0' .. '9' | 'A' .. 'F' then
                  Last := Current - 1;

                  exit;
               end if;

               Current := Current + 1;
            end loop;

            Last_Code := To_Code_Point (Buffer (First .. Last));

            if Last /= Buffer'Last then
               raise Program_Error;
            end if;
         end;
      end if;
   end Get_Code_Point_Range;

   ---------------
   -- Get_Field --
   ---------------

   function Get_Field
     (Self  : File_Loader;
      Index : Field_Index) return Wide_Wide_String is
   begin
      return
        Self.Buffer (Self.Fields (Index).First .. Self.Fields (Index).Last);
   end Get_Field;

   ---------------
   -- Get_Field --
   ---------------

   function Get_Field
     (Self  : File_Loader;
      Index : Field_Index) return UCD.Code_Point_Vectors.Vector is
   begin
      return Parse_Sequence_Of_Code_Unit (Self.Get_Field (Index));
   end Get_Field;

   ---------------
   -- Get_Field --
   ---------------

   procedure Get_Field
     (Self  : File_Loader;
      Index : Field_Index;
      Tag   : out Unbounded_Wide_Wide_String;
      Data  : out UCD.Code_Point_Vectors.Vector)
   is
      Buffer  : constant Wide_Wide_String := Self.Get_Field (Index);
      First   : Positive;
      Current : Positive;

   begin
      if Buffer'First > Buffer'Last then
         Tag := Null_Unbounded_Wide_Wide_String;
         Data := UCD.Code_Point_Vectors.Empty_Vector;

         return;
      end if;

      First := Buffer'First;

      if Buffer (First) = '<' then
         Current := First + 1;

         while Buffer (Current) /= '>' loop
            Current := Current + 1;
         end loop;

         Tag := To_Unbounded_Wide_Wide_String (Buffer (First .. Current));
         First := Current + 1;

         --  Skip spaces

         for J in First .. Buffer'Last loop
            First := J;

            exit when Buffer (J) /= ' ';
         end loop;

      else
         Tag := Null_Unbounded_Wide_Wide_String;
      end if;

      Data := Parse_Sequence_Of_Code_Unit (Buffer (First .. Buffer'Last));
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

   ---------------------------------
   -- Parse_Sequence_Of_Code_Unit --
   ---------------------------------

   function Parse_Sequence_Of_Code_Unit
     (Buffer : Wide_Wide_String) return UCD.Code_Point_Vectors.Vector
   is
      First   : Positive;
      Last    : Positive;
      Current : Positive;

   begin
      return Result : UCD.Code_Point_Vectors.Vector do
         First := Buffer'First;

         while First <= Buffer'Last loop
            Last    := Buffer'Last;
            Current := First;

            while Current <= Buffer'Last loop
               if Buffer (Current) not in '0' .. '9' | 'A' .. 'F' then
                  Last := Current - 1;

                  exit;
               end if;

               Current := Current + 1;
            end loop;

            Result.Append (To_Code_Point (Buffer (First .. Last)));

            First := Last + 1;

            --  Skip spaces

            for J in First .. Buffer'Last loop
               First := J;

               exit when Buffer (J) /= ' ';
            end loop;
         end loop;
      end return;
   end Parse_Sequence_Of_Code_Unit;

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

               if Field_First = Field_Last
                 and then Self.Buffer (Field_Last) = ' '
               then
                  Field_First := Field_First + 1;

               else
                  for J in reverse Field_First .. Field_Last loop
                     Field_Last := J;

                     exit when Self.Buffer (J) /= ' ';
                  end loop;
               end if;

               Self.Fields (Current_Field) := (Field_First, Field_Last);
               Current_Field := Current_Field + 1;

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

   -------------------
   -- To_Code_Point --
   -------------------

   function To_Code_Point (Item : Wide_Wide_String) return UCD.Code_Point is
   begin
      return Result : UCD.Code_Point := 0 do
         for J in Item'Range loop
            Result := Result * 16;

            case Item (J) is
               when '0' .. '9' =>
                  Result :=
                    Result
                      + Wide_Wide_Character'Pos (Item (J))
                      - Wide_Wide_Character'Pos ('0');

               when 'A' .. 'F' =>
                  Result :=
                    Result
                      + Wide_Wide_Character'Pos (Item (J))
                      - Wide_Wide_Character'Pos ('A')
                      + 10;

               when others =>
                  raise Constraint_Error;
            end case;
         end loop;
      end return;
   end To_Code_Point;

end UCD.Data_File_Loaders;
