------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
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

with System.Storage_Elements;

package body VSS.Implementation.String_Handlers is

   use type VSS.Unicode.Code_Point;

   ---------------
   -- Ends_With --
   ---------------

   not overriding function Ends_With
     (Self           : Abstract_String_Handler;
      Data           : VSS.Implementation.Strings.String_Data;
      Suffix_Handler : Abstract_String_Handler'Class;
      Suffix_Data    : VSS.Implementation.Strings.String_Data) return Boolean
   is
      Self_Handler   : Abstract_String_Handler'Class
        renames Abstract_String_Handler'Class (Self);
      Self_Data      : VSS.Implementation.Strings.String_Data renames Data;

      Self_Position   : VSS.Implementation.Strings.Cursor;
      Suffix_Position : VSS.Implementation.Strings.Cursor;

   begin
      Self_Handler.After_Last_Character (Self_Data, Self_Position);
      Suffix_Handler.After_Last_Character (Suffix_Data, Suffix_Position);

      while
        Self_Handler.Backward (Self_Data, Self_Position)
          and Suffix_Handler.Backward (Suffix_Data, Suffix_Position)
      loop
         if Self_Handler.Element (Self_Data, Self_Position)
              /= Suffix_Handler.Element (Suffix_Data, Suffix_Position)
         then
            return False;
         end if;
      end loop;

      return True;
   end Ends_With;

   ------------------------
   -- First_UTF16_Offset --
   ------------------------

   not overriding function First_UTF16_Offset
     (Self     : Abstract_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.UTF16_Code_Unit_Index
   is
      use type VSS.Unicode.UTF16_Code_Unit_Offset;

      Handler : Abstract_String_Handler'Class
        renames Abstract_String_Handler'Class (Self);
      Aux     : VSS.Implementation.Strings.Cursor;

   begin
      if Position.UTF16_Offset >= 0 then
         return Position.UTF16_Offset;

      else
         Handler.Before_First_Character (Data, Aux);

         while Aux.Index /= Position.Index
           and then Handler.Forward (Data, Aux)
         loop
            null;
         end loop;
      end if;

      return Aux.UTF16_Offset;
   end First_UTF16_Offset;

   ----------
   -- Hash --
   ----------

   not overriding procedure Hash
     (Self      : Abstract_String_Handler;
      Data      : VSS.Implementation.Strings.String_Data;
      Generator : in out VSS.Implementation.FNV_Hash.FNV_1a_Generator)
   is
      Handler  : Abstract_String_Handler'Class
        renames Abstract_String_Handler'Class (Self);
      Position : VSS.Implementation.Strings.Cursor;
      Code     : VSS.Unicode.Code_Point;

   begin
      Handler.Before_First_Character (Data, Position);

      while Handler.Forward (Data, Position) loop
         Code := Handler.Element (Data, Position);

         VSS.Implementation.FNV_Hash.Hash
           (Generator,
            System.Storage_Elements.Storage_Element (Code and 16#0000_00FF#));
         Code := Code / 16#0000_0100#;
         VSS.Implementation.FNV_Hash.Hash
           (Generator,
            System.Storage_Elements.Storage_Element (Code and 16#0000_00FF#));
         Code := Code / 16#0000_0100#;
         VSS.Implementation.FNV_Hash.Hash
           (Generator,
            System.Storage_Elements.Storage_Element (Code and 16#0000_00FF#));
         Code := Code / 16#0000_0100#;
         VSS.Implementation.FNV_Hash.Hash
           (Generator,
            System.Storage_Elements.Storage_Element (Code and 16#0000_00FF#));
      end loop;
   end Hash;

   --------------
   -- Is_Equal --
   --------------

   not overriding function Is_Equal
     (Self       : Abstract_String_Handler;
      Data       : VSS.Implementation.Strings.String_Data;
      Other      : Abstract_String_Handler'Class;
      Other_Data : VSS.Implementation.Strings.String_Data) return Boolean
   is
      Left_Handler   : Abstract_String_Handler'Class
        renames Abstract_String_Handler'Class (Self);
      Left_Data      : VSS.Implementation.Strings.String_Data renames Data;
      Right_Handler  : Abstract_String_Handler'Class renames Other;
      Right_Data     : VSS.Implementation.Strings.String_Data
        renames Other_Data;

      Left_Position  : VSS.Implementation.Strings.Cursor;
      Right_Position : VSS.Implementation.Strings.Cursor;
      Left_Code      : VSS.Unicode.Code_Point;
      Right_Code     : VSS.Unicode.Code_Point;

   begin
      Left_Handler.Before_First_Character (Left_Data, Left_Position);
      Right_Handler.Before_First_Character (Right_Data, Right_Position);

      while
        Left_Handler.Forward (Left_Data, Left_Position)
          and Right_Handler.Forward (Right_Data, Right_Position)
      loop
         Left_Code  := Left_Handler.Element (Left_Data, Left_Position);
         Right_Code := Right_Handler.Element (Right_Data, Right_Position);

         if Left_Code /= Right_Code then
            return False;
         end if;
      end loop;

      return
        not Left_Handler.Has_Character (Left_Data, Left_Position)
          and not Right_Handler.Has_Character (Right_Data, Right_Position);
   end Is_Equal;

   -------------
   -- Is_Less --
   -------------

   not overriding function Is_Less
     (Self       : Abstract_String_Handler;
      Data       : VSS.Implementation.Strings.String_Data;
      Other      : Abstract_String_Handler'Class;
      Other_Data : VSS.Implementation.Strings.String_Data) return Boolean
   is
      Left_Handler   : Abstract_String_Handler'Class
        renames Abstract_String_Handler'Class (Self);
      Left_Data      : VSS.Implementation.Strings.String_Data renames Data;
      Right_Handler  : Abstract_String_Handler'Class renames Other;
      Right_Data     : VSS.Implementation.Strings.String_Data
        renames Other_Data;

      Left_Position  : VSS.Implementation.Strings.Cursor;
      Right_Position : VSS.Implementation.Strings.Cursor;
      Left_Code      : VSS.Unicode.Code_Point;
      Right_Code     : VSS.Unicode.Code_Point;

   begin
      Left_Handler.Before_First_Character (Left_Data, Left_Position);
      Right_Handler.Before_First_Character (Right_Data, Right_Position);

      while
        Left_Handler.Forward (Left_Data, Left_Position)
          and Right_Handler.Forward (Right_Data, Right_Position)
      loop
         Left_Code  := Left_Handler.Element (Left_Data, Left_Position);
         Right_Code := Right_Handler.Element (Right_Data, Right_Position);

         if Left_Code /= Right_Code then
            return Left_Code < Right_Code;
         end if;
      end loop;

      return Right_Handler.Has_Character (Right_Data, Right_Position);
   end Is_Less;

   ----------------------
   -- Is_Less_Or_Equal --
   ----------------------

   not overriding function Is_Less_Or_Equal
     (Self       : Abstract_String_Handler;
      Data       : VSS.Implementation.Strings.String_Data;
      Other      : Abstract_String_Handler'Class;
      Other_Data : VSS.Implementation.Strings.String_Data) return Boolean
   is
      Left_Handler   : Abstract_String_Handler'Class
        renames Abstract_String_Handler'Class (Self);
      Left_Data      : VSS.Implementation.Strings.String_Data renames Data;
      Right_Handler  : Abstract_String_Handler'Class renames Other;
      Right_Data     : VSS.Implementation.Strings.String_Data
        renames Other_Data;

      Left_Position  : VSS.Implementation.Strings.Cursor;
      Right_Position : VSS.Implementation.Strings.Cursor;
      Left_Code      : VSS.Unicode.Code_Point;
      Right_Code     : VSS.Unicode.Code_Point;

   begin
      Left_Handler.Before_First_Character (Left_Data, Left_Position);
      Right_Handler.Before_First_Character (Right_Data, Right_Position);

      while
        Left_Handler.Forward (Left_Data, Left_Position)
          and Right_Handler.Forward (Right_Data, Right_Position)
      loop
         Left_Code  := Left_Handler.Element (Left_Data, Left_Position);
         Right_Code := Right_Handler.Element (Right_Data, Right_Position);

         if Left_Code /= Right_Code then
            return Left_Code < Right_Code;
         end if;
      end loop;

      return
        Right_Handler.Has_Character (Right_Data, Right_Position)
          or not Left_Handler.Has_Character (Left_Data, Left_Position);
   end Is_Less_Or_Equal;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self           : Abstract_String_Handler;
      Data           : in out VSS.Implementation.Strings.String_Data;
      Suffix_Handler : Abstract_String_Handler'Class;
      Suffix_Data    : VSS.Implementation.Strings.String_Data)
   is
      Handler  : Abstract_String_Handler'Class
        renames Abstract_String_Handler'Class (Self);
      Position : VSS.Implementation.Strings.Cursor;
      Code     : VSS.Unicode.Code_Point;
   begin
      Suffix_Handler.Before_First_Character (Suffix_Data, Position);

      while Suffix_Handler.Forward (Suffix_Data, Position) loop
         Code := Suffix_Handler.Element (Suffix_Data, Position);
         Handler.Append (Data, Code);
      end loop;
   end Append;

   -----------
   -- Slice --
   -----------

   not overriding procedure Slice
     (Self   : Abstract_String_Handler;
      Source : VSS.Implementation.Strings.String_Data;
      From   : VSS.Implementation.Strings.Cursor;
      To     : VSS.Implementation.Strings.Cursor;
      Target : out VSS.Implementation.Strings.String_Data)
   is
      Handler  : Abstract_String_Handler'Class
        renames Abstract_String_Handler'Class (Self);
      Current  : VSS.Implementation.Strings.Cursor;

   begin
      if From.Index <= To.Index then
         Target :=
           (In_Place => True,
            Capacity => 0,
            Storage  => (others => 0),
            Padding  => <>);
         --  XXX Should Initialize subprogram be added to string handler to
         --  be used in cases like this?
         Current := From;

         VSS.Implementation.Strings.Handler (Target).Append
           (Target, Handler.Element (Source, Current));

         while Handler.Forward (Source, Current)
           and then Current.Index <= To.Index
         loop
            VSS.Implementation.Strings.Handler (Target).Append
              (Target, Handler.Element (Source, Current));
         end loop;

      else
         Target := VSS.Implementation.Strings.Null_String_Data;
      end if;
   end Slice;

   -----------------
   -- Starts_With --
   -----------------

   not overriding function Starts_With
     (Self           : Abstract_String_Handler;
      Data           : VSS.Implementation.Strings.String_Data;
      Prefix_Handler : Abstract_String_Handler'Class;
      Prefix_Data    : VSS.Implementation.Strings.String_Data) return Boolean
   is
      Self_Handler   : Abstract_String_Handler'Class
        renames Abstract_String_Handler'Class (Self);
      Self_Data      : VSS.Implementation.Strings.String_Data renames Data;

      Self_Position   : VSS.Implementation.Strings.Cursor;
      Prefix_Position : VSS.Implementation.Strings.Cursor;

   begin
      Self_Handler.Before_First_Character (Self_Data, Self_Position);
      Prefix_Handler.Before_First_Character (Prefix_Data, Prefix_Position);

      while
        Self_Handler.Forward (Self_Data, Self_Position)
          and Prefix_Handler.Forward (Prefix_Data, Prefix_Position)
      loop
         if Self_Handler.Element (Self_Data, Self_Position)
              /= Prefix_Handler.Element (Prefix_Data, Prefix_Position)
         then
            return False;
         end if;
      end loop;

      return True;
   end Starts_With;

   -----------------------
   -- Last_UTF16_Offset --
   -----------------------

   not overriding function Last_UTF16_Offset
     (Self     : Abstract_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.UTF16_Code_Unit_Index
   is
      use type VSS.Unicode.UTF16_Code_Unit_Offset;

      Handler : Abstract_String_Handler'Class
        renames Abstract_String_Handler'Class (Self);
      Aux     : VSS.Implementation.Strings.Cursor;
      Dummy   : Boolean;

   begin
      if Position.UTF16_Offset >= 0 then
         Aux := Position;

      else
         Handler.Before_First_Character (Data, Aux);

         while Aux.Index /= Position.Index
           and then Handler.Forward (Data, Aux)
         loop
            null;
         end loop;
      end if;

      Dummy := Handler.Forward (Data, Aux);

      return Aux.UTF16_Offset - 1;
   end Last_UTF16_Offset;

end VSS.Implementation.String_Handlers;
