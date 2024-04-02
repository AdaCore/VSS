--
--  Copyright (C) 2020-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with System.Storage_Elements;

with VSS.Implementation.String_Configuration;
with VSS.Implementation.UTF8_Encoding;
with VSS.Strings;

package body VSS.Implementation.Text_Handlers is

   use type VSS.Unicode.Code_Point;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self   : in out Abstract_String_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      Suffix : VSS.Implementation.Strings.String_Data;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset)
   is
      Handler        : Abstract_String_Handler'Class
        renames Abstract_String_Handler'Class (Self);
      Suffix_Handler : constant not null
        VSS.Implementation.Strings.Constant_Text_Handler_Access :=
          VSS.Implementation.Strings.Constant_Handler (Suffix);
      Position       : VSS.Implementation.Strings.Cursor;
      Code           : VSS.Unicode.Code_Point;

   begin
      Suffix_Handler.Before_First_Character (Suffix, Position);

      while Suffix_Handler.Forward (Suffix, Position) loop
         Code := Suffix_Handler.Element (Suffix, Position);
         Handler.Append (Data, Code, Offset);
      end loop;
   end Append;

   ------------------
   -- Compute_Size --
   ------------------

   not overriding procedure Compute_Size
     (Self   : Abstract_String_Handler;
      Data   : VSS.Implementation.Strings.String_Data;
      From   : VSS.Implementation.Strings.Cursor;
      To     : VSS.Implementation.Strings.Cursor;
      Size   : out VSS.Implementation.Strings.Cursor_Offset)
   is
      use type VSS.Unicode.UTF16_Code_Unit_Offset;
      use type VSS.Unicode.UTF8_Code_Unit_Offset;

      Handler       : Abstract_String_Handler'Class
        renames Abstract_String_Handler'Class (Self);
      From_Position : VSS.Implementation.Strings.Cursor;
      To_Position   : VSS.Implementation.Strings.Cursor;
      Success       : Boolean with Unreferenced;

   begin
      if From.Index > To.Index then
         Size := (0, 0, 0);

      else
         if From.UTF8_Offset < 0 or From.UTF16_Offset < 0 then
            --  Some of UTF* offset of From must be resolved first.

            Handler.Before_First_Character (Data, From_Position);

            while From_Position.Index /= From.Index
              and then Handler.Forward (Data, From_Position)
            loop
               null;
            end loop;

         else
            From_Position := From;
         end if;

         if To.UTF8_Offset < 0 or To.UTF16_Offset < 0 then
            --  Some of UTF* offset of To must be resolved first.

            To_Position := From_Position;

            while To_Position.Index /= To.Index
              and then Handler.Forward (Data, To_Position)
            loop
               null;
            end loop;

         else
            To_Position := To;
         end if;

         Success := Handler.Forward (Data, To_Position);

         Size.Index_Offset := To_Position.Index - From_Position.Index;
         Size.UTF8_Offset  :=
           To_Position.UTF8_Offset - From_Position.UTF8_Offset;
         Size.UTF16_Offset :=
           To_Position.UTF16_Offset - From_Position.UTF16_Offset;
      end if;
   end Compute_Size;

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

   -----------------------
   -- First_UTF8_Offset --
   -----------------------

   not overriding function First_UTF8_Offset
     (Self     : Abstract_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.UTF8_Code_Unit_Index
   is
      use type VSS.Unicode.UTF8_Code_Unit_Offset;

      Handler : Abstract_String_Handler'Class
        renames Abstract_String_Handler'Class (Self);
      Aux     : VSS.Implementation.Strings.Cursor;

   begin
      if Position.UTF8_Offset >= 0 then
         return Position.UTF8_Offset;

      else
         Handler.Before_First_Character (Data, Aux);

         while Aux.Index /= Position.Index
           and then Handler.Forward (Data, Aux)
         loop
            null;
         end loop;
      end if;

      return Aux.UTF8_Offset;
   end First_UTF8_Offset;

   ---------------------
   -- Forward_Element --
   ---------------------

   not overriding function Forward_Element
     (Self     : Abstract_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : in out VSS.Implementation.Strings.Cursor;
      Element  : out VSS.Unicode.Code_Point'Base) return Boolean is
   begin
      if Abstract_String_Handler'Class (Self).Forward (Data, Position) then
         Element :=
           Abstract_String_Handler'Class (Self).Element (Data, Position);

         return True;

      else
         Element := VSS.Implementation.Strings.No_Character;

         return False;
      end if;
   end Forward_Element;

   -----------------------
   -- From_UTF_8_String --
   -----------------------

   not overriding procedure From_UTF_8_String
     (Self    : in out Abstract_String_Handler;
      Item    : Ada.Strings.UTF_Encoding.UTF_8_String;
      Data    : out VSS.Implementation.Strings.String_Data;
      Success : out Boolean)
   is
      use type VSS.Unicode.UTF8_Code_Unit_Offset;

      Handler    : Abstract_String_Handler'Class
        renames Abstract_String_Handler'Class (Self);
      UTF8_Data  : constant
        VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
          (0 .. VSS.Unicode.UTF8_Code_Unit_Offset (Item'Length - 1))
        with Import, Address => Item'Address;
      UTF8_Index : VSS.Unicode.UTF8_Code_Unit_Index := UTF8_Data'First;
      Code       : VSS.Unicode.Code_Point'Base;
      Error      : VSS.Implementation.UTF8_Encoding.UTF8_Decode_Error;
      Offset     : VSS.Implementation.Strings.Cursor_Offset;

   begin
         raise Program_Error;
         --  XXX VADIM
      --  Handler.Initialize (Data);
      --  Success := True;
      --
      --  loop
      --     exit when UTF8_Index > UTF8_Data'Last;
      --
      --     VSS.Implementation.UTF8_Encoding.Decode
      --       (UTF8_Data, UTF8_Index, Code, Success, Error);
      --
      --     exit when not Success;
      --
      --     Handler.Append (Data, Code, Offset);
      --  end loop;
   end From_UTF_8_String;

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

   ------------
   -- Insert --
   ------------

   not overriding procedure Insert
     (Self   : in out Abstract_String_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      From   : VSS.Implementation.Strings.Cursor;
      Item   : VSS.Implementation.Strings.String_Data;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset)
   is
      Item_Handler  : constant not null
        VSS.Implementation.Strings.Constant_Text_Handler_Access :=
          VSS.Implementation.Strings.Constant_Handler (Item);
      Item_Position : VSS.Implementation.Strings.Cursor;
      Position      : VSS.Implementation.Strings.Cursor := From;
      Code          : VSS.Unicode.Code_Point;
      Success       : Boolean with Unreferenced;
      Handler       : VSS.Implementation.Strings.Variable_Text_Handler_Access;

   begin
      if Item_Handler.Is_Empty (Item) then
         return;
      end if;

      Item_Handler.Before_First_Character (Item, Item_Position);

      while Item_Handler.Forward (Item, Item_Position) loop
         Code := Item_Handler.Element (Item, Item_Position);

         Handler := VSS.Implementation.Strings.Variable_Handler (Data);
         Handler.Insert (Data, Position, Code, Offset);
         Success :=
           VSS.Implementation.Strings.Constant_Handler
             (Data).Forward (Data, Position);
      end loop;
   end Insert;

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

   -------------
   -- Is_Null --
   -------------

   not overriding function Is_Null
     (Self : Abstract_String_Handler;
      Data : VSS.Implementation.Strings.String_Data) return Boolean is (False);

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

   ----------------------
   -- Last_UTF8_Offset --
   ----------------------

   not overriding function Last_UTF8_Offset
     (Self     : Abstract_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.UTF8_Code_Unit_Index
   is
      use type VSS.Unicode.UTF8_Code_Unit_Offset;

      Handler : Abstract_String_Handler'Class
        renames Abstract_String_Handler'Class (Self);
      Aux     : VSS.Implementation.Strings.Cursor;
      Dummy   : Boolean;

   begin
      if Position.UTF8_Offset >= 0 then
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

      return Aux.UTF8_Offset - 1;
   end Last_UTF8_Offset;

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
      Handler        : Abstract_String_Handler'Class
        renames Abstract_String_Handler'Class (Self);
      Current        : VSS.Implementation.Strings.Cursor;
      Offset         : VSS.Implementation.Strings.Cursor_Offset := (0, 0, 0);
      Target_Handler :
        VSS.Implementation.Strings.Variable_Text_Handler_Access;

   begin
         raise Program_Error;
         --  XXX VADIM
      --  if From.Index <= To.Index then
      --     VSS.Implementation.String_Configuration.In_Place_Handler.Initialize
      --       (Target);
      --     Current := From;
      --
      --     Target_Handler :=
      --       VSS.Implementation.Strings.Variable_Handler (Target);
      --     Target_Handler.Append
      --       (Target, Handler.Element (Source, Current), Offset);
      --
      --     while Handler.Forward (Source, Current)
      --       and then Current.Index <= To.Index
      --     loop
      --        Target_Handler :=
      --          VSS.Implementation.Strings.Variable_Handler (Target);
      --        Target_Handler.Append
      --          (Target, Handler.Element (Source, Current), Offset);
      --     end loop;
      --
      --  else
      --     Target := VSS.Implementation.Strings.Null_String_Data;
      --  end if;
   end Slice;

   -----------
   -- Split --
   -----------

   not overriding procedure Split
     (Self             : Abstract_String_Handler;
      Data             : VSS.Implementation.Strings.String_Data;
      Separator        : VSS.Unicode.Code_Point;
      Keep_Empty_Parts : Boolean;
      Case_Sensitivity : VSS.Strings.Case_Sensitivity;
      Items            : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access)
   is
      procedure Append;
      --  Append found substring to the results

      Handler  : Abstract_String_Handler'Class
        renames Abstract_String_Handler'Class (Self);
      Current  : VSS.Implementation.Strings.Cursor;
      Previous : VSS.Implementation.Strings.Cursor;
      From     : VSS.Implementation.Strings.Cursor;
      Success  : Boolean with Unreferenced;

      ------------
      -- Append --
      ------------

      procedure Append is
         Item : VSS.Implementation.Strings.String_Data;

      begin
         if Current.Index /= From.Index then
            Handler.Slice (Data, From, Previous, Item);
            VSS.Implementation.String_Vectors.Append (Items, Item);
            VSS.Implementation.Strings.Unreference (Item);

         elsif Keep_Empty_Parts then
            VSS.Implementation.String_Vectors.Append
              (Items, VSS.Implementation.Strings.Null_String_Data);
         end if;
      end Append;

   begin
      Handler.Before_First_Character (Data, From);
      Success := Handler.Forward (Data, From);

      Handler.Before_First_Character (Data, Current);
      Previous := Current;

      while Handler.Forward (Data, Current) loop
         if Handler.Element (Data, Current) = Separator then
            Append;

            From    := Current;
            Success := Handler.Forward (Data, From);
         end if;

         Previous := Current;
      end loop;

      Append;
   end Split;

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

   ---------------------
   -- Unsafe_Capacity --
   ---------------------

   function Unsafe_Capacity
     (Self : Abstract_String_Handler'Class)
      return VSS.Implementation.Strings.Character_Count
   is
      pragma Warnings (Off, """Data"" overlays smaller object");
      Data : constant VSS.Implementation.Strings.String_Data
        with Import, Convention => Ada, Address => Self'Address;
      pragma Warnings (On, """Data"" overlays smaller object");

   begin
      return Data.Capacity;
   end Unsafe_Capacity;

end VSS.Implementation.Text_Handlers;
