--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with VSS.Implementation.GCC;
with VSS.Implementation.Line_Iterators;
with VSS.Implementation.Text_Storages;
with VSS.Implementation.String_Vectors;
with VSS.Implementation.UTF8_Encoding;
with VSS.Implementation.UTF8_Strings.Mutable_Operations;
with VSS.Strings;

package body VSS.Implementation.UTF8_Strings is

   use type System.Address;
   use type VSS.Implementation.Strings.Character_Offset;
   use type VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
   use type VSS.Unicode.Code_Point;
   use type VSS.Unicode.UTF8_Code_Unit_Offset;
   use type VSS.Unicode.UTF16_Code_Unit_Offset;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Self : in out UTF8_String_Data) is
   begin
      if Is_SSO (Self) then
         Self.Storage_Address := Self.Manager'Address;
      end if;
   end Adjust;

   --------------------------
   -- After_Last_Character --
   --------------------------

   procedure After_Last_Character
     (Self     : UTF8_String_Data;
      Position : in out VSS.Implementation.Strings.Cursor) is
   begin
      Position :=
        (Index        => Self.Length + 1,
         UTF8_Offset  => Self.Size,
         UTF16_Offset => 0);
   end After_Last_Character;

   ------------------------
   -- At_First_Character --
   ------------------------

   procedure At_First_Character
     (Self     : UTF8_String_Data;
      Position : in out VSS.Implementation.Strings.Cursor)
   is
      pragma Unreferenced (Self);

   begin
      Position := (Index => 1, UTF8_Offset => 0, UTF16_Offset => 0);
   end At_First_Character;

   --------------
   -- Backward --
   --------------

   function Backward
     (Text     : UTF8_String_Data;
      Position : aliased in out VSS.Implementation.Strings.Cursor)
      return Boolean is
   begin
      if Position.Index = 0 then
         return False;

      else
         Unchecked_Backward (Text, Position);
      end if;

      return Position.Index > 0;
   end Backward;

   ----------------------------
   -- Before_First_Character --
   ----------------------------

   procedure Before_First_Character
     (Self     : UTF8_String_Data;
      Position : in out VSS.Implementation.Strings.Cursor)
   is
      pragma Unreferenced (Self);

   begin
      Position := (Index => 0, UTF8_Offset => -1, UTF16_Offset => -1);
   end Before_First_Character;

   ------------------
   -- Compute_Size --
   ------------------

   procedure Compute_Size
     (Text   : UTF8_String_Data;
      From   : VSS.Implementation.Strings.Cursor;
      To     : VSS.Implementation.Strings.Cursor;
      Size   : out VSS.Implementation.Strings.Cursor_Offset)
   is
      From_Position : aliased VSS.Implementation.Strings.Cursor;
      To_Position   : aliased VSS.Implementation.Strings.Cursor;
      Success       : Boolean with Unreferenced;

   begin
      if From.Index > To.Index then
         Size := (0, 0, 0);

      else
         if From.UTF8_Offset < 0 or From.UTF16_Offset < 0 then
            --  Some of UTF* offset of From must be resolved first.

            Before_First_Character (Text, From_Position);

            while From_Position.Index /= From.Index
              and then Forward (Text, From_Position)
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
              and then Forward (Text, To_Position)
            loop
               null;
            end loop;

         else
            To_Position := To;
         end if;

         Success := Forward (Text, To_Position);

         Size.Index_Offset := To_Position.Index - From_Position.Index;
         Size.UTF8_Offset  :=
           To_Position.UTF8_Offset - From_Position.UTF8_Offset;
         Size.UTF16_Offset :=
           To_Position.UTF16_Offset - From_Position.UTF16_Offset;
      end if;
   end Compute_Size;

   -------------
   -- Element --
   -------------

   function Element
     (Self     : UTF8_String_Data;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.Code_Point'Base
   is
      Storage : constant VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
        (0 .. Self.Size)
        with Import, Address => Self.Storage_Address;

   begin
      if Position.Index < 1
        or else Position.Index > Self.Length
      then
         return VSS.Implementation.Strings.No_Character;
      end if;

      return
        VSS.Implementation.UTF8_Encoding.Unchecked_Decode
          (Storage, Position.UTF8_Offset);
   end Element;

   ---------------
   -- Ends_With --
   ---------------

   function Ends_With
     (Text : UTF8_String_Data; Suffix : UTF8_String_Data) return Boolean is
   begin
      if Suffix.Size = 0 then
         return True;

      elsif Text.Size < Suffix.Size then
         return False;
      end if;

      declare
         Text_Storage   : constant
           VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
             (0 .. Text.Size - 1)
           with Import, Address => Text.Storage_Address;
         Suffix_Storage : constant
           VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
             (0 .. Suffix.Size - 1)
           with Import, Address => Suffix.Storage_Address;

      begin
         return
           Text_Storage (Text.Size - Suffix.Size .. Text.Size - 1)
             = Suffix_Storage;
      end;
   end Ends_With;

   ---------------
   -- Ends_With --
   ---------------

   function Ends_With
     (Text   : UTF8_String_Data;
      Suffix : VSS.Unicode.Code_Point) return Boolean is
   begin
      if Text.Size = 0 then
         return False;
      end if;

      declare
         Text_Storage : constant
           VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
             (0 .. Text.Size - 1)
           with Import, Address => Text.Storage_Address;
         Offset       : VSS.Unicode.UTF8_Code_Unit_Offset := Text.Size;
         Code         : VSS.Unicode.Code_Point;

      begin
         VSS.Implementation.UTF8_Encoding.Unchecked_Backward_Decode
           (Text_Storage, Offset, Code);

         return Code = Suffix;
      end;
   end Ends_With;

   ------------------------
   -- First_UTF16_Offset --
   ------------------------

   function First_UTF16_Offset
     (Text     : UTF8_String_Data;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.UTF16_Code_Unit_Index
   is
      Aux : aliased VSS.Implementation.Strings.Cursor;

   begin
      if Position.UTF16_Offset >= 0 then
         return Position.UTF16_Offset;

      else
         Before_First_Character (Text, Aux);

         while Aux.Index /= Position.Index
           and then Forward (Text, Aux)
         loop
            null;
         end loop;
      end if;

      return Aux.UTF16_Offset;
   end First_UTF16_Offset;

   -------------
   -- Forward --
   -------------

   function Forward
     (Text     : UTF8_String_Data;
      Position : aliased in out VSS.Implementation.Strings.Cursor)
      return Boolean is
   begin
      if Position.Index > Text.Length then
         return False;

      else
         Unchecked_Forward (Text, Position);
      end if;

      return Position.Index <= Text.Length;
   end Forward;

   ---------------------
   -- Forward_Element --
   ---------------------

   function Forward_Element
     (Text     : UTF8_String_Data;
      Position : aliased in out VSS.Implementation.Strings.Cursor;
      Element  : out VSS.Unicode.Code_Point'Base) return Boolean
   is
      Storage : constant VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
        (0 .. Text.Size)
        with Import, Address => Text.Storage_Address;
      Code    : VSS.Unicode.Code_Point'Base :=
        VSS.Implementation.Strings.No_Character;
      Result  : Boolean := False;

   begin
      if Position.Index <= Text.Length then
         Unchecked_Forward (Text, Position);

         if Position.Index <= Text.Length then
            Code :=
              VSS.Implementation.UTF8_Encoding.Unchecked_Decode
                (Storage, Position.UTF8_Offset);
            Result := True;
         end if;
      end if;

      Element := Code;

      return Result;
   end Forward_Element;

   -------------------
   -- Has_Character --
   -------------------

   function Has_Character
     (Text     : UTF8_String_Data;
      Position : VSS.Implementation.Strings.Cursor) return Boolean is
   begin
      return Position.Index in 1 .. Text.Length;
   end Has_Character;

   ----------
   -- Hash --
   ----------

   procedure Hash
     (Text      : UTF8_String_Data;
      Generator : in out VSS.Implementation.FNV_Hash.FNV_1a_Generator)
   is
      Position : aliased VSS.Implementation.Strings.Cursor;
      Code     : VSS.Unicode.Code_Point;

   begin
      Before_First_Character (Text, Position);

      while Forward (Text, Position) loop
         Code := Element (Text, Position);

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
   -- Is_Empty --
   --------------

   function Is_Empty (Self : UTF8_String_Data) return Boolean is
   begin
      return Self.Size = 0;
   end Is_Empty;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal
     (Left : UTF8_String_Data; Right : UTF8_String_Data) return Boolean is
   begin
      if Left.Size /= Right.Size then
         return False;

      elsif Left.Size = 0 then
         return True;

      else
         declare
            Left_Storage  : constant
              VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
                (0 .. Left.Size - 1)
              with Import, Address => Left.Storage_Address;
            Right_Storage : constant
              VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
                (0 .. Right.Size - 1)
              with Import, Address => Right.Storage_Address;

         begin
            return Left_Storage = Right_Storage;
         end;
      end if;
   end Is_Equal;

   -------------
   -- Is_Less --
   -------------

   function Is_Less
     (Left : UTF8_String_Data; Right : UTF8_String_Data) return Boolean is
   begin
      if Is_Empty (Left) then
         return not Is_Empty (Right);

      elsif Is_Empty (Right) then
         return False;
      end if;

      declare
         Left_Storage  : constant
           VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
             (0 .. Left.Size - 1)
           with Import, Address => Left.Storage_Address;
         Right_Storage : constant
           VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
             (0 .. Right.Size - 1)
           with Import, Address => Right.Storage_Address;

      begin
         return Left_Storage < Right_Storage;
      end;
   end Is_Less;

   ----------------------
   -- Is_Less_Or_Equal --
   ----------------------

   function Is_Less_Or_Equal
     (Left : UTF8_String_Data; Right : UTF8_String_Data) return Boolean is
   begin
      if Is_Empty (Left) then
         return True;

      elsif Is_Empty (Right) then
         return False;
      end if;

      declare
         Left_Storage  : constant
           VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
             (0 .. Left.Size - 1)
           with Import, Address => Left.Storage_Address;
         Right_Storage : constant
           VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
             (0 .. Right.Size - 1)
           with Import, Address => Right.Storage_Address;

      begin
         return Left_Storage <= Right_Storage;
      end;
   end Is_Less_Or_Equal;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Self : UTF8_String_Data) return Boolean is
   begin
      return Self.Storage_Address = System.Null_Address;
   end Is_Null;

   ------------
   -- Is_SSO --
   ------------

   function Is_SSO (Self : UTF8_String_Data) return Boolean is
      use type Interfaces.Unsigned_32;

   begin
      return Self.Flags = 0;
   end Is_SSO;

   -----------------------
   -- Last_UTF16_Offset --
   -----------------------

   function Last_UTF16_Offset
     (Text     : UTF8_String_Data;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.UTF16_Code_Unit_Index
   is
      Aux   : aliased VSS.Implementation.Strings.Cursor;
      Dummy : Boolean;

   begin
      if Position.UTF16_Offset >= 0 then
         Aux := Position;

      else
         Before_First_Character (Text, Aux);

         while Aux.Index /= Position.Index
           and then Forward (Text, Aux)
         loop
            null;
         end loop;
      end if;

      Dummy := Forward (Text, Aux);

      return Aux.UTF16_Offset - 1;
   end Last_UTF16_Offset;

   ----------------------
   -- Last_UTF8_Offset --
   ----------------------

   function Last_UTF8_Offset
     (Text     : UTF8_String_Data;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.UTF8_Code_Unit_Index
   is
      Aux   : aliased VSS.Implementation.Strings.Cursor := Position;
      Dummy : Boolean;

   begin
      Dummy := Forward (Text, Aux);

      return Aux.UTF8_Offset - 1;
   end Last_UTF8_Offset;

   ---------------
   -- Reference --
   ---------------

   procedure Reference (Self : in out UTF8_String_Data) is
   begin
      if Is_SSO (Self) then
         if Self.Storage_Address /= System.Null_Address then
            Self.Storage_Address := Self.Manager'Address;
         end if;

      else
         declare
            Manager :
              VSS.Implementation.Text_Storages.Abstract_Text_Storage
              with Import, Address => Self.Manager'Address;

         begin
            VSS.Implementation.Text_Storages.Abstract_Text_Storage'Class
              (Manager).Reference;
         end;
      end if;
   end Reference;

   -----------
   -- Slice --
   -----------

   procedure Slice
     (Text   : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      From   : VSS.Implementation.Strings.Cursor;
      To     : VSS.Implementation.Strings.Cursor;
      Result : out VSS.Implementation.UTF8_Strings.UTF8_String_Data)
   is
      Text_Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
        (0 .. Text.Size)
        with Import, Address => Text.Storage_Address;
      After        : VSS.Implementation.Strings.Cursor := To;
      Size         : VSS.Unicode.UTF8_Code_Unit_Count;
      Length       : VSS.Implementation.Strings.Character_Count;

   begin
      if From.Index > To.Index then
         Result := Default_UTF8_String_Data;

         return;
      end if;

      if To.Index <= Text.Length then
         Unchecked_Forward (Text, After);
      end if;

      Size   := After.UTF8_Offset - From.UTF8_Offset;
      Length := After.Index - From.Index;

      Mutable_Operations.Initialize (Result, Size);

      declare
         Result_Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
           (0 .. Size)
           with Import, Address => Result.Storage_Address;

      begin
         Result_Storage (0 .. Size - 1) :=
           Text_Storage (From.UTF8_Offset .. After.UTF8_Offset - 1);
         Result.Size   := Size;
         Result.Length := Length;
         Result_Storage (Result.Size) := 16#00#;
      end;
   end Slice;

   -----------
   -- Split --
   -----------

   procedure Split
     (Text             : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Separator        : VSS.Unicode.Code_Point;
      Keep_Empty_Parts : Boolean;
      Items            : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access)
   is
      procedure Append;
      --  Append found substring to the results

      Current  : aliased VSS.Implementation.Strings.Cursor;
      Previous : VSS.Implementation.Strings.Cursor;
      From     : aliased VSS.Implementation.Strings.Cursor;
      Success  : Boolean with Unreferenced;

      ------------
      -- Append --
      ------------

      procedure Append is
         Item : VSS.Implementation.UTF8_Strings.UTF8_String_Data;

      begin
         if Current.Index /= From.Index then
            Slice (Text, From, Previous, Item);
            VSS.Implementation.String_Vectors.Append_And_Move_Ownership
              (Items, Item);

         elsif Keep_Empty_Parts then
            VSS.Implementation.String_Vectors.Append (Items, Item);
         end if;
      end Append;

   begin
      Before_First_Character (Text, From);
      Success := Forward (Text, From);

      Before_First_Character (Text, Current);
      Previous := Current;

      while Forward (Text, Current) loop
         if Element (Text, Current) = Separator then
            Append;

            From    := Current;
            Success := Forward (Text, From);
         end if;

         Previous := Current;
      end loop;

      Append;
   end Split;

   -----------------
   -- Split_Lines --
   -----------------

   procedure Split_Lines
     (Text            : UTF8_String_Data;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean;
      Lines           : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access)
   is
      Initial    : VSS.Implementation.Strings.Cursor :=
        (Index => 0, UTF8_Offset => -1, UTF16_Offset => -1);
      At_First   : aliased VSS.Implementation.Strings.Cursor;
      At_Last    : aliased VSS.Implementation.Strings.Cursor;
      After_Last : aliased VSS.Implementation.Strings.Cursor;
      Terminator : VSS.Implementation.Strings.Cursor;

   begin
      VSS.Implementation.String_Vectors.Unreference (Lines);

      while VSS.Implementation.Line_Iterators.Forward
        (Text,
         Terminators,
         Initial,
         At_First,
         At_Last,
         Terminator)
      loop
         Initial := At_Last;

         if VSS.Implementation.Strings.Is_Invalid (Terminator) then
            After_Last := At_Last;
            Unchecked_Forward (Text, After_Last);
            --  Dummy      := Text.Forward (After_Last);

         elsif Keep_Terminator then
            After_Last := At_Last;
            Unchecked_Forward (Text, After_Last);
            --  Dummy      := Text.Forward (After_Last);

         else
            After_Last := Terminator;
         end if;

         declare
            Storage   : constant
              VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
                (0 .. Text.Size)
              with Import, Address => Text.Storage_Address;
            Line_Text : VSS.Implementation.UTF8_Strings.UTF8_String_Data;

         begin
            if After_Last.UTF8_Offset - At_First.UTF8_Offset = 0 then
               VSS.Implementation.UTF8_Strings.Mutable_Operations.Initialize
                 (Line_Text, 0);

            else
               VSS.Implementation.UTF8_Strings.Mutable_Operations.Initialize
                 (Line_Text,
                  Storage (At_First.UTF8_Offset .. After_Last.UTF8_Offset - 1),
                  After_Last.Index - At_First.Index);
            end if;

            VSS.Implementation.String_Vectors.Append_And_Move_Ownership
              (Lines, Line_Text);
         end;
      end loop;
   end Split_Lines;

   -----------------
   -- Starts_With --
   -----------------

   function Starts_With
     (Text : UTF8_String_Data; Prefix : UTF8_String_Data) return Boolean is
   begin
      if Prefix.Size = 0 then
         return True;

      elsif Text.Size < Prefix.Size then
         return False;
      end if;

      declare
         Text_Storage   : constant
           VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
             (0 .. Prefix.Size - 1)
           with Import, Address => Text.Storage_Address;
         Prefix_Storage : constant
           VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
             (0 .. Prefix.Size - 1)
           with Import, Address => Prefix.Storage_Address;

      begin
         return Text_Storage = Prefix_Storage;
      end;
   end Starts_With;

   -----------------
   -- Starts_With --
   -----------------

   function Starts_With
     (Text   : UTF8_String_Data;
      Prefix : VSS.Unicode.Code_Point) return Boolean is
   begin
      if Text.Size = 0 then
         return False;
      end if;

      declare
         Text_Storage : constant
           VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
             (0 .. VSS.Implementation.UTF8_Encoding
                     .Code_Point_Max_Encoded_Length - 1)
           with Import, Address => Text.Storage_Address;

      begin
         return
           VSS.Implementation.UTF8_Encoding.Unchecked_Decode (Text_Storage, 0)
              = Prefix;
      end;
   end Starts_With;

   ---------------------
   -- To_UTF_8_String --
   ---------------------

   function To_UTF_8_String
     (Self : UTF8_String_Data) return Ada.Strings.UTF_Encoding.UTF_8_String is
   begin
      if Self.Size = 0 then
         return "";

      else
         declare
            Result : constant Ada.Strings.UTF_Encoding.UTF_8_String
              (1 .. Natural (Self.Size))
                with Import, Address => Self.Storage_Address;
         begin
            return Result;
         end;
      end if;
   end To_UTF_8_String;

   ------------------------
   -- Unchecked_Backward --
   ------------------------

   procedure Unchecked_Backward
     (Self     : UTF8_String_Data;
      Position : in out VSS.Implementation.Strings.Cursor)
   is
      Storage : constant VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
        (0 .. Self.Size)
        with Import, Address => Self.Storage_Address;

   begin
      Position.Index        := Position.Index - 1;
      Position.UTF8_Offset  := Position.UTF8_Offset - 1;
      Position.UTF16_Offset := Position.UTF16_Offset - 1;

      if Position.Index /= 0 then
         loop
            declare
               Code : constant VSS.Unicode.UTF8_Code_Unit :=
                 Storage (Position.UTF8_Offset);

            begin
               case Code is
                  when 16#80# .. 16#BF# =>
                     Position.UTF8_Offset  := Position.UTF8_Offset - 1;

                  when 16#00# .. 16#7F#
                     | 16#C2# .. 16#DF#
                     | 16#E0# .. 16#EF# =>

                     exit;

                  when 16#F0# .. 16#F4# =>
                     Position.UTF16_Offset := Position.UTF16_Offset - 1;
                     exit;

                  when others =>
                     raise Program_Error with "string data is corrupted";
               end case;
            end;
         end loop;
      end if;
   end Unchecked_Backward;

   -------------------------------
   -- Unchecked_Backward_Decode --
   -------------------------------

   procedure Unchecked_Backward_Decode
     (Text   : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Offset : in out VSS.Unicode.UTF8_Code_Unit_Index;
      Code   : out VSS.Unicode.Code_Point)
   is
      Storage : constant VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
        (0 .. Text.Size)
        with Import, Address => Text.Storage_Address;

   begin
      VSS.Implementation.UTF8_Encoding.Unchecked_Backward_Decode
        (Storage, Offset, Code);
   end Unchecked_Backward_Decode;

   ------------------------------
   -- Unchecked_Decode_Forward --
   ------------------------------

   procedure Unchecked_Decode_Forward
     (Text   : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Offset : in out VSS.Unicode.UTF8_Code_Unit_Index;
      Code   : out VSS.Unicode.Code_Point)
   is
      Storage : constant VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
        (0 .. Text.Size)
        with Import, Address => Text.Storage_Address;

   begin
      VSS.Implementation.UTF8_Encoding.Unchecked_Decode_Forward
        (Storage, Offset, Code);
   end Unchecked_Decode_Forward;

   -----------------------
   -- Unchecked_Forward --
   -----------------------

   procedure Unchecked_Forward
     (Self     : UTF8_String_Data;
      Position : in out VSS.Implementation.Strings.Cursor)
   is
      pragma Suppress (Overflow_Check);
      pragma Suppress (Range_Check);
      --  These checks slowdown execution, but can happen on large text
      --  data or invalid input only. Would be nice to verify that they
      --  are impossible, or modify something to make them impossible.

      Storage : constant VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
        (0 .. Self.Size)
        with Import, Address => Self.Storage_Address;

   begin
      Position.Index := @ + 1;

      if Position.Index = 1 then
         Position.UTF8_Offset  := 0;
         Position.UTF16_Offset := 0;

         return;
      end if;

      declare
         use type Interfaces.Integer_32;
         use type VSS.Unicode.UTF8_Code_Unit;

         --  This code is based on the fact that starting byte of the
         --  multibyte sequence in UTF-8 has N most significant bits set
         --  to one followed by zero bit. So, first byte of the sequence
         --  is negated and number of leading zero bits is counting.

         Code   : constant VSS.Unicode.UTF8_Code_Unit :=
           Storage (Position.UTF8_Offset);
         Length : constant Interfaces.Integer_32 :=
           VSS.Implementation.GCC.clz (Interfaces.Unsigned_32 (not Code))
             - 24;

      begin
         if Code <= 16#7F# then
            Position.UTF8_Offset  := @ + 1;
            Position.UTF16_Offset := @ + 1;

         else
            Position.UTF8_Offset  :=
              @ + VSS.Unicode.UTF8_Code_Unit_Offset (Length);
            Position.UTF16_Offset :=
              @ + VSS.Unicode.UTF16_Code_Unit_Offset (Length / 4 + 1);
         end if;
      end;
   end Unchecked_Forward;

   -----------------
   -- Unreference --
   -----------------

   procedure Unreference (Self : in out UTF8_String_Data) is
   begin
      if not Is_SSO (Self) then
         declare
            Manager :
              VSS.Implementation.Text_Storages.Abstract_Text_Storage
              with Import, Address => Self.Manager'Address;

         begin
            VSS.Implementation.Text_Storages.Abstract_Text_Storage'Class
              (Manager).Unreference;
         end;
      end if;

      Self.Manager         := [others => 0];
      Self.Storage_Address := System.Null_Address;
      Self.Flags           := 0;
      Self.Size            := 0;
      Self.Length          := 0;
   end Unreference;

end VSS.Implementation.UTF8_Strings;
