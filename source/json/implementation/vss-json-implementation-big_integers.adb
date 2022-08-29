--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

package body VSS.JSON.Implementation.Big_Integers is

   use type Interfaces.Unsigned_64;

   procedure Scalar_Add
     (Left     : Interfaces.Unsigned_64;
      Right    : Interfaces.Unsigned_64;
      Result   : aliased out Interfaces.Unsigned_64;
      Overflow : out Interfaces.Unsigned_64);
   --  Add of two 64-bit unsigned integers. Return result and overflow.

   procedure Scalar_Multiply
     (Left     : Interfaces.Unsigned_64;
      Right    : Interfaces.Unsigned_64;
      Result   : out Interfaces.Unsigned_64;
      Overflow : in out Interfaces.Unsigned_64);
   --  Multiplication of two 64-bit unsigned integers into 128-bit values,
   --  add of carry. Result is splitted into high and low 64-bit unsigned
   --  integers. On x86_64 it is optimized into few instructions.

   procedure Add
     (Self  : in out Big_Integer;
      Value : Interfaces.Unsigned_64;
      From  : Interfaces.Integer_32);
   --  Add scalar value to bigint starting from offset. Used in grade school
   --  multiplication.

   procedure Add
     (Self : in out Big_Integer; Y : Limb_Array; From : Interfaces.Integer_32);
   --  Add bigint to bigint starting from index. Used in grade school
   --  multiplication

   procedure Multiply
     (Self : in out Big_Integer;
      Y    : Limb_Array);

   procedure Clear (Self : in out Big_Integer);

   procedure Normalize (Self : in out Big_Integer);
   --  Normalize the big integer, so most-significant zero limbs are removed.

   procedure Push
     (Self : in out Big_Integer;
      Limb : Limb_Type)
     with Pre => Self.Last < Self.Data'Last;

   procedure Resize
     (Self   : in out Big_Integer;
      Length : Interfaces.Integer_32;
      Fill   : Limb_Type);
   --  Resize the vector, without bounds checking if the new size is longer
   --  than the vector, assign value to each appended item.

   procedure Set (Self : in out Big_Integer; Value : Limb_Array)
     with Pre => Value (Value'Last) /= 0;

   procedure Shift_Left
     (Self : in out Big_Integer; Amount : Interfaces.Integer_32);
   --  Move the limbs left by `n` bits.

   procedure Shift_Left_Bits
     (Self : in out Big_Integer; Amount : Interfaces.Integer_32)
     with Pre => Amount in 1 .. Limb_Type'Size - 1;
   --  Shift left each limb n bits, carrying over to the new limb.

   procedure Shift_Left_Limbs
     (Self : in out Big_Integer; Amount : Interfaces.Integer_32)
     with Pre => Amount in Big_Integer_Limb_Array'Range
                   and Self.Last + Amount in Big_Integer_Limb_Array'Range;
   --  Move the limbs left by `n` limbs.

   function clz
     (Value : Interfaces.Unsigned_64) return Interfaces.Integer_32
     with Import,
          Convention    => Intrinsic,
          External_Name => "__builtin_clzll";

   ---------
   -- Add --
   ---------

   procedure Add
     (Self  : in out Big_Integer;
      Value : Interfaces.Unsigned_64) is
   begin
      Add (Self, Value, 0);
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (Self  : in out Big_Integer;
      Value : Interfaces.Unsigned_64;
      From  : Interfaces.Integer_32)
   is
      Carry : Interfaces.Unsigned_64 := Value;

   begin
      for J in From .. Self.Last loop
         exit when Carry = 0;

         Scalar_Add (Self.Data (J), Carry, Self.Data (J), Carry);
      end loop;

      if Carry /= 0 then
         Push (Self, Carry);
      end if;
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (Self : in out Big_Integer; Y : Limb_Array; From : Interfaces.Integer_32)
   is
      C  : Interfaces.Unsigned_64 := 0;
      C1 : Interfaces.Unsigned_64;
      C2 : Interfaces.Unsigned_64;
      XI : aliased Limb_Type;
      YI : Limb_Type;

   begin
      if Self.Last < From + Y'Length then
         Resize (Self, From + Y'Length, 0);
      end if;

      for Offset in 0 .. Interfaces.Integer_32 (Y'Length) - 1 loop
         XI := Self.Data (From + Offset);
         YI := Y (Y'First + Offset);
         C1 := 0;
         C2 := 0;

         Scalar_Add (XI, YI, XI, C1);

         if C /= 0 then
            Scalar_Add (XI, 1, XI, C2);
         end if;

         Self.Data (From + Offset) := XI;
         C := C1 or C2;
      end loop;

      if C /= 0 then
         --  Handle overflow

         Add (Self, 1, Y'Length + From);
      end if;
   end Add;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Big_Integer) is
   begin
      Self.Last := -1;
   end Clear;

   -------------
   -- Compare --
   -------------

   function Compare
     (Self  : Big_Integer;
      Other : Big_Integer) return Compare_Kind is
   begin
      if Self.Last > Other.Last then
         return Greater;

      elsif Self.Last < Other.Last then
         return Less;

      else
         for J in reverse Self.Data'First .. Self.Last loop
            declare
               X : constant Limb_Type := Self.Data (J);
               Y : constant Limb_Type := Other.Data (J);

            begin
               if X > Y then
                  return Greater;

               elsif X < Y then
                  return Less;
               end if;
            end;
         end loop;

         return Equal;
      end if;
   end Compare;

   -----------------
   -- Get_High_64 --
   -----------------

   procedure Get_High_64
     (Self      : Big_Integer;
      Value     : out Interfaces.Unsigned_64;
      Truncated : out Boolean)
   is
      Aux_L : Limb_Type;
      Aux_P : Limb_Type;
      L     : Interfaces.Integer_32;
      R     : Interfaces.Integer_32;

   begin
      if Self.Last = -1 then
         Value     := 0;
         Truncated := False;

      elsif Self.Last = Self.Data'First then
         Aux_L := Self.Data (Self.Last);
         L     := clz (Aux_L);

         Value := Interfaces.Shift_Left (Aux_L, Natural (L));
         Truncated := False;

      else
         Aux_L := Self.Data (Self.Last);
         Aux_P := Self.Data (Self.Last - 1);

         L := clz (Aux_L);

         if L = 0 then
            Truncated := Aux_P /= 0;
            Value := Aux_L;

         else
            R := 64 - L;
            Truncated := Interfaces.Shift_Left (Aux_P, Natural (L)) /= 0;
            Value :=
              Interfaces.Shift_Left (Aux_L, Natural (L))
              or Interfaces.Shift_Right (Aux_P, Natural (R));
         end if;

         Truncated :=
           @ or VSS.JSON.Implementation.Big_Integers.Non_Zero (Self, 2);
      end if;
   end Get_High_64;

   --------------
   -- Multiply --
   --------------

   procedure Multiply
     (Self  : in out Big_Integer;
      Value : Interfaces.Unsigned_64)
   is
      Carry : Interfaces.Unsigned_64 := 0;

   begin
      for J in Self.Data'First .. Self.Last loop
         Scalar_Multiply (Self.Data (J), Value, Self.Data (J), Carry);
      end loop;

      if Carry /= 0 then
         Push (Self, Carry);
      end if;
   end Multiply;

   --------------
   -- Multiply --
   --------------

   procedure Multiply
     (Self : in out Big_Integer;
      Y    : Limb_Array) is
   begin
      if Y'Length = 1 then
         Multiply (Self, Y (Y'First));

      else
         declare
            ZS : constant Big_Integer := Self;
            Y0 : Limb_Type;
            YJ : Limb_Type;
            ZI : Big_Integer;

         begin
            if Y'Length /= 0 then
               Y0 := Y (Y'First);
               Multiply (Self, Y0);

               for J in Y'First + 1 .. Y'Last loop
                  YJ := Y (J);

                  if YJ /= 0 then
                     Set (ZI, ZS.Data (ZS.Data'First .. ZS.Last));
                     Multiply (ZI, YJ);
                     Add (Self, ZI.Data (ZI.Data'First .. ZI.Last), J);
                  end if;
               end loop;
            end if;

            Normalize (Self);
         end;
      end if;
   end Multiply;

   ----------------------
   -- Multiply_Power_2 --
   ----------------------

   procedure Multiply_Power_2
     (Self : in out Big_Integer; Exponent : Interfaces.Integer_32) is
   begin
      Shift_Left (Self, Exponent);
   end Multiply_Power_2;

   ----------------------
   -- Multiply_Power_5 --
   ----------------------

   procedure Multiply_Power_5
     (Self : in out Big_Integer; Exponent : Interfaces.Integer_32)
   is
      Large_Step       : constant := 135;
      Large_Power_Of_5 : constant Limb_Array (0 .. 4) :=
        [1414648277510068013, 9180637584431281687, 4539964771860779200,
         10482974169319127550, 198276706040285095];

      Small_Step       : constant := 27;
      Small_Power_Of_5 : constant Limb_Type := 7450580596923828125;

      Tail_Power_Of_5  : constant Limb_Array (0 .. Small_Step) :=
        [1, 5, 25, 125, 625, 3125, 15625, 78125, 390625,
         1953125, 9765625, 48828125, 244140625, 1220703125,
         6103515625, 30517578125, 152587890625, 762939453125,
         3814697265625, 19073486328125, 95367431640625, 476837158203125,
         2384185791015625, 11920928955078125, 59604644775390625,
         298023223876953125, 1490116119384765625, 7450580596923828125];

      E                : Interfaces.Integer_32 := Exponent;

   begin
      while E >= Large_Step loop
         Multiply (Self, Large_Power_Of_5);
         E := @ - Large_Step;
      end loop;

      while E >= Small_Step loop
         Multiply (Self, Small_Power_Of_5);
         E := @ - Small_Step;
      end loop;

      if E /= 0 then
         Multiply (Self, Tail_Power_Of_5 (E));
      end if;
   end Multiply_Power_5;

   -----------------------
   -- Multiply_Power_10 --
   -----------------------

   procedure Multiply_Power_10
     (Self : in out Big_Integer; Exponent : Interfaces.Integer_32) is
   begin
      Multiply_Power_5 (Self, Exponent);
      Multiply_Power_2 (Self, Exponent);
   end Multiply_Power_10;

   --------------
   -- Non_Zero --
   --------------

   function Non_Zero
     (Self : Big_Integer; From : Interfaces.Integer_32) return Boolean is
   begin
      for J in reverse Self.Data'First .. Self.Last - From loop
         if Self.Data (J) /= 0 then
            return True;
         end if;
      end loop;

      return False;
   end Non_Zero;

   ---------------
   -- Normalize --
   ---------------

   procedure Normalize (Self : in out Big_Integer) is
   begin
      while Self.Last >= 0 and then Self.Data (Self.Last) = 0 loop
         Self.Last := @ - 1;
      end loop;
   end Normalize;

   ----------
   -- Push --
   ----------

   procedure Push
     (Self : in out Big_Integer;
      Limb : Limb_Type) is
   begin
      Self.Last := @ + 1;
      Self.Data (Self.Last) := Limb;
   end Push;

   ------------
   -- Resize --
   ------------

   procedure Resize
     (Self   : in out Big_Integer;
      Length : Interfaces.Integer_32;
      Fill   : Limb_Type) is
   begin
      if Length > Self.Last + 1 then
         Self.Data (Self.Last + 1 .. Length - 1) := [others => Fill];
         Self.Last := Length - 1;

      else
         Self.Last := Length - 1;
      end if;
   end Resize;

   ----------------
   -- Scalar_Add --
   ----------------

   procedure Scalar_Add
     (Left     : Interfaces.Unsigned_64;
      Right    : Interfaces.Unsigned_64;
      Result   : aliased out Interfaces.Unsigned_64;
      Overflow : out Interfaces.Unsigned_64)
   is
      function Add
        (Left   : Interfaces.Unsigned_64;
         Right  : Interfaces.Unsigned_64;
         Result : access Interfaces.Unsigned_64)
         return Boolean
        with Import,
             Convention    => Intrinsic,
             External_Name => "__builtin_add_overflow";

   begin
      Overflow := (if Add (Left, Right, Result'Access) then 1 else 0);
   end Scalar_Add;

   ---------------------
   -- Scalar_Multiply --
   ---------------------

   procedure Scalar_Multiply
     (Left     : Interfaces.Unsigned_64;
      Right    : Interfaces.Unsigned_64;
      Result   : out Interfaces.Unsigned_64;
      Overflow : in out Interfaces.Unsigned_64)
   is
      use type Interfaces.Unsigned_128;

      R : constant Interfaces.Unsigned_128 :=
        Interfaces.Unsigned_128 (Left) * Interfaces.Unsigned_128 (Right)
          + Interfaces.Unsigned_128 (Overflow);

   begin
      Result   := Interfaces.Unsigned_64 (R mod 2 ** 64);
      Overflow := Interfaces.Unsigned_64 (R / 2 ** 64);
   end Scalar_Multiply;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self : in out Big_Integer; Value : Interfaces.Unsigned_64) is
   begin
      Clear (Self);
      Push (Self, Value);
      Normalize (Self);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (Self : in out Big_Integer; Value : Limb_Array) is
   begin
      Self.Last := Value'Length - 1;
      Self.Data (Self.Data'First .. Self.Last) := Value;
   end Set;

   ----------------
   -- Shift_Left --
   ----------------

   procedure Shift_Left
     (Self : in out Big_Integer; Amount : Interfaces.Integer_32)
   is
      Reminder : constant Interfaces.Integer_32 := Amount mod Limb_Type'Size;
      Divider  : constant Interfaces.Integer_32 := Amount / Limb_Type'Size;

   begin
      if Reminder /= 0 then
         Shift_Left_Bits (Self, Reminder);
      end if;

      if Divider /= 0 then
         Shift_Left_Limbs (Self, Divider);
      end if;
   end Shift_Left;

   ---------------------
   -- Shift_Left_Bits --
   ---------------------

   procedure Shift_Left_Bits
     (Self : in out Big_Integer; Amount : Interfaces.Integer_32)
   is
      --  Internally, for each item, we shift left by n, and add the previous
      --  right shifted limb-bits.
      --  For example, we transform (for u8) shifted left 2, to:
      --       b10100100 b01000010
      --       b10 b10010001 b00001000

      Left_Amount  : constant Interfaces.Integer_32 := Amount;
      Right_Amount : constant Interfaces.Integer_32 := Limb_Type'Size - Amount;
      Previous     : Limb_Type := 0;
      Aux          : Limb_Type;

   begin
      for J in Self.Data'First .. Self.Last loop
         Aux := Self.Data (J);
         Self.Data (J) :=
           Interfaces.Shift_Left (Aux, Natural (Left_Amount))
             or Interfaces.Shift_Right (Previous, Natural (Right_Amount));
         Previous := Aux;
      end loop;

      Aux := Interfaces.Shift_Right (Previous, Natural (Right_Amount));

      if Aux /= 0 then
         Push (Self, Aux);
      end if;
   end Shift_Left_Bits;

   ----------------------
   -- Shift_Left_Limbs --
   ----------------------

   procedure Shift_Left_Limbs
     (Self : in out Big_Integer; Amount : Interfaces.Integer_32) is
   begin
      if Self.Last /= -1 then
         Self.Data (Self.Data'First + Amount .. Self.Last + Amount) :=
           Self.Data (Self.Data'First .. Self.Last);
         Self.Data (Self.Data'First .. Self.Data'First + Amount - 1) :=
           [others => 0];
         Self.Last := @ + Amount;
      end if;
   end Shift_Left_Limbs;

   ----------
   -- Size --
   ----------

   function Size (Self : Big_Integer) return Interfaces.Integer_32 is
      L : constant Interfaces.Integer_32 :=
        (if Self.Last = -1 then 0 else clz (Self.Data (Self.Last)));

   begin
      return (Self.Last + 1) * Limb_Type'Size - L;
   end Size;

end VSS.JSON.Implementation.Big_Integers;
