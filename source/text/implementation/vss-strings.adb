------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

with VSS.Implementation.FNV_Hash;
with VSS.Strings.Configuration;
with VSS.Strings.Iterators.Characters.Internals;
with VSS.Strings.Texts;

package body VSS.Strings is

   ---------
   -- "<" --
   ---------

   function "<"
     (Left  : Virtual_String;
      Right : Virtual_String) return Boolean
   is
      Left_Handler  : constant access
        VSS.Implementation.String_Handlers.Abstract_String_Handler'Class :=
          Left.Handler;
      Right_Handler : constant access
        VSS.Implementation.String_Handlers.Abstract_String_Handler'Class :=
          Right.Handler;

   begin
      if Right_Handler = null then
         return False;

      elsif Left_Handler = null then
         return not Right_Handler.Is_Empty (Right.Data);

      else
         return
           Left_Handler.Is_Less (Left.Data, Right_Handler.all, Right.Data);
      end if;
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<="
     (Left  : Virtual_String;
      Right : Virtual_String) return Boolean
   is
      Left_Handler  : constant access
        VSS.Implementation.String_Handlers.Abstract_String_Handler'Class :=
          Left.Handler;
      Right_Handler : constant access
        VSS.Implementation.String_Handlers.Abstract_String_Handler'Class :=
          Right.Handler;

   begin
      if Left_Handler = null then
         return True;

      elsif Right_Handler = null then
         return Left_Handler.Is_Empty (Left.Data);

      else
         return
           Left_Handler.Is_Less_Or_Equal
             (Left.Data, Right_Handler.all, Right.Data);
      end if;
   end "<=";

   ---------
   -- "=" --
   ---------

   overriding function "="
     (Left  : Virtual_String;
      Right : Virtual_String) return Boolean
   is
      Left_Handler  : constant access
        VSS.Implementation.String_Handlers.Abstract_String_Handler'Class :=
          Left.Handler;
      Right_Handler : constant access
        VSS.Implementation.String_Handlers.Abstract_String_Handler'Class :=
          Right.Handler;

   begin
      if Left_Handler = null and Right_Handler = null then
         return True;

      elsif Left_Handler = null xor Right_Handler = null then
         return Left.Is_Empty and Right.Is_Empty;

      else
         return
           Left_Handler.Is_Equal (Left.Data, Right_Handler.all, Right.Data);
      end if;
   end "=";

   ---------
   -- ">" --
   ---------

   function ">"
     (Left  : Virtual_String;
      Right : Virtual_String) return Boolean
   is
      Left_Handler  : constant access
        VSS.Implementation.String_Handlers.Abstract_String_Handler'Class :=
          Left.Handler;
      Right_Handler : constant access
        VSS.Implementation.String_Handlers.Abstract_String_Handler'Class :=
          Right.Handler;

   begin
      if Left_Handler = null then
         return False;

      elsif Right_Handler = null then
         return not Left_Handler.Is_Empty (Left.Data);

      else
         return
           not Left_Handler.Is_Less_Or_Equal
                 (Left.Data, Right_Handler.all, Right.Data);
      end if;
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">="
     (Left  : Virtual_String;
      Right : Virtual_String) return Boolean
   is
      Left_Handler  : constant access
        VSS.Implementation.String_Handlers.Abstract_String_Handler'Class :=
          Left.Handler;
      Right_Handler : constant access
        VSS.Implementation.String_Handlers.Abstract_String_Handler'Class :=
          Right.Handler;

   begin
      if Right_Handler = null then
         return True;

      elsif Left_Handler = null then
         return Right_Handler.Is_Empty (Right.Data);

      else
         return
           not Left_Handler.Is_Less
                 (Left.Data, Right_Handler.all, Right.Data);
      end if;
   end ">=";

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Virtual_String) is
      Handler : constant access
        VSS.Implementation.String_Handlers.Abstract_String_Handler'Class :=
          Self.Handler;

   begin
      if Handler /= null then
         Handler.Reference (Self.Data);
      end if;
   end Adjust;

   ----------------------
   -- Character_Length --
   ----------------------

   function Character_Length
     (Self : Virtual_String'Class) return Character_Count
   is
      Handler : constant access
        VSS.Implementation.String_Handlers.Abstract_String_Handler'Class :=
          Self.Handler;

   begin
      return
        (if Handler = null
         then 0
         else Character_Count (Handler.Length (Self.Data)));
   end Character_Length;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Self  : in out Referal_Limited_Base'Class;
      Owner : not null Magic_String_Access) is
   begin
      if Owner.Head = null then
         Owner.Head := Self'Unchecked_Access;
         Owner.Tail := Self'Unchecked_Access;

      else
         Owner.Tail.Next := Self'Unchecked_Access;
         Self.Previous := Owner.Tail;
         Owner.Tail := Self'Unchecked_Access;
      end if;

      Self.Owner := Owner;
   end Connect;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect
     (Self : in out Referal_Limited_Base'Class) is
   begin
      if Self.Owner /= null then
         if Self.Owner.Head = Self'Unchecked_Access then
            Self.Owner.Head := Self.Owner.Head.Next;
         end if;

         if Self.Owner.Tail = Self'Unchecked_Access then
            Self.Owner.Tail := Self.Owner.Tail.Previous;
         end if;

         if Self.Previous /= null then
            Self.Previous.Next := Self.Next;
         end if;

         if Self.Next /= null then
            Self.Next.Previous := Self.Previous;
         end if;

         Self.Owner    := null;
         Self.Previous := null;
         Self.Next     := null;
      end if;
   end Disconnect;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Virtual_String) is
      Handler : constant access
        VSS.Implementation.String_Handlers.Abstract_String_Handler'Class :=
          Self.Handler;

   begin
      --  Invalidate and disconnect all referals

      while Self.Head /= null loop
         Self.Head.Invalidate;
         Self.Head.Disconnect;
      end loop;

      --  Unreference shared data

      if Handler /= null then
         Handler.Unreference (Self.Data);
      end if;
   end Finalize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Referal_Limited_Base) is
   begin
      if Self.Owner /= null then
         Referal_Limited_Base'Class (Self).Invalidate;
         Self.Disconnect;
      end if;
   end Finalize;

   ---------------------
   -- First_Character --
   ---------------------

   function First_Character
     (Self : Virtual_String'Class)
      return VSS.Strings.Iterators.Characters.Character_Iterator is
   begin
      return
        VSS.Strings.Iterators.Characters.Internals.First_Character (Self);
   end First_Character;

   -------------
   -- Handler --
   -------------

   function Handler
     (Self : Virtual_String'Class)
      return access
        VSS.Implementation.String_Handlers.Abstract_String_Handler'Class is
   begin
      if Self.Data.In_Place then
         return VSS.Strings.Configuration.In_Place_Handler;

      else
         return Self.Data.Handler;
      end if;
   end Handler;

   ----------
   -- Hash --
   ----------

   function Hash (Self : Virtual_String'Class) return Hash_Type is
      Handler   : constant access
        VSS.Implementation.String_Handlers.Abstract_String_Handler'Class :=
          Self.Handler;
      Generator : VSS.Implementation.FNV_Hash.FNV_1a_Generator;

   begin
      if Handler /= null then
         Handler.Hash (Self.Data, Generator);
      end if;

      return
        VSS.Strings.Hash_Type (VSS.Implementation.FNV_Hash.Value (Generator));
   end Hash;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Virtual_String'Class) return Boolean is
      Handler : constant access
        VSS.Implementation.String_Handlers.Abstract_String_Handler'Class :=
          Self.Handler;

   begin
      return Handler = null or else Handler.Is_Empty (Self.Data);
   end Is_Empty;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Self : Virtual_String'Class) return Boolean is
   begin
      return Self.Handler = null;
   end Is_Null;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Self   : out Virtual_String) is
   begin
      raise Program_Error with "Not implemented";
   end Read;

   ------------
   -- Starts --
   ------------

   function Starts
     (Self   : Virtual_String'Class;
      Prefix : Virtual_String'Class) return Boolean
   is
      use type VSS.Implementation.Strings.Character_Count;

      Self_Handler   : constant access
        VSS.Implementation.String_Handlers.Abstract_String_Handler'Class :=
          Self.Handler;
      Prefix_Handler : constant access
        VSS.Implementation.String_Handlers.Abstract_String_Handler'Class :=
          Prefix.Handler;

   begin
      if Prefix_Handler = null then
         return True;

      elsif Self_Handler = null then
         return Prefix_Handler.Is_Empty (Prefix.Data);

      elsif Self_Handler.Length (Self.Data)
              < Prefix_Handler.Length (Prefix.Data)
      then
         return False;

      else
         return
           Self_Handler.Starts (Self.Data, Prefix_Handler.all, Prefix.Data);
      end if;
   end Starts;

   -------------------
   -- To_Magic_Text --
   -------------------

   function To_Magic_Text
     (Self : Virtual_String) return VSS.Strings.Texts.Magic_Text is
   begin
      return (Ada.Finalization.Controlled with
                Data => <>,
                --  Data => (if Self.Data = null
                --           then null
                --           else Self.Data.To_Text),
                Head => null,
                Tail => null);
   end To_Magic_Text;

   -----------------------
   -- To_Virtual_String --
   -----------------------

   function To_Virtual_String
     (Item : Wide_Wide_String) return Virtual_String
   is
      Success : Boolean;

   begin
      return Result : Virtual_String do
         --  First, attempt to place data in the storage inside the object of
         --  Magic_String type.

         VSS.Strings.Configuration.In_Place_Handler.From_Wide_Wide_String
           (Item, Result.Data, Success);

         if not Success then
            --  Operation may fail for two reasons: source data is not
            --  well-formed UTF-8 or there is not enoght memory to store
            --  string in in-place storage.

            VSS.Strings.Configuration.Default_Handler.From_Wide_Wide_String
              (Item, Result.Data, Success);
         end if;

         if not Success then
            raise Constraint_Error with "Ill-formed UTF-8 data";
         end if;
      end return;
   end To_Virtual_String;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Self   : Virtual_String) is
   begin
      raise Program_Error with "Not implemented";
   end Write;

end VSS.Strings;
