--
--  Copyright (C) 2020, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Warnings (Off, "is an internal GNAT unit");
with Ada.Strings.Unbounded.Aux;
pragma Warnings (On, "is an internal GNAT unit");

package body VSS.Stream_Element_Vectors.Conversions is

   ------------------------------
   -- To_Stream_Element_Vector --
   ------------------------------

   function To_Stream_Element_Vector
     (Item : Ada.Streams.Stream_Element_Array) return Stream_Element_Vector is
   begin
      return Result : Stream_Element_Vector do
         declare
            Length : constant Ada.Streams.Stream_Element_Offset :=
              Ada.Streams.Stream_Element_Offset (Item'Length);

         begin
            Result.Data :=
              new Data_Record'
                (Size    => Length,
                 Length  => Length,
                 Storage => Item);
         end;
      end return;
   end To_Stream_Element_Vector;

   -------------------------------------
   -- Unchecked_From_Unbounded_String --
   -------------------------------------

   function Unchecked_From_Unbounded_String
     (Item : Ada.Strings.Unbounded.Unbounded_String)
      return Stream_Element_Vector
   is
      Source        : Ada.Strings.Unbounded.Aux.Big_String_Access;
      Source_Length : Natural;

   begin
      Ada.Strings.Unbounded.Aux.Get_String (Item, Source, Source_Length);

      return Result : Stream_Element_Vector do
         declare
            Length : constant Ada.Streams.Stream_Element_Offset :=
              Ada.Streams.Stream_Element_Offset (Source_Length);

            subtype Bounded_Stream_Element_Array is
              Ada.Streams.Stream_Element_Array (1 .. Length);

            Aux : Bounded_Stream_Element_Array
              with Address => Source.all'Address;

         begin
            Result.Data :=
              new Data_Record'
                (Size    => Length,
                 Length  => Length,
                 Storage => Aux);
         end;
      end return;
   end Unchecked_From_Unbounded_String;

   -------------------------
   -- Unchecked_To_String --
   -------------------------

   function Unchecked_To_String
     (Item : Stream_Element_Vector'Class) return String
   is
      use type Ada.Streams.Stream_Element_Offset;

   begin
      if Item.Data = null or else Item.Data.Length = 0 then
         return "";
      end if;

      declare
         Result : String (1 .. Natural (Item.Data.Length))
           with Address => Item.Data.Storage (Item.Data.Storage'First)'Address;

      begin
         return Result;
      end;
   end Unchecked_To_String;

end VSS.Stream_Element_Vectors.Conversions;
