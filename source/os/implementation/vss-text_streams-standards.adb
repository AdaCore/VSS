--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Standard input/output streams as text streams.

with Ada.Streams;
with Interfaces.C_Streams;

with VSS.Implementation.Line_Terminator;
with VSS.Stream_Element_Vectors.Internals;
with VSS.Strings.Converters.Encoders;

package body VSS.Text_Streams.Standards is

   use type Interfaces.C_Streams.FILEs;
   use type Interfaces.C_Streams.size_t;

   type Standard_Output_Text_Stream is
     limited new VSS.Text_Streams.Output_Text_Stream with
   record
      Encoder    : VSS.Strings.Converters.Encoders.Virtual_String_Encoder;
      Terminator : VSS.Strings.Line_Terminator := VSS.Strings.LF;
      Stream     : Interfaces.C_Streams.FILEs :=
        Interfaces.C_Streams.NULL_Stream;
      Error      : VSS.Strings.Virtual_String;
   end record;

   overriding procedure Put
     (Self    : in out Standard_Output_Text_Stream;
      Item    : VSS.Characters.Virtual_Character;
      Success : in out Boolean);

   overriding procedure Put
     (Self    : in out Standard_Output_Text_Stream;
      Item    : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   overriding procedure Put_Line
     (Self    : in out Standard_Output_Text_Stream;
      Item    : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   overriding procedure New_Line
     (Self    : in out Standard_Output_Text_Stream;
      Success : in out Boolean);

   overriding function Has_Error
     (Self : Standard_Output_Text_Stream) return Boolean;

   overriding function Error_Message
     (Self : Standard_Output_Text_Stream) return VSS.Strings.Virtual_String;

   -------------------
   -- Error_Message --
   -------------------

   overriding function Error_Message
     (Self : Standard_Output_Text_Stream) return VSS.Strings.Virtual_String is
   begin
      return Self.Error;
   end Error_Message;

   ---------------
   -- Has_Error --
   ---------------

   overriding function Has_Error
     (Self : Standard_Output_Text_Stream) return Boolean is
   begin
      return not Self.Error.Is_Empty;
   end Has_Error;

   --------------
   -- New_Line --
   --------------

   overriding procedure New_Line
     (Self    : in out Standard_Output_Text_Stream;
      Success : in out Boolean) is
   begin
      Self.Put
        (VSS.Implementation.Line_Terminator.Sequence (Self.Terminator),
         Success);
   end New_Line;

   ---------
   -- Put --
   ---------

   overriding procedure Put
     (Self    : in out Standard_Output_Text_Stream;
      Item    : VSS.Characters.Virtual_Character;
      Success : in out Boolean)
   is
      Buffer : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      Length : Ada.Streams.Stream_Element_Count;
      Data   :
        VSS.Stream_Element_Vectors.Internals.Stream_Element_Array_Access;
      Size   : Interfaces.C_Streams.size_t;

   begin
      if not Success or not Self.Error.Is_Empty then
         Success := False;

         return;
      end if;

      if Self.Stream = Interfaces.C_Streams.NULL_Stream then
         Self.Error := "File is not open";
         Success    := False;

         return;
      end if;

      Self.Encoder.Encode (Item, Buffer);

      if not Buffer.Is_Empty then
         VSS.Stream_Element_Vectors.Internals.Data_Constant_Access
           (Buffer, Length, Data);

         Size :=
           Interfaces.C_Streams.fwrite
             (Data (Data'First)'Address,
              1,
              Interfaces.C_Streams.size_t (Length),
              Self.Stream);

         if Size /= Interfaces.C_Streams.size_t (Length) then
            Self.Error := "File IO error";
            Success    := False;
         end if;
      end if;
   end Put;

   ---------
   -- Put --
   ---------

   overriding procedure Put
     (Self    : in out Standard_Output_Text_Stream;
      Item    : VSS.Strings.Virtual_String;
      Success : in out Boolean)
   is
      Buffer : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      Length : Ada.Streams.Stream_Element_Count;
      Data   :
        VSS.Stream_Element_Vectors.Internals.Stream_Element_Array_Access;
      Size   : Interfaces.C_Streams.size_t;

   begin
      if not Success or not Self.Error.Is_Empty then
         Success := False;

         return;
      end if;

      if Self.Stream = Interfaces.C_Streams.NULL_Stream then
         Self.Error := "File is not open";
         Success    := False;

         return;
      end if;

      Self.Encoder.Encode (Item, Buffer);

      if not Buffer.Is_Empty then
         VSS.Stream_Element_Vectors.Internals.Data_Constant_Access
           (Buffer, Length, Data);

         Size :=
           Interfaces.C_Streams.fwrite
             (Data (Data'First)'Address,
              1,
              Interfaces.C_Streams.size_t (Length),
              Self.Stream);

         if Size /= Interfaces.C_Streams.size_t (Length) then
            Self.Error := "File IO error";
            Success    := False;
         end if;
      end if;
   end Put;

   --------------
   -- Put_Line --
   --------------

   overriding procedure Put_Line
     (Self    : in out Standard_Output_Text_Stream;
      Item    : VSS.Strings.Virtual_String;
      Success : in out Boolean) is
   begin
      Self.Put (Item, Success);
      Self.New_Line (Success);
   end Put_Line;
   --------------------
   -- Standard_Error --
   --------------------

   function Standard_Error return VSS.Text_Streams.Output_Text_Stream'Class is
   begin
      return Result : Standard_Output_Text_Stream :=
        (Encoder     => <>,
         Terminator  => VSS.Strings.LF,
         Stream      => Interfaces.C_Streams.stderr,
         Error       => <>)
      do
         Result.Encoder.Initialize ("utf-8");
         --  XXX Encoding must be detected from application's locale.
         --  XXX Line terminator must be detected by platform.
      end return;
   end Standard_Error;

   ---------------------
   -- Standard_Output --
   ---------------------

   function Standard_Output return VSS.Text_Streams.Output_Text_Stream'Class is
   begin
      return Result : Standard_Output_Text_Stream :=
        (Encoder     => <>,
         Terminator  => VSS.Strings.LF,
         Stream      => Interfaces.C_Streams.stdout,
         Error       => <>)
      do
         Result.Encoder.Initialize ("utf-8");
         --  XXX Encoding must be detected from application's locale.
         --  XXX Line terminator must be detected by platform.
      end return;
   end Standard_Output;

end VSS.Text_Streams.Standards;
