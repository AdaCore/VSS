--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Unchecked_Deallocation;

with VSS.Stream_Element_Vectors.Internals;
with VSS.Strings.Converters.Decoders.ISO88591;
with VSS.Strings.Converters.Decoders.ISO88592;
with VSS.Strings.Converters.Decoders.UTF8;

package body VSS.Strings.Converters.Decoders is

   procedure Free is
     new Ada.Unchecked_Deallocation (Abstract_Decoder'Class, Decoder_Access);

   type Decoder_Factory is
     access function (Flags : Converter_Flags) return Decoder_Access;
   --  Factory function to create decoder.

   type Registry_Record is record
      Encoding_Name : VSS.Strings.Virtual_String;
      Factory       : Decoder_Factory;
   end record;

   --  This registry contains encoding names and aliases defined by
   --  IANA character sets database
   --
   --  https://www.iana.org/assignments/character-sets/character-sets.xhtml
   --
   --  and Encodings specification
   --
   --  https://encoding.spec.whatwg.org/
   --
   --  All names are transformed by the algorithm used by the To_Encoding_Name
   --  function, thus binary compare is enough to check the encoding name.

   Registry : constant array (Positive range <>) of Registry_Record :=
     --  UTF-8
     (("csutf8",         UTF8.Factory'Access),
      ("utf8",           UTF8.Factory'Access),
      ("unicode11utf8",  UTF8.Factory'Access),
      ("unicode20utf8",  UTF8.Factory'Access),
      ("xunicode20utf8", UTF8.Factory'Access),

      --  ISO-8859-1
      ("isoir100",       ISO88591.Factory'Access),
      ("iso88591",       ISO88591.Factory'Access),
      ("latin1",         ISO88591.Factory'Access),
      ("l1",             ISO88591.Factory'Access),
      ("ibm819",         ISO88591.Factory'Access),
      ("cp819",          ISO88591.Factory'Access),
      ("csisolatin1",    ISO88591.Factory'Access),
      ("iso885911987",   ISO88591.Factory'Access),

      --  ISO-8859-2
      ("isoir101",       ISO88592.Factory'Access),
      ("iso88592",       ISO88592.Factory'Access),
      ("latin2",         ISO88592.Factory'Access),
      ("l2",             ISO88592.Factory'Access),
      ("csisolatin2",    ISO88592.Factory'Access),
      ("iso885921987",   ISO88592.Factory'Access)
     );

   ------------
   -- Decode --
   ------------

   function Decode
     (Self : in out Virtual_String_Decoder'Class;
      Data : VSS.Stream_Element_Vectors.Stream_Element_Vector)
      return VSS.Strings.Virtual_String
   is
      use type Ada.Streams.Stream_Element_Offset;

      Length  : Ada.Streams.Stream_Element_Count;
      Storage :
        VSS.Stream_Element_Vectors.Internals.Stream_Element_Array_Access;

   begin
      VSS.Stream_Element_Vectors.Internals.Data_Constant_Access
        (Data, Length, Storage);

      return Result : VSS.Strings.Virtual_String do
         if Length /= 0 and Self.Decoder /= null then
            Self.Decoder.Decode (Storage (1 .. Length), Result.Data);
         end if;
      end return;
   end Decode;

   ------------
   -- Decode --
   ------------

   function Decode
     (Self : in out Virtual_String_Decoder'Class;
      Data : Ada.Streams.Stream_Element_Array)
      return VSS.Strings.Virtual_String is
   begin
      return Result : VSS.Strings.Virtual_String do
         if Data'Length /= 0 and Self.Decoder /= null then
            Self.Decoder.Decode (Data, Result.Data);
         end if;
      end return;
   end Decode;

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message
     (Self : Virtual_String_Decoder'Class) return VSS.Strings.Virtual_String is
   begin
      if Self.Decoder /= null then
         return Self.Decoder.Error_Message;

      else
         return VSS.Strings.Empty_Virtual_String;
      end if;
   end Error_Message;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Virtual_String_Decoder) is
   begin
      Free (Self.Decoder);
   end Finalize;

   ---------------
   -- Has_Error --
   ---------------

   function Has_Error (Self : Virtual_String_Decoder'Class) return Boolean is
   begin
      if Self.Decoder /= null then
         return Self.Decoder.Has_Error;

      else
         return False;
      end if;
   end Has_Error;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self     : in out Virtual_String_Decoder'Class;
      Encoding : VSS.Strings.Virtual_String;
      Flags    : Converter_Flags := Default_Converter_Flags)
   is
      Encoding_Name : constant VSS.Strings.Virtual_String :=
        VSS.Strings.Converters.To_Encoding_Name (Encoding);

   begin
      Free (Self.Decoder);

      for Item of Registry loop
         if Encoding_Name = Item.Encoding_Name then
            Self.Decoder := Item.Factory (Flags);

            exit;
         end if;
      end loop;
   end Initialize;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Self : Virtual_String_Decoder'Class) return Boolean is
   begin
      return Self.Decoder /= null;
   end Is_Valid;

   -----------------
   -- Reset_State --
   -----------------

   procedure Reset_State (Self : in out Virtual_String_Decoder'Class) is
   begin
      if Self.Decoder /= null then
         Self.Decoder.Reset_State;
      end if;
   end Reset_State;

end VSS.Strings.Converters.Decoders;
