--
--  Copyright (C) 2021-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with Ada.Unchecked_Deallocation;

with VSS.Stream_Element_Vectors.Internals;
with VSS.Strings.Converters.Decoders.EUCJP;
with VSS.Strings.Converters.Decoders.GB18030;
with VSS.Strings.Converters.Decoders.ISO88591;
with VSS.Strings.Converters.Decoders.ISO88592;
with VSS.Strings.Converters.Decoders.ISO88595;
with VSS.Strings.Converters.Decoders.ISO88596;
with VSS.Strings.Converters.Decoders.ISO88597;
with VSS.Strings.Converters.Decoders.ISO88598;
with VSS.Strings.Converters.Decoders.ISO88599;
with VSS.Strings.Converters.Decoders.ISO885915;
with VSS.Strings.Converters.Decoders.KOI8R;
with VSS.Strings.Converters.Decoders.ShiftJIS;
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
     [("csutf8",              UTF8.Factory'Access),
      ("utf8",                UTF8.Factory'Access),
      ("unicode11utf8",       UTF8.Factory'Access),
      ("unicode20utf8",       UTF8.Factory'Access),
      ("xunicode20utf8",      UTF8.Factory'Access),

      --  ISO-8859-1
      ("isoir100",            ISO88591.Factory'Access),
      ("iso88591",            ISO88591.Factory'Access),
      ("latin1",              ISO88591.Factory'Access),
      ("l1",                  ISO88591.Factory'Access),
      ("ibm819",              ISO88591.Factory'Access),
      ("cp819",               ISO88591.Factory'Access),
      ("csisolatin1",         ISO88591.Factory'Access),
      ("iso885911987",        ISO88591.Factory'Access),

      --  ISO-8859-2
      ("isoir101",            ISO88592.Factory'Access),
      ("iso88592",            ISO88592.Factory'Access),
      ("latin2",              ISO88592.Factory'Access),
      ("l2",                  ISO88592.Factory'Access),
      ("csisolatin2",         ISO88592.Factory'Access),
      ("iso885921987",        ISO88592.Factory'Access),

      --  ISO-8859-5
      ("isoir144",            ISO88595.Factory'Access),
      ("iso88595",            ISO88595.Factory'Access),
      ("cyrillic",            ISO88595.Factory'Access),
      ("csisolatincyrillic",  ISO88595.Factory'Access),
      ("iso885951988",        ISO88595.Factory'Access),

      --  ISO-8859-6
      ("isoir127",            ISO88596.Factory'Access),
      ("iso88596",            ISO88596.Factory'Access),
      ("ecma114",             ISO88596.Factory'Access),
      ("asmo708",             ISO88596.Factory'Access),
      ("arabic",              ISO88596.Factory'Access),
      ("csisolatinarabic",    ISO88596.Factory'Access),
      ("csiso88596e",         ISO88596.Factory'Access),
      ("csiso88596i",         ISO88596.Factory'Access),
      ("iso88596e",           ISO88596.Factory'Access),
      ("iso88596i",           ISO88596.Factory'Access),
      ("iso885961987",        ISO88596.Factory'Access),

      --  ISO-8859-7
      ("isoir126",            ISO88597.Factory'Access),
      ("iso88597",            ISO88597.Factory'Access),
      ("elot928",             ISO88597.Factory'Access),
      ("ecma118",             ISO88597.Factory'Access),
      ("greek",               ISO88597.Factory'Access),
      ("greek8",              ISO88597.Factory'Access),
      ("csisolatingreek",     ISO88597.Factory'Access),
      ("iso885971987",        ISO88597.Factory'Access),
      ("suneugreek",          ISO88597.Factory'Access),

      --  ISO-8859-8
      ("isoir138",            ISO88598.Factory'Access),
      ("iso88598",            ISO88598.Factory'Access),
      ("hebrew",              ISO88598.Factory'Access),
      ("csisolatinhebrew",    ISO88598.Factory'Access),
      ("csiso88598e",         ISO88598.Factory'Access),
      ("iso88598e",           ISO88598.Factory'Access),
      ("iso885981988",        ISO88598.Factory'Access),
      ("visual",              ISO88598.Factory'Access),

      --  ISO-8859-9
      ("isoir148",            ISO88599.Factory'Access),
      ("iso88599",            ISO88599.Factory'Access),
      ("latin5",              ISO88599.Factory'Access),
      ("l5",                  ISO88599.Factory'Access),
      ("csisolatin5",         ISO88599.Factory'Access),
      ("iso885991989",        ISO88599.Factory'Access),

      --  ISO-8859-15
      ("iso885915",           ISO885915.Factory'Access),
      ("latin9",              ISO885915.Factory'Access),
      ("csiso885915",         ISO885915.Factory'Access),
      ("csisolatin9",         ISO885915.Factory'Access),
      ("l9",                  ISO885915.Factory'Access),

      --  GB18030
      ("csgb2312",            GB18030.Factory'Access),
      ("cp936",               GB18030.Factory'Access),
      ("ms936",               GB18030.Factory'Access),
      ("windows936",          GB18030.Factory'Access),
      ("csgbk",               GB18030.Factory'Access),
      ("csgb18030",           GB18030.Factory'Access),
      ("chinese",             GB18030.Factory'Access),
      ("csiso58gb231280",     GB18030.Factory'Access),
      ("gb2312",              GB18030.Factory'Access),
      ("gb231280",            GB18030.Factory'Access),
      ("gbk",                 GB18030.Factory'Access),
      ("isoir58",             GB18030.Factory'Access),
      ("xgbk",                GB18030.Factory'Access),
      ("gb18030",             GB18030.Factory'Access),

      --  EUC-JP
      ("cseucpkdfmtjapanese", EUCJP.Factory'Access),
      ("eucjp",               EUCJP.Factory'Access),
      ("xeucjp",              EUCJP.Factory'Access),

      --  Shift-JIS
      ("mskanji",             ShiftJIS.Factory'Access),
      ("csshiftjis",          ShiftJIS.Factory'Access),
      ("ms932",               ShiftJIS.Factory'Access),
      ("shiftjis",            ShiftJIS.Factory'Access),
      ("sjis",                ShiftJIS.Factory'Access),
      ("windows31j",          ShiftJIS.Factory'Access),
      ("xsjis",               ShiftJIS.Factory'Access),

      --  KOI8-R
      ("koi8r",               KOI8R.Factory'Access),
      ("cskoi8r",             KOI8R.Factory'Access),
      ("koi",                 KOI8R.Factory'Access),
      ("koi8",                KOI8R.Factory'Access)
     ];

   Empty_Data : constant Ada.Streams.Stream_Element_Array (1 .. 0) :=
     [others => <>];

   ------------
   -- Decode --
   ------------

   function Decode
     (Self        : in out Virtual_String_Decoder'Class;
      Data_Chunk  : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      End_Of_Data : Boolean := True)
      return VSS.Strings.Virtual_String
   is
      use type Ada.Streams.Stream_Element_Offset;
      use type
        VSS.Stream_Element_Vectors.Internals.Stream_Element_Array_Access;

      Length  : Ada.Streams.Stream_Element_Count;
      Storage :
        VSS.Stream_Element_Vectors.Internals.Stream_Element_Array_Access;

   begin
      VSS.Stream_Element_Vectors.Internals.Data_Constant_Access
        (Data_Chunk, Length, Storage);

      return Result : VSS.Strings.Virtual_String do
         if Self.Decoder /= null then
            Self.Decoder.Decode
              ((if Storage /= null then Storage (1 .. Length) else Empty_Data),
               End_Of_Data,
               Result.Data);
         end if;
      end return;
   end Decode;

   ------------
   -- Decode --
   ------------

   function Decode
     (Self        : in out Virtual_String_Decoder'Class;
      Data_Chunk  : Ada.Streams.Stream_Element_Array;
      End_Of_Data : Boolean := True)
      return VSS.Strings.Virtual_String is
   begin
      return Result : VSS.Strings.Virtual_String do
         if Self.Decoder /= null then
            Self.Decoder.Decode (Data_Chunk, End_Of_Data, Result.Data);
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
