------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                    Copyright (C) 2021-2022, AdaCore                      --
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

with Ada.Streams;

with VSS.Stream_Element_Vectors.Conversions;
with VSS.Strings.Converters.Decoders;

procedure Test_Converters is
   use all type VSS.Strings.Converters.Converter_Flag;

   D1 : constant Ada.Streams.Stream_Element_Array :=
     (16#EF#, 16#BB#, 16#BF#);
   --  BOM only

   D2 : constant Ada.Streams.Stream_Element_Array :=
     (16#C0#, 16#AF#, 16#E0#, 16#80#,  16#BF#, 16#F0#, 16#81#, 16#82#,
      16#41#);
   E2 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String
       ((Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#0041#)));
   --  Unicode 13.0, Table 3-8.

   D3 : constant Ada.Streams.Stream_Element_Array :=
     (16#ED#, 16#A0#, 16#80#, 16#ED#,  16#BF#, 16#BF#, 16#ED#, 16#AF#,
      16#41#);
   E3 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String
       ((Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#0041#)));
   --  Unicode 13.0, Table 3-9.

   D4 : constant Ada.Streams.Stream_Element_Array :=
     (16#F4#, 16#91#, 16#92#, 16#93#,  16#FF#, 16#41#, 16#80#, 16#BF#,
      16#42#);
   E4 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String
       ((Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#0041#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#0042#)));
   --  Unicode 13.0, Table 3-10.

   D5 : constant Ada.Streams.Stream_Element_Array :=
     (16#E1#, 16#80#, 16#E2#, 16#F0#,  16#91#, 16#92#, 16#F1#, 16#BF#,
      16#41#);
   E5 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String
       ((Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#0041#)));
   --  Unicode 13.0, Table 3-11.

   D6 : constant Ada.Streams.Stream_Element_Array :=
     (16#41#, 16#D0#, 16#91#, 16#E0#,  16#A4#, 16#95#, 16#F0#, 16#90#,
      16#8C#, 16#88#);
   E6 : constant VSS.Strings.Virtual_String := "AБक𐌈";

   procedure Do_Test
     (Encoded   : Ada.Streams.Stream_Element_Array;
      Decoded   : VSS.Strings.Virtual_String;
      Has_Error : Boolean);
   --  Run decoder for UTF-8 encoding in two modes: block and incremental,
   --  and check result.

   -------------
   -- Do_Test --
   -------------

   procedure Do_Test
     (Encoded   : Ada.Streams.Stream_Element_Array;
      Decoded   : VSS.Strings.Virtual_String;
      Has_Error : Boolean) is
   begin
      --  Stream_Element_Array and block mode

      declare
         use type VSS.Strings.Virtual_String;

         Decoder : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
         Result  : VSS.Strings.Virtual_String;

      begin
         Decoder.Initialize ("utf-8");

         if not Decoder.Is_Valid then
            raise Program_Error;
         end if;

         Result := Decoder.Decode (Encoded);

         if Result /= Decoded then
            raise Program_Error;
         end if;

         if Decoder.Has_Error /= Has_Error then
            raise Program_Error;
         end if;

         if Decoder.Error_Message.Is_Empty and Has_Error then
            raise Program_Error;
         end if;
      end;

      --  Stream_Element_Vector and block mode

      declare
         use type VSS.Strings.Virtual_String;

         Decoder : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
         Result  : VSS.Strings.Virtual_String;

      begin
         Decoder.Initialize ("utf-8");

         if not Decoder.Is_Valid then
            raise Program_Error;
         end if;

         Result :=
           Decoder.Decode
             (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
                (Encoded));

         if Result /= Decoded then
            raise Program_Error;
         end if;

         if Decoder.Has_Error /= Has_Error then
            raise Program_Error;
         end if;

         if Decoder.Error_Message.Is_Empty and Has_Error then
            raise Program_Error;
         end if;
      end;

      --  Stream_Element_Array and incremental mode

      declare
         use type VSS.Strings.Virtual_String;

         Decoder : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
         Result  : VSS.Strings.Virtual_String;

      begin
         Decoder.Initialize ("utf-8");

         if not Decoder.Is_Valid then
            raise Program_Error;
         end if;

         for J in Encoded'Range loop
            Result.Append
              (Decoder.Decode
                 (Ada.Streams.Stream_Element_Array'((1 => Encoded (J)))));
         end loop;

         if Result /= Decoded then
            raise Program_Error;
         end if;

         if Decoder.Has_Error /= Has_Error then
            raise Program_Error;
         end if;

         if Decoder.Error_Message.Is_Empty and Has_Error then
            raise Program_Error;
         end if;
      end;

      --  Stream_Element_Vector and incremental mode

      declare
         use type VSS.Strings.Virtual_String;

         Decoder : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
         Result  : VSS.Strings.Virtual_String;

      begin
         Decoder.Initialize ("utf-8");

         if not Decoder.Is_Valid then
            raise Program_Error;
         end if;

         for J in Encoded'Range loop
            Result.Append
              (Decoder.Decode
                 (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
                    ((1 => Encoded (J)))));
         end loop;

         if Result /= Decoded then
            raise Program_Error;
         end if;

         if Decoder.Has_Error /= Has_Error then
            raise Program_Error;
         end if;

         if Decoder.Error_Message.Is_Empty and Has_Error then
            raise Program_Error;
         end if;
      end;
   end Do_Test;

begin
   --  Check invalid state of the decoder after object declaration without
   --  initialization.

   declare
      Decoder : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;

   begin
      if Decoder.Is_Valid then
         raise Program_Error;
      end if;
   end;

   --  Check conversion of empty data

   declare
      D : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
      S : VSS.Strings.Virtual_String;
      E : VSS.Stream_Element_Vectors.Stream_Element_Vector;

   begin
      D.Initialize ("utf-8");

      if not D.Is_Valid then
         raise Program_Error;
      end if;

      S := D.Decode (E);

      if not S.Is_Empty then
         raise Program_Error;
      end if;
   end;

   --  Check processing of the BOM

   declare
      D : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
      S : VSS.Strings.Virtual_String;

   begin
      D.Initialize ("utf-8", (Process_BOM => True, others => False));

      if not D.Is_Valid then
         raise Program_Error;
      end if;

      S := D.Decode (D1);

      if not S.Is_Empty then
         raise Program_Error;
      end if;
   end;

   declare
      D : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
      S : VSS.Strings.Virtual_String;

   begin
      D.Initialize ("utf-8", (Process_BOM => True, others => False));

      if not D.Is_Valid then
         raise Program_Error;
      end if;

      for J in D1'Range loop
         S.Append
           (D.Decode (Ada.Streams.Stream_Element_Array'((1 => D1 (J)))));
      end loop;

      if not S.Is_Empty then
         raise Program_Error;
      end if;
   end;

   Do_Test (D2, E2, True);
   Do_Test (D3, E3, True);
   Do_Test (D4, E4, True);
   Do_Test (D5, E5, True);
   Do_Test (D6, E6, False);
end Test_Converters;
