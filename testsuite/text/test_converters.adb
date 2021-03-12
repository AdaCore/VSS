------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with VSS.Stream_Element_Buffers.Conversions;
with VSS.Strings.Converters.Decoders;

procedure Test_Converters is
   use all type VSS.Strings.Converters.Converter_Flag;

   D1 : VSS.Stream_Element_Buffers.Stream_Element_Buffer :=
     VSS.Stream_Element_Buffers.Conversions.Unchecked_From_Stream_Element_Array
       ((16#EF#, 16#BB#, 16#BF#));

   D2 : VSS.Stream_Element_Buffers.Stream_Element_Buffer :=
     VSS.Stream_Element_Buffers.Conversions.Unchecked_From_Stream_Element_Array
       ((16#C0#, 16#AF#, 16#E0#, 16#80#,  16#BF#, 16#F0#, 16#81#, 16#82#,
         16#41#));
   E2 : VSS.Strings.Virtual_String :=
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

   D3 : VSS.Stream_Element_Buffers.Stream_Element_Buffer :=
     VSS.Stream_Element_Buffers.Conversions.Unchecked_From_Stream_Element_Array
       ((16#ED#, 16#A0#, 16#80#, 16#ED#,  16#BF#, 16#BF#, 16#ED#, 16#AF#,
         16#41#));
   E3 : VSS.Strings.Virtual_String :=
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

   D4 : VSS.Stream_Element_Buffers.Stream_Element_Buffer :=
     VSS.Stream_Element_Buffers.Conversions.Unchecked_From_Stream_Element_Array
       ((16#F4#, 16#91#, 16#92#, 16#93#,  16#FF#, 16#41#, 16#80#, 16#BF#,
         16#42#));
   E4 : VSS.Strings.Virtual_String :=
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

   D5 : VSS.Stream_Element_Buffers.Stream_Element_Buffer :=
     VSS.Stream_Element_Buffers.Conversions.Unchecked_From_Stream_Element_Array
       ((16#E1#, 16#80#, 16#E2#, 16#F0#,  16#91#, 16#92#, 16#F1#, 16#BF#,
         16#41#));
   E5 : VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String
       ((Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#0041#)));
   --  Unicode 13.0, Table 3-11.

   D6 : constant VSS.Stream_Element_Buffers.Stream_Element_Buffer :=
     VSS.Stream_Element_Buffers.Conversions.Unchecked_From_Stream_Element_Array
       ((16#41#, 16#D0#, 16#91#, 16#E0#,  16#A4#, 16#95#, 16#F0#, 16#90#,
         16#8C#, 16#88#));
   E6 : constant VSS.Strings.Virtual_String := "AБक𐌈";

begin
   declare
      D : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
      S : VSS.Strings.Virtual_String;

   begin
      D.Initialize ("utf-8", (Ignore_BOM => True, others => False));
      S := D.Decode (D1);

      if not S.Is_Empty then
         raise Program_Error;
      end if;
   end;

   declare
      D : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
      S : VSS.Strings.Virtual_String;

   begin
      D.Initialize ("utf-8", (Ignore_BOM => True, others => False));

      for J in 1 .. D1.Length loop
         S.Append
           (D.Decode
              (VSS.Stream_Element_Buffers.Conversions.Unchecked_From_Stream_Element_Array
                 ((1 => D1 (J)))));
      end loop;

      if not S.Is_Empty then
         raise Program_Error;
      end if;
   end;

   declare
      use type VSS.Strings.Virtual_String;

      D : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
      S : VSS.Strings.Virtual_String;

   begin
      D.Initialize ("utf-8", (Ignore_BOM => True, others => False));
      S := D.Decode (D2);

      if S /= E2 then
         raise Program_Error;
      end if;
   end;

   declare
      use type VSS.Strings.Virtual_String;

      D : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
      S : VSS.Strings.Virtual_String;

   begin
      D.Initialize ("utf-8", (Ignore_BOM => True, others => False));

      for J in 1 .. D2.Length loop
         S.Append
           (D.Decode
              (VSS.Stream_Element_Buffers.Conversions.Unchecked_From_Stream_Element_Array
                 ((1 => D2 (J)))));
      end loop;

      if S /= E2 then
         raise Program_Error;
      end if;
   end;

   declare
      use type VSS.Strings.Virtual_String;

      D : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
      S : VSS.Strings.Virtual_String;

   begin
      D.Initialize ("utf-8", (Ignore_BOM => True, others => False));
      S := D.Decode (D3);

      if S /= E3 then
         raise Program_Error;
      end if;
   end;

   declare
      use type VSS.Strings.Virtual_String;

      D : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
      S : VSS.Strings.Virtual_String;

   begin
      D.Initialize ("utf-8", (Ignore_BOM => True, others => False));

      for J in 1 .. D3.Length loop
         S.Append
           (D.Decode
              (VSS.Stream_Element_Buffers.Conversions.Unchecked_From_Stream_Element_Array
                 ((1 => D3 (J)))));
      end loop;

      if S /= E3 then
         raise Program_Error;
      end if;
   end;

   declare
      use type VSS.Strings.Virtual_String;

      D : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
      S : VSS.Strings.Virtual_String;

   begin
      D.Initialize ("utf-8", (Ignore_BOM => True, others => False));
      S := D.Decode (D4);

      if S /= E4 then
         raise Program_Error;
      end if;
   end;

   declare
      use type VSS.Strings.Virtual_String;

      D : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
      S : VSS.Strings.Virtual_String;

   begin
      D.Initialize ("utf-8", (Ignore_BOM => True, others => False));

      for J in 1 .. D4.Length loop
         S.Append
           (D.Decode
              (VSS.Stream_Element_Buffers.Conversions.Unchecked_From_Stream_Element_Array
                 ((1 => D4 (J)))));
      end loop;

      if S /= E4 then
         raise Program_Error;
      end if;
   end;

   declare
      use type VSS.Strings.Virtual_String;

      D : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
      S : VSS.Strings.Virtual_String;

   begin
      D.Initialize ("utf-8", (Ignore_BOM => True, others => False));
      S := D.Decode (D5);

      if S /= E5 then
         raise Program_Error;
      end if;
   end;

   declare
      use type VSS.Strings.Virtual_String;

      D : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
      S : VSS.Strings.Virtual_String;

   begin
      D.Initialize ("utf-8", (Ignore_BOM => True, others => False));

      for J in 1 .. D5.Length loop
         S.Append
           (D.Decode
              (VSS.Stream_Element_Buffers.Conversions.Unchecked_From_Stream_Element_Array
                 ((1 => D5 (J)))));
      end loop;

      if S /= E5 then
         raise Program_Error;
      end if;
   end;

   declare
      use type VSS.Strings.Virtual_String;

      D : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
      S : VSS.Strings.Virtual_String;

   begin
      D.Initialize ("utf-8", (Ignore_BOM => True, others => False));
      S := D.Decode (D6);

      if S /= E6 then
         raise Program_Error;
      end if;
   end;

   declare
      use type VSS.Strings.Virtual_String;

      D : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
      S : VSS.Strings.Virtual_String;

   begin
      D.Initialize ("utf-8", (Ignore_BOM => True, others => False));

      for J in 1 .. D6.Length loop
         S.Append
           (D.Decode
              (VSS.Stream_Element_Buffers.Conversions.Unchecked_From_Stream_Element_Array
                 ((1 => D6 (J)))));
      end loop;

      if S /= E6 then
         raise Program_Error;
      end if;
   end;
end Test_Converters;
