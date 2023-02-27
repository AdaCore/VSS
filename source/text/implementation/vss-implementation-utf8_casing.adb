--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Implementation.UCD_Casing;
with VSS.Implementation.UTF8_String_Handlers;

package body VSS.Implementation.UTF8_Casing is

   -----------------------
   -- Convert_Case_Full --
   -----------------------

   procedure Convert_Case_Full
     (Source_Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Source_Size    : VSS.Unicode.UTF8_Code_Unit_Count;
      Mapping        :
        VSS.Implementation.UCD_Casing_UTF8.Mapping_Data_Offset_Array;
      To_Lower       : Boolean;
      Result_Data    : in out VSS.Implementation.Strings.String_Data)
   is
      use type VSS.Unicode.UTF8_Code_Unit_Offset;

      Code    : VSS.Unicode.Code_Point;
      Start   : VSS.Unicode.UTF8_Code_Unit_Offset;
      Offset  : VSS.Unicode.UTF8_Code_Unit_Offset := 0;
      Context : VSS.Implementation.UCD_Casing.Casing_Context;

   begin
      while Offset < Source_Size loop
         Start := Offset;
         VSS.Implementation.UTF8_Encoding.Unchecked_Decode_Forward
           (Source_Storage, Offset, Code);

         declare
            use type VSS.Unicode.Code_Point;

            Info : constant
              VSS.Implementation.UCD_Casing_UTF8.Contextual_Mapping_Information
                := Get_Contextual_Case_Mapping_Information (Mapping, Code);
            Skip : Boolean := False;

         begin
            if To_Lower and Context.Final_Sigma and Code = 16#03A3# then
               declare
                  Suffix_Offset : VSS.Unicode.UTF8_Code_Unit_Offset := Offset;
                  Match         : Boolean := False;
                  Suffix_Info   :
                    VSS.Implementation.UCD_Casing_UTF8
                      .Contextual_Mapping_Information;

               begin
                  while Suffix_Offset < Source_Size loop
                     VSS.Implementation.UTF8_Encoding.Unchecked_Decode_Forward
                       (Source_Storage, Suffix_Offset, Code);
                     Suffix_Info :=
                       Get_Contextual_Case_Mapping_Information (Mapping, Code);

                     if Suffix_Info.Case_Ignorable then
                        null;

                     else
                        Match := Suffix_Info.Cased;

                        exit;
                     end if;
                  end loop;

                  if not Match then
                     --  Replace by 03C2

                     VSS.Implementation.UTF8_String_Handlers.Unchecked_Append
                       (Result_Data, (16#CF#, 16#82#), 0, 2, 1);
                     Skip := True;
                  end if;
               end;
            end if;

            VSS.Implementation.UCD_Casing.Apply (Context, Info.Context_Change);

            if not Skip then
               if Info.Changes then
                  VSS.Implementation.UTF8_String_Handlers.Unchecked_Append
                    (Result_Data,
                     VSS.Implementation.UCD_Casing_UTF8.UTF8_Data_Table,
                     Info.Offset,
                     Info.Count,
                     Info.Length);

               else
                  VSS.Implementation.UTF8_String_Handlers.Unchecked_Append
                    (Result_Data, Source_Storage, Start, Offset - Start, 1);
               end if;
            end if;
         end;
      end loop;
   end Convert_Case_Full;

   -------------------------
   -- Convert_Case_Simple --
   -------------------------

   procedure Convert_Case_Simple
     (Source_Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Source_Size    : VSS.Unicode.UTF8_Code_Unit_Count;
      Mapping        :
        VSS.Implementation.UCD_Casing_UTF8.Mapping_Data_Offset_Array;
      Result_Data    : in out VSS.Implementation.Strings.String_Data)
   is
      use type VSS.Unicode.UTF8_Code_Unit_Offset;

      Code   : VSS.Unicode.Code_Point;
      Start  : VSS.Unicode.UTF8_Code_Unit_Offset;
      Offset : VSS.Unicode.UTF8_Code_Unit_Offset := 0;

   begin
      while Offset < Source_Size loop
         Start := Offset;
         VSS.Implementation.UTF8_Encoding.Unchecked_Decode_Forward
           (Source_Storage, Offset, Code);

         declare
            Info : constant
              VSS.Implementation.UCD_Casing_UTF8.Simplified_Mapping_Information
                := Get_Simplified_Case_Mapping_Information (Mapping, Code);

         begin
            if Info.Changes then
               VSS.Implementation.UTF8_String_Handlers.Unchecked_Append
                 (Result_Data,
                  VSS.Implementation.UCD_Casing_UTF8.UTF8_Data_Table,
                  Info.Offset,
                  Info.Count,
                  Info.Length);

            else
               VSS.Implementation.UTF8_String_Handlers.Unchecked_Append
                 (Result_Data, Source_Storage, Start, Offset - Start, 1);
            end if;
         end;
      end loop;
   end Convert_Case_Simple;

   ---------------------------------------------
   -- Get_Contextual_Case_Mapping_Information --
   ---------------------------------------------

   function Get_Contextual_Case_Mapping_Information
     (Mapping : VSS.Implementation.UCD_Casing_UTF8.Mapping_Data_Offset_Array;
      Code    : VSS.Unicode.Code_Point)
      return VSS.Implementation.UCD_Casing_UTF8.Contextual_Mapping_Information
   is
      use type VSS.Implementation.UCD_Casing_UTF8.Mapping_Data_Offset;
      use type VSS.Unicode.Code_Point;

      Group  : constant VSS.Implementation.UCD_Casing_UTF8.Mapping_Group :=
        VSS.Implementation.UCD_Casing_UTF8.Mapping_Group
          (Code / VSS.Implementation.UCD_Casing_UTF8.Mapping_Group_Size);
      Offset : constant
        VSS.Implementation.UCD_Casing_UTF8.Mapping_Data_Offset :=
          VSS.Implementation.UCD_Casing_UTF8.Mapping_Data_Offset
            (Code mod VSS.Implementation.UCD_Casing_UTF8.Mapping_Group_Size);

   begin
      return
        VSS.Implementation.UCD_Casing_UTF8.Contextual_Mapping_Data_Table
          (Mapping (Group) + Offset);
   end Get_Contextual_Case_Mapping_Information;

   ---------------------------------------------
   -- Get_Simplified_Case_Mapping_Information --
   ---------------------------------------------

   function Get_Simplified_Case_Mapping_Information
     (Mapping : VSS.Implementation.UCD_Casing_UTF8.Mapping_Data_Offset_Array;
      Code    : VSS.Unicode.Code_Point)
      return VSS.Implementation.UCD_Casing_UTF8.Simplified_Mapping_Information
   is
      use type VSS.Implementation.UCD_Casing_UTF8.Mapping_Data_Offset;
      use type VSS.Unicode.Code_Point;

      Group  : constant VSS.Implementation.UCD_Casing_UTF8.Mapping_Group :=
        VSS.Implementation.UCD_Casing_UTF8.Mapping_Group
          (Code / VSS.Implementation.UCD_Casing_UTF8.Mapping_Group_Size);
      Offset : constant
        VSS.Implementation.UCD_Casing_UTF8.Mapping_Data_Offset :=
          VSS.Implementation.UCD_Casing_UTF8.Mapping_Data_Offset
            (Code mod VSS.Implementation.UCD_Casing_UTF8.Mapping_Group_Size);

   begin
      return
        VSS.Implementation.UCD_Casing_UTF8.Simplified_Mapping_Data_Table
          (Mapping (Group) + Offset);
   end Get_Simplified_Case_Mapping_Information;

end VSS.Implementation.UTF8_Casing;
