--
--  Copyright (C) 2021-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Unicode;

package body VSS.Implementation.Line_Iterators is

   Line_Feed           : constant VSS.Unicode.Code_Point := 16#00_000A#;
   Line_Tabulation     : constant VSS.Unicode.Code_Point := 16#00_000B#;
   Form_Feed           : constant VSS.Unicode.Code_Point := 16#00_000C#;
   Carriage_Return     : constant VSS.Unicode.Code_Point := 16#00_000D#;
   Next_Line           : constant VSS.Unicode.Code_Point := 16#00_0085#;
   Line_Separator      : constant VSS.Unicode.Code_Point := 16#00_2028#;
   Paragraph_Separator : constant VSS.Unicode.Code_Point := 16#00_2029#;

   --------------
   -- Backward --
   --------------

   function Backward
     (Text                : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Terminators         : VSS.Strings.Line_Terminator_Set;
      Initial_Position    : VSS.Implementation.Strings.Cursor;
      First_Position      : aliased out VSS.Implementation.Strings.Cursor;
      Last_Position       : out VSS.Implementation.Strings.Cursor;
      Terminator_Position : out VSS.Implementation.Strings.Cursor)
      return Boolean
   is
      use type VSS.Unicode.Code_Point;

      Current_Position : aliased VSS.Implementation.Strings.Cursor :=
        Initial_Position;
      Aux_Position     : aliased VSS.Implementation.Strings.Cursor;
      LF_Found         : Boolean := False;
      Dummy            : Boolean;

   begin
      if not VSS.Implementation.UTF8_Strings.Backward (Text, Current_Position)
      then
         --  There is no any characters before initial position.

         First_Position      :=
           VSS.Implementation.Strings.Position_Before_First_Character;
         Last_Position       :=
           VSS.Implementation.Strings.Position_Before_First_Character;
         Terminator_Position := (others => <>);

         return False;
      end if;

      Last_Position       := Current_Position;
      First_Position      := Current_Position;
      Terminator_Position := (others => <>);

      declare
         C : constant VSS.Unicode.Code_Point :=
           VSS.Implementation.UTF8_Strings.Element (Text, Current_Position);

      begin
         case C is
            when Line_Feed =>
               if Terminators (VSS.Strings.CRLF) then
                  --  Set flag to check for CR prefix when CRLF line
                  --  termination sequence is enabled. First position
                  --  and terminator's position will be set later in this
                  --  case.

                  LF_Found := True;
               end if;

               if Terminators (VSS.Strings.LF) then
                  --  Set first position and terminator position when LF only
                  --  line termination sequence is enabled.

                  First_Position      := Current_Position;
                  Terminator_Position := Current_Position;

               elsif not Terminators (VSS.Strings.CRLF) then
                  Current_Position := Initial_Position;
               end if;

            when Line_Tabulation =>
               if Terminators (VSS.Strings.VT) then
                  First_Position      := Current_Position;
                  Terminator_Position := Current_Position;

               else
                  Current_Position := Initial_Position;
               end if;

            when Form_Feed =>
               if Terminators (VSS.Strings.FF) then
                  First_Position      := Current_Position;
                  Terminator_Position := Current_Position;

               else
                  Current_Position := Initial_Position;
               end if;

            when Carriage_Return =>
               if Terminators (VSS.Strings.CR) then
                  First_Position      := Current_Position;
                  Terminator_Position := Current_Position;

               else
                  Current_Position := Initial_Position;
               end if;

            when Next_Line =>
               if Terminators (VSS.Strings.NEL) then
                  First_Position      := Current_Position;
                  Terminator_Position := Current_Position;

               else
                  Current_Position := Initial_Position;
               end if;

            when Line_Separator =>
               if Terminators (VSS.Strings.LS) then
                  First_Position      := Current_Position;
                  Terminator_Position := Current_Position;

               else
                  Current_Position := Initial_Position;
               end if;

            when Paragraph_Separator =>
               if Terminators (VSS.Strings.PS) then
                  First_Position      := Current_Position;
                  Terminator_Position := Current_Position;

               else
                  Current_Position := Initial_Position;
               end if;

            when others =>
               Current_Position := Initial_Position;
         end case;
      end;

      if LF_Found then
         Aux_Position := Current_Position;

         if VSS.Implementation.UTF8_Strings.Backward (Text, Aux_Position) then
            if VSS.Implementation.UTF8_Strings.Element (Text, Aux_Position)
                  = Carriage_Return
            then
               Current_Position    := Aux_Position;
               First_Position      := Aux_Position;
               Terminator_Position := Aux_Position;
            end if;
         end if;
      end if;

      while VSS.Implementation.UTF8_Strings.Backward (Text, Current_Position)
      loop
         declare
            C : constant VSS.Unicode.Code_Point :=
              VSS.Implementation.UTF8_Strings.Element (Text, Current_Position);
            D : Boolean with Unreferenced;

         begin
            case C is
               when Line_Feed =>
                  if Terminators (VSS.Strings.LF) then
                     exit;

                  elsif Terminators (VSS.Strings.CRLF) then
                     Aux_Position := Current_Position;

                     if VSS.Implementation.UTF8_Strings.Backward
                       (Text, Aux_Position)
                     then
                        if VSS.Implementation.UTF8_Strings.Element
                             (Text, Aux_Position) = Carriage_Return
                        then
                           exit;
                        end if;
                     end if;

                  else
                     raise Program_Error;
                  end if;

               when Line_Tabulation =>
                  exit when Terminators (VSS.Strings.VT);

               when Form_Feed =>
                  exit when Terminators (VSS.Strings.FF);

               when Carriage_Return =>
                  exit when Terminators (VSS.Strings.CR);

               when Next_Line =>
                  exit when Terminators (VSS.Strings.NEL);

               when Line_Separator =>
                  exit when Terminators (VSS.Strings.LS);

               when Paragraph_Separator =>
                  exit when Terminators (VSS.Strings.PS);

               when others =>
                  null;
            end case;

            First_Position := Current_Position;
         end;
      end loop;

      return True;
   end Backward;

   -------------
   -- Forward --
   -------------

   function Forward
     (Text                : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Terminators         : VSS.Strings.Line_Terminator_Set;
      Initial_Position    : VSS.Implementation.Strings.Cursor;
      First_Position      : aliased out VSS.Implementation.Strings.Cursor;
      Last_Position       : aliased out VSS.Implementation.Strings.Cursor;
      Terminator_Position : out VSS.Implementation.Strings.Cursor)
      return Boolean
   is
      CR_Found : Boolean := False;
      Dummy    : Boolean;

   begin
      First_Position      := Initial_Position;
      Last_Position       := Initial_Position;
      Terminator_Position := (others => <>);

      if not VSS.Implementation.UTF8_Strings.Forward (Text, First_Position)
      then
         Last_Position       := Initial_Position;
         Terminator_Position := (others => <>);

         return False;
      end if;

      while VSS.Implementation.UTF8_Strings.Forward (Text, Last_Position) loop
         declare
            use type VSS.Unicode.Code_Point;

            C : constant VSS.Unicode.Code_Point :=
              VSS.Implementation.UTF8_Strings.Element (Text, Last_Position);

         begin
            if CR_Found then
               if C /= Line_Feed and Terminators (VSS.Strings.CR) then
                  --  It is special case to handle single CR when both CR and
                  --  CRLF are allowed.

                  CR_Found := False;
                  Dummy    :=
                    VSS.Implementation.UTF8_Strings.Backward
                      (Text, Last_Position);

                  exit;
               end if;
            end if;

            case C is
               when Line_Feed =>
                  if Terminators (VSS.Strings.CRLF) and CR_Found then
                     CR_Found := False;

                     exit;

                  elsif Terminators (VSS.Strings.LF) then
                     Terminator_Position := Last_Position;

                     exit;
                  end if;

               when Line_Tabulation =>
                  if Terminators (VSS.Strings.VT) then
                     Terminator_Position := Last_Position;

                     exit;
                  end if;

               when Form_Feed =>
                  if Terminators (VSS.Strings.FF) then
                     Terminator_Position := Last_Position;

                     exit;
                  end if;

               when Carriage_Return =>
                  if Terminators (VSS.Strings.CRLF) then
                     Terminator_Position := Last_Position;
                     CR_Found            := True;

                  elsif Terminators (VSS.Strings.CR) then
                     Terminator_Position := Last_Position;

                     exit;
                  end if;

               when Next_Line =>
                  if Terminators (VSS.Strings.NEL) then
                     Terminator_Position := Last_Position;

                     exit;
                  end if;

               when Line_Separator =>
                  if Terminators (VSS.Strings.LS) then
                     Terminator_Position := Last_Position;

                     exit;
                  end if;

               when Paragraph_Separator =>
                  if Terminators (VSS.Strings.PS) then
                     Terminator_Position := Last_Position;

                     exit;
                  end if;

               when others =>
                  null;
            end case;
         end;
      end loop;

      if CR_Found then
         if Terminators (VSS.Strings.CR) then
            --  It is special case to handle single CR at the end of string
            --  when both CR and CRLF are allowed.

            Dummy :=
              VSS.Implementation.UTF8_Strings.Backward (Text, Last_Position);

         else
            --  CR at the end of the string is not a line terminator sequence.

            Terminator_Position := (others => <>);
         end if;
      end if;

      if VSS.Implementation.Strings.Is_Invalid (Terminator_Position) then
         --  Last_Position should point to the last character of the line
         --  found, but when line terminator sequence is now found it points
         --  to the character after last character of the string. Thus, it
         --  should be moved one character backward.

         Dummy :=
           VSS.Implementation.UTF8_Strings.Backward (Text, Last_Position);
      end if;

      return True;
   end Forward;

end VSS.Implementation.Line_Iterators;
