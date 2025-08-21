--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Characters.Latin;
with VSS.Strings.Character_Iterators;
with VSS.Strings.Cursors.Iterators.Characters;

package body VSS.Strings.Converters is

   ----------------------
   -- To_Encoding_Name --
   ----------------------

   function To_Encoding_Name
     (Encoding_Label : VSS.Strings.Virtual_String'Class)
      return VSS.Strings.Virtual_String
   is
      use type VSS.Characters.Virtual_Character;

      type States is (Leading_WS, Non_Zero, Some_Character, Trailing_WS);

      Iterator  : VSS.Strings.Character_Iterators.Character_Iterator :=
        Encoding_Label.Before_First_Character;
      Character : VSS.Characters.Virtual_Character;
      State     : States := Leading_WS;

   begin
      return Encoding_Name : VSS.Strings.Virtual_String do
         while Iterator.Forward loop
            Character := Iterator.Element;

            case Character is
               when VSS.Characters.Latin.Character_Tabulation
                  | VSS.Characters.Latin.Line_Feed
                  | VSS.Characters.Latin.Form_Feed
                  | VSS.Characters.Latin.Carriage_Return
                  | VSS.Characters.Latin.Space
               =>
                  --  ASCII whitespace, allowed at the beginning and at the
                  --  end of the label only. Ignore them.

                  case State is
                     when Leading_WS | Trailing_WS =>
                        null;

                     when others =>
                        State := Trailing_WS;
                  end case;

               when VSS.Characters.Latin.Exclamation_Mark
                  | VSS.Characters.Latin.Number_Sign
                  | VSS.Characters.Latin.Dollar_Sign
                  | VSS.Characters.Latin.Percent_Sign
                  | VSS.Characters.Latin.Ampersand
                  | VSS.Characters.Latin.Apostrophe
                  | VSS.Characters.Latin.Plus_Sign
                  | VSS.Characters.Latin.Hyphen_Minus
                  | VSS.Characters.Latin.Circumflex_Accent
                  | VSS.Characters.Latin.Low_Line
                  | VSS.Characters.Latin.Grave_Accent
                  | VSS.Characters.Latin.Left_Curly_Bracket
                  | VSS.Characters.Latin.Right_Curly_Bracket
                  | VSS.Characters.Latin.Tilde
               =>
                  --  Ignored.

                  case State is
                     when Leading_WS =>
                        State := Some_Character;

                     when Some_Character | Non_Zero =>
                        null;

                     when Trailing_WS =>
                        Encoding_Name.Clear;

                        return;
                  end case;

               when VSS.Characters.Latin.Digit_Zero
                  ..  VSS.Characters.Latin.Digit_Nine
               =>

                  case State is
                     when Leading_WS =>
                        State := Some_Character;

                     when Non_Zero | Some_Character =>
                        null;

                     when Trailing_WS =>
                        Encoding_Name.Clear;

                        return;
                  end case;

                  if Character /= VSS.Characters.Latin.Digit_Zero then
                     State := Non_Zero;

                     Encoding_Name.Append (Character);

                  elsif State = Non_Zero then
                     Encoding_Name.Append (Character);
                  end if;

               when VSS.Characters.Latin.Latin_Capital_Letter_A
                  .. VSS.Characters.Latin.Latin_Capital_Letter_Z
               =>
                  case State is
                     when Leading_WS | Non_Zero =>
                        State := Some_Character;

                     when Some_Character =>
                        null;

                     when Trailing_WS =>
                        Encoding_Name.Clear;

                        return;
                  end case;

                  Encoding_Name.Append
                    (VSS.Characters.Get_Simple_Lowercase_Mapping (Character));

               when VSS.Characters.Latin.Latin_Small_Letter_A
                  .. VSS.Characters.Latin.Latin_Small_Letter_Z
               =>
                  case State is
                     when Leading_WS | Non_Zero =>
                        State := Some_Character;

                     when Some_Character =>
                        null;

                     when Trailing_WS =>
                        Encoding_Name.Clear;

                        return;
                  end case;

                  Encoding_Name.Append (Character);

               when others =>
                  Encoding_Name.Clear;

                  return;
            end case;
         end loop;
      end return;
   end To_Encoding_Name;

end VSS.Strings.Converters;
