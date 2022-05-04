------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2022, AdaCore                      --
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

with GNAT.String_Split;

package body GPR2.Builtin is

   -----------------
   -- Alternative --
   -----------------

   function Alternative (Value, Alternative : Value_Type) return Value_Type is
   begin
      return (if Value = "" then "" else Alternative);
   end Alternative;

   -------------
   -- Default --
   -------------

   function Default (Value, Default : Value_Type) return Value_Type is
   begin
      return (if Value = "" then Default else Value);
   end Default;

   --------------
   -- External --
   --------------

   function External
     (Context       : GPR2.Context.Object;
      Variable      : Name_Type;
      Default_Value : Source_Reference.Value.Object :=
                        Source_Reference.Value.Undefined;
      Sloc          : Source_Reference.Object :=
                        Source_Reference.Undefined)
      return Source_Reference.Value.Object is
   begin
      if Context.Contains (Variable) then
         return Source_Reference.Value.Object
           (Source_Reference.Value.Create
              ((if Source_Reference.Object (Default_Value).Is_Defined
                then Source_Reference.Object (Default_Value)
                else Sloc),
               Context (Variable)));

      elsif Default_Value.Is_Defined then
         return Default_Value;

      else
         raise Project_Error
           with "undefined external reference """ & String (Variable) & '"';
      end if;
   end External;

   ----------------------
   -- External_As_List --
   ----------------------

   function External_As_List
     (Context   : GPR2.Context.Object;
      Variable  : Name_Type;
      Separator : Value_Not_Empty) return Containers.Value_List
   is
      use GNAT.String_Split;

      Result : Containers.Value_List;
   begin
      if Context.Contains (Variable) then
         declare
            Str    : constant String := String'(Context (Variable));
            Slices : Slice_Set;
         begin
            Create (Slices, Str, String (Separator), Mode => Single);

            for K in 1 .. Slice_Count (Slices) loop
               declare
                  Value : constant String := Slice (Slices, K);
               begin
                  --  We ingnore empty values at the start or at the end
                  if Value /= "" or else K not in 1 | Slice_Count (Slices) then
                     Result.Append (Value);
                  end if;
               end;
            end loop;
         end;
      end if;

      return Result;
   end External_As_List;

   -----------
   -- Lower --
   -----------

   function Lower
     (Value : Value_Type) return Value_Type is
   begin
      return Characters.Handling.To_Lower (String (Value));
   end Lower;

   -----------
   -- Match --
   -----------

   function Match
     (Value, Pattern : Value_Type;
      Regex          : GNAT.Regpat.Pattern_Matcher;
      Replacement    : Value_Type) return Value_Type
   is
      use GNAT;
      use type GNAT.Regpat.Match_Location;

      Matches : Regpat.Match_Array (0 .. Regpat.Paren_Count (Regex));
      R       : Unbounded_String;
      I       : Natural := Replacement'First;
      Found   : Boolean := False;
   begin
      Regpat.Match (Regex, Value, Matches);

      Found := Matches (0) /= Regpat.No_Match;

      if Found then
         if Replacement = "" then
            if Matches'Last = 1 then
               --  No replacement and a single match group, returns the
               --  matching pattern.
               return Value (Matches (1).First .. Matches (1).Last);
            else
               --  No replacement and no match group, just replace by the
               --  pattern.
               return Pattern;
            end if;

         else
            --  Check for replacement pattern \n and replace them by the
            --  corresponding matching group.

            while I <= Replacement'Last loop
               if Replacement (I) = '\'
                 and then I < Replacement'Last
                 and then Replacement (I + 1) in '0' .. '9'
               then
                  declare
                     P : constant Natural :=
                           Natural'Value (String'(1 => Replacement (I + 1)));
                  begin
                     if P <= Matches'Length
                          and then
                        Matches (P) /= Regpat.No_Match
                     then
                        Append
                          (R,
                           Value (Matches (P).First .. Matches (P).Last));
                     end if;
                  end;

                  I := I + 1;
               else
                  Append (R, Replacement (I));
               end if;

               I := I + 1;
            end loop;
         end if;
      end if;

      return To_String (R);
   end Match;

   -----------
   -- Upper --
   -----------

   function Upper
     (Value : Value_Type) return Value_Type is
   begin
      return Characters.Handling.To_Upper (String (Value));
   end Upper;

end GPR2.Builtin;
