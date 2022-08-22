--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with GNAT.String_Split;

package body GPR2.Builtin is

   -----------------
   -- Alternative --
   -----------------

   function Alternative
     (Value, Alternative_Value : Value_Type) return Value_Type is
   begin
      return (if Value = "" then "" else Alternative_Value);
   end Alternative;

   function Alternative
     (List, Alternative : Containers.Source_Value_List)
      return Containers.Source_Value_List is
   begin
      return (if List.Is_Empty
              then Containers.Source_Value_Type_List.Empty
              else Alternative);
   end Alternative;

   -------------
   -- Default --
   -------------

   function Default (Value, Default_Value : Value_Type) return Value_Type is
   begin
      return (if Value = "" then Default_Value else Value);
   end Default;

   function Default
     (List, Default : Containers.Source_Value_List)
      return Containers.Source_Value_List is
   begin
      return (if List.Is_Empty then Default else List);
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

   ----------------
   -- Filter_Out --
   ----------------

   function Filter_Out
     (List  : Containers.Source_Value_List;
      Regex : GNAT.Regexp.Regexp)
      return Containers.Source_Value_List
   is
      R : Containers.Source_Value_List;
   begin
      for E of List loop
         if not GNAT.Regexp.Match (E.Text, Regex) then
            R.Append (E);
         end if;
      end loop;

      return R;
   end Filter_Out;

   -------------
   -- Item_At --
   -------------

   function Item_At
     (List  : Containers.Source_Value_List;
      Index : Integer) return Value_Type
   is
      I : constant Positive :=
            (if Index > 0
             then Index
             else Positive (List.Length) + Index + 1);
   begin
      return List (I).Text;
   end Item_At;

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

   -------------------
   -- Remove_Prefix --
   -------------------

   function Remove_Prefix
     (Value, Pattern : Value_Type) return Value_Type is
   begin
      if Pattern'Length <= Value'Length
           and then
         Value (Value'First .. Value'First + Pattern'Length - 1) = Pattern
      then
         return Value (Value'First + Pattern'Length .. Value'Last);
      else
         return Value;
      end if;
   end Remove_Prefix;

   -------------------
   -- Remove_Suffix --
   -------------------

   function Remove_Suffix
     (Value, Pattern : Value_Type) return Value_Type is
   begin
      if Pattern'Length <= Value'Length
           and then
         Value (Value'Last - Pattern'Length + 1 .. Value'Last) = Pattern
      then
         return Value (Value'First .. Value'Last - Pattern'Length);
      else
         return Value;
      end if;
   end Remove_Suffix;

   -----------
   -- Upper --
   -----------

   function Upper
     (Value : Value_Type) return Value_Type is
   begin
      return Characters.Handling.To_Upper (String (Value));
   end Upper;

end GPR2.Builtin;
