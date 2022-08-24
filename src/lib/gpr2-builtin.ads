--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This package contains the implementation of all GPR built-ins like
--  External, External_As_List, Split. The built-in specs are following closely
--  the actual grammar so basically they take a set of string parameters and
--  should return a single string or a list of string.

with Ada.Characters.Handling;

with GNAT.Regpat;

with GPR2.Containers;
with GPR2.Context;
with GPR2.Source_Reference.Value;

package GPR2.Builtin is

   use Ada;

   use type GPR2.Containers.Source_Value_List;
   use type GPR2.Containers.Count_Type;

   function External
     (Context       : GPR2.Context.Object;
      Variable      : Name_Type;
      Default_Value : Source_Reference.Value.Object :=
                        Source_Reference.Value.Undefined;
      Sloc          : Source_Reference.Object :=
                        Source_Reference.Undefined)
      return Source_Reference.Value.Object
     with Post =>
       (if Context.Contains (Variable)
        then External'Result.Text = Context (Variable));
   --  The External built-in. Returns the value for Variable either in the
   --  context if found or the default value otherwise. If no default value
   --  is specified, the exception is raised.

   function External_As_List
     (Context   : GPR2.Context.Object;
      Variable  : Name_Type;
      Separator : Value_Not_Empty) return Containers.Value_List;
   --  The External_As_List built-in. Returns a list of values corresponding
   --  to the data found in context's Variable split using the given separator.

   function Split
     (Value     : Value_Type;
      Separator : Value_Not_Empty) return Containers.Value_List
     renames Containers.Create;
   --  The Split built-in. Returns a list of values corresponding
   --  to the string value split using the given separator.

   function Lower (Value : Value_Type) return Value_Type
     with Post => Lower'Result = Characters.Handling.To_Lower (Value);
   --  Lower-case Value

   function Upper (Value : Value_Type) return Value_Type
     with Post => Upper'Result = Characters.Handling.To_Upper (Value);
   --  Upper-case value

   function Default (Value, Default_Value : Value_Type) return Value_Type
     with Post => Default'Result
                    = (if Value = "" then Default_Value else Value);
   --  The default built-in, returns Default if Value is empty

   function Remove_Prefix
     (Value, Pattern : Value_Type) return Value_Type
     with Post => Remove_Prefix'Result'Length <= Value'Length;
   --  The Remove_Prefix built-in, returns Default if Value is empty

   function Remove_Suffix
     (Value, Pattern : Value_Type) return Value_Type
     with Post => Remove_Suffix'Result'Length <= Value'Length;
   --  The Remove_Suffix built-in, returns Default if Value is empty

   function Default
     (List, Default : Containers.Source_Value_List)
      return Containers.Source_Value_List
     with Post => Builtin.Default'Result
                    = (if List.Is_Empty then Default else List);
   --  The default built-in, returns Default if List is empty

   function Alternative
     (Value, Alternative_Value : Value_Type) return Value_Type
     with Post => Alternative'Result
                    = (if Value = "" then "" else Alternative_Value);
   --  The alternative built-in, returns Alternative is Value is not empty

   function Alternative
     (List, Alternative : Containers.Source_Value_List)
      return Containers.Source_Value_List
     with Post => Builtin.Alternative'Result
                    = (if List.Is_Empty
                       then Containers.Source_Value_Type_List.Empty
                       else Alternative);
   --  The default built-in, returns Default if List is empty

   function Item_At
     (List  : Containers.Source_Value_List;
      Index : Integer) return Value_Type
     with Pre  => Containers.Count_Type (abs Index) <= List.Length
                  and then Index /= 0,
          Post => List (if Index > 0
                        then Index
                        else Integer (List.Length) + Index + 1).Text =
                     Item_At'Result;
   --  Returns the value at position Index in List

   function Filter_Out
     (List  : Containers.Source_Value_List;
      Regex : GNAT.Regexp.Regexp)
      return Containers.Source_Value_List;
   --  Returns a list containing only item matching the regex

   function Match
     (Value, Pattern : Value_Type;
      Regex          : GNAT.Regpat.Pattern_Matcher;
      Replacement    : Value_Type) return Value_Type;

end GPR2.Builtin;
