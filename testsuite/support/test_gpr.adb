--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with GNAT.IO;
with GPR2;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Variable;
with GPR2.Project.Name_Values;
with GPR2.Project.Configuration;
with GPR2.Project.Attribute_Index;
with Ada.Containers;
with Test_Assert;

package body Test_GPR is

   package A renames Test_Assert;
   package IO renames GNAT.IO;
   package PRA renames GPR2.Project.Registry.Attribute;

   use type PRA.Value_Kind;
   use type Ada.Containers.Count_Type;

   function Put_Tree_Messages
      (Tree : GPR2.Project.Tree.Object)
      return Integer;

   ----------------------
   -- Assert_Attribute --
   ----------------------

   procedure Assert_Attribute
      (View                 : GPR2.Project.View.Object;
       Name                 : String;
       Pkg                  : String  := "";
       Index                : String  := "";
       Index_Case_Sensitive : Boolean := True;
       At_Pos               : GPR2.Unit_Index := GPR2.No_Index;
       Value                : GPR2.Containers.Name_List)
   is
      use GPR2.Containers.Source_Value_Type_List;
      use GPR2.Containers.Name_Type_List;
      use GPR2;
      Attr_Index  : GPR2.Project.Attribute_Index.Object;
      Attr_Value  : GPR2.Project.Attribute.Object;
      Attr_Values : GPR2.Containers.Source_Value_List;
      Q_Name      : constant GPR2.Q_Attribute_Id :=
                      (+Optional_Name_Type (Pkg),
                       +Name_Type (Name));
      PRA_Def     : constant GPR2.Project.Registry.Attribute.Def :=
                      GPR2.Project.Registry.Attribute.Get (Q_Name);
   begin
      if Index'Length > 0 then
         Attr_Index := GPR2.Project.Attribute_Index.Create
            (Value          => Index,
             Case_Sensitive => PRA.Is_Case_Sensitive
                                 (Index, PRA_Def.Index_Type));
      end if;

      Attr_Value := View.Attribute
         (Name   => Q_Name,
          Index  => Attr_Index,
          At_Pos => At_Pos);

      if not Attr_Value.Is_Defined then
         A.Assert (False,
                   "attribute " & Name & " is undefined");
         return;
      end if;

      if Attr_Value.Kind /= PRA.List then
         A.Assert (False,
                   "expect a list value attribute");
         return;
      end if;

      Attr_Values := Attr_Value.Values;

      if Length (Attr_Values) = Length (Value) then
         for J in First_Index (Attr_Values) .. Last_Index (Attr_Values) loop
            A.Assert
              (Element (Attr_Values, J).Text,
               String (Element (Value, J)),
               "comparing element" & J'Img);
         end loop;

      else
         A.Assert
           (Integer (Length (Attr_Values)), Integer (Length (Value)),
            "expect the proper number of items in the list");
         GNAT.IO.Put ("values are:");

         for J in First_Index (Attr_Values) .. Last_Index (Attr_Values) loop
            if J = First_Index (Attr_Values) then
               GNAT.IO.Put (" ");
            else
               GNAT.IO.Put (", ");
            end if;
            GNAT.IO.Put (String (Element (Attr_Values, J).Text));
         end loop;

         GNAT.IO.New_Line;
      end if;
   end Assert_Attribute;

   procedure Assert_Attribute
      (View                 : GPR2.Project.View.Object;
       Name                 : String;
       Pkg                  : String  := "";
       Index                : String  := "";
       Index_Case_Sensitive : Boolean := True;
       At_Pos               : GPR2.Unit_Index := GPR2.No_Index;
       Value                : String)
   is
      Attr_Index : GPR2.Project.Attribute_Index.Object;
      Attr_Value : GPR2.Project.Attribute.Object;
   begin
      if Index'Length > 0 then
         Attr_Index := GPR2.Project.Attribute_Index.Create
            (Value          => Index,
             Case_Sensitive => True);
      end if;
      Attr_Value := View.Attribute
         (Name   => (GPR2."+" (GPR2.Optional_Name_Type (Pkg)),
                     GPR2."+" (GPR2.Optional_Name_Type (Name))),
          Index  => Attr_Index,
          At_Pos => At_Pos);
      A.Assert (Attr_Value.Is_Defined,
                "expect the attribute '" & Name &
                  (if Index'Length > 0
                   then " (" & Index & ") "
                   else "") &
                  "' to be defined");
      if Attr_Value.Is_Defined then
         A.Assert (Attr_Value.Kind = PRA.Single,
                   "expect a single value attribute");
         A.Assert (Attr_Value.Value.Text, Value, "check value");
      end if;
   end Assert_Attribute;

   ----------------------------
   -- Assert_Attribute_Error --
   ----------------------------

   procedure Assert_Attribute_Error
      (View                 : GPR2.Project.View.Object;
       Name                 : String;
       Pkg                  : String  := "";
       Index                : String  := "";
       Index_Case_Sensitive : Boolean := True;
       At_Pos               : GPR2.Unit_Index := GPR2.No_Index)
   is
      Attr_Index : GPR2.Project.Attribute_Index.Object;
      Attr_Value : GPR2.Project.Attribute.Object;
   begin
      if Index'Length > 0 then
         Attr_Index := GPR2.Project.Attribute_Index.Create
            (Value          => Index,
             Case_Sensitive => True);
      end if;
      begin
         Attr_Value := View.Attribute
         (Name   => (GPR2."+" (GPR2.Optional_Name_Type (Pkg)),
                     GPR2."+" (GPR2.Optional_Name_Type (Name))),
          Index  => Attr_Index,
          At_Pos => At_Pos);
         A.Assert (False, "no exception raised");
      exception
         when GPR2.Attribute_Error =>
            A.Assert (True, "attribute_error raised");
      end;
   end Assert_Attribute_Error;

   ----------------------------------
   -- Assert_Attribute_Not_Defined --
   ----------------------------------

   procedure Assert_Attribute_Not_Defined
     (View                 : GPR2.Project.View.Object;
      Name                 : String;
      Pkg                  : String  := "";
      Index                : String  := "";
      Index_Case_Sensitive : Boolean := True;
       At_Pos               : GPR2.Unit_Index := GPR2.No_Index)
   is
      Attr_Index : GPR2.Project.Attribute_Index.Object;
      Attr_Value : GPR2.Project.Attribute.Object;
   begin
      if Index'Length > 0 then
         Attr_Index := GPR2.Project.Attribute_Index.Create
            (Value          => Index,
             Case_Sensitive => True);
      end if;
      Attr_Value := View.Attribute
         (Name   => (GPR2."+" (GPR2.Optional_Name_Type (Pkg)),
                     GPR2."+" (GPR2.Optional_Name_Type (Name))),
          Index  => Attr_Index,
          At_Pos => At_Pos);
      A.Assert
         (not Attr_Value.Is_Defined,
          "attribute '" & Name & "' should not be defined");
   end Assert_Attribute_Not_Defined;

   ---------------------
   -- Assert_Variable --
   ---------------------

   procedure Assert_Variable
      (View     : GPR2.Project.View.Object;
       Variable : String;
       Value    : String)
   is
      use all type GPR2.Context.Context_Kind;
      use all type GPR2.Project.Name_Values.Value_Kind;
   begin
      IO.Put ("assess if " & String (View.Name) & "." & Variable &
                "=" & Value);

      if View.Context = GPR2.Context.Aggregate then
         IO.Put_Line (" (aggregate context)");
      else
         IO.New_Line;
      end if;

      A.Assert (View.Has_Variables
                  (Name => GPR2.Optional_Name_Type (Variable)),
                "view " & String (View.Name) & " should have variable " &
                Variable & " defined");

      declare
         Var : GPR2.Project.Variable.Object;
      begin
         Var := View.Variable (GPR2.Optional_Name_Type (Variable));
         A.Assert (Var.Is_Defined, "expect the variable to be defined");
         IO.Put_Line (Variable & " (image): " & Var.Image);
         if Var.Kind = Single then
            A.Assert (Var.Value.Text, Value);
         else
            A.Assert (False, "variable " & Variable & " is of type list");
         end if;
      end;
   end Assert_Variable;

   -----------------------
   -- Put_Tree_Messages --
   -----------------------

   function Put_Tree_Messages
      (Tree : GPR2.Project.Tree.Object)
      return Integer
   is
      use all type GPR2.Message.Level_Value;
      Error_Message_Count : Integer := 0;
   begin
      if Tree.Has_Messages then
         for M of Tree.Log_Messages.all loop
            if M.Level = GPR2.Message.Error then
               Error_Message_Count := Error_Message_Count + 1;
            end if;
            IO.Put_Line (M.Format);
         end loop;
      end if;
      return Error_Message_Count;
   end Put_Tree_Messages;

   -------------------------
   -- Load_With_No_Errors --
   -------------------------

   procedure Load_With_No_Errors
      (Tree             : in out GPR2.Project.Tree.Object;
       Filename         : String;
       Context          : GPR2.Context.Object := GPR2.Context.Empty;
       Config_Filename  : String := "";
       Load_Source_List : Boolean := False)
   is
      Error_Message_Count : Integer := 0;
      Config_Project : GPR2.Project.Configuration.Object;
   begin
      if Config_Filename'Length > 0 then
         Config_Project := GPR2.Project.Configuration.Load
            (GPR2.Path_Name.Create_File
               (GPR2.Filename_Optional (Config_Filename)));
      end if;

      begin
         Tree.Load
            (GPR2.Path_Name.Create_File (GPR2.Filename_Optional (Filename)),
             Context,
             Config => Config_Project);
      exception
         when GPR2.Project_Error =>
            IO.Put_Line ("messages during project loading error" & Filename);
            Error_Message_Count := Put_Tree_Messages (Tree);
            A.Assert (Error_Message_Count > 0,
                      "got at least 1 error message");
            raise GPR2.Project_Error;
      end;

      IO.Put_Line ("messages during project loading " & Filename);
      Error_Message_Count := Put_Tree_Messages (Tree);
      A.Assert (Error_Message_Count, 0,
                "expect no error messages");
      if Load_Source_List then
         Tree.Update_Sources;
      end if;
   end Load_With_No_Errors;

end Test_GPR;
