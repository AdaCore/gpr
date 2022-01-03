------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2021-2022, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

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
      Attr_Index  : GPR2.Project.Attribute_Index.Object;
      Attr_Value  : GPR2.Project.Attribute.Object;
      Attr_Values : GPR2.Containers.Source_Value_List;
   begin
      if Index'Length > 0 then
         Attr_Index := GPR2.Project.Attribute_Index.Create
            (Value          => Index,
             Case_Sensitive => True);
      end if;
      Attr_Value := View.Attribute
         (Name   => GPR2."+" (GPR2.Optional_Name_Type (Name)),
          Pack   => GPR2."+" (GPR2.Optional_Name_Type (Pkg)),
          Index  => Attr_Index,
          At_Pos => At_Pos);
      A.Assert (Attr_Value.Is_Defined,
                "expect the attribute " & Name & " to be defined");
      if Attr_Value.Is_Defined then
         A.Assert (Attr_Value.Kind = PRA.List,
                   "expect a list value attribute");
         Attr_Values := Attr_Value.Values;
         A.Assert
           (Integer (Length (Attr_Values)), Integer (Length (Value)),
            "expect the proper number of items in the list");

         if Length (Attr_Values) = Length (Value) then
            for J in First_Index (Attr_Values) .. Last_Index (Attr_Values) loop
               A.Assert
                  (Element (Attr_Values, J).Text,
                   String (Element (Value, J)),
                   "comparing element" & J'Img);
            end loop;
         end if;
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
         (Name   => GPR2."+" (GPR2.Optional_Name_Type (Name)),
          Pack   => GPR2."+" (GPR2.Optional_Name_Type (Pkg)),
          Index  => Attr_Index,
          At_Pos => At_Pos);
      A.Assert (Attr_Value.Is_Defined,
                "expect the attribute '" & Name & "' to be defined");
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
         (Name   => GPR2."+" (GPR2.Optional_Name_Type (Name)),
          Pack   => GPR2."+" (GPR2.Optional_Name_Type (Pkg)),
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
         (Name   => GPR2."+" (GPR2.Optional_Name_Type (Name)),
          Pack   => GPR2."+" (GPR2.Optional_Name_Type (Pkg)),
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
      (Tree     : GPR2.Project.Tree.Object;
       View     : String;
       Variable : String;
       Value    : String;
       Aggregate_Context : Boolean := False)
   is
      V : GPR2.Project.View.Object;
      Context : GPR2.Context.Context_Kind := GPR2.Context.Root;
      use all type GPR2.Context.Context_Kind;
      use all type GPR2.Project.Name_Values.Value_Kind;
   begin
      if Aggregate_Context then
         Context := GPR2.Context.Aggregate;
      end if;
      IO.Put ("assess if " & View & "." & Variable & "=" & Value);
      if Context = GPR2.Context.Aggregate then
         IO.Put_Line (" (aggregate context)");
      else
         IO.New_Line;
      end if;
      A.Assert
         (Tree.Has_View_For
            (GPR2.Optional_Name_Type (View), Context),
         "tree should have view " & View);
      V := Tree.View_For (GPR2.Optional_Name_Type (View), Context);
      A.Assert (V.Has_Variables (Name => GPR2.Optional_Name_Type (Variable)),
                "view " & View & " should have variable " &
                Variable & " defined");

      declare
         Var : GPR2.Project.Variable.Object;
      begin
         Var := V.Variable (GPR2.Optional_Name_Type (Variable));
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
               (GPR2.Filename_Optional (Config_Filename)), "all");
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
