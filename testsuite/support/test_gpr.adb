------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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
with GPR2.Project.Variable;
with GPR2.Project.View;
with GPR2.Project.Name_Values;
with Test_Assert;

package body Test_GPR is

   package A renames Test_Assert;
   package IO renames GNAT.IO;

   function Put_Tree_Messages
      (Tree : GPR2.Project.Tree.Object)
      return Integer;

   --------------------
   -- Asser_Variable --
   --------------------

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
      (Tree     : in out GPR2.Project.Tree.Object;
       Filename : String;
       Context  : GPR2.Context.Object := GPR2.Context.Empty)
   is
      Error_Message_Count : Integer := 0;
   begin
      begin
         Tree.Load
            (GPR2.Path_Name.Create_File (GPR2.Filename_Optional (Filename)),
             Context);
      exception
         when GPR2.Project_Error =>
            IO.Put_Line ("messages during project loading error" & Filename);
            Error_Message_Count := Put_Tree_Messages (Tree);
            A.Assert (Error_Message_Count > 0,
                      "expect at least 1 error message");
            raise GPR2.Project_Error;
      end;

      IO.Put_Line ("messages during project loading " & Filename);
      Error_Message_Count := Put_Tree_Messages (Tree);
      A.Assert (Error_Message_Count, 0,
                "expect no error messages");
   end Load_With_No_Errors;

end Test_GPR;
